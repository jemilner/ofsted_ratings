library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(DataExplorer)
library(mice)

info_file <- "data/england_school_information.csv"
census_file <- "data/england_census.csv"
funding_file <- "data/england_cfr.csv"
workforce_file <- "data/england_swf.csv"

########################
## school information ##
########################
info_tbl <- read_csv(
    file = info_file,
    col_types = cols(.default = "c")
) %>%
    select(all_of(c(
        "URN",
        "SCHSTATUS",
        "MINORGROUP",
        "SCHOOLTYPE",
        "ISPRIMARY",
        "ISSECONDARY",
        "ISPOST16",
        "AGELOW",
        "AGEHIGH",
        "GENDER",
        "RELCHAR",
        "ADMPOL",
        "OFSTEDRATING"
    )))

#quick data explore 
create_report(
    data = info_tbl,
    output_file = "info.html",
    output_dir = paste0(getwd(), "/explore")
)

#just going to look at open secondary schools
secondary_tbl <- info_tbl %>%
    filter(SCHSTATUS == "Open") %>%
    filter(as.integer(AGELOW) == 11) %>%
    filter(as.integer(AGEHIGH) >= 16) %>%
    #filtered out special schools
    #(on the basis they will have a different set of requirements as to what makes a good school)
    filter(MINORGROUP != "Special school") %>%
    #also took out independent schools (are these private?)
    #they have different profile in the data anyway
    #so a stratified analysis might be more useful anyway
    filter(!grepl("independent", SCHOOLTYPE, ignore.case = TRUE)) %>%
    filter(!grepl("independent", MINORGROUP, ignore.case = TRUE)) %>%
    #took out niche school types
    filter(SCHOOLTYPE != "Service children's education") %>%
    filter(SCHOOLTYPE != "Studio schools") %>%
    filter(SCHOOLTYPE != "University technical college") %>%
    select(-c(
        "SCHSTATUS",
        "ISPRIMARY",
        "ISSECONDARY",
        "AGELOW",
        "AGEHIGH"
    ))

#quick data explore
create_report(
    data = secondary_tbl,
    output_file = "secondary_full.html",
    output_dir = paste0(getwd(), "/explore")
)
#see if any systematic reason for missing ofsted ratings
#(nothing major)
create_report(
    data = secondary_tbl %>%
        filter(is.na(OFSTEDRATING)),
    output_file = "secondary_missing_ofsted.html",
    output_dir = paste0(getwd(), "/explore")
)

#get secondary school ids to filter other data sets
secondary_ids <- secondary_tbl %>%
    pull(URN)

###################
## school census ##
###################
#mostly just want percent cols
census_tbl <- read_csv(
    file = census_file,
    col_types = cols(.default = "c")
) %>%
    select(all_of(c(
        "URN",
        "NOR",
        "PSENELSE",
        "PSENELK",
        "PNUMENGFL",
        "PNUMFSMEVER"
    ))) %>%
    filter(URN %in% secondary_ids)

#check for missing values
setNames(
    unlist(
        map(
            names(census_tbl),
            function(x) sum(is.na(census_tbl %>% pull(x)))
        )
    ),
    names(census_tbl)
)

#############
## funding ##
############
#not interested in historical columns
funding_tbl <- read_csv(
    file = funding_file,
    col_types = cols(.default = "c")
) %>%
    select(all_of(c(
        "URN",
        "PUPILS",
        "FSM",
        "FSMBAND",
        "GRANTFUNDING",
        "SELFGENERATEDINCOME",
        "TOTALINCOME",
        "TEACHINGSTAFF",
        "SUPPLYTEACHERS",
        "EDUCATIONSUPPORTSTAFF",
        "PREMISES",
        "BACKOFFICE",
        "CATERING",
        "OTHERSTAFF",
        "ENERGY",
        "LEARNINGRESOURCES",
        "ICT",
        "BOUGHTINPROFESSIONALSERVICES",
        "OTHER",
        "TOTALEXPENDITURE",
        "PGRANTFUNDING",
        "PSELFGENERATEDINCOME",
        "PTEACHINGSTAFF",
        "PSUPPLYTEACHERS",
        "PEDUCATIONSUPPORTSTAFF",
        "PPREMISES",
        "PBACKOFFICE",
        "PCATERING",
        "POTHERSTAFF",
        "PENERGY",
        "PLEARNINGRESOURCES",
        "PICT",
        "PBOUGHTINPROFESSIONALSERVICES",
        "POTHER"
    ))) %>%
    filter(URN %in% secondary_ids)

#secondary_ids contains > 3000 ids
#only ~600 of these are in the funding data
#so will ignore it all

###############
## workforce ##
###############
#only interested in relative cols
#but need to bring through some absolute cols to calc them
workforce_tbl <- read_csv(
    file = workforce_file,
    col_types = cols(.default = "c")
) %>%
    select(all_of(c(
        "URN",
        "PUPILTEACHERRATIO" = "Pupil:     Teacher Ratio",
        "TAFTE" = "Total Number of Teaching Assistants (Full-time Equivalent)",
        "SUPPORTFTE" = "Total Number of Non Classroom-based School Support Staff, Excluding Auxiliary Staff (Full-Time Equivalent)"
    ))) %>%
    filter(URN %in% secondary_ids) %>%
    #TODO add these back in later when I've thought about how to handle them
    #as it stands, they cause a div by 0 error when calculating ratios
    filter(as.numeric(TAFTE) > 0)

##########
## join ##
##########
joined_tbl <- secondary_tbl %>%
    left_join(
        census_tbl,
        by = c("URN" = "URN")
    ) %>%
    left_join(
        workforce_tbl,
        by = c("URN" = "URN")
    )

create_report(
    data = joined_tbl,
    output_file = "joined.html",
    output_dir = paste0(getwd(), "/explore")
)

####################
## data cleansing ##
####################
cleansed_tbl <- joined_tbl %>%
    #remove NA oftsed ratings
    filter(!is.na(OFSTEDRATING)) %>%
    #more staff ratios
    mutate(PUPILTARATIO = as.integer(NOR) / as.numeric(TAFTE)) %>%
    mutate(PUPILSUPPORTRATIO = as.integer(NOR) / as.numeric(SUPPORTFTE)) %>%
    select(-c(TAFTE, SUPPORTFTE)) %>%
    #reduce gender to mixed or not
    mutate(MIXED = ifelse(GENDER == "Mixed", 1, 0)) %>%
    select(-GENDER) %>%
    #assumption: ADMPOL: 'not applicable' can be wrapped up in 'non-selective'
    mutate(ADMPOL_SELECTIVE = case_when(
        ADMPOL == "Not applicable" ~ 0,
        ADMPOL == "Non-selective" ~ 0,
        is.na(ADMPOL) ~ as.numeric(NA),
        TRUE ~ 1
    )) %>%
    select(-ADMPOL) %>%
    #assumption: RELCHAR: 'does not apply' can be wrapped up in 'none'
    mutate(RELCHAR_YES = case_when(
        RELCHAR == "Does not apply" ~ 0,
        RELCHAR == "None" ~ 0,
        is.na(RELCHAR) ~ as.numeric(NA),
        TRUE ~ 1
    )) %>%
    select(-RELCHAR) %>%
    #make minorgroup binary
    mutate(MINORGROUP_ACADEMY = case_when(
        MINORGROUP == "Academy" ~ 1,
        TRUE ~ 0
    )) %>%
    select(-MINORGROUP) %>%
    #column formatting
    mutate(ISPOST16 = as.integer(ISPOST16)) %>%
    mutate(NOR = as.integer(NOR)) %>%
    mutate(PSENELSE = as.numeric(str_replace(PSENELSE, "%", ""))) %>%
    mutate(PSENELK = as.numeric(str_replace(PSENELK, "%", ""))) %>%
    mutate(PNUMENGFL = as.numeric(str_replace(PNUMENGFL, "%", ""))) %>%
    mutate(PNUMFSMEVER = as.numeric(str_replace(PNUMFSMEVER, "%", ""))) %>%
    mutate(PUPILTEACHERRATIO = as.numeric(PUPILTEACHERRATIO)) %>%
    #reduce outliers effects
    mutate(PUPILTARATIOLOG = log10(PUPILTARATIO)) %>%
    mutate(PUPILSUPPORTRATIOLOG = log10(PUPILSUPPORTRATIO)) %>%
    mutate(SCHOOLTYPE = as.factor(SCHOOLTYPE)) %>%
    mutate(OUTSTANDING = ifelse(OFSTEDRATING == "Outstanding", 1, 0)) %>%
    select(-OFSTEDRATING)

create_report(
    data = cleansed_tbl,
    output_file = "cleansed.html",
    output_dir = paste0(getwd(), "/explore")
)

################
## imputation ##
################
#setup mice model vector
#default to pmm
multi_imp_method <- setNames(
    rep("pmm", ncol(cleansed_tbl)),
    names(cleansed_tbl)
)

#ignore
ignore_vars <- c(
    "URN",
    "OUTSTANDING"
)
#unordered categorical variables
# polyreg_vars <- c("RELCHAR_YES", "ADMPOL_SELECTIVE")

multi_imp_method[ignore_vars] <- ""
# multi_imp_method[polyreg_vars] <- "polyreg"

#setup mice matrix
multi_imp_mat <- diag(
    ncol(cleansed_tbl)
)
colnames(multi_imp_mat) <- rownames(multi_imp_mat) <- names(cleansed_tbl)

multi_imp_mat[, ignore_vars] <- 0
multi_imp_mat[, names(cleansed_tbl)[!names(cleansed_tbl) %in% ignore_vars]] <- 1

diag(multi_imp_mat) <- 0

#run imputation
num_imputations <- 5
imputations <- mice(
    cleansed_tbl,
    m = num_imputations, 
    predictorMatrix = multi_imp_mat, 
    method = multi_imp_method#,
    # print = FALSE
)

create_report(
    data = mice::complete(imputations, num_imputations),
    output_file = "imputated.html",
    output_dir = paste0(getwd(), "/explore")
)

saveRDS(imputations, "data/imputed_data.rds")
