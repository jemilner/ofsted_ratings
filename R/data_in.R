library(dplyr)
library(readr)
library(tidyr)
library(stringr)
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
        "LANAME",
        "LA",
        "ESTAB",
        "POSTCODE",
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
        "OFSTEDRATING",
        "OFSTEDLASTINSP"
    )))

#quick data explore 
# create_report(
#     data = info_tbl,
#     output_file = "info_full.html",
#     output_dir = paste0(getwd(), "/explore")
# )

#just going to look at open secondary schools
#filtered out special schools (they will have a different set of requirements as to what makes a good school)
#also took out other independent schools (private?) - different profile in the data anyway
secondary_tbl <- info_tbl %>%
    filter(SCHSTATUS == "Open") %>%
    filter(as.integer(AGELOW) == 11) %>%
    filter(as.integer(AGEHIGH) >= 16) %>%
    filter(MINORGROUP != "Special school") %>%
    filter(SCHOOLTYPE != "Other independent school") %>%
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
#see if any obvious reason for missing ofsted ratings?
create_report(
    data = secondary_tbl %>%
        filter(is.na(OFSTEDRATING)),
    output_file = "secondary_missing_ofsted.html",
    output_dir = paste0(getwd(), "/explore")
)

#nothing major aside from MINORGROUP is a bit different
secondary_tbl %>%
    pull(MINORGROUP) %>%
    table_pec()
secondary_tbl %>% 
    filter(is.na(OFSTEDRATING)) %>%
    pull(MINORGROUP) %>%
    table_pec()

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
        "PNORG", #don't need boy equivalent
        "PSENELSE",
        "PSENELK",
        "PNUMENGFL",
        "PNUMFSMEVER"
    )))

# create_report(
#     data = census_tbl,
#     output_file = "census.html",
#     output_dir = paste0(getwd(), "/explore")
# )

#############
## funding ##
############
#dont want 
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
    )))

create_report(
    data = funding_tbl,
    output_file = "funding.html",
    output_dir = paste0(getwd(), "/explore")
)

#FSMBAND completely missing
funding_tbl <- funding_tbl %>%
    select(-FSMBAND)

#funding_tbl has a duplicate row - remove it
funding_tbl <- funding_tbl %>%
    distinct()

###############
## workforce ##
###############
#avoid absolute cols
#e.g., number of teachers as it depends on school size
workforce_tbl <- read_csv(
    file = workforce_file,
    col_types = cols(.default = "c")
) %>%
    select(all_of(c(
        "URN",
        "PUPILTEACHERRATIO" = "Pupil:     Teacher Ratio"
    ))) %>%
    filter(!is.na(URN))

##########
## join ##
##########
joined_tbl <- secondary_tbl %>%
    left_join(
        census_tbl,
        by = c("URN" = "URN")
    ) %>%
    left_join(
        funding_tbl,
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

joined_tbl <- joined_tbl %>%
    #most the funding tbl is missing for secondary schools
    select(-c(
        "PUPILS",
        "FSM",
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
    )) %>%
    #removes rows without an ofsted rating
    filter(!is.na(OFSTEDRATING))

create_report(
    data = joined_tbl,
    output_file = "joined2.html",
    output_dir = paste0(getwd(), "/explore")
)

####################
## data cleansing ##
####################
cleansed_tbl <- joined_tbl %>%
    mutate(ISPOST16 = as.integer(ISPOST16)) %>%
    mutate(NOR = as.integer(NOR)) %>%
    mutate(PNORG = as.numeric(str_replace(PNORG, "%", ""))) %>%
    mutate(PSENELSE = as.numeric(str_replace(PSENELSE, "%", ""))) %>%
    mutate(PSENELK = as.numeric(str_replace(PSENELK, "%", ""))) %>%
    mutate(PNUMENGFL = as.numeric(str_replace(PNUMENGFL, "%", ""))) %>%
    mutate(PNUMFSMEVER = as.numeric(str_replace(PNUMFSMEVER, "%", ""))) %>%
    mutate(PUPILTEACHERRATIO = as.numeric(PUPILTEACHERRATIO)) %>%
    mutate(LANAME = as.factor(LANAME)) %>%
    mutate(MINORGROUP = as.factor(MINORGROUP)) %>%
    mutate(SCHOOLTYPE = as.factor(SCHOOLTYPE)) %>%
    mutate(GENDER = as.factor(GENDER)) %>%
    mutate(RELCHAR = as.factor(RELCHAR)) %>%
    mutate(ADMPOL = as.factor(ADMPOL)) %>%
    mutate(outstanding = ifelse(OFSTEDRATING == "Outstanding", 1, 0)) %>%
    mutate(good = ifelse(OFSTEDRATING == "Good", 1, 0)) %>%
    mutate(req_improve = ifelse(OFSTEDRATING == "Requires improvement", 1, 0)) %>%
    mutate(serious_weakness = ifelse(OFSTEDRATING == "Serious Weaknesses", 1, 0)) %>%
    mutate(special_measures = ifelse(OFSTEDRATING == "Special Measures", 1, 0))

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
    "LANAME",
    "LA",
    "ESTAB",
    "POSTCODE",
    "OFSTEDRATING",
    "OFSTEDLASTINSP"
)
#unordered categorical variables
polyreg_vars <- c("RELCHAR", "ADMPOL")

multi_imp_method[ignore_vars] <- ""
multi_imp_method[polyreg_vars] <- "polyreg"

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