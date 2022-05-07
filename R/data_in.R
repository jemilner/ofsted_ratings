library(dplyr)
library(readr)
library(tidyr)
library(DataExplorer)

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
#any obvious reason for missing ofsted ratings?
# create_report(
#     data = info_tbl %>%
#         filter(is.na(OFSTEDRATING)),
#     output_file = "info_missing_ofsted.html",
#     output_dir = paste0(getwd(), "/explore")
# )

#just going to look at open secondary schools
secondary_tbl <- info_tbl %>%
    filter(SCHSTATUS == "Open") %>%
    filter(as.integer(AGELOW) == 11) %>%
    filter(as.integer(AGEHIGH) >= 16) %>%
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
        "LA",
        "ESTAB",
        "SCHOOLTYPE",
        "NOR",
        "PNORG",
        "PNORB",
        "PSENELSE",
        "PSENELK",
        "PNUMENGFL",
        "PNUMFSMEVER"
    )))

create_report(
    data = census_tbl,
    output_file = "census_full.html",
    output_dir = paste0(getwd(), "/explore")
)

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
    output_file = "funding_full.html",
    output_dir = paste0(getwd(), "/explore")
)

#FSMBAND completely missing
funding_tbl <- funding_tbl %>%
    select(-FSMBAND)

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
        "LA" = "LA Number",
        "ESTAB" = "Establishment Number",
        "PUPILTEACHERRATIO" = "Pupil:     Teacher Ratio"
    )))
