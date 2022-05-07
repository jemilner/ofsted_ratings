library(dplyr)
library(readr)
library(tidyr)
library(DataExplorer)

school_info_file <- "data/england_school_information.csv"
school_census_file <- "data/england_census.csv"
school_funding_file <- "data/england_cfr.csv"
school_workforce_file <- "data/england_swf.csv"

########################
## school information ##
########################
school_info_tbl <- read_csv(
    file = school_info_file,
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
#     data = school_info_tbl,
#     output_file = "school_info_full.html",
#     output_dir = paste0(getwd(), "/explore")
# )
#any obvious reason for missing ofsted ratings?
# create_report(
#     data = school_info_tbl %>%
#         filter(is.na(OFSTEDRATING)),
#     output_file = "school_info_missing_ofsted.html",
#     output_dir = paste0(getwd(), "/explore")
# )

#just going to look at open secondary schools
secondary_tbl <- school_info_tbl %>%
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
school_census_tbl <- read_csv(
    file = school_census_file,
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
    data = school_census_tbl,
    output_file = "census_full.html",
    output_dir = paste0(getwd(), "/explore")
)


