library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
#library(curl)

create_well_tibble <- function(remove_code_cols = TRUE, remove_original_dates = TRUE, long_status_description = FALSE, remove_ba_contacts = TRUE) {
  # Takes the WellList.txt file provided by the Alberta Energy Regulator and converts it to a tidy tibble with all of the code columns
  # replaced with their full descriptions and other detailed information from the supplementary data files and documents.
  #
  # Args:
  #   remove_code_cols: Removes the original code columns from the final tibble so that their associated data columns will be returned.
  #   remove_original_dates: Removes the date columns containing the date in its original string format.
  #   long_status_description: Use the longer status descriptions provided by the AER. Ex; CRUDE OIL vs CR-OIL.
  #   remove_ba_contacts: Removes the contact information for each business associate such as address and phone number.
  #
  # Returns:
  #   A tibble in which all of the relevant information contained in extra tables and documents has been relationally joined to the
  #   official AER well list.

  # Column Names are sourced from the official layout doucument (https://www.aer.ca/documents/sts/St37-Listofwellslayout.pdf)
  # for the Alberta Energy Regulator WellList.txt file (https://www.aer.ca/documents/sts/ST37.zip)
  # The layout is only provided as a pdf and split across multpiple pages so I can't see a reasonable method to automate it's extraction.
  well_list_col_names <- c("UWI-DISPLAY-FORMAT",
                           "KEY-LIST-OF-WELLS",
                           "UPDATE-FLAG",
                           "WELL-NAME",
                           "FIELD-CODE",
                           "POOL-CODE",
                           "OS-AREA-CODE",
                           "OS-DEP-CODE",
                           "LICENSE-NO",
                           "LICENCE-STATUS",
                           "LICENSE-ISSUE-DATE",
                           "LICENSEE-CODE",
                           "AGENT-CODE",
                           "OPERATOR-CODE",
                           "FIN-DRL-DATE",
                           "WELL-TOTAL-DEPTH",
                           "WELL-STAT-CODE",
                           "WELL-STAT-DATE"
  )

  # Pre-defined col_types to prevent the yyyymmdd formatted dates from being turned into integers and to make sure WELL-TOTAL-DEPTH
  # is parsed as a double. These types will make it easier to transform the data into a more usable form later.
  well_list_col_types <- list(col_character(), # UWI-DISPLAY-FORMAT
                              col_character(), # KEY-LIST-OF-WELLS
                              col_character(), # UPDATE-FLAG
                              col_character(), # WELL-NAME
                              col_integer(),   # FIELD-CODE
                              col_integer(), # POOL-CODE
                              col_character(), # OS-AREA-CODE
                              col_character(), # OS-DEP-CODE
                              col_character(), # LICENSE-NO
                              col_character(), # LICENSE-STATUS
                              col_character(), # LICENSE-ISSUE-DATE
                              col_character(), # LICENSEE-CODE
                              col_character(), # AGENT-CODE
                              col_character(), # OPERATOR-CODE
                              col_character(), # FIN-DRL-DATE
                              col_double(),    # WELL-TOTAL-DEPTH
                              col_character(), # WELL-STAT-CODE
                              col_character()  # WELL-STAT-DATE
  )

  well_list <- read_tsv("WellList.txt", col_names = well_list_col_names, col_types = well_list_col_types)

  # UPDATE-FLAG is a defunct field that is no longer populated with data so it can be removed.
  well_list <- well_list %>% select(-`UPDATE-FLAG`)

  well_list <- add_field(well_list)
  well_list <- add_pool(well_list)
  well_list <- add_oilsands_area_deposit(well_list)
  well_list <- convert_aer_dates(well_list)
  well_list <- add_aer_status(well_list, long_status_description)
  well_list <- add_business_associates(well_list)
  well_list <- reorder_columns(well_list)

  if (remove_original_dates == TRUE) {
    well_list <- remove_original_date_columns(well_list)
  }

  if (remove_ba_contacts == TRUE) {
    well_list <- remove_ba_contact_columns(well_list)
  }

  if (remove_code_cols == TRUE) {
    well_list <- remove_code_columns(well_list)
  }

  return(well_list)
}

convert_aer_dates <- function(well_list) {
  # Converting the dates from simple yyyymmdd strings to real dates is easy with lubridate. Approximately 4000 records with final drill
  # dates of "00000000" have been converted to NAs. This doesn't appear to be an issue and converting these non-dates to NAs seems
  # reasonable.
  well_list <- well_list %>% rename(`ORIGINAL-LICENSE-ISSUE-DATE` = `LICENSE-ISSUE-DATE`)
  well_list <- well_list %>% mutate(`LICENSE-ISSUE-DATE` = ymd(`ORIGINAL-LICENSE-ISSUE-DATE`))
  well_list <- well_list %>% mutate(`FINAL-DRILL-DATE` = ymd(`FIN-DRL-DATE`))
  well_list <- well_list %>% mutate(`WELL-STATUS-DATE` = ymd(`WELL-STAT-DATE`))

  return(well_list)
}

add_aer_status <- function(well_list, long_status_description) {

  # These well status code tables are necessary to turn the "WELL-STAT-CODE" column into human readble statuses later.
  # The csv files were extracted from the above layout document using tabula (http://tabula.technology/)
  well_status_codes_fluid <- read_csv("well_status_codes_fluid.csv")
  well_status_codes_mode <- read_csv("well_status_codes_mode.csv")
  well_status_codes_type <- read_csv("well_status_codes_type.csv")
  well_status_codes_structure <- read_csv("well_status_codes_structure.csv")

  # Splitting the four components of the well status into their own columns makes it simple to left join the descriptive versions
  # to the table later.
  well_list <- well_list %>% separate(`WELL-STAT-CODE`, into = c("FLUID-CODE", "MODE-CODE", "TYPE-CODE", "STRUCTURE-CODE", "UNUSED-STATUS-CODE"), sep = c(2,4,6,8), convert = TRUE)

  # Using the left_join functions provided by dplyr, because they are similar to SQL that I'm familiar with.
  well_list <- well_list %>% left_join(well_status_codes_fluid %>% rename(FLUID = `Short Description`, `FLUID-LONG` = Description), c("FLUID-CODE" = "Value"))
  well_list <- well_list %>% left_join(well_status_codes_mode %>% rename(MODE = `Short Description`, `MODE-LONG` = Description), c("MODE-CODE" = "Value"))
  well_list <- well_list %>% left_join(well_status_codes_type %>% rename(TYPE = `Short Description`, `TYPE-LONG` = Description), c("TYPE-CODE" = "Value"))
  well_list <- well_list %>% left_join(well_status_codes_fluid %>% rename(STRUCTURE = `Short Description`, `STRUCTURE-LONG` = Description), c("STRUCTURE-CODE" = "Value"))

  # There is a fifth status code column that currently only contains the value "00" and appears to be unused. There should be no issues
  # with removing it.
  well_list <- well_list %>% select(-`UNUSED-STATUS-CODE`)

  if (long_status_description == TRUE) {
    well_list <- well_list %>% select(-FLUID, -MODE, -TYPE, -STRUCTURE) %>%
      rename(FLUID = `FLUID-LONG`, MODE = `MODE-LONG`, TYPE = `TYPE-LONG`, STRUCTURE = `STRUCTURE-LONG`)
  } else {
    well_list <- well_list %>% select(-`FLUID-LONG`, -`MODE-LONG`, -`TYPE-LONG`, -`STRUCTURE-LONG`)
  }

  return(well_list)
}


add_business_associates <- function(well_list) {

  # The Business Asscoiate codes are used to map Licensee, Operator, and Agent codes to proper business names and contact information
  # https://www.aer.ca/data-and-publications/statistical-reports/st104
  # The imported excel file contains some junk data due to a two row merged cell header, and a bunch of disclaimers at the bottom of
  # the file, I remove these by filtering the BA column code for a regex that matches any four character combination of alphanumerics
  business_associate_codes <- read_excel("BusinessAssociate_Codes.xlsx", skip = 2)
  business_associate_codes <- business_associate_codes %>% filter(str_detect(.$`BA Code`, "^....$"))

  # The well list from the AER comes with a five character long licensee, operator, and agent code whereas the business associate codes are
  # only five charaters long. The well list codes appear to always be padded with an extra zero on the end.
  well_list <- well_list %>% mutate(`LICENSEE-CODE` = str_sub(`LICENSEE-CODE`, 1, 4))
  well_list <- well_list %>% mutate(`AGENT-CODE` = str_sub(`AGENT-CODE`, 1, 4))
  well_list <- well_list %>% mutate(`OPERATOR-CODE` = str_sub(`OPERATOR-CODE`, 1, 4))

  well_list <- well_list %>% left_join(business_associate_codes %>% rename(LICENSEE = `Company Name`, `LICENSEE-ADDRESS` = Address, `LICENSEE-PHONE` = Phone), c("LICENSEE-CODE" = "BA Code"))
  well_list <- well_list %>% left_join(business_associate_codes %>% rename(AGENT = `Company Name`, `AGENT-ADDRESS` = Address, `AGENT-PHONE` = Phone), c("AGENT-CODE" = "BA Code"))
  well_list <- well_list %>% left_join(business_associate_codes %>% rename(OPERATOR = `Company Name`, `OPERATOR-ADDRESS` = Address, `OPERATOR-PHONE` = Phone), c("OPERATOR-CODE" = "BA Code"))

  return(well_list)
}

add_field <- function(well_list) {

  # The Field and pool code files are sourced in delimited format from https://www.aer.ca/data-and-publications/statistical-reports/st103
  # http://aer.ca/data/codes/FieldList.txt
  well_field_codes <- read_tsv("FieldList.txt")

  well_list <- well_list %>% left_join(
    well_field_codes %>% rename(FIELD = `Field Name`, `FIELD-ABBREV` = `Field Abbrev`, `FIELD-CENTRE` = `Field Centre`),
    c("FIELD-CODE" = "Field Code"))

  well_list <- well_list %>% select(-`FIELD-ABBREV`)

  return(well_list)
}

add_pool <- function(well_list) {

  # http://aer.ca/data/codes/FieldPoolList.txt
  well_pool_codes <- read_tsv("FieldPoolList.txt")

  # http://aer.ca/data/codes/CommingledPoolList.txt
  #well_commingled_pool_codes <- read_tsv("CommingledPoolList.txt")

  # I'm not entirely sure about the Pool Codes. There are 232 Production Pool Codes that occur in the well list that do no appear to occur
  # in the well_pool_codes table. I am also not entirely sure whether the production or geoloical pool code is the one that should be joined
  # to the well list pool code. For now I will leave out the pool data when constructing the complete well list. I suspect it is the
  # production pool and will go with that for now.
  well_list <- well_list %>% left_join(
    well_pool_codes %>% select(-`Field Name`, -`Geological Pool Code`, -`Geological Pool Name`) %>%
      distinct() %>% rename(`PRODUCTION-POOL` = `Production Pool Name`, CONFIDENTIAL = Confidential),
    c("FIELD-CODE" = "Field Code", "POOL-CODE" = "Production Pool Code"))

  return(well_list)
}

add_oilsands_area_deposit <- function(well_list) {

  # Oilsands area and deposit codes
  oilsands_area_codes <- read_csv("PRAOilSandsAreaCodes.csv")
  oilsands_deposit_codes <- read_csv("PRAOilSandsAreaDepositCodes.csv")

  # Oilsands area codes and deposit codes are mostly blank, but some of the well records have codes and values.
  well_list <- well_list %>% left_join(oilsands_area_codes %>% rename(`OILSANDS-AREA-NAME` = AreaName), c("OS-AREA-CODE" = "AreaCode"))
  well_list <- well_list %>% left_join(oilsands_deposit_codes %>% select(-AreaName) %>% rename(`OILSANDS-DEPOSIT-NAME` = DepositName), c("OS-AREA-CODE" = "AreaCode", "OS-DEP-CODE" = "DepositCode"))

  return(well_list)
}

reorder_columns <- function(well_list) {

  # Reorder the columns in well_list to match the order in the raw data file before all the joins occurred.
  well_list <- well_list %>% select(`UWI-DISPLAY-FORMAT`,
                                    `KEY-LIST-OF-WELLS`,
                                    `WELL-NAME`,
                                    `FIELD-CODE`,
                                    `FIELD`,
                                    `FIELD-CENTRE`,
                                    `POOL-CODE`,
                                    `PRODUCTION-POOL`,
                                    `CONFIDENTIAL`,
                                    `OS-AREA-CODE`,
                                    `OILSANDS-AREA-NAME`,
                                    `OS-DEP-CODE`,
                                    `OILSANDS-DEPOSIT-NAME`,
                                    `LICENSE-NO`,
                                    `LICENCE-STATUS`,
                                    `ORIGINAL-LICENSE-ISSUE-DATE`,
                                    `LICENSE-ISSUE-DATE`,
                                    `LICENSEE-CODE`,
                                    `LICENSEE`,
                                    `LICENSEE-ADDRESS`,
                                    `LICENSEE-PHONE`,
                                    `AGENT-CODE`,
                                    `AGENT`,
                                    `AGENT-ADDRESS`,
                                    `AGENT-PHONE`,
                                    `OPERATOR-CODE`,
                                    `OPERATOR`,
                                    `OPERATOR-ADDRESS`,
                                    `OPERATOR-PHONE`,
                                    `FIN-DRL-DATE`,
                                    `FINAL-DRILL-DATE`,
                                    `WELL-TOTAL-DEPTH`,
                                    `FLUID-CODE`,
                                    `MODE-CODE`,
                                    `TYPE-CODE`,
                                    `STRUCTURE-CODE`,
                                    `FLUID`,
                                    `MODE`,
                                    `TYPE`,
                                    `STRUCTURE`,
                                    `WELL-STAT-DATE`,
                                    `WELL-STATUS-DATE`)

  return(well_list)
}

remove_original_date_columns <- function(well_list) {
  well_list <- well_list %>% select(-`ORIGINAL-LICENSE-ISSUE-DATE`, -`FIN-DRL-DATE`, -`WELL-STAT-DATE`)

  return(well_list)
}

remove_ba_contact_columns <- function(well_list) {
  well_list <- well_list %>% select(-`LICENSEE-ADDRESS`, -`LICENSEE-PHONE`, -`AGENT-ADDRESS`, -`AGENT-PHONE`, -`OPERATOR-ADDRESS`, - `OPERATOR-PHONE`)

  return(well_list)
}

remove_code_columns <- function(well_list) {
  well_list <- well_list %>% select(-`FLUID-CODE`, -`MODE-CODE`, -`TYPE-CODE`, -`STRUCTURE-CODE`)
  well_list <- well_list %>% select(-`LICENSEE-CODE`, -`AGENT-CODE`, -`OPERATOR-CODE`)
  well_list <- well_list %>% select(-`FIELD-CODE`, -`POOL-CODE`, -`OS-AREA-CODE`, -`OS-DEP-CODE`)

  return(well_list)
}
