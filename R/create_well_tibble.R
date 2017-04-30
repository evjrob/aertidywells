#' Create a tibble conatining the official AER well list data.
#'
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @param remove_code_cols A Boolean. Should the original code columns be
#'   removed from the final tibble?
#' @param remove_original_dates A Boolean. Should the the columns containing the
#'   original dates as character vectors be removed?.
#' @param long_status_description A Boolean. Should the longer status
#'   descriptions provided by the AER be used?
#' @param remove_ba_contacts A Boolean. Should the additional business associate
#'   contact info such as address and phone number be removed?
#' @return A tibble of the well list data with all the supporting data joined to
#'   the appropriate records.
#' @examples
#' create_well_tibble()
#' create_well_tibble(data_dir = "some/directory/", remove_code_cols = FALSE, remove_original_dates = FALSE, long_status_description = TRUE, remove_ba_contacts = FALSE)
#' @export
create_well_tibble <- function(data_dir = "data/", remove_code_cols = TRUE, remove_original_dates = TRUE,
                               long_status_description = FALSE, remove_ba_contacts = TRUE) {

  # There will be too many issues building the tibble if any of the data files are missing
  missing_files <- !found_required_files(data_dir)
  if(any(missing_files)) {
    prompt_user_to_download(data_dir)
    stop(stringr::str_c("The required data files we're not found in the specified directory: ", data_dir))
  }

  # Column Names are sourced from the official layout doucument
  # (https://www.aer.ca/documents/sts/St37-Listofwellslayout.pdf) for the
  # Alberta Energy Regulator WellList.txt file
  # (https://www.aer.ca/documents/sts/ST37.zip) The layout is only provided as a
  # pdf and split across multpiple pages so I can't see a reasonable method to
  # automate it's extraction.
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

  # Pre-defined col_types to prevent the yyyymmdd formatted dates from being
  # turned into integers and to make sure WELL-TOTAL-DEPTH is parsed as a
  # double. These types will make it easier to transform the data into a more
  # usable form later.
  well_list_col_types <- list(readr::col_character(), # UWI-DISPLAY-FORMAT
                              readr::col_character(), # KEY-LIST-OF-WELLS
                              readr::col_character(), # UPDATE-FLAG
                              readr::col_character(), # WELL-NAME
                              readr::col_integer(),   # FIELD-CODE
                              readr::col_integer(), # POOL-CODE
                              readr::col_character(), # OS-AREA-CODE
                              readr::col_character(), # OS-DEP-CODE
                              readr::col_character(), # LICENSE-NO
                              readr::col_character(), # LICENSE-STATUS
                              readr::col_character(), # LICENSE-ISSUE-DATE
                              readr::col_character(), # LICENSEE-CODE
                              readr::col_character(), # AGENT-CODE
                              readr::col_character(), # OPERATOR-CODE
                              readr::col_character(), # FIN-DRL-DATE
                              readr::col_double(),    # WELL-TOTAL-DEPTH
                              readr::col_character(), # WELL-STAT-CODE
                              readr::col_character()  # WELL-STAT-DATE
  )

  well_list <- readr::read_tsv(stringr::str_c(data_dir,"/WellList.txt"), col_names = well_list_col_names, col_types = well_list_col_types)

  # UPDATE-FLAG is a defunct field that is no longer populated with data so it
  # can be removed.
  well_list <- well_list %>% dplyr::select(-`UPDATE-FLAG`)

  well_list <- add_field(well_list, data_dir)
  well_list <- add_pool(well_list, data_dir)
  well_list <- add_oilsands_area_deposit(well_list, data_dir)
  well_list <- convert_aer_dates(well_list)
  well_list <- add_aer_status(well_list, data_dir, long_status_description)
  well_list <- add_business_associates(well_list, data_dir)
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

#' A helper function for create_well_tibble(). Converts the provided date strings to real dates
#'
#' @param well_list A tibble containing the well_list with unconverted dates
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return The well tibble with additional columns containing the properly converted dates
#' @keywords internal
#' @noRd
convert_aer_dates <- function(well_list) {
  # Converting the dates from simple yyyymmdd strings to real dates is easy with
  # lubridate. Approximately 4000 records with final drill dates of "00000000"
  # have been converted to NAs. This doesn't appear to be an issue and
  # converting these non-dates to NAs seems reasonable.
  well_list <- well_list %>% dplyr::rename(`ORIGINAL-LICENSE-ISSUE-DATE` = `LICENSE-ISSUE-DATE`)
  well_list <- well_list %>% dplyr::mutate(`LICENSE-ISSUE-DATE` = lubridate::ymd(`ORIGINAL-LICENSE-ISSUE-DATE`))
  well_list <- well_list %>% dplyr::mutate(`FINAL-DRILL-DATE` = lubridate::ymd(`FIN-DRL-DATE`, quiet = TRUE))
  well_list <- well_list %>% dplyr::mutate(`WELL-STATUS-DATE` = lubridate::ymd(`WELL-STAT-DATE`))

  return(well_list)
}

#' A helper function for create_well_tibble(). Left joins the status data to the tibble.
#'
#' @param well_list A tibble containing the well_list with status codes
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @param long_status_description A Boolean. Should the longer status
#'   descriptions provided by the AER be used?
#' @return The well tibble with additional columns containing the components of
#'   the well status
#' @keywords internal
#' @noRd
add_aer_status <- function(well_list, data_dir, long_status_description) {

  # Splitting the four components of the well status into their own columns
  # makes it simple to left join the descriptive versions to the table later.
  well_list <- well_list %>% tidyr::separate(`WELL-STAT-CODE`, into = c("FLUID-CODE", "MODE-CODE", "TYPE-CODE", "STRUCTURE-CODE", "UNUSED-STATUS-CODE"), sep = c(2,4,6,8), convert = TRUE)

  # Using the left_join functions provided by dplyr, because they are similar to
  # SQL that I'm familiar with.
  well_list <- well_list %>% dplyr::left_join(well_status_codes_fluid %>% dplyr::rename(FLUID = `Short Description`, `FLUID-LONG` = Description), c("FLUID-CODE" = "Value"))
  well_list <- well_list %>% dplyr::left_join(well_status_codes_mode %>% dplyr::rename(MODE = `Short Description`, `MODE-LONG` = Description), c("MODE-CODE" = "Value"))
  well_list <- well_list %>% dplyr::left_join(well_status_codes_type %>% dplyr::rename(TYPE = `Short Description`, `TYPE-LONG` = Description), c("TYPE-CODE" = "Value"))
  well_list <- well_list %>% dplyr::left_join(well_status_codes_fluid %>% dplyr::rename(STRUCTURE = `Short Description`, `STRUCTURE-LONG` = Description), c("STRUCTURE-CODE" = "Value"))

  # There is a fifth status code column that currently only contains the value
  # "00" and appears to be unused. There should be no issues with removing it.
  well_list <- well_list %>% dplyr::select(-`UNUSED-STATUS-CODE`)

  if (long_status_description == TRUE) {
    well_list <- well_list %>% dplyr::select(-FLUID, -MODE, -TYPE, -STRUCTURE) %>%
      dplyr::rename(FLUID = `FLUID-LONG`, MODE = `MODE-LONG`, TYPE = `TYPE-LONG`, STRUCTURE = `STRUCTURE-LONG`)
  } else {
    well_list <- well_list %>% dplyr::select(-`FLUID-LONG`, -`MODE-LONG`, -`TYPE-LONG`, -`STRUCTURE-LONG`)
  }

  return(well_list)
}

#' A helper function for create_well_tibble(). Left joins the business associate data to the tibble.
#'
#' @param well_list A tibble containing the well_list with business associate codes
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return The well tibble with additional columns containing the licensee,
#'   agent, and operator data.
#' @keywords internal
#' @noRd
add_business_associates <- function(well_list, data_dir) {

  # The Business Asscoiate codes are used to map Licensee, Operator, and Agent
  # codes to proper business names and contact information
  # https://www.aer.ca/data-and-publications/statistical-reports/st104 The
  # imported excel file contains some junk data due to a two row merged cell
  # header, and a bunch of disclaimers at the bottom of the file, I remove these
  # by filtering the BA column code for a regex that matches any four character
  # combination of alphanumerics
  business_associate_codes <- readxl::read_excel(stringr::str_c(data_dir,"BusinessAssociate_Codes.xlsx"), skip = 2)
  business_associate_codes <- business_associate_codes %>% dplyr::filter(stringr::str_detect(.$`BA Code`, "^....$"))

  # The well list from the AER comes with a five character long licensee,
  # operator, and agent code whereas the business associate codes are only five
  # charaters long. The well list codes appear to always be padded with an extra
  # zero on the end.
  well_list <- well_list %>% dplyr::mutate(`LICENSEE-CODE` = stringr::str_sub(`LICENSEE-CODE`, 1, 4))
  well_list <- well_list %>% dplyr::mutate(`AGENT-CODE` = stringr::str_sub(`AGENT-CODE`, 1, 4))
  well_list <- well_list %>% dplyr::mutate(`OPERATOR-CODE` = stringr::str_sub(`OPERATOR-CODE`, 1, 4))

  well_list <- well_list %>% dplyr::left_join(business_associate_codes %>% dplyr::rename(LICENSEE = `Company Name`, `LICENSEE-ADDRESS` = Address, `LICENSEE-PHONE` = Phone), c("LICENSEE-CODE" = "BA Code"))
  well_list <- well_list %>% dplyr::left_join(business_associate_codes %>% dplyr::rename(AGENT = `Company Name`, `AGENT-ADDRESS` = Address, `AGENT-PHONE` = Phone), c("AGENT-CODE" = "BA Code"))
  well_list <- well_list %>% dplyr::left_join(business_associate_codes %>% dplyr::rename(OPERATOR = `Company Name`, `OPERATOR-ADDRESS` = Address, `OPERATOR-PHONE` = Phone), c("OPERATOR-CODE" = "BA Code"))

  return(well_list)
}

#' A helper function for create_well_tibble(). Left joins the field data to the tibble.
#'
#' @param well_list A tibble containing the well_list with field codes
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return The well tibble with additional columns containing the field and field centre
#' @keywords internal
#' @noRd
add_field <- function(well_list, data_dir) {

  # Defining the column types ensures the file gets parsed correctly
  field_col_types <- list(readr::col_integer(),   # Field Code
                          readr::col_character(), # Field Name
                          readr::col_character(), # Field Abbrev
                          readr::col_character()  # Field Centre
  )

  # The Field and pool code files are sourced in delimited format from
  # https://www.aer.ca/data-and-publications/statistical-reports/st103
  # http://aer.ca/data/codes/FieldList.txt
  well_field_codes <- readr::read_tsv(stringr::str_c(data_dir,"FieldList.txt"), col_types = field_col_types)

  well_list <- well_list %>% dplyr::left_join(
    well_field_codes %>% dplyr::rename(FIELD = `Field Name`, `FIELD-ABBREV` = `Field Abbrev`, `FIELD-CENTRE` = `Field Centre`),
    c("FIELD-CODE" = "Field Code"))

  well_list <- well_list %>% dplyr::select(-`FIELD-ABBREV`)

  return(well_list)
}

#' A helper function for create_well_tibble(). Left joins the pool data to the tibble.
#'
#' @param well_list A tibble containing the well_list with field codes
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return The well tibble with additional columns containing the pool and confidential flag
#' @keywords internal
#' @noRd
add_pool <- function(well_list, data_dir) {

  # Defining the column types ensures the file gets parsed correctly
  pool_col_types <- list( readr::col_character(), # Field Name
                          readr::col_character(), # Production Pool Name
                          readr::col_integer(),   # Field Code
                          readr::col_integer(),   # Production Pool Code
                          readr::col_integer(),   # Geological Pool Code
                          readr::col_character(), # Geological Pool Name
                          readr::col_character()  # Confidential
  )

  # http://aer.ca/data/codes/FieldPoolList.txt
  well_pool_codes <- readr::read_tsv(stringr::str_c(data_dir,"FieldPoolList.txt"), col_types = pool_col_types)

  # http://aer.ca/data/codes/CommingledPoolList.txt
  #well_commingled_pool_codes <- read_tsv(stringr::str_c(data_dir,"CommingledPoolList.txt"))

  # I'm not entirely sure about the Pool Codes. There are 232 Production Pool
  # Codes that occur in the well list that do no appear to occur in the
  # well_pool_codes table. I am also not entirely sure whether the production or
  # geoloical pool code is the one that should be joined to the well list pool
  # code. For now I will leave out the pool data when constructing the complete
  # well list. I suspect it is the production pool and will go with that for
  # now.
  well_list <- well_list %>% dplyr::left_join(
    well_pool_codes %>% dplyr::select(-`Field Name`, -`Geological Pool Code`, -`Geological Pool Name`) %>%
      dplyr::distinct() %>% dplyr::rename(`PRODUCTION-POOL` = `Production Pool Name`, CONFIDENTIAL = Confidential),
    c("FIELD-CODE" = "Field Code", "POOL-CODE" = "Production Pool Code"))

  return(well_list)
}


#' A helper function for create_well_tibble(). Left joins the oilsands area and deposit data to the tibble.
#'
#' @param well_list A tibble containing the well_list with os area and deposit codes
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return The well tibble with additional columns containing the oilsands area and deposit
#' @keywords internal
#' @noRd
add_oilsands_area_deposit <- function(well_list, data_dir) {

  # Defining the column types ensures the file gets parsed correctly
  os_area_col_types <- list(readr::col_character(), # AreaCode
                            readr::col_character()  # AreaName
  )

  os_dep_col_types <- list(readr::col_character(), # AreaName
                           readr::col_character(), # DepositName
                           readr::col_character(), # AreaCode
                           readr::col_character()  # DepositCode
  )

  # Oilsands area and deposit codes
  oilsands_area_codes <- readr::read_csv(stringr::str_c(data_dir,"PRAOilSandsAreaCodes.csv"), col_types = os_area_col_types)
  oilsands_deposit_codes <- readr::read_csv(stringr::str_c(data_dir,"PRAOilSandsAreaDepositCodes.csv"), col_types = os_dep_col_types)

  # Oilsands area codes and deposit codes are mostly blank, but some of the well records have codes and values.
  well_list <- well_list %>% dplyr::left_join(oilsands_area_codes %>% dplyr::rename(`OILSANDS-AREA-NAME` = AreaName), c("OS-AREA-CODE" = "AreaCode"))
  well_list <- well_list %>% dplyr::left_join(oilsands_deposit_codes %>% dplyr::select(-AreaName) %>% dplyr::rename(`OILSANDS-DEPOSIT-NAME` = DepositName), c("OS-AREA-CODE" = "AreaCode", "OS-DEP-CODE" = "DepositCode"))

  return(well_list)
}

#' A helper function for create_well_tibble(). Reorder the columns in the AER well tibble to match the order provided in WellList.txt
#'
#' @param well_list A tibble containing the well_list with field codes
#' @return The well tibble with columns that follow the orginal order
#' @keywords internal
#' @noRd
reorder_columns <- function(well_list) {

  # Reorder the columns in well_list to match the order in the raw data file
  # before all the joins occurred.
  well_list <- well_list %>% dplyr::select(`UWI-DISPLAY-FORMAT`,
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

#' A helper function for create_well_tibble(). Remove the unconverted original date columns.
#'
#' @param well_list A tibble containing the expanded well_list
#' @return The well tibble with original date columns removed
#' @keywords internal
#' @noRd
remove_original_date_columns <- function(well_list) {
  well_list <- well_list %>% dplyr::select(-`ORIGINAL-LICENSE-ISSUE-DATE`, -`FIN-DRL-DATE`, -`WELL-STAT-DATE`)

  return(well_list)
}

#' A helper function for create_well_tibble(). Remove the business associate contact columns.
#'
#' @param well_list A tibble containing the expanded well_list
#' @return The well tibble with business associate contact information removed
#' @keywords internal
#' @noRd
remove_ba_contact_columns <- function(well_list) {
  well_list <- well_list %>% dplyr::select(-`LICENSEE-ADDRESS`, -`LICENSEE-PHONE`, -`AGENT-ADDRESS`, -`AGENT-PHONE`, -`OPERATOR-ADDRESS`, - `OPERATOR-PHONE`)

  return(well_list)
}

#' A helper function for create_well_tibble(). Remove the code columns used to join status, field, pool etc.
#'
#' @param well_list A tibble containing the expanded well_list
#' @return The well tibble with all code columns removed.
#' @keywords internal
#' @noRd
remove_code_columns <- function(well_list) {
  well_list <- well_list %>% dplyr::select(-`FLUID-CODE`, -`MODE-CODE`, -`TYPE-CODE`, -`STRUCTURE-CODE`)
  well_list <- well_list %>% dplyr::select(-`LICENSEE-CODE`, -`AGENT-CODE`, -`OPERATOR-CODE`)
  well_list <- well_list %>% dplyr::select(-`FIELD-CODE`, -`POOL-CODE`, -`OS-AREA-CODE`, -`OS-DEP-CODE`)

  return(well_list)
}
