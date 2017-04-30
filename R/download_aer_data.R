data_file_urls = c(well_list = "https://www.aer.ca/documents/sts/ST37.zip",
                   pool_codes = "http://aer.ca/data/codes/FieldPoolList.txt",
                   field_codes = "http://aer.ca/data/codes/FieldList.txt",
                   oil_sands_area_codes = "https://www.petrinex.gov.ab.ca/bbreports/PRAOilSandsAreaCodes.csv",
                   oil_sands_deposit_codes = "https://www.petrinex.gov.ab.ca/bbreports/PRAOilSandsAreaDepositCodes.csv",
                   business_associate_codes = "http://www.aer.ca/data/codes/BusinessAssociate_Codes.xlsx")

data_file_download_names = c(well_list = "ST37.zip",
                    pool_codes = "FieldPoolList.txt",
                    field_codes = "FieldList.txt",
                    oil_sands_areacodes = "PRAOilSandsAreaCodes.csv",
                    oil_sands_deposit_codes = "PRAOilSandsAreaDepositCodes.csv",
                    business_associate_codes = "BusinessAssociate_Codes.xlsx")

data_file_names = c(well_list = "WellList.txt",
                    pool_codes = "FieldPoolList.txt",
                    field_codes = "FieldList.txt",
                    oil_sands_areacodes = "PRAOilSandsAreaCodes.csv",
                    oil_sands_deposit_codes = "PRAOilSandsAreaDepositCodes.csv",
                    business_associate_codes = "BusinessAssociate_Codes.xlsx")

#' Check a directory for the files necessary to build the AER well list tibble
#'
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return A logical vector for each of the required files.
#' @examples
#' found_required_files()
#' found_requred_files(data_dir = "some/other/directory/")
found_required_files <- function(data_dir) {
  file.exists(stringr::str_c(data_dir, data_file_names))
}

#' Craft a string telling the user what file was missing an where to download it
#'
#' @param data_dir The directory where the files were searched for.
#' @param missing_file_names The names of the files that were not found.
#' @param missing_file_urls The urls at which the missing files can be downloaded.
#' @return A character vector containing messages prompting the user to download any missing files
required_file_messages <- function(data_dir, missing_file_names, missing_file_urls) {
  stringr::str_c("The file ", missing_file_names, " was not found in ", data_dir, ". Please download it at: ", missing_file_urls, "\n")
}

#' Generate messages prompting the user to download any files that are missing.
#'
#' @param data_dir A path to a directory containing the necessary AER data files.
#' @return Messages describing the missing files and the urls from where they can downloaded.
#' @examples
#' prompt_user_to_download()
#' prompt_user_to_download(data_dir = "some/other/directory/")
prompt_user_to_download <- function(data_dir) {

  missing_files <- !found_required_files(data_dir)
  missing_file_names <- data_file_names[missing_files]
  missing_file_urls <- data_file_urls[missing_files]

  if (any(missing_files)) {
    message(required_file_messages(data_dir, missing_file_names, missing_file_urls))
  } else {
    message(stringr::str_c("All the necessary data files were found in ", data_dir, "."))
  }
}
