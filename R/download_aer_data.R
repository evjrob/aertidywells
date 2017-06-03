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
#' @keywords internal
#' @noRd
found_required_files <- function(data_dir) {
  file.exists(stringr::str_c(data_dir, data_file_names))
}

#' Craft a string telling the user what file was missing an where to download it
#'
#' @param data_dir The directory where the files were searched for.
#' @param missing_file_names The names of the files that were not found.
#' @param missing_file_urls The urls at which the missing files can be downloaded.
#' @return A character vector containing messages prompting the user to download any missing files
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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

#' Attempt to download the the missing files to the specified directory.
#'
#' @param data_dir A path to a directory where the necessary AER data files should be saved.
#' @examples
#' download_aer_data()
#' download_aer_data(data_dir = "some/other/directory/")
#' @export
download_aer_data <- function(data_dir = "data/") {
  download_permission <- readline(prompt = stringr::str_c("The required files were not found in the specified directory: ", data_dir, ". Would you like to downlad them now automatically? [Y/n]:"))

  # Check four reasonable inputs that shoud all clearly be interpretted as yes.
  if (download_permission == "y" || download_permission == "Y" || download_permission == "Yes" || download_permission == "yes") {
    if (!dir.exists(data_dir)) {
      dir.create(data_dir)
    }

    missing_files <- !found_required_files(data_dir)
    missing_file_names <- data_file_download_names[missing_files]
    missing_file_urls <- data_file_urls[missing_files]

    for (i in 1:length(missing_file_urls)) {
      download.file(missing_file_urls[[i]], stringr::str_c(data_dir, missing_file_names[[i]]))

      # The WellList.txt file comes inside a zip archive. The WellList.txt file needs to be extracted.
      if (names(missing_file_urls)[[i]] == "well_list") {

        # Windows can't handle trailing slashes on directory names so we need to check for those
        exdir_name <- data_dir
        if ((.Platform$OS.type == "windows") && (grepl("*/$|*\\\\$", data_dir))) {
          exdir_name <- stringr::str_sub(data_dir, 1, stringr::str_length(data_dir) - 1)
        }
        unzip(stringr::str_c(data_dir, missing_file_names[[i]]), exdir = exdir_name, junkpaths = TRUE)
      }
    }
  } else {
    prompt_user_to_download(data_dir)
  }
}
