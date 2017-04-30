#' Create the internal datatables necessary to interpret the well status codes
#'
#' @return Save the necessary data tables to R/sysdata.rda using the files in raw-data/.
#' @keywords internal
#' @noRd
create_status_code_tables <- function() {
  status_col_types <- list( readr::col_integer(),   # Value
                            readr::col_character(), # Short Description
                            readr::col_character()  # Description
  )

  # These well status code tables are necessary to turn the "WELL-STAT-CODE"
  # column into human readble statuses later. The csv files were extracted from
  # the above layout document using tabula (http://tabula.technology/)
  well_status_codes_fluid <- readr::read_csv("raw-data/well_status_codes_fluid.csv", col_types = status_col_types)
  well_status_codes_mode <- readr::read_csv("raw-data/well_status_codes_mode.csv", col_types = status_col_types)
  well_status_codes_type <- readr::read_csv("ra-data/well_status_codes_type.csv", col_types = status_col_types)
  well_status_codes_structure <- readr::read_csv("raw-data/well_status_codes_structure.csv", col_types = status_col_types)

  devtools::use_data(well_status_codes_fluid, well_status_codes_mode, well_status_codes_structure, well_status_codes_type, internal = TRUE)
}
