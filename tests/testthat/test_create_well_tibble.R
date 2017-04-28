library(aertidywells)
library(tibble)
context("Create well tibble")

test_that("create_well_tibble produces a tibble with the same number of rows as the original file", {
  file_row_count <- nrow(readr::read_tsv("extdata/WellList.txt", col_names = FALSE ))
  tibble_row_count <- nrow(create_well_tibble())
  expect_equal(tibble_row_count, file_row_count)
})

test_that("create_well_tibble produces an error if the necessary data is not present in the specified data directory", {
  expect_error(create_well_tibble("not a directory"))
})

test_that("convert_aer_dates produces a warning if any LICENSE-ISSUE-DATEs dont convert nicely", {
  well_list <- tribble(
    ~`LICENSE-ISSUE-DATE`, ~`FIN-DRL-DATE`, ~`WELL-STAT-DATE`,
    "", "19900520", "19900520"
  )
  expect_warning(convert_aer_dates(well_list), "All formats failed to parse. No formats found.")
})

test_that("convert_aer_dates produces a warning if any WELL-STAT-DATEs dont convert nicely", {
  well_list <- tribble(
    ~`LICENSE-ISSUE-DATE`, ~`FIN-DRL-DATE`, ~`WELL-STAT-DATE`,
    "19900520", "19900520", ""
  )
  expect_warning(convert_aer_dates(well_list), "All formats failed to parse. No formats found.")
})
