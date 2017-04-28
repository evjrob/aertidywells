library(aertidywells)
context("Create well tibble")

test_that("create_well_tibble produces a tibble with the same number of rows as the original file", {
  file_row_count <- nrow(readr::read_tsv("extdata/WellList.txt", col_names = FALSE ))
  tibble_row_count <- nrow(create_well_tibble())
  expect_equal(tibble_row_count, file_row_count)
})
