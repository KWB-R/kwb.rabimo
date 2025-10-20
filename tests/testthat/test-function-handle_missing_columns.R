# library(testthat)
test_that("handle_missing_columns() works", {
  
  handle_missing <- kwb.rabimo:::handle_missing_columns
  
  expect_error(handle_missing())
  
  area_with_missing <- kwb.utils::removeColumns(
    kwb.rabimo::generate_rabimo_area("code", green_roof_int = 0.1),
    columnsToRemove = c("main_frac", "green_roof_int")
  )

  expect_message(expect_message(result_1 <- handle_missing(area_with_missing)))
  expect_silent(result_2 <- handle_missing(area_with_missing, silent = TRUE))

  expect_identical(result_1$green_roof_int, 0)
  expect_identical(result_2$green_roof_int, 0)
  
  expect_identical(result_1$main_frac, 1)
  expect_identical(result_2$main_frac, 1)
})
