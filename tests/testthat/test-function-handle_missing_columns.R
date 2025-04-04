# library(testthat)
test_that("handle_missing_columns() works", {
  f <- kwb.rabimo:::handle_missing_columns
  expect_error(f())
})
