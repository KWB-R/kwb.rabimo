# library(testthat)
test_that("get_green_roof_table() works", {
  f <- kwb.rabimo:::get_green_roof_table
  expect_error(f())
})
