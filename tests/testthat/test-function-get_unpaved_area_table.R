# library(testthat)
test_that("get_unpaved_area_table() works", {
  f <- kwb.rabimo:::get_unpaved_area_table
  expect_error(f())
})
