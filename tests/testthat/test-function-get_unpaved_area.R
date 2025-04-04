# library(testthat)
test_that("get_unpaved_area() works", {
  f <- kwb.rabimo:::get_unpaved_area
  expect_error(f())
})
