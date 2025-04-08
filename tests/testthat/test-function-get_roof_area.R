# library(testthat)
test_that("get_roof_area() works", {
  f <- kwb.rabimo:::get_roof_area
  expect_error(f())
})
