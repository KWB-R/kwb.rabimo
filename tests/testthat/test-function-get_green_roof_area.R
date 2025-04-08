# library(testthat)
test_that("get_green_roof_area() works", {
  f <- kwb.rabimo:::get_green_roof_area
  expect_error(f())
})
