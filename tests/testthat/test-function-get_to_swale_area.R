# library(testthat)
test_that("get_to_swale_area() works", {
  f <- kwb.rabimo:::get_to_swale_area
  expect_error(f())
})
