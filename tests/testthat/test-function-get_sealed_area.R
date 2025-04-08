# library(testthat)
test_that("get_sealed_area() works", {
  f <- kwb.rabimo:::get_sealed_area
  expect_error(f())
})
