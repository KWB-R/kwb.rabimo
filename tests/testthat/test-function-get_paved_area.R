# library(testthat)
test_that("get_paved_area() works", {
  f <- kwb.rabimo:::get_paved_area
  expect_error(f())
})
