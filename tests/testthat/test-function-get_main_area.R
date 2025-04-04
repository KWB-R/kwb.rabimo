# library(testthat)
test_that("get_main_area() works", {
  f <- kwb.rabimo:::get_main_area
  expect_error(f())
})
