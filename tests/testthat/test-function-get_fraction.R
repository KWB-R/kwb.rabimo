# library(testthat)
test_that("get_fraction() works", {
  f <- kwb.rabimo:::get_fraction
  expect_error(f())
})
