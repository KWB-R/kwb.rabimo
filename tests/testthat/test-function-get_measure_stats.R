# library(testthat)
test_that("get_measure_stats() works", {
  f <- kwb.rabimo::get_measure_stats
  expect_error(f())
})
