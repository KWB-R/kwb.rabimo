# library(testthat)
test_that("get_weight() works", {
  f <- kwb.rabimo:::get_weight
  expect_error(f())
})
