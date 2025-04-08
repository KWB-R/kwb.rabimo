#library(testthat)
test_that("distribute_shares() works", {
  f <- kwb.rabimo:::distribute_shares
  expect_error(f())
})
