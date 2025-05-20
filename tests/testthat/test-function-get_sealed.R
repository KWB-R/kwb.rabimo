# library(testthat)
test_that("get_sealed() works", {
  f <- kwb.rabimo:::get_sealed
  expect_error(f())
})
