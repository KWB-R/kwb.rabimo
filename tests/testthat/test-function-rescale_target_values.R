# library(testthat)
test_that("rescale_target_values() works", {
  f <- kwb.rabimo:::rescale_target_values
  expect_error(f())
})
