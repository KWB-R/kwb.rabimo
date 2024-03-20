#library(testthat)

test_that("prepare_input_data() works", {

  f <- kwb.rabimo:::prepare_input_data

  expect_error(f())

})
