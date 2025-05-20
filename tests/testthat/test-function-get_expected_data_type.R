#library(testthat)

test_that("get_expected_data_type() works", {

  f <- kwb.rabimo:::get_expected_data_type

  expect_no_error(f())
  expect_length(f("a"), 0L)
  expect_identical(f("code"), c(code = "character"))

})
