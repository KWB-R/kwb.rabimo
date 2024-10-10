#library(testthat)

test_that("check_or_convert_data_types() works", {

  f <- kwb.rabimo:::check_or_convert_data_types

  expect_error(f())

  data <- data.frame(a = 1)
  types <- c(a = "integer")

  expect_error(f(data, types, convert = FALSE))
  expect_output(result <- f(data, types, convert = TRUE))
  expect_identical(result, data.frame(a = 1L))
})
