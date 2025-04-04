#library(testthat)

test_that("data_to_natural() works", {

  f <- kwb.rabimo::data_to_natural

  expect_error(f())
  expect_error(f(data.frame(), "make sure .*all required columns"))

  data <- kwb.rabimo::rabimo_inputs_2020$data
  result <- f(data)

  expect_identical(names(result), names(data))
})
