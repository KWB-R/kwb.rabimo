# library(testthat)
test_that("get_main_area() works", {
  f <- kwb.rabimo:::get_main_area
  expect_error(f())
  expect_no_error(f(kwb.rabimo::rabimo_inputs_2020$data))
  expect_no_error(f(kwb.rabimo::rabimo_inputs_2025$data))
})
