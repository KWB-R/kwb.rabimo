#library(testthat)

test_that("generate_rabimo_area() works", {

  f <- kwb.rabimo::generate_rabimo_area

  expect_no_error(data <- f())

  expect_no_error(expect_message(kwb.rabimo::run_rabimo(
    silent = TRUE,
    data = data,
    config = kwb.rabimo::rabimo_inputs_2025$config,
    controls = kwb.rabimo::define_controls()
  )))

})
