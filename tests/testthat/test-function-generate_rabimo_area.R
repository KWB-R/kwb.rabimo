#library(testthat)

test_that("generate_rabimo_area() works", {

  f <- kwb.rabimo::generate_rabimo_area

  expect_error(f())

  expect_no_error(expect_output(kwb.rabimo::run_rabimo(
    data = f(code = "a_code"),
    config = kwb.rabimo::rabimo_inputs_2020$config,
    controls = kwb.rabimo::define_controls()
  )))

})
