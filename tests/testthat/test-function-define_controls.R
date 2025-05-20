#library(testthat)
test_that("define_controls() works", {

  f <- kwb.rabimo::define_controls

  controls <- f()

  expected_names <- c(
    "check", "use_abimo_bagrov_solver", "reproduce_abimo_error",
    "output_format", "intermediates"
  )

  expect_true(all(expected_names %in% names(controls)))

})
