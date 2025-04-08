# library(testthat)
test_that("rescale_target_values() works", {

  f <- kwb.rabimo:::rescale_target_values
  expect_error(f())

  new_targets <- list(green_roof = 0.1, unpaved = 0.1, to_swale = 0.1)


  f(new_targets, blocks = kwb.rabimo::rabimo_inputs_2020$data)
  f(new_targets, blocks = kwb.rabimo::rabimo_inputs_2025$data)
})
