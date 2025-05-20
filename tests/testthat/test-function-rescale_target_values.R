#library(testthat)
test_that("rescale_target_values() works", {

  f <- kwb.rabimo:::rescale_target_values
  expect_error(f())

  new_targets <- list(green_roof = 0.1, unpaved = 0.1, to_swale = 0.1)

  f(new_targets, blocks = kwb.rabimo::rabimo_inputs_2020$data)
  f(new_targets, blocks = kwb.rabimo::rabimo_inputs_2025$data)

  expect_error(f(
    list(green_roof = 0.7, unpaved = 0.1, to_swale = 0.1),
    blocks = kwb.rabimo::rabimo_inputs_2025$data
  ))

  blocks <- kwb.rabimo::generate_rabimo_area("a", roof = 0)
  # case reported by Luise
  new_targets <- list(green_roof = 0, unpaved = 0.995489083, to_swale = 0)
  expect_no_error(result <- f(new_targets, blocks = blocks))
  expect_identical(new_targets, result)

  new_targets <- list(green_roof = 0.1, unpaved = 0.995489083, to_swale = 0)
  expect_no_error(result <- f(new_targets, blocks = blocks))
  expect_identical(result$green_roof, 0)

  blocks <- kwb.rabimo::generate_rabimo_area("a", roof = 0, pvd = 0)
  # case reported by Luise
  new_targets <- list(green_roof = 0, unpaved = 1, to_swale = 0)
  expect_no_error(result <- f(new_targets, blocks = blocks))
  expect_identical(new_targets, result)

  new_targets <- list(green_roof = 0, unpaved = 1, to_swale = 0.1)
  expect_no_error(result <- f(new_targets, blocks = blocks))
  expect_identical(result$to_swale, 0)
})
