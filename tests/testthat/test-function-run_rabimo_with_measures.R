


#library(testthat)
test_that("run_rabimo_with_measures() works", {

  f <- kwb.rabimo::run_rabimo_with_measures

  expect_error(f())

  data <- kwb.rabimo::rabimo_inputs_2020$data
  blocks <- data[sample(seq_len(nrow(data)), 10L), ]

  stats <- kwb.rabimo:::get_measure_stats(blocks)

  measures_max <- list(
    green_roof = stats$green_roof[["max"]],
    unpaved = stats$unpaved[["max"]],
    to_swale = stats$to_swale[["max"]]
  )

  measures_too_big_1 <- list(
    green_roof = stats$green_roof[["max"]] + 0.01,
    unpaved = stats$unpaved[["max"]],
    to_swale = stats$to_swale[["max"]]
  )

  measures_too_big_2 <- list(
    green_roof = stats$green_roof[["max"]],
    unpaved = stats$unpaved[["max"]] + 0.01,
    to_swale = stats$to_swale[["max"]]
  )

  measures_too_big_3 <- list(
    green_roof = stats$green_roof[["max"]],
    unpaved = stats$unpaved[["max"]],
    to_swale = stats$to_swale[["max"]] + 0.01
  )

  result <- f(blocks, measures = measures_max)
  expect_true(all(result$surface_runoff == 0))

  expect_error(f(blocks, measures = measures_too_big_1))
  expect_error(f(blocks, measures = measures_too_big_2))
  expect_error(f(blocks, measures = measures_too_big_3))
})
