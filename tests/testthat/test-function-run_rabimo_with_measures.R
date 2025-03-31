#library(testthat)
test_that("run_rabimo_with_measures() works", {

  f <- kwb.rabimo::run_rabimo_with_measures

  expect_error(f())

  data <- kwb.rabimo::rabimo_inputs_2020$data
  blocks <- data[sample(seq_len(nrow(data)), 10L), ]

  stats <- kwb.rabimo:::get_measure_stats(blocks)

  safety_factor <- 0.999

  measures_max <- list(
    green_roof = safety_factor * stats$green_roof$max,
    unpaved = safety_factor * stats$unpaved$max,
    to_swale = safety_factor * stats$to_swale$max
  )

  measures_too_big_1 <- list(
    green_roof = measures_max$green_roof + 0.01,
    unpaved = measures_max$unpaved,
    to_swale = measures_max$to_swale
  )

  measures_too_big_2 <- list(
    green_roof = measures_max$green_roof,
    unpaved = measures_max$unpaved + 0.01,
    to_swale = measures_max$to_swale
  )

  measures_too_big_3 <- list(
    green_roof = measures_max$green_roof,
    unpaved = measures_max$unpaved,
    to_swale = measures_max$to_swale + 0.01
  )

  expect_output(result <- f(blocks, measures = measures_max))
  expect_true(all(result$surface_runoff == 0))

  expect_error(f(blocks, measures = measures_too_big_1))
  expect_error(f(blocks, measures = measures_too_big_2))
  expect_error(f(blocks, measures = measures_too_big_3))
})
