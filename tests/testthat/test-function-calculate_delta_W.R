#library(testthat)

test_that("calculate_delta_w() works", {

  f <- kwb.rabimo::calculate_delta_w

  expect_error(f())

  # Define inputs
  natural <- data.frame(
    code = "a",
    evapor = 1, # evaporation = 1
    infiltr = 2, # infiltration = 2
    runoff = 3 # runoff = 3
  )

  urban <- data.frame(
    code = "a",
    evapor = 4, # evaporation = 4
    infiltr = 5, # infiltration = 5
    runoff = 6
  )

  df_3 <- data.frame(
    code = c("a", "b", "c"),
    evapor = 1:3, # evaporation = 1
    infiltr = 2:4, # infiltration = 2
    runoff = 3:5 # runoff = 3
  )

  df_2 <- data.frame(
    code = c("a", "c"),
    evapor = 4:5, # evaporation = 4
    infiltr = 5:6, # infiltration = 5
    runoff = 6:7
  )

  expect_error(f(natural = natural, urban = urban, implementation = 4L))

  # three implementations return the same results
  result_1 <- f(natural = natural, urban = urban, implementation = 1L)
  result_2 <- f(natural = natural, urban = urban, implementation = 2L)
  result_3 <- f(natural = natural, urban = urban, implementation = 3L)

  expect_true(is.data.frame(result_1))
  expect_true(is.data.frame(result_2))
  expect_true(is.data.frame(result_3))

  expect_identical(names(result_1), "delta_w")
  expect_identical(names(result_2), c("code", "delta_w"))
  expect_identical(names(result_3), c("code", "delta_w"))

  f(df_3, df_2, implementation = 1L)
  f(df_3, df_2, implementation = 2L)
  f(df_3, df_2, implementation = 3L)

  expect_error(f(df_2, df_3, implementation = 1L))
  expect_error(f(df_2, df_3, implementation = 2L))
  expect_error(f(df_2, df_3, implementation = 3L))

})
