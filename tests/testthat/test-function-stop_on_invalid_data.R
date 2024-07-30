#library(testthat)

test_that("stop_on_invalid_data() works", {

  f <- kwb.rabimo:::stop_on_invalid_data

  expect_error(f())

  data <- data.frame(
    code = "a",
    prec_yr = 1,
    prec_s = 1,
    epot_yr = 1,
    epot_s = 1
  )

  expect_error(f(data[1L, ]), "There are missing columns")

})
