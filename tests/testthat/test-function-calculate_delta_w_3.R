#library(testthat)
test_that("calculate_delta_w_3() works", {

  f <- kwb.rabimo:::calculate_delta_w_3

  expect_error(f())

  result <- f(
    natural = data.frame(
      code = "a",
      evapor = 1,
      infiltr = 2,
      runoff = 3
    ),
    urban = data.frame(
      code = "a",
      evapor = 4,
      infiltr = 5,
      runoff = 6
    )
  )

  expect_true(is.data.frame(result))
  expect_identical(c("code", "delta_w"), names(result))

})
