#library(testthat)

test_that("calculate_delta_w() works", {

  f <- kwb.rabimo::calculate_delta_w

  expect_error(f())

  # Define inputs
  natural <- data.frame(
    code = "a",
    evapor = 10,
    infiltr = 2,
    runoff = 3
  )

  urban <- data.frame(
    code = "a",
    evapor = 4,
    infiltr = 5,
    runoff = 6
  )

  df_3 <- data.frame(
    code = c("a", "b", "c"),
    evapor = c(10, 2, 3),
    infiltr = c(2, 7, 4),
    runoff = c(1, 5, 8)
  )
  stopifnot(all.equal(rowSums(df_3[, -1L]), 13:15))

  df_2 <- data.frame(
    code = c("a", "c"),
    evapor = c(4, 5),
    infiltr = c(5, 6),
    runoff = c(4, 4)
  )
  stopifnot(all.equal(rowSums(df_2[, -1L]), c(13, 15)))

  base_check <- function(result) {
    expect_true(is.data.frame(result))
    expect_identical(names(result), c("code", "delta_w"))
    result
  }

  base_check(result_1 <- f(natural, urban))
  base_check(result_2 <- f(urban, natural))
  expect_equal(result_1, result_2)

  base_check(result_32 <- f(df_3, df_2))
  base_check(result_23 <- f(df_2, df_3))

  expect_true(all.equal(result_32$code, c("a", "c")))
  expect_true(all.equal(result_32, result_23))
})
