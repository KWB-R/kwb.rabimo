#library(testthat)
test_that("check_sum_up_to_1_or_0() works", {

  f <- kwb.rabimo:::check_sum_up_to_1_or_0

  expect_error(f())

  expect_error(
    f(data = data.frame(a = "a"), columns = "a"),
    "There are non-numeric columns"
  )
  
  expect_output(expect_error(f(
    data.frame(
      code = c("btf1", "btf2"),
      a = 1:2,
      b = 2:3,
      c = 3:4
    ),
    columns = c("a", "c")
  )))

  expect_null(f(
    data.frame(
      code = paste0("code_", 1:3),
      a = c(0, 1, 0),
      b = c(0, 1, 0),
      c = c(0, 0, 1)
    ),
    columns = c("a", "c")
  ))

})
