#library(testthat)

test_that("get_column_selection() works", {

  f <- kwb.rabimo:::get_column_selection

  expect_silent(result <- f())
  expect_identical(result[1L], "code")
  expect_false("sealed" %in% result)
})

