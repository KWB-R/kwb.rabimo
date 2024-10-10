#library(testthat)

test_that("get_column_renamings() works", {

  f <- kwb.rabimo:::get_column_renamings

  result <- f()

  expect_type(result, "list")
  expect_true("CODE" %in% names(result))
  expect_true("code" %in% unlist(result))
})
