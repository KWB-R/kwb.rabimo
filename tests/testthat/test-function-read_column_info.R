#library(testthat)

test_that("read_column_info() works", {

  f <- kwb.rabimo:::read_column_info

  result <- f()

  expect_s3_class(result, "data.frame")

  expect_identical(names(result), c(
    "rabimo_berlin",
    "abimo_berlin",
    "by_100",
    "meaning",
    "unit",
    "type",
    "data_type",
    "default"
  ))

  expect_true(all(nzchar(result$meaning)))

})
