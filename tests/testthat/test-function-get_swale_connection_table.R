# library(testthat)
test_that("get_swale_connection_table() works", {
  f <- kwb.rabimo:::get_swale_connection_table
  expect_error(f())
})
