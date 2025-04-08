test_that("clean_stop() works", {

  f <- kwb.rabimo:::clean_stop

  expect_error(f())
  expect_error(f("abc"), "abc")

})
