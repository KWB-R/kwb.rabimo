test_that("bagrov() works", {

  f <- kwb.rabimo:::bagrov

  expect_error(f())

  expect_equal(f(bagf = 1, x0 = 0), 0)
  expect_equal(f(bagf = 10, x0 = 0), 0)
  expect_equal(f(bagf = 100, x0 = 0), 0)
})
