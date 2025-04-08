library(testthat)

test_that("yearly_height_to_volume_flow() works", {
  f <- kwb.rabimo:::yearly_height_to_volume_flow
  expect_error(f())
  expect_equal(f(1, 100000), 3.171)
})
