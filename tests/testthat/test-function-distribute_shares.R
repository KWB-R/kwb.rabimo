# library(testthat)
test_that("distribute_shares() works", {

  f <- kwb.rabimo:::distribute_shares

  expect_error(f())

  result <- f(
    partial_areas = 1:10,
    base_areas = 2:11,
    complementary_areas = 3:12,
    target_value = 1
  )

  expect_true(is.data.frame(result))

  expect_identical(
    names(result),
    c("area", "fraction", "consider", "ref_area", "delta")
  )

  expect_identical(nrow(result), 10L)

})
