test_that("estimate_days_of_growth_1() works", {

  f <- kwb.rabimo:::estimate_days_of_growth_1

  expect_error(f())

  expect_identical(f(land_type = "urban", veg_class = 10), 60)

  expect_error(f("urban", 1:2))
  expect_error(f(c("urban", "urban"), 1))
  expect_error(f(c("urban", "urban"), 1:2))

})
