test_that("estimate_days_of_growth() works", {

  f <- kwb.rabimo:::estimate_days_of_growth

  expect_error(f())
  expect_error(f("urban", 1:2))
  expect_error(f(rep("urban", 2), 1))

  expect_true(all(f(rep("urban", 50), 1:50) == 60L))
  expect_true(all(f(rep("urban", 50), 51:100) == 75L))
})
