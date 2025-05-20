test_that("calculate_bagrov_table() works", {

  f <- kwb.rabimo:::calculate_bagrov_table

  expect_error(f())

  result <- f(n_values = 0.1)
  expect_identical(names(result), c("E_a_over_E_p", "P_over_E_p", "n_value"))
  expect_true(all(result$n_value == 0.1))
  expect_true(all(diff(result$E_a_over_E_p) > 0))
  expect_true(all(diff(result$P_over_E_p) > 0))
})
