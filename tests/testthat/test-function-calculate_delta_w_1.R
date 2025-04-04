test_that("calculate_delta_w_1() works", {

  f <- kwb.rabimo:::calculate_delta_w_1

  expect_error(f())

  result <- f(
    natural = data.frame(
      code = "a",
      evaporation = 1,
      infiltration = 2,
      runoff = 3
    ),
    urban = data.frame(
      code = "a",
      evaporation = 4,
      infiltration = 5,
      runoff = 6
    )
  )

  expect_true(is.data.frame(result))
  expect_identical("delta_w", names(result))
})
