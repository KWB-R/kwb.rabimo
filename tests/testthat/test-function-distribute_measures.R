test_that("distribute_measures() works", {

  f <- kwb.rabimo:::distribute_measures

  expect_error(f())

  blocks <- data.frame(
    total_area = 100,
    main_frac = 1,
    roof = 0.5,
    pvd = 0.5,
    green_roof = 0,
    to_swale = 0
  )

  targets <- c(green_roof = 0.5, unpaved = 0.5, to_swale = 0.5)
  result <- f(blocks, targets)

  expect_identical(result$green_roof, targets[["green_roof"]])
  expect_identical(result$to_swale, targets[["to_swale"]])

})
