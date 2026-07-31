# library(testthat)
test_that("summer_correction() works", {

  f <- kwb.rabimo:::summer_correction

  expect_error(f())

  water_availability <- 1:1000
  correction_factor <- f(water_availability, epot_summer = 1000)

  #plot(water_availability, correction_factor)
  
  expect_true(all(correction_factor > 0.6))
  expect_true(all(correction_factor < 1.6))
})
