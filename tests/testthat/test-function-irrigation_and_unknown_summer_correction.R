#library(testthat)
test_that("irrigation_and_unknown_summer_correction() works", {

  f <- kwb.rabimo:::irrigation_and_unknown_summer_correction

  expect_error(f())

  irrigation <- 1:1000
  correction_factor <- f(irrigation)
  
  # We expect the maximum at 374, why?
  expect_equal(irrigation[which.max(correction_factor)], 374)
})
