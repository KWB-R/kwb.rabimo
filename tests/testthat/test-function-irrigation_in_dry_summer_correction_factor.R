test_that("irrigation_and_unknown_summer_correction() works", {

  f <- kwb.rabimo:::irrigation_and_unknown_summer_correction

  expect_error(f())

})
