test_that("create_fraction_accessor() works", {

  f <- kwb.rabimo:::create_fraction_accessor(
    data = data.frame(areaFractionA = 1)
  )

  expect_identical(f("A"), 1)

})
