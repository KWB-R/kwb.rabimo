#library(testthat)

test_that("actual_evaporation_waterbody_or_pervious() works", {

  f <- kwb.rabimo:::actual_evaporation_waterbody_or_pervious

  expect_error(f())

  usage_tuple <- data.frame(
    land_type = "a",
    irrigation = 100L,
    veg_class = 10L
  )

  climate <- data.frame(
    prec_yr = 600L,
    prec_s = 100L,
    epot_yr = 600L,
    epot_s = 300L
  )

  soil_properties <- data.frame(
    mean_potential_capillary_rise_rate = 1,
    potential_capillary_rise = 1,
    depth_to_water_table = 15,
    g02 = 1
  )

  result <- f(usage_tuple, climate, soil_properties)

  expect_type(result, "double")
  expect_length(result, 1L)

  expect_identical(names(attributes(result)), "bagrovUnsealed")
})

