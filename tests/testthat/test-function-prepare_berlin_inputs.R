#library(testthat)

test_that("prepare_berlin_inputs() works", {

  f <- kwb.rabimo::prepare_berlin_inputs

  expect_error(f())

  data_file <- tempfile(fileext = ".dbf")

  foreign::write.dbf(kwb.abimo::abimo_input_2019, data_file)

  result <- f(data_file, dbg = FALSE)

  expect_type(result, "list")

  expect_identical(names(result), c("data", "config"))

  expect_identical(names(result$data), c(
    "code", "prec_yr", "prec_s", "epot_yr", "epot_s", "district", "total_area",
    "area_main", "area_road", "main_frac", "roof", "green_roof", "swg_roof",
    "pvd", "swg_pvd",
    "srf1_pvd", "srf2_pvd", "srf3_pvd", "srf4_pvd", "srf5_pvd",
    "road_frac", "pvd_r", "swg_pvd_r", "srf1_pvd_r", "srf2_pvd_r",
    "srf3_pvd_r", "srf4_pvd_r", "sealed", "to_swale", "gw_dist", "ufc30",
    "ufc150", "land_type", "veg_class", "irrigation", "block_type"
  ))

  expect_identical(names(result$config), c(
    c("runoff_factors", "bagrov_values", "result_digits",
      "irrigation_to_zero", "swale")
  ))
})
