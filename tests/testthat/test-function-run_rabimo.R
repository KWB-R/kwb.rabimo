#library(testthat)

test_that("run_rabimo() reproduces previous results", {
  config <- kwb.rabimo::rabimo_inputs_2020$config
  data <- kwb.rabimo::rabimo_inputs_2020$data
  results <- kwb.rabimo::run_rabimo(data, config)
  result <- colMeans(results[, c("runoff", "infiltr", "evapor")])
  expected_result <- c(runoff = 162.5073, infiltr = 184.4515, evapor = 284.8178)
  expect_equal(round(result, 4L), expected_result)
})

test_that("run_rabimo() works", {

  f <- kwb.rabimo::run_rabimo

  expect_error(f())

  data <- data.frame(
    code = "a",
    land_type = "a",
    prec_yr = 100L,
    prec_s = 100L,
    epot_yr = c(1L, 2L, 4L),
    epot_s = 123L,
    ufc30 = -123,
    ufc150 = 1.2,
    gw_dist = c(-1,-2,1),
    veg_class = 1.0,
    irrigation = -1L,
    main_frac = c(1, 1, 0.3),
    roof = c(0.1, 0.2, 0.3),
    green_roof = 0.0,
    swg_roof = 0.2,
    srf1_pvd = 0.5,
    srf2_pvd = 0.5,
    srf3_pvd = c(0, 0, 0),
    srf4_pvd = 0,
    srf5_pvd = 0,
    srf1_pvd_r = 0,
    srf2_pvd_r = 0.1,
    srf3_pvd_r = 0.9001,
    srf4_pvd_r = 0,
    pvd = c(0.2, 0.4, 0.5),
    swg_pvd = c(0, 0, 0),
    road_frac = 0.1,
    pvd_r = 0,
    swg_pvd_r = c(0.2, 1, 0),
    to_swale = 0.0,
    total_area = 100
  )

  config <- list(
    bagrov_values = c(
      roof = 1,
      green_roof = 1,
      surface1 = 2,
      surface2 = 3,
      surface3 = 4,
      surface4 = 5,
      surface5 = 6
    ),
    runoff_factors = c(
      roof = -1,
      green_roof = 1,
      surface1 = -2,
      surface2 = -3,
      surface3 = -4,
      surface4 = -5,
      surface5 = -6
    ),
    swale = list(swale_evaporation_factor = 1)
  )

  expect_output(result <- f(data, config, controls = define_controls()))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == nrow(data))

})
