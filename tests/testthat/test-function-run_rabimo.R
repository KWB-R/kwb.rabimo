#library(testthat)

test_that("run_rabimo() works", {

  f <- kwb.rabimo::run_rabimo

  expect_error(f())

  data <- data.frame(
    code = "a",
    land_type = "a",
    prec_yr = 100,
    prec_s = 100L,
    epot_yr = c(1,2,4),
    epot_s = 12.3,
    ufc30 = -123,
    ufc150 = 1.2,
    gw_dist = c(-1,-2,1),
    veg_class = 1,
    irrigation = -1,
    main_fraction = c(1, 1, 0.3),
    roof = c(0.1, 0.2, 0.3),
    swg_roof = 0.2,
    srf1_pvd = 0.5,
    srf2_pvd = 0.5,
    srf3_pvd = c(0, 0, 0),
    srf4_pvd = 0,
    srf1_pvd_rd = 0,
    srf2_pvd_rd = 0.1,
    srf3_pvd_rd = 0.9001,
    srf4_pvd_rd = 0,
    pvd = c(0.2, 0.4, 0.5),
    swg_pvd = c(0, 0, 0),
    road_fraction = 0.1,
    pvd_rd = 0,
    swg_pvd_rd = c(0.2, 1, 0),
    sealed = 0.1,
    total_area = 100
  )

  config <- list(
    bagrov_values = c(
      roof = 1,
      surface1 = 2,
      surface2 = 3,
      surface3 = 4,
      surface4 = 5,
      surface5 = 6
    ),
    runoff_factors = c(
      roof = -1,
      surface1 = -2,
      surface2 = -3,
      surface3 = -4,
      surface4 = -5,
      surface5 = -6
    )
  )

  expect_output(f(data, config))

})
