#library(testthat)

test_that("run_rabimo() reproduces previous results", {
  config <- kwb.rabimo::rabimo_inputs_2020$config
  data <- kwb.rabimo::rabimo_inputs_2020$data
  expect_message(results <- kwb.rabimo::run_rabimo(data, config))
  result <- colMeans(results[, c("runoff", "infiltr", "evapor")])
  expected_result <- c(runoff = 162.5073, infiltr = 184.4515, evapor = 284.8178)
  expect_equal(round(result, 4L), expected_result)
})

test_that("run_rabimo() works", {

  run <- kwb.rabimo::run_rabimo

  expect_error(run())

  data <- data.frame(
    code = "area_1",
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
    swale = list(
      swale_evaporation_factor = 1
    )
  )

  expect_output(suppressMessages(
    result_1 <- run(data, config, controls = define_controls(), silent = FALSE)
  ))
  expect_message(
    result_2 <- run(data, config, controls = define_controls(), silent = TRUE)
  )

  expect_s3_class(result_1, "data.frame")
  expect_true(nrow(result_1) == nrow(data))
  expect_identical(result_1, result_2)
})

test_that("run_rabimo() keeps the row order", {
  inputs <- kwb.rabimo::rabimo_inputs_2020
  data <- inputs$data[sample(nrow(inputs$data), 10L), ]
  expect_message(result <- kwb.rabimo::run_rabimo(data, config = inputs$config))
  expect_identical(data$code, result$code)
})

test_that("run_rabimo() keeps geometry if data inherits from 'sf'", {
  inputs <- kwb.rabimo::rabimo_inputs_2025
  data <- inputs$data[sample(nrow(inputs$data), 10L), ]
  expect_true("sf" %in% class(data))
  expect_message(
    result <- kwb.rabimo::run_rabimo(data, config = inputs$config)
  )
  expect_true("sf" %in% class(result))
})

test_that("Full connection to swales results in zero runoff", {
  generate <- kwb.rabimo::generate_rabimo_area
  data <- rbind(
    generate("area_0",                    green_roof = 0, to_swale = 0), 
    generate("all_swale",                 green_roof = 0, to_swale = 1), 
    generate("all_swale_plus_green_roof", green_roof = 1, to_swale = 1), 
    generate("all_swale_plus_green_roof", green_roof = 0, to_swale = 1, pvd = 0), 
    generate("all_swale_plus_both",       green_roof = 1, to_swale = 1, pvd = 0)
  )
  config <- kwb.rabimo::rabimo_inputs_2025$config
  result <- kwb.rabimo::run_rabimo(data, config, silent = TRUE)
  expect_true(all(result$runoff[startsWith(result$code, "all_swale")] == 0))
})

test_that("Abimo can simulate intensive green roofs", {
  # generate <- kwb.rabimo::generate_rabimo_area
  # data <- rbind(
  #   generate("area_0"), 
  #   generate("area_1")
  # )
  # config <- kwb.rabimo::rabimo_inputs_2025$config
  # result <- kwb.rabimo::run_rabimo(data, config, silent = TRUE)
  # expect_true(all(result$runoff[startsWith(result$code, "all_swale")] == 0))
})
