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

  features <- jsonlite::fromJSON('
    [
      {
        "code": "0000000001000016",
        "prec_yr": 632,
        "prec_s": 333,
        "epot_yr": 660,
        "epot_s": 530,
        "district": "1",
        "total_area": 4951.8538,
        "area_main": 4951.8538,
        "area_rd": 0,
        "main_frac": 1,
        "roof": 0.009,
        "green_roof": 0,
        "swg_roof": 1,
        "pvd": 0.9736,
        "swg_pvd": 1,
        "srf1_pvd": 0.33,
        "srf2_pvd": 0.15,
        "srf3_pvd": 0.16,
        "srf4_pvd": 0,
        "srf5_pvd": 0.36,
        "road_frac": 0,
        "pvd_r": 0,
        "swg_pvd_r": 1,
        "srf1_pvd_r": 0,
        "srf2_pvd_r": 0,
        "srf3_pvd_r": 0,
        "srf4_pvd_r": 0,
        "sealed": 0.9826,
        "to_swale": 0,
        "gw_dist": 2.8,
        "ufc30": 12,
        "ufc150": 10,
        "land_type": "urban",
        "veg_class": 35,
        "irrigation": 0,
        "block_type": "300_road"
      },
      {
        "code": "0000000001000017",
        "prec_yr": 632,
        "prec_s": 333,
        "epot_yr": 660,
        "epot_s": 530,
        "district": "1",
        "total_area": 4951.8538,
        "area_main": 4951.8538,
        "area_rd": 0,
        "main_frac": 1.0,
        "roof": 0.009,
        "green_roof": 0,
        "swg_roof": 1,
        "pvd": 0.9736,
        "swg_pvd": 1,
        "srf1_pvd": 0.33,
        "srf2_pvd": 0.15,
        "srf3_pvd": 0.16,
        "srf4_pvd": 0,
        "srf5_pvd": 0.36,
        "road_frac": 0,
        "pvd_r": 0,
        "swg_pvd_r": 1,
        "srf1_pvd_r": 0,
        "srf2_pvd_r": 0,
        "srf3_pvd_r": 0,
        "srf4_pvd_r": 0,
        "sealed": 0.9826,
        "to_swale": 0,
        "gw_dist": 2.8,
        "ufc30": 12,
        "ufc150": 10,
        "land_type": "urban",
        "veg_class": 35,
        "irrigation": 0,
        "block_type": "300_road"
      }
    ]')

  measure_stats <- kwb.rabimo::get_measure_stats(blocks)
  sprintf("%0.10f", measure_stats$green_roof$max)

  features <- kwb.rabimo:::check_or_convert_data_types(
    features,
    types = kwb.rabimo:::get_expected_data_type(),
    convert = TRUE,
    dbg = FALSE
  )

  expect_no_error(expect_output(
    kwb.rabimo::run_rabimo_with_measures(features, measures = list(
      green_roof = 0.009, to_swale = 0, unpaved = 0.3
    ))
  ))

  expect_error(
    kwb.rabimo::run_rabimo_with_measures(features, measures = list(
      green_roof = 0.00900001, to_swale = 0, unpaved = 0.3
    ))
  )

})
