#library(testthat)

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

features <- kwb.rabimo:::check_or_convert_data_types(
  features,
  types = kwb.rabimo:::get_expected_data_type(),
  convert = TRUE,
  dbg = FALSE
)

test_that("run_rabimo_with_measures(old_version = TRUE) works", {

  run_rabimo_with_measures <- kwb.rabimo::run_rabimo_with_measures

  expect_error(run_rabimo_with_measures())

  test_me <- function(data) {
    blocks <- data[sample(seq_len(nrow(data)), 10L), ]
    stats <- kwb.rabimo:::get_measure_stats(blocks)
    safety_factor <- 0.999
    
    measures_max <- list(
      green_roof = safety_factor * stats$green_roof$max,
      unpaved = safety_factor * stats$unpaved$max,
      to_swale = safety_factor * stats$to_swale$max
    )
    
    measures_too_big_1 <- list(
      green_roof = measures_max$green_roof + 0.01,
      unpaved = measures_max$unpaved,
      to_swale = measures_max$to_swale
    )
    
    measures_too_big_2 <- list(
      green_roof = measures_max$green_roof,
      unpaved = measures_max$unpaved + 0.01,
      to_swale = measures_max$to_swale
    )
    
    measures_too_big_3 <- list(
      green_roof = measures_max$green_roof,
      unpaved = measures_max$unpaved,
      to_swale = measures_max$to_swale + 0.01
    )
    
    expect_output(result <- run_rabimo_with_measures(
      blocks, measures = measures_max, old_version = TRUE
    ))
    
    expect_true(all(result$surface_runoff == 0))
    
    expect_error(run_rabimo_with_measures(
      blocks, measures = measures_too_big_1, old_version = TRUE
    ))
    
    expect_error(run_rabimo_with_measures(
      blocks, measures = measures_too_big_2, old_version = TRUE
    ))
    
    expect_error(run_rabimo_with_measures(
      blocks, measures = measures_too_big_3, old_version = TRUE
    ))
  }
  
  test_me(data = kwb.rabimo::rabimo_inputs_2020$data)
  test_me(data = kwb.rabimo::rabimo_inputs_2025$data)
  
  expect_no_error(expect_output(
    kwb.rabimo::run_rabimo_with_measures(
      features, 
      measures = list(green_roof = 0.009, to_swale = 0, unpaved = 0.3), 
      old_version = TRUE
    )
  ))
  
  expect_error(
    kwb.rabimo::run_rabimo_with_measures(
      features, 
      measures = list(green_roof = 0.00900001, to_swale = 0, unpaved = 0.3),
      old_version = TRUE
    )
  )
})
