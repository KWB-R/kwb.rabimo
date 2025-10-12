#library(testthat)

# Define globals
{
  FEATURES <- kwb.rabimo:::check_or_convert_data_types(
    types = kwb.rabimo:::get_expected_data_type(), 
    convert = TRUE, 
    dbg = FALSE, 
    data = jsonlite::fromJSON(
      '[
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
      }
    ]'
    )
  )
  
  SAFETY_FACTOR <- 0.999
  RUN <- kwb.rabimo::run_rabimo_with_measures
  RUN_OLD <- function(...) RUN(..., old_version = TRUE, silent = TRUE)
  GET_MAX <- function(x) lapply(kwb.rabimo:::get_measure_stats(x), `[[`, "max")
  APPLY_MEASURES <- kwb.rabimo:::apply_measures_to_blocks
  DATASETS <- lapply(
    X = list(
      d2020 = kwb.rabimo::rabimo_inputs_2020$data,
      d2025 = kwb.rabimo::rabimo_inputs_2025$data
    ), 
    FUN = function(df) {
      df[sample(seq_len(nrow(df)), 10L), ]
    }
  )
  ADD_DELTA <- function(x, element, delta) {
    x[[element]] <- x[[element]] + 0.01
    x
  }
}

test_that("run_rabimo_with_measures(old_version = TRUE) works", {
  
  expect_error(RUN())
  
  for (blocks in DATASETS) {
    
    #blocks <- DATASETS$d2020
    m_max <- as.list(SAFETY_FACTOR * unlist(GET_MAX(blocks)))
    
    expect_no_error(result <- RUN_OLD(blocks, measures = m_max))
    expect_true(all(result$surface_runoff == 0))
    
    expect_error(RUN_OLD(blocks, measures = ADD_DELTA(m_max, "green_roof")))
    expect_error(RUN_OLD(blocks, measures = ADD_DELTA(m_max, "unpaved")))
    expect_error(RUN_OLD(blocks, measures = ADD_DELTA(m_max, "to_swale")))
    
  } # end of for (data in DATASETS)
  
  measures <- list(green_roof = 0.009, to_swale = 0, unpaved = 0.3)
  expect_no_error(RUN_OLD(FEATURES, measures = measures))
  expect_error(RUN_OLD(FEATURES, measures = ADD_DELTA(measures, "green_roof")))

})

test_that("Full connection to swales results in zero runoff", {
  
  run <- function(blocks, measures) {
    kwb.rabimo::run_rabimo_with_measures(
      blocks = blocks, 
      measures = measures, 
      config = kwb.rabimo::rabimo_inputs_2025$config, 
      silent = TRUE
    )
  }
  
  # different versions of sealed = 0.3
  blocks <- kwb.rabimo::generate_rabimo_area(
    code = as.character(1:3), 
    roof = c(0.0, 0.1, 0.2), 
    pvd  = c(0.3, 0.2, 0.1)
  )
  
  measures <- list(green_roof = NA, unpaved = NA, to_swale = 0.3)
  result <- run(blocks, measures)
  expect_true(all(result$runoff == 0))

  # max. green_roof = mean(roof) = 0.1
  # max. unpaved = mean(1 - roof) = 0.9
  m_max <- GET_MAX(blocks)
  # correct max. to_swale
  m_max$to_swale <- GET_MAX(
    APPLY_MEASURES(blocks, global_share_unpaved = m_max$unpaved)
  )$to_swale
  
  measures <- list(green_roof = 0.1, unpaved = 0.9, to_swale = 0.1)
  result <- run(blocks, measures)
  expect_true(all(result$runoff == 0))
  
})
