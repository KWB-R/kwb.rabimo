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
  
  SAFETY_FACTOR <- 0.9999
  RUN_NEW <- function(...) kwb.rabimo::run_rabimo_with_measures(..., silent = TRUE)
  RUN_OLD <- function(...) RUN_NEW(..., old_version = TRUE)
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
  CORRECT_TO_SWALE_MAX <- function(m, blocks) {
    m$to_swale <- NA
    m$to_swale <- GET_MAX(APPLY_MEASURES(blocks, m))$to_swale
    m
  }
}

test_that("run_rabimo_with_measures(old_version = TRUE) works", {
  
  expect_error(RUN_OLD())
  expect_error(RUN_NEW())
  
  for (blocks in DATASETS) {
    
    #blocks <- DATASETS$d2020
    m_max <- as.list(SAFETY_FACTOR * unlist(GET_MAX(blocks)))
    
    # The maximum values were ok in the old version
    expect_no_error(result <- RUN_OLD(blocks, measures = m_max))
    expect_true(all(result$surface_runoff == 0))

    # Exceeding any maximum value results in an error
    expect_error(RUN_OLD(blocks, measures = ADD_DELTA(m_max, "green_roof")))
    expect_error(RUN_OLD(blocks, measures = ADD_DELTA(m_max, "unpaved")))
    expect_error(RUN_OLD(blocks, measures = ADD_DELTA(m_max, "to_swale")))
    
    # The maximum values lead to an error in the new version because after
    # maximum unpaving there is nothing left to be connected to swales
    expect_error(expect_warning(RUN_NEW(blocks, measures = m_max)))

    # However, we can recalculate the maximum "to_swale"
    expect_no_error(
      result <- RUN_NEW(blocks, measures = CORRECT_TO_SWALE_MAX(m_max, blocks))
    )
    expect_true(all(result$runoff < 0.1))

  } # end of for (data in DATASETS)
  
  measures <- list(green_roof = 0.009, to_swale = 0, unpaved = 0.3)
  expect_no_error(RUN_OLD(FEATURES, measures = measures))
  expect_error(RUN_OLD(FEATURES, measures = ADD_DELTA(measures, "green_roof")))

  expect_no_error(RUN_NEW(FEATURES, measures))
  expect_error(RUN_NEW(FEATURES, ADD_DELTA(measures, "green_roof")))
  
})

test_that("Full connection to swales results in zero runoff", {
  
  CONFIG <- kwb.rabimo::rabimo_inputs_2025$config

  # different versions of sealed = 0.3
  blocks <- kwb.rabimo::generate_rabimo_area(
    code = as.character(1:3), 
    roof = c(0.0, 0.1, 0.2), 
    pvd  = c(0.3, 0.2, 0.1)
  )
  
  check_result <- function(result) {
    expect_true(all(result$runoff == 0))
  }

  measures <- list(green_roof = NA, unpaved = NA, to_swale = 0.3)
  result <- RUN_NEW(blocks, measures, config = CONFIG)
  check_result(result)

  # max. green_roof = mean(roof) = 0.1
  # max. unpaved = mean(1 - roof) = 0.9
  # correct max. to_swale
  m_max <- CORRECT_TO_SWALE_MAX(GET_MAX(blocks), blocks)
  result <- RUN_NEW(blocks, m_max, config = CONFIG)
  check_result(result)
})
