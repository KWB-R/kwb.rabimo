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
  
  #FEATURES
  SAFETY_FACTOR <- 0.9999
  RUN <- function(...) kwb.rabimo::run_rabimo_with_measures(..., silent = TRUE)
  GET_MAX <- function(x) lapply(kwb.rabimo:::get_measure_stats(x), `[[`, "max")
  APPLY_MEASURES <- kwb.rabimo:::apply_measures_to_blocks
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
  
  expect_error(RUN())
  
  sample_size <- 100L
  seeds <- sample(1e10, 5)
  
  for (seed in seeds) {
    
    #seed <- seeds[1L]
    #writeLines(paste("seed:", seed))
    
    DATASETS <- lapply(
      X = list(
        d2020 = kwb.rabimo::rabimo_inputs_2020$data,
        d2025 = kwb.rabimo::rabimo_inputs_2025$data
      ), 
      FUN = function(df) {
        df[sample(seq_len(nrow(df)), sample_size), ]
      }
    )
    
    for (blocks in DATASETS) {
      
      #blocks <- DATASETS$d2025
      m_max_old <- as.list(SAFETY_FACTOR * unlist(GET_MAX(blocks)))
      m_max_new <- CORRECT_TO_SWALE_MAX(m_max_old, blocks)

      # The maximum values lead to an error in the new version because after
      # maximum unpaving there is nothing left to be connected to swales
      expect_error(suppressWarnings(RUN(blocks, measures = m_max_old)))
      
      # However, with the corrected maximum value for "to_swale" it works
      # The new version should not produce runoff with the well-calculated values
      expect_no_error(suppressWarnings(result <- RUN(blocks, measures = m_max_new)))

      expect_true(all(result$runoff == 0))
      #expect_true(all(result$runoff < 0.1))

      # Exceeding any maximum value results in an error
      expect_error(suppressWarnings(RUN(blocks, measures = ADD_DELTA(m_max_new, "green_roof"))))
      expect_error(suppressWarnings(RUN(blocks, measures = ADD_DELTA(m_max_new, "unpaved"))))
      expect_error(suppressWarnings(RUN(blocks, measures = ADD_DELTA(m_max_new, "to_swale"))))

    } # end of for (data in DATASETS)
  }

  # Testing the features that caused problems as reported by Luise
  measures <- list(green_roof = 0.009, to_swale = 0, unpaved = 0.3)
  expect_no_error(RUN(FEATURES, measures = measures))
  expect_error(suppressWarnings(RUN(FEATURES, measures = ADD_DELTA(measures, "green_roof"))))

  expect_no_error(RUN(FEATURES, measures))
  expect_error(suppressWarnings(RUN(FEATURES, ADD_DELTA(measures, "green_roof"))))
})

test_that("Full connection to swales results in zero runoff", {
  
  CONFIG <- kwb.rabimo::rabimo_inputs_2025$config

  # different versions of sealed = 0.3
  blocks <- kwb.rabimo::generate_rabimo_area(
    code = as.character(1:3), 
    roof = c(0.0, 0.1, 0.2), 
    pvd  = c(0.3, 0.2, 0.1)
  )
  
  check_for_no_runoff <- function(result) {
    expect_true(all(result$runoff == 0))
  }

  measures <- list(green_roof = NA, unpaved = NA, to_swale = 0.3)
  result <- RUN(blocks, measures, config = CONFIG)
  check_for_no_runoff(result)

  # max. green_roof = mean(roof) = 0.1
  # max. unpaved = mean(1 - roof) = 0.9
  # correct max. to_swale
  m_max <- CORRECT_TO_SWALE_MAX(GET_MAX(blocks), blocks)
  result <- RUN(blocks, m_max, config = CONFIG)
  check_for_no_runoff(result)
})
