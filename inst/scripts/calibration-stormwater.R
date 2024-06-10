# Calibration script to set stormwater-management parameters
# based on the WABILA equations
kwb.utils::assignPackageObjects("kwb.rabimo");`%>%` <- magrittr::`%>%`

# import config object
config <- kwb.rabimo::rabimo_inputs_2020$config

# Method:
#   - generate test areas
#   - model them in Wabila
#   - model them in Rabimo, compare results and assess differences

# main -------------------------------------------------------------------------
if (FALSE) {

  # CASE 1) simple roof area, 100% connected to the sewer ~~~~~~~~~~~~~~~~~~~~~~
  simple_roof_area <- generate_rabimo_area(
    code = "only_roof", roof = 1, pvd = 0
  )

  wabila_roof <- calculate_wabila_roof(
    area = simple_roof_area, retention_height = 0.5
  )

  factor_simple_roof <- get_area_balance(area = simple_roof_area,
                                         config = config,
                                         wabila_results = wabila_roof)
  factor_simple_roof
  calculate_delta_mod(factor_simple_roof["rabimo",],
                      factor_simple_roof["wabila",], has_codes = FALSE)

  # CASE 2) green roof area: the whole roof is green ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  green_roof_area <- generate_rabimo_area(
    code = "only_green_roof", roof = 1, green_roof = 1, pvd = 0)

  wabila_green_roof <- calculate_wabila_green_roof(
    area = green_roof_area, height = 100, kf = 70, w_diff = 0.5)


  factor_green_roof <- get_area_balance(area = green_roof_area,
                                        config = config,
                                        wabila_results = wabila_green_roof)

  # case 3) simple paved area, no roof, no unsealed surface ~~~~~~~~~~~~~~~~~~~~
  paved_area <- generate_rabimo_area(
    code = "all_paved", roof = 0, pvd = 1, swg_pvd = 0.38,
    srf3_pvd = 0.1, srf4_pvd = 0.2)

  wabila_paved <- calculate_wabila_paved(
    area = paved_area, joint_perc = 8, retention = 1, kf = 36, w_diff = 0.15)

  factors_paved_area <- get_area_balance(area = paved_area,
                                         config = config,
                                         wabila_results = wabila_paved)

  # CASE 4) only garden area, non constructed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  only_garden_area <- generate_rabimo_area(code ="garden", roof = 0, pvd = 0,
                                           veg_class = 20)

  wabila_garden <- c(0.0, 0.4, 0.6)

  factors_only_garden <- get_area_balance(area = only_garden_area,
                                          config = config,
                                          wabila_results = wabila_garden)

  # case 5) mixed roof/garden area ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 50% roof and 50% garden. The runoff from the roof goes into the sewer.
  mixed_roof_garden_area <- generate_rabimo_area(code = "roof_and_garden",
                                                 roof = 0.5, pvd = 0,
                                                 veg_class = 20)

  roof_fraction <- mixed_roof_garden_area$roof
  wabila_mix_roof_garden <- wabila_roof * roof_fraction +
    wabila_garden * (1 - roof_fraction)

  factors_mixed_garden <- get_area_balance(area = mixed_roof_garden_area,
                                           config = config,
                                           wabila_results = wabila_mix_roof_garden)

  # case 6) area connected to an infiltration swale ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  area_to_swale <- generate_rabimo_area(code = "roof_to_swale",
                                        roof = 0.5, pvd = 0, to_swale = 1,
                                        veg_class = 20)
  # problem: "free" area for the swale must be taken into account
  # when comparing results with wabila simulation

  # Wabila simulation:
  # calculate first water-balance from roof area (how?)
  discharge_into_swale <- mixed_roof_garden_area$prec_yr *
    factors_mixed_garden["rabimo","runoff"]

  wabila_swale <- calculate_wabila_swale(
    area = area_to_swale, kf = 14)

  roof_fraction <- area_to_swale$roof
  swale_fraction <- attr(wabila_swale, "estimated swale area")*area_to_swale$roof
  garden_fraction <- 1 - roof_fraction - swale_fraction

  wabila_swale_roof_garden <- c(
    a = swale_fraction * wabila_swale[1] + garden_fraction * wabila_garden[1],
    g = roof_fraction * (wabila_roof[1] + wabila_roof[2]) + garden_fraction * wabila_garden[2] + swale_fraction * wabila_swale[2],
    v = roof_fraction * wabila_roof[3] + swale_fraction * wabila_swale[3] + garden_fraction * wabila_garden[3]
  )

  # modify config to match wabila simulation
  config$swale["swale_evaporation_factor"] <- 0.05

  factors_roof_to_swale <- get_area_balance(area = area_to_swale,
                                            config = config,
                                            wabila_results = wabila_swale_roof_garden)
}

# get_area_balance -------------------------------------------------------------
get_area_balance <- function(
    area,
    config,
    rabimo_results = NULL,
    wabila_results = NULL,
    result_cols = c("surface_runoff", "infiltration", "evaporation"),
    ...
)
{
  results <- if (is.null(rabimo_results)) {
    run_rabimo(
      data = area,
      config = config,
      controls = kwb.rabimo::define_controls(...)
    )
  } else {
    rabimo_results
  }

  results <- results %>%
    select_columns(result_cols)

  precipitation <- area[["prec_yr"]]

  factors <- results %>%
    magrittr::divide_by(precipitation) %>%
    setNames(c("surface_runoff","infiltration","evaporation")) %>%
    as.matrix() %>%
    magrittr::set_rownames("rabimo")

  if (length(wabila_results) == 3){
    factors <- factors %>%
      rbind(wabila_results) %>%
      magrittr::set_rownames(c("rabimo", "wabila"))
  }

  return(factors)
}

# TESTS for calculate_delta_mod()
# vectors
# test_res_1a <- c(surface_runoff = 2, infiltration = 10, evaporation = 8)
# test_res_1b <- c(surface_runoff = 10, infiltration = 0, evaporation = 10)
#
# # matrices
# water_balance_vars = c("surface_runoff", "infiltration", "evaporation")
# test_res_2a <- matrix(c(30, 20, 60, 50, 10, 30), nrow = 2, dimnames = list(NULL, water_balance_vars))
# test_res_2b <- matrix(c(60, 20, 20, 80, 20, 0), nrow = 2, dimnames = list(NULL, water_balance_vars))
#
# # data frames
# test_res_3a <- data.frame(surface_runoff = c(40,0,33), infiltration = c(20,0,33),  evaporation = c(40,100,34))
# test_res_3b <-  data.frame(surface_runoff = c(50,100,100), infiltration = c(30,0,0),  evaporation = c(20,0,0))
#
#
# # data frames with codes
# test_res_4a <- cbind(test_res_3a, code = c("area1", "area2", "area3"))
# test_res_4b <- cbind(test_res_3b, code = c("area1", "area2", "area3"))
#
#
# calculate_delta_mod(test_res_1a, test_res_1b, has_codes = F)
# calculate_delta_mod(test_res_2a, test_res_2b, has_codes = F)
# calculate_delta_mod(test_res_3a, test_res_3b, has_codes = F)
# calculate_delta_mod(test_res_4a, test_res_4b, has_codes = T)

