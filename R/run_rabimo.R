# run_rabimo -------------------------------------------------------------------

#' Run R-Abimo, the R-implementation of Water Balance Model Abimo
#'
#' @param data data frame with columns as returned by
#'   \code{\link{prepare_input_data}}
#' @param config configuration object (list) as returned by
#'   \code{\link{abimo_config_to_config}}
#' @param simulate_abimo logical indicating whether or not to simulate exactly
#'   what Abimo does (including obvious errors!).
#'   Default: \code{TRUE}!
#' @return data frame with columns as returned by Abimo
#' @export
run_rabimo <- function(data, config, simulate_abimo = TRUE)
{
  # Provide functions and variables for debugging
  # kwb.utils::assignPackageObjects("kwb.rabimo");simulate_abimo = TRUE

  #
  # Go to inst/extdata/test-rabimo.R to provide data and config for debugging
  #

  # Check whether data has the expected structure
  stop_on_invalid_data(data)

  # Check whether config has the expected structure
  stop_on_invalid_config(config)

  # Create accessor functions to data columns and config elements
  fetch_data <- create_accessor(data)
  fetch_config <- create_accessor(config)

  # Get climate data
  climate <- cat_and_run(
    "Collecting climate related data",
    get_climate(data)
  )

  # Prepare soil properties for all rows. They are required to calculate the
  # actual evapotranspiration of unsealed areas. In the case of water bodies,
  # all values are 0.0. (hsonne: really?)
  soil_properties <- cat_and_run(
    "Preparing soil property data for all block areas",
    expr = get_soil_properties(
      land_type = fetch_data("land_type"),
      veg_class = fetch_data("veg_class"),
      depth_to_water_table = fetch_data("gw_dist"),
      field_capacity_30 = fetch_data("ufc30"),
      field_capacity_150 = fetch_data("ufc150"),
      dbg = FALSE
    )
  )

  # Precalculate all results of realEvapoTranspiration()
  evaporation_sealed <- cat_and_run(
    "Precalculating actual evapotranspirations for impervious areas",
    expr = fetch_config("bagrov_values") %>%
      lapply(function(x) {
        real_evapo_transpiration(
          potential_evaporation = select_columns(climate, "epot_yr"),
          x_ratio = select_columns(climate, "x_ratio"),
          bagrov_parameter = rep(x, nrow(data)),
          use_abimo_algorithm = simulate_abimo
        )
      }) %>%
      do.call(what = data.frame)
  )

  # Precalculate all results of actualEvaporationWaterbodyOrPervious()
  evaporation_unsealed <- cat_and_run(
    paste(
      "Precalculating actual evapotranspirations for waterbodies or pervious",
      "areas"
    ),
    actual_evaporation_waterbody_or_pervious(
      usage_tuple = fetch_data(c("land_type", "veg_class", "irrigation")),
      climate = climate,
      soil_properties = soil_properties,
      min_size_for_parallel = 100L,
      #digits = 3L,
      use_abimo_algorithm = simulate_abimo
    )
  )

  runoff_all <- climate[["prec_yr"]] - cbind(
    evaporation_sealed,
    unsealed = evaporation_unsealed
  )

  # Runoff for all sealed areas (including roofs)

  # Calculate roof related variables

  # total runoff of roof areas
  # (total runoff, contains both surface runoff and infiltration components)
  runoff_roof <- select_columns(runoff_all, "roof")

  # Provide runoff coefficients for impervious surfaces
  runoff_factors <- fetch_config("runoff_factors")

  # actual runoff from roof surface (area based, with no infiltration)
  runoff_roof_actual <- with(data, main_fraction * roof * swg_roof) *
    runoff_factors[["roof"]] * runoff_roof

  # actual infiltration from roof surface (area based, with no runoff)
  infiltration_roof_actual <- with(data, main_fraction * roof * (1-swg_roof)) *
    runoff_roof

  # Calculate runoff for all surface classes at once
  # (contains both surface runoff and infiltration components)

  # choose columns related to surface classes
  runoff_sealed <- filter_elements(runoff_all, "surface")
  # head(runoff_sealed)

  # Runoff from the actual partial areas that are sealed and connected
  # (road and non-road) areas (for all surface classes at once)

  runoff_factor_matrix <- expand_to_matrix(
    x = filter_elements(runoff_factors, "surface"),
    nrow = nrow(data)
  )

  unbuilt_surface_fractions <- fetch_data(paste0("srf", 1:4,"_pvd"))
  road_surface_fractions <- fetch_data(paste0("srf", 1:4,"_pvd_rd"))

  runoff_sealed_actual <-  runoff_sealed * (
    with(data, main_fraction * pvd * swg_pvd) * unbuilt_surface_fractions +
      with(data, road_fraction * pvd_rd * swg_pvd_rd) * road_surface_fractions
  ) *
    runoff_factor_matrix

  # infiltration of sealed surfaces
  # (road and non-road) areas (for all surface classes at once)
  infiltration_sealed_actual <- runoff_sealed * (
    with(data, main_fraction * pvd) * unbuilt_surface_fractions +
      with(data, road_fraction * pvd_rd) * road_surface_fractions) -
    runoff_sealed_actual

  #Total Runoff of unsealed surfaces (unsealedSurface_RUV)
  runoff_unsealed <- climate[["prec_yr"]] - as.numeric(evaporation_unsealed)

  # Infiltration of road (unsealed areas)
  infiltration_unsealed_roads <-
    with(data, road_fraction * (1-pvd_rd)) *
  runoff_sealed[, ncol(runoff_sealed)] # last (less sealed) surface class

  # Infiltration from unsealed non-road surfaces (old: riuv)
  # original C++ code (check if correct):
  # infiltration.unsealedSurfaces = (
  #   100.0F - current_area.mainPercentageSealed()
  # ) / 100.0F * runoff.unsealedSurface_RUV;

  # fraction_unsealed <- if (simulate_abimo) {
  #   #fetch("areaFractionMain") * # ??? TODO: VERIFY THIS ????
  #   (1 - fetch_data("mainFractionSealed"))
  # } else {
  #   get_fraction("main/!sealed")
  # }

  fraction_unsealed <- if(simulate_abimo) {
    with(data, 1 - sealed)
  } else {
    with(data, main_fraction * (1 - sealed))
  }

  infiltration_unsealed_surfaces <- fraction_unsealed * runoff_unsealed

  # Calculate infiltration rate 'RI' for entire block partial area (mm/a)

  total_infiltration <-
    infiltration_roof_actual +
    infiltration_unsealed_surfaces +
    infiltration_unsealed_roads +
    rowSums(infiltration_sealed_actual)

  # Calculate runoff 'ROW' for entire block area (FLGES + STR_FLGES) (mm/a)

  total_surface_runoff <- runoff_roof_actual +
    #orig.: runoff_unsealed_roads <- was set to zero in the master branch
    rowSums(runoff_sealed_actual)

  # Calculate "total system losses" 'R' due to runoff and infiltration
  # for entire block partial area
  total_runoff <- total_surface_runoff + total_infiltration

  # Calculate evaporation 'VERDUNST' by subtracting 'R', the sum of
  # runoff and infiltration from precipitation of entire year,
  # multiplied by precipitation correction factor
  total_evaporation <- climate[["prec_yr"]] - total_runoff

  # Provide total area for calculation of "flows"
  total_area <- fetch_data("total_area")

  # Calculate volume 'rowvol' from runoff (qcm/s)
  surface_runoff_flow <- yearly_height_to_volume_flow(
    total_surface_runoff, total_area
  )

  # Calculate volume 'rivol' from infiltration rate (qcm/s)
  infiltration_flow <- yearly_height_to_volume_flow(
    total_infiltration, total_area
  )

  # Calculate volume of "system losses" 'rvol' due to surface runoff and
  # infiltration
  total_runoff_flow <- surface_runoff_flow + infiltration_flow

  # Provide mapping between local variable names and ABIMO-output columns
  name_mapping <- list(
    code = "CODE",
    total_runoff = "R",
    total_surface_runoff = "ROW",
    total_infiltration = "RI",
    total_runoff_flow = "RVOL",
    surface_runoff_flow = "ROWVOL",
    infiltration_flow = "RIVOL",
    total_area = "FLAECHE",
    total_evaporation = "VERDUNSTUN"
  )

  # Compose result data frame. Use mget() to get the result vectors from the
  # local environment and put them into the data frame
  result_data <- cbind(
    fetch_data("code", drop = FALSE),
    mget(names(name_mapping)[-1L])
  )

  # Provide the same columns as Abimo does
  abimo_result <- rename_and_select(result_data, name_mapping)

  # Round all columns to three digits (skip first column: "CODE")
  abimo_result[-1L] <- lapply(abimo_result[-1L], round, 3L)

  # Return intermediate results as attributes
  structure(
    abimo_result,
    data = data,
    result_data = result_data,
    intermediates = list(
      climate = climate,
      soil_properties = soil_properties,
      evaporation_sealed = evaporation_sealed,
      evaporation_unsealed = evaporation_unsealed,
      roof = list(
        evaporation_roof = evaporation_sealed[["roof"]],
        runoff_roof = runoff_roof,
        runoff_roof_actual = runoff_roof_actual,
        infiltration_roof_actual = infiltration_roof_actual
      ),
      surface = list(
        evaporation_sealed = evaporation_sealed[, -1L],
        runoff_sealed = runoff_sealed,
        runoff_sealed_actual = runoff_sealed_actual,
        infiltration_sealed_actual = infiltration_sealed_actual
      ),
      runoff = cbind(
        unsealedSurface = runoff_unsealed,
        unsealedRoads = runoff_sealed[, ncol(runoff_sealed)],
        sealed = runoff_sealed
      ),
      fraction_unsealed = fraction_unsealed
    )
  )
}

# get_climate: provides climate relevant input data ----------------------------
get_climate <- function(input)
{
  climate <- select_columns(input, c("prec_yr", "prec_s", "epot_yr", "epot_s"))

  climate[["x_ratio"]] <- climate[["prec_yr"]] / climate[["epot_yr"]]

  climate
}

# yearly_height_to_volume_flow -------------------------------------------------

#' Convert Yearly Height (mm) to Volume Flow (unit?)
#'
#' @param height height in mm
#' @param area area in square metres
#' @export
yearly_height_to_volume_flow <- function(height, area)
{
  height * 3.171 * area / 100000.0
}
