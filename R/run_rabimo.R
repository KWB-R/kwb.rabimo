# run_rabimo -------------------------------------------------------------------

#' Run R-Abimo, the R-implementation of Water Balance Model Abimo
#'
#' @param input_data data frame with columns as required by Abimo
#' @param config configuration object (list) as returned by
#'   \code{kwb.abimo:::read_config()}
#' @param simulate_abimo logical of length one indicating whether or not to
#'   simulate exactly what Abimo does (including obvious errors!).
#'   Default: \code{TRUE}!
#' @return data frame with columns as returned by Abimo
#' @export
run_rabimo <- function(input_data, config, simulate_abimo = TRUE)
{
  # Prepare input data frame: rename columns, add fraction columns and
  # "usage tuple" columns: "usage", "yield", "irrigation"
  input <- cat_and_run(
    "Preparing input data (e.g. adding usage tuple)",
    prepare_input_data(reset_row_names(input_data))
  )

  # Create accessor functions to input columns and config elements
  fetch_input <- create_accessor(input)
  fetch_config <- create_accessor(config)
  get_fraction <- create_fraction_accessor(input)

  # Prepare precipitation data for all rows
  precipitation <- cat_and_run(
    "Preparing precipitation data for all block areas",
    get_precipitation(
      fetch_input("precipitationYear"),
      fetch_input("precipitationSummer"),
      fetch_config("precipitation_correction_factor")
    )
  )

  # Prepare potential evaporation data for all rows
  pot_evaporation <- cat_and_run(
    "Preparing potential evaporation data for all block areas",
    get_potential_evaporation(
      is_waterbody = (fetch_input("usage") == "waterbody_G"),
      district = fetch_input("district"),
      lookup = fetch_config("potential_evaporation")
    )
  )

  precipitation_per_year <- select_columns(precipitation, "per_year")
  pot_evaporation_per_year <- select_columns(pot_evaporation, "per_year")

  pot_evaporation[["x_ratio"]] <-
    precipitation_per_year / pot_evaporation_per_year

  # Prepare soil properties for all rows. They are required to calculate the
  # actual evapotranspiration of unsealed areas. In the case of water bodies,
  # all values are 0.0. (hsonne: really?)
  soil_properties <- cat_and_run(
    "Preparing soil property data for all block areas",
    expr = get_soil_properties(
      usage = fetch_input("usage"),
      yield = fetch_input("yield"),
      depth_to_water_table = fetch_input("depthToWaterTable"),
      field_capacity_30 = fetch_input("fieldCapacity_30"),
      field_capacity_150 = fetch_input("fieldCapacity_150"),
      default_for_waterbodies = 0,
      dbg = FALSE
    )
  )

  # Precalculate all results of realEvapoTranspiration()
  real_evaporation <- cat_and_run(
    "Precalculating real evapotranspirations for all input combinations",
    expr = fetch_config("bagrov_values") %>%
      lapply(function(x) {
        real_evapo_transpiration(
          potential_evaporation = pot_evaporation_per_year,
          x_ratio = pot_evaporation[["x_ratio"]],
          bagrov_parameter = rep(x, nrow(input)),
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
      usage_tuple = fetch_input(c("usage", "yield", "irrigation")),
      potential_evaporation = pot_evaporation,
      soil_properties = soil_properties,
      precipitation = precipitation,
      min_size_for_parallel = 100L,
      #digits = 3L,
      use_abimo_algorithm = simulate_abimo
    )
  )

  # Provide Bagrov parameters (efficiencies) and runoff coefficients
  runoff_factors <- fetch_config("runoff_factors")

  # Calculate roof related variables
  key_roof <- "Dachflaechen"

  runoff_roof_actual_factor <-
    get_fraction("main/builtSealed/connected") *
    runoff_factors[[key_roof]]

  infiltration_roof_actual_factor <-
    get_fraction("main/builtSealed/!connected")

  # total runoff of roof areas
  # (total runoff, contains both surface runoff and infiltration components)
  runoff_roof <- precipitation_per_year - real_evaporation[[key_roof]]

  # actual runoff from roof surface (area based, with no infiltration)
  runoff_roof_actual <- runoff_roof_actual_factor * runoff_roof

  # actual infiltration from roof surface (area based, with no runoff)
  infiltration_roof_actual <- infiltration_roof_actual_factor * runoff_roof

  unbuilt_surface_fractions <- fetch_input(
    paste0("unbuiltSealedFractionSurface", 1:4)
  )

  road_surface_fractions <- fetch_input(
    paste0("roadSealedFractionSurface", 1:4)
  )

  # -1: remove roof column
  evaporation_sealed <- real_evaporation[, -1L]

  # Calculate runoff for all surface classes at once
  # (contains both surface runoff and infiltration components)
  runoff_sealed <- precipitation_per_year - evaporation_sealed

  # Runoff from the actual partial areas that are sealed and connected
  # (road and non-road) areas (for all surface classes at once)

  # [-1L]: exclude the runoff factor for roofs
  runoff_factor_matrix <- expand_to_matrix(
    x = runoff_factors[-1L],
    nrow = nrow(input)
  )

  runoff_sealed_actual <- runoff_factor_matrix * (
    get_fraction("main/unbuiltSealed/connected") * unbuilt_surface_fractions +
      get_fraction("road/roadSealed/connected") * road_surface_fractions
  ) *
    runoff_sealed

  # infiltration of sealed surfaces (for all surface classes at once)
  infiltration_sealed_actual <- (
    get_fraction("main/unbuiltSealed") * unbuilt_surface_fractions +
      get_fraction("road/roadSealed") * road_surface_fractions
  ) *
    runoff_sealed -
    runoff_sealed_actual

  # Initialise result data frame
  result_data <- data.frame(code = fetch_input("CODE"))

  # Surface-Runoff of unsealed surfaces (unsealedSurface_RUV)
  runoff_unsealed <- precipitation_per_year - evaporation_unsealed

  # Infiltration of road (unsealed areas)
  infiltration_unsealed_roads <-
    get_fraction("road/!roadSealed") *
    # last surface class
    runoff_sealed[, ncol(runoff_sealed)]

  # Infiltration from unsealed non-road surfaces (old: riuv)

  fraction_unsealed <- if (simulate_abimo) {
    #fetch("areaFractionMain") * # ??? TODO: VERIFY THIS ????
    (1 - fetch_input("mainFractionSealed"))
  } else {
    get_fraction("main/!sealed")
  }

  # original C++ code (check if correct):
  # infiltration.unsealedSurfaces = (
  #   100.0F - current_area.mainPercentageSealed()
  # ) / 100.0F * runoff.unsealedSurface_RUV;

  infiltration_unsealed_surfaces <- fraction_unsealed * runoff_unsealed

  # Calculate infiltration rate 'RI' for entire block partial area (mm/a)

  total_infiltration <-
    infiltration_roof_actual +
    infiltration_unsealed_surfaces +
    infiltration_unsealed_roads +
    rowSums(infiltration_sealed_actual)

  result_data[["total_infiltration"]] <- total_infiltration

  # Calculate runoff 'ROW' for entire block area (FLGES + STR_FLGES) (mm/a)

  total_surface_runoff <- runoff_roof_actual +
    #orig.: runoff_unsealed_roads <- was set to zero in the master branch
    rowSums(runoff_sealed_actual)

  result_data[["total_surface_runoff"]] <- total_surface_runoff

  # Calculate "total system losses" 'R' due to runoff and infiltration
  # for entire block partial area
  total_runoff <- total_surface_runoff + total_infiltration

  result_data[["total_runoff"]] <- total_runoff

  # Calculate evaporation 'VERDUNST' by subtracting 'R', the sum of
  # runoff and infiltration from precipitation of entire year,
  # multiplied by precipitation correction factor
  total_evaporation <- precipitation_per_year - total_runoff

  result_data[["total_evaporation"]] <- total_evaporation

  # Helper function to calculate flow, relative to total area
  to_flow <- function(column) {
    yearly_height_to_volume_flow(
      height = select_columns(result_data, column),
      area = select_columns(input, "totalArea")
    )
  }

  # Calculate volume 'rowvol' from runoff (qcm/s)
  result_data[["surface_runoff_flow"]] <- to_flow("total_surface_runoff")

  # Calculate volume 'rivol' from infiltration rate (qcm/s)
  result_data[["infiltration_flow"]] <- to_flow("total_infiltration")

  # Calculate volume of "system losses" 'rvol' due to surface runoff and
  # infiltration
  result_data[["total_runoff_flow"]] <-
    result_data[["surface_runoff_flow"]] +
    result_data[["infiltration_flow"]]

  result_data[["total_area"]] <- select_columns(input, "totalArea")

  # Provide the same columns as Abimo does
  abimo_result <- rename_and_select(
    result_data,
    list(
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
  )

  # Round all columns to three digits (skip first column: "CODE")
  abimo_result[-1L] <- lapply(abimo_result[-1L], round, 3L)

  # Return intermediate results as attributes
  structure(
    abimo_result,
    input = input,
    result_data = result_data,
    intermediates = list(
      precipitation = precipitation,
      pot_evaporation = pot_evaporation,
      soil_properties = soil_properties,
      real_evaporation = real_evaporation,
      evaporation_unsealed = evaporation_unsealed,
      roof = list(
        evaporation_roof = real_evaporation[[key_roof]],
        runoff_roof = runoff_roof,
        runoff_roof_actual = runoff_roof_actual,
        infiltration_roof_actual = infiltration_roof_actual
      ),
      surface = list(
        evaporation_sealed = evaporation_sealed,
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
