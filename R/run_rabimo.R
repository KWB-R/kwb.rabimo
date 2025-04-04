# run_rabimo -------------------------------------------------------------------

#' Run R-Abimo, the R-implementation of Water Balance Model Abimo
#'
#' @param data data frame similar to
#'   \code{\link{rabimo_inputs_2025}$data}
#' @param config configuration object (list) similar to
#'   \code{\link{rabimo_inputs_2025}$config}
#' @param controls list of settings that control how the function should behave.
#'   Use \code{\link{define_controls}} to define such a list. The default is
#'   the list returned by \code{define_controls()}.
#' @return data frame with columns as returned by Abimo
#' @export
#' @examples
#' # Get input data and config for Berlin (version 2020)
#' inputs_2020 <- kwb.rabimo::rabimo_inputs_2020
#'
#' # Randomly select 1000 blocks (to reduce runtime)
#' data <- inputs_2020$data
#' data <- data[sample(seq_len(nrow(data)), size = 1000L), ]
#'
#' # Run R-Abimo
#' results_2020 <- kwb.rabimo::run_rabimo(data, inputs_2020$config)
#'
#' # Get input data and config for Berlin (version 2025)
#' inputs_2025 <- kwb.rabimo::rabimo_inputs_2025
#'
#' # Randomly select 1000 blocks (to reduce runtime)
#' data <- inputs_2025$data
#' data <- data[sample(seq_len(nrow(data)), size = 1000L), ]
#'
#' # Run R-Abimo
#' results_2025 <- kwb.rabimo::run_rabimo(data, inputs_2025$config)
run_rabimo <- function(data, config, controls = define_controls())
{
  # Provide functions and variables for debugging
  # (Go to inst/scripts/test-rabimo.R to provide data and config for debugging)
  if (FALSE)
  {
    kwb.utils::assignPackageObjects("kwb.rabimo")
    inputs <- kwb.utils:::get_cached("rabimo_inputs_2020")
    data <- inputs$data
    config <- inputs$config
    controls <- define_controls()
    `%>%` <- magrittr::`%>%`
  }

  # If road-area-specific columns are missing, create them
  data <- handle_missing_columns(data)

  # Provide function to access the list of controls
  control <- create_accessor(controls)

  # Check whether data and config have the expected structures
  if (isTRUE(control("check"))) {
    stop_on_invalid_data(data)
    stop_on_invalid_config(config)
  }

  # Get climate data
  climate <- cat_and_run(
    "Collecting climate related data",
    get_climate(data)
  )

  # Create accessor functions to data columns and config elements
  fetch_data <- create_accessor(data)
  fetch_config <- create_accessor(config)
  fetch_climate <- create_accessor(climate)

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

  # Pre-calculate all results of realEvapoTranspiration()
  evaporation_sealed <- cat_and_run(
    "Precalculating actual evapotranspirations for impervious areas",
    expr = fetch_config("bagrov_values") %>%
      lapply(function(x) {
        real_evapo_transpiration(
          potential_evaporation = fetch_climate("epot_yr"),
          x_ratio = fetch_climate("x_ratio"),
          bagrov_parameter = rep(x, nrow(data)),
          use_abimo_algorithm = control("use_abimo_bagrov_solver")
        )
      }) %>%
      do.call(what = data.frame)
  )

  # Pre-calculate all results of actualEvaporationWaterbodyOrPervious()
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
      use_abimo_algorithm = control("use_abimo_bagrov_solver")
    )
  )

  runoff_all <- fetch_climate("prec_yr") - cbind(
    evaporation_sealed,
    unsealed = evaporation_unsealed
  )

  # Runoff for all sealed areas (including roofs)

  # Calculate roof related variables

  # total runoff of roof areas
  # (total runoff, contains both surface runoff and infiltration components)
  runoff_roof <- select_columns(runoff_all, "roof")
  runoff_green_roof <- select_columns(runoff_all, "green_roof")

  # Provide runoff coefficients for impervious surfaces
  runoff_factors <- fetch_config("runoff_factors")

  # actual runoff from roof surface (area based, with no infiltration)
  runoff_roof_actual <- with(data, main_frac *
                               roof * (1 - green_roof) * swg_roof) *
    runoff_factors[["roof"]] * runoff_roof

  # actual runoff from green roof surface (area based, with no infiltration)
  runoff_green_roof_actual <- with(data, main_frac *
                                     roof * green_roof * swg_roof) *
    runoff_factors[["roof"]] * runoff_green_roof

  # actual infiltration from roof surface (area based, with no runoff)
  infiltration_roof_actual <- with(data, main_frac * roof *
                                     (1-green_roof) * (1-swg_roof)) *
    runoff_roof

  # actual infiltration from green_roof surface (area based, with no runoff)
  infiltration_green_roof_actual <- with(data, main_frac * roof *
                                           green_roof * (1-swg_roof)) *
    runoff_green_roof


  # Calculate runoff for all surface classes at once
  # (contains both surface runoff and infiltration components)

  # Identify active surface class columns in input data
  surface_cols_no_rd <- matching_names(data, pattern_no_roads())
  surface_cols_rd <- matching_names(data, pattern_roads())
  digits <- gsub("\\D", "", surface_cols_no_rd)
  surface_class_names <- paste0("surface",digits)

  # choose columns related to surface classes
  runoff_sealed <- select_columns(runoff_all, surface_class_names)
  # head(runoff_sealed)

  # Runoff from the actual partial areas that are sealed and connected
  # (road and non-road) areas (for all surface classes at once)

  runoff_factor_matrix <- expand_to_matrix(
    x = runoff_factors[surface_class_names],
    nrow = nrow(data)
  )

  unbuilt_surface_fractions <- fetch_data(surface_cols_no_rd)
  road_surface_fractions <- fetch_data(surface_cols_rd)

  # add an empty column in road_surface_fraction to match dimension if needed
  if (!identical(length(surface_cols_no_rd), length(surface_cols_rd))) {
    road_surface_fractions$srf5_pvd_r <- 0
  }

  runoff_sealed_actual <-  runoff_sealed * (
    with(data, main_frac * pvd * swg_pvd) * unbuilt_surface_fractions +
      with(data, road_frac * pvd_r * swg_pvd_r) * road_surface_fractions
  ) *
    runoff_factor_matrix

  # infiltration of sealed surfaces
  # (road and non-road) areas (for all surface classes at once)
  infiltration_sealed_actual <- runoff_sealed * (
    with(data, main_frac * pvd) * unbuilt_surface_fractions +
      with(data, road_frac * pvd_r) * road_surface_fractions) -
    runoff_sealed_actual

  # Total Runoff of unsealed surfaces (unsealedSurface_RUV)
  runoff_unsealed <- fetch_climate("prec_yr") - as.numeric(evaporation_unsealed) # why as.numeric()?

  # Infiltration of road (unsealed areas)
  infiltration_unsealed_roads <-
    with(data, road_frac * (1 - pvd_r)) *
    runoff_sealed[, ncol(runoff_sealed)] # last (less sealed) surface class

  fraction_unsealed <- with(
    data,
    ifelse(control("reproduce_abimo_error"), 1, main_frac) * (1 - (roof + pvd))
  )

  infiltration_unsealed_surfaces <- fraction_unsealed * runoff_unsealed

  # Calculate runoff 'ROW' for entire block area (FLGES + STR_FLGES) (mm/a)
  total_surface_runoff <- (
    runoff_roof_actual + runoff_green_roof_actual +
      #orig.: runoff_unsealed_roads <- was set to zero in the master branch
      rowSums(runoff_sealed_actual))

  # Calculate infiltration rate 'RI' for entire block partial area (mm/a)
  total_infiltration <-
    (infiltration_roof_actual +
       infiltration_green_roof_actual +
       infiltration_unsealed_surfaces +
       infiltration_unsealed_roads +
       rowSums(infiltration_sealed_actual))

  # Correct Surface Runoff and Infiltration if area has an infiltration swale
  swale_delta <- total_surface_runoff * (fetch_data("to_swale"))
  total_surface_runoff <- total_surface_runoff - swale_delta
  total_infiltration <- total_infiltration +
    swale_delta * (1 - fetch_config("swale")[["swale_evaporation_factor"]])

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
  result_data_raw <- cbind(
    fetch_data("code", drop = FALSE),
    mget(names(name_mapping)[-1L])
  )

  output_format <- control("output_format")

  result_data <- if (output_format == "abimo") {

    # Provide the same columns as Abimo does
    rename_columns(result_data_raw, name_mapping)

  } else if (output_format == "rabimo") {

    data.frame(
      code = result_data_raw$code,
      area = result_data_raw$total_area,
      runoff = result_data_raw$total_surface_runoff,
      infiltr = result_data_raw$total_infiltration,
      evapor = result_data_raw$total_evaporation
    )

  } else {

    clean_stop("controls$output_format must be either 'abimo' or 'rabimo'.")
  }

  # Round all columns to three digits (skip first column: "CODE")
  result_data[-1L] <- lapply(result_data[-1L], round, 3L)

  if (isFALSE(control("intermediates"))) {
    return(result_data)
  }

  # Return intermediate results as attributes
  structure(
    result_data,
    data = data,
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

# handle_missing_columns -------------------------------------------------------
handle_missing_columns <- function(data)
{
  road_specific_columns <- c(
    "road_frac", "pvd_r", "swg_pvd_r",
    "srf1_pvd_r", "srf2_pvd_r", "srf3_pvd_r", "srf4_pvd_r"
  )

  missing_road_columns <- setdiff(road_specific_columns, names(data))

  if (length(missing_road_columns)) {
    for (column in missing_road_columns) {
      data[[column]] <- 0
    }
  }

  if (! "main_frac" %in% names(data)) {
    data$main_frac <- 1
  }

  data
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

#' Define List of "Controls"
#'
#' Define a list of settings that control how the main function
#' \code{\link{run_rabimo}} should behave.
#'
#' @param check logical indicating whether the check functions are executed.
#'   Default: \code{TRUE}.
#' @param use_abimo_bagrov_solver logical indicating whether or not to use the
#'   (fast!) algorithm implemented in Abimo to solve the Bagrov equations.
#'   Default: \code{TRUE}.
#' @param reproduce_abimo_error logical indicating whether or not to reproduce
#'   the error that is contained in Abimo (missing area fraction factor).
#'   Default: \code{FALSE}.
#' @param output_format one of "abimo" (upper case columns: CODE, R, ROW, RI,
#'   RVOL, ROWVOL, RIVOL, FLAECHE, VERDUNSTUN), "rabimo" (lower case columns:
#'   code, surface_runoff, infiltration, evaporation).
#' @param intermediates logical indicating whether the intermediate results are
#'   returned as attributes. Default: \code{FALSE}.
#' @returns list with the arguments of this function as list elements
#' @export
#' @examples
#' inputs <- kwb.rabimo::rabimo_inputs_2020
#' result <- run_rabimo(inputs$data, inputs$config)
#' colMeans(as.matrix(result[, -1L]))
define_controls <- function(
    check = TRUE,
    use_abimo_bagrov_solver = TRUE,
    reproduce_abimo_error = FALSE,
    output_format = "rabimo",
    intermediates = FALSE
)
{
  list(
    check = check,
    use_abimo_bagrov_solver = use_abimo_bagrov_solver,
    reproduce_abimo_error = reproduce_abimo_error,
    output_format = output_format,
    intermediates = intermediates
  )
}
