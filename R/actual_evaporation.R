# actual_evaporation_waterbody_or_pervious -------------------------------------

#' Calculate Actual Evapotranspiration for Waterbodies or Pervious Areas
#'
#' @param usage_tuple list as returned by \code{\link{get_usage_tuple}}
#' @param climate list with elements \code{epot.year}, \code{epot.summer}
#'   (potential evaporation in mm per year and in the summer period,
#'   respecively), \code{prec.year}, \code{prec.summer} (precipitation in mm
#'   per year and in the summer period, respectively).
#' @param soil_properties list as returned by \code{\link{get_soil_properties}}
#' @param dbg logical indicating whether or not to show debug messages
#' @param \dots further arguments passed to \code{\link{real_evapo_transpiration}}
#'   such as \code{run_parallel}, \code{blocksize}
#' @param digits optional. If given, the BAGROV parameter values are rounded to
#'   this number of digits. This reduces the number of BAGROV curves that need
#'   to be calculated and thus improves the performance (by reducing the
#'   precision of the output)
#' @export
actual_evaporation_waterbody_or_pervious <- function(
    usage_tuple,
    climate,
    soil_properties,
    dbg = TRUE,
    ...,
    digits = NULL
)
{
  ep_year <- select_elements(climate, "epot.year")

  # Initialise result vector
  y <- numeric(length = length(ep_year))

  # for water bodies return the potential evaporation
  # ??? in this test version not implemented ???
  # TODO: Check with Francesco
  usages <- select_elements(usage_tuple, "usage")
  is_water <- (usages == "waterbody_G")

  y[is_water] <- ep_year[is_water]

  # if all block areas are waterbodies, return
  if (all(is_water)) {
    return(y)
  }

  # indices of entries related to any other usage
  i <- which(!is_water)

  # otherwise calculate the real evapotranspiration
  stopifnot(all(ep_year[i] > 0)) # ???

  # determine the BAGROV parameter(s) for unsealed surfaces
  bagrov_parameter <- get_bagrov_parameter_unsealed(
    g02 = select_elements(soil_properties, "g02")[i],
    usage = usages[i],
    yield = select_elements(usage_tuple, "yield")[i],
    irrigation = select_elements(usage_tuple, "irrigation")[i],
    prec_summer = select_elements(climate, "prec.summer")[i],
    epot_summer = select_elements(climate, "epot.summer")[i],
    mean_potential_capillary_rise_rate = select_elements(
      soil_properties, "mean_potential_capillary_rise_rate"
    )[i]
  )

  if (!is.null(digits)) {
    bagrov_parameter <- cat_and_run(
      sprintf("Rounding BAGROV parameters to %d digits", digits),
      round(bagrov_parameter, digits)
    )
  }

  cat_if(dbg, sprintf(
    "Range of calculated %sn-value(s): %s\n",
    ifelse(is.null(digits), "", "and rounded "),
    paste(range(bagrov_parameter), collapse = " - ")
  ))

  y[i] <- real_evapo_transpiration(
    potential_evaporation = ep_year[i],
    x_ratio = (
      select_elements(climate, "prec.year")[i] +
        select_elements(soil_properties, "mean_potential_capillary_rise_rate")[i] +
        select_elements(usage_tuple, "irrigation")[i]
    ) / ep_year[i],
    bagrov_parameter = bagrov_parameter,
    ...
  )

  rises <- select_elements(soil_properties, "potential_capillary_rise_TAS")
  depths <- select_elements(soil_properties, "depth_to_water_table")

  # indices of entries related to non-water usage and capillary rises < 0
  j <- which(!is_water & rises < 0)

  y[j] <- y[j] + (ep_year[j] - y[j]) * exp(depths[j] / rises[j])

  nas <- rep(NA_real_, length(y))

  structure(y, bagrovUnsealed = data.frame(
    bagrovEff = `[<-`(nas, i, bagrov_parameter),
    factor_dry = `[<-`(nas, i, get_attribute(bagrov_parameter, "factor_dry")),
    factor_wet = `[<-`(nas, i, get_attribute(bagrov_parameter, "factor_wet"))
  ))
}

# get_bagrov_parameter_unsealed (C++ name: getEffectivityParameter) ------------
get_bagrov_parameter_unsealed <- function(
    g02,
    usage,
    yield,
    irrigation,
    prec_summer,
    epot_summer,
    mean_potential_capillary_rise_rate
)
{
  # Initialise result vector
  y <- numeric(length = length(g02))

  is_forest <- (usage == "forested_W")
  no_forest <- !is_forest

  y[is_forest] <- lookup_bagrov_forest(g02[is_forest])

  factor_dry <- ifelse(
    test = irrigation > 0 & isDrySummer(
      prec_summer,
      epot_summer
    ),
    yes = irrigation_in_dry_summer_correction_factor(irrigation[no_forest]),
    no = 1
  )

  y[no_forest] <- lookup_bagrov_unsealed(g02[no_forest], yield[no_forest]) *
    factor_dry[no_forest]

  # in case of a "wet" summer, correct the BAGROV parameter with a factor
  factor_wet <- ifelse(
    test = is_wet_summer(prec_summer, epot_summer),
    yes = wet_summer_correction_factor(
      water_availability =
        prec_summer +
        irrigation +
        mean_potential_capillary_rise_rate,
      epot_summer = epot_summer
    ),
    no = 1
  )

  structure(
    y * factor_wet,
    factor_dry = factor_dry,
    factor_wet = factor_wet
  )
}

# lookup_bagrov_forest ---------------------------------------------------------
lookup_bagrov_forest <- function(g02)
{
  n <- length(g02)

  if (n == 0L) {
    return(numeric(0))
  }

  breaks <- c(-Inf, 10.0, 25.0, Inf)
  values <- c(3.0,  4.0,  8.0)

  index <- if (n > 1L) {
    findInterval(g02, breaks, left.open = TRUE)
  } else if (g02 <= breaks[2L]) {
    1L
  } else if (g02 <= breaks[3L]) {
    2L
  } else {
    3L
  }

  values[index]
}

# lookup_bagrov_unsealed -------------------------------------------------------
lookup_bagrov_unsealed <- function(g02, yield, do_correction = TRUE)
{
  # Calculate the k index (integer)
  k <- yield_to_k_index(yield)

  # Calculate result based on the k index
  y <-
    BAGROV_COEFFICIENTS[k] +
    BAGROV_COEFFICIENTS[k + 1L] * g02 +
    BAGROV_COEFFICIENTS[k + 2L] * g02^2

  # Return y if no correction is required
  if (!do_correction) {
    return(y)
  }

  # Apply correction where needed
  i <- which(
    (y >= 2.0 & yield < 60) |
    (g02 >= 20.0 & yield >= 60)
  )

  y[i] <-
    BAGROV_COEFFICIENTS[k[i] - 2L] * g02[i] +
    BAGROV_COEFFICIENTS[k[i] - 1L]

  y
}

# yield_to_k_index -------------------------------------------------------------
yield_to_k_index <- function(yield)
{
  k <- as.integer(ifelse(yield < 50, yield / 5, yield / 10 + 5))

  # make sure that k is at least 1
  k <- pmax(1L, k)

  # if k is at least 4, reduce it by one
  selected <- k >= 4L
  k[selected] <- k[selected] - 1L

  5L * pmin(k, 13L) - 2L
}

# BAGROV_COEFFICIENTS ----------------------------------------------------------

# Coefficients for linear or squared equations used to calculate the BAGROV
# parameters
BAGROV_COEFFICIENTS <- c(
  0.04176, -0.647 , 0.218  ,  0.01472, 0.0002089,
  0.04594, -0.314 , 0.417  ,  0.02463, 0.0001143,
  0.05177, -0.010 , 0.596  ,  0.02656, 0.0002786,
  0.05693,  0.033 , 0.676  ,  0.0279 , 0.00035  ,
  0.06162,  0.176 , 0.773  ,  0.02809, 0.0004695,
  0.06962,  0.24  , 0.904  ,  0.02562, 0.0007149,
  0.0796 ,  0.31  , 1.039  ,  0.0288 , 0.0008696,
  0.07998,  0.7603, 1.2    ,  0.0471 , 0.000293 ,
  0.08762,  1.019 , 1.373  ,  0.04099, 0.0014141,
  0.11833,  1.1334, 1.95   ,  0.0525 , 0.00125  ,
  0.155  ,  1.5   , 2.64999,  0.0725 , 0.001249 ,
  0.20041,  2.0918, 3.69999,  0.08   , 0.001999 ,
  0.33895,  3.721 , 6.69999, -0.07   , 0.013
)

# isDrySummer ------------------------------------------------------------------
# TODO: Remove redundancy with is_wet_summer.
# Variables are (almost!) one another's opposite!
isDrySummer <- function(prec_summer, epot_summer)
{
  prec_summer <= 0 & epot_summer <= 0
}

# irrigation_in_dry_summer_correction_factor -----------------------------------
irrigation_in_dry_summer_correction_factor <- function(irrigation)
{
  0.9985 + 0.00284 * irrigation - 0.00000379762 * irrigation^2
}

# is_wet_summer ----------------------------------------------------------------
# TODO: Remove redundancy with isDrySummer.
# Variables are (almost!) one another's opposite!
is_wet_summer <- function(prec_summer, epot_summer)
{
  prec_summer > 0 & epot_summer > 0
}

# wet_summer_correction_factor -------------------------------------------------
wet_summer_correction_factor <- function(
    water_availability, epot_summer, use_abimo_approx = TRUE
)
{
  xout <- water_availability / epot_summer
  x <- WET_SUMMER_CORRECTION_MATRIX[, "water_availability"]
  y <- WET_SUMMER_CORRECTION_MATRIX[, "correction_factor"]

  if (use_abimo_approx) {
    interpolate(x = x, y = y, xout = xout)
  } else {
    select_columns(stats::approx(x = x, y = y, xout = xout, rule = 2L), "y")
  }
}

# WET_SUMMER_CORRECTION_MATRIX -------------------------------------------------
WET_SUMMER_CORRECTION_MATRIX <- matrix(
  ncol = 2L,
  byrow = TRUE,
  dimnames = list(
    NULL,
    c("water_availability", "correction_factor")
  ),
  data = c(
    0.45, 0.65,
    0.50, 0.75,
    0.55, 0.82,
    0.60, 0.90,
    0.65, 1.00,
    0.70, 1.06,
    0.75, 1.15,
    0.80, 1.22,
    0.85, 1.30,
    0.90, 1.38,
    0.95, 1.47,
    1.00, 1.55,
    1.05, 1.63,
    1.10, 1.70
  )
)
