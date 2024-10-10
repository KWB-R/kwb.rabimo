# get_soil_properties ----------------------------------------------------------

#' Calculate Soil Properties
#'
#' Provide variables that are relevant to calculate the actual evaporation for
#' unsealed areas
#'
#' @param land_type land_type string, one of "vegetationless", "waterbody",
#'   "horticultural", "urban", "forested"
#' @param veg_class vegetation class
#' @param depth_to_water_table depth to water table
#' @param field_capacity_30 field capacity in 30 cm depth
#' @param field_capacity_150 field capacity in 150 cm depth
#' @param dbg logical indicating whether or not to show debug messages
#' @export
get_soil_properties <- function(
    land_type,
    veg_class,
    depth_to_water_table,
    field_capacity_30,
    field_capacity_150,
    dbg = FALSE
)
{
  # Nothing to do for waterbodies
  is_waterbody <- land_type_is_waterbody(land_type)

  # Feldkapazitaet
  usable_field_capacity <- ifelse(
    test = is_waterbody,
    yes = 0,
    no = estimate_water_holding_capacity(
      f30 = field_capacity_30,
      f150 = field_capacity_150,
      is_forest = land_type_is_forest(land_type)
    )
  )

  # pot. Aufstiegshoehe TAS = FLUR - mittl. Durchwurzelungstiefe TWS
  # potentielle Aufstiegshoehe
  potential_capillary_rise <- ifelse(
    test = is_waterbody,
    yes = 0,
    no = depth_to_water_table - get_rooting_depth(land_type, veg_class)
  )

  # mittlere pot. kapillare Aufstiegsrate kr (mm/d) des Sommerhalbjahres
  # Kapillarer Aufstieg pro Jahr ID_KR neu, old: KR
  mean_potential_capillary_rise_rate_raw <- ifelse(
    test = is_waterbody,
    yes = 0,
    no = get_mean_potential_capillary_rise_rate(
      potential_capillary_rise,
      usable_field_capacity,
      days_of_growth = estimate_days_of_growth(land_type, veg_class),
      dbg = dbg
    )
  )

  # Make sure that e.g. 14.999999991 becomes 15, not 14
  mean_potential_capillary_rise_rate <- as.integer(round(
    mean_potential_capillary_rise_rate_raw,
    digits = 12L
  ))

  # Add G02 values as they depend only on the usable field capacity
  g02 <- lookup_g02(usable_field_capacity)

  # Return all result vectors in a data frame
  data.frame(
    depth_to_water_table = depth_to_water_table,
    usable_field_capacity = usable_field_capacity,
    potential_capillary_rise = potential_capillary_rise,
    mean_potential_capillary_rise_rate_raw = mean_potential_capillary_rise_rate_raw,
    mean_potential_capillary_rise_rate = mean_potential_capillary_rise_rate,
    g02 = g02
  )
}

# estimate_water_holding_capacity ----------------------------------------------
estimate_water_holding_capacity <- function(f30, f150, is_forest)
{
  n <- length(f30)

  stopifnot(length(f150) == n)
  stopifnot(length(is_forest) == n)

  # Initialise result vector
  y <- rep(NA_real_, n)

  # Smaller value of f30, f150 at each index
  min_capacity <- pmin(f30, f150)

  # Special case 1: smaller value below 1
  todo <- min_capacity < 1
  y[todo] <- 13.0

  # Special case 2: minor difference
  todo <- !todo & abs(f30 - f150) < min_capacity
  y[todo] <- as.double(ifelse(is_forest[todo], f150[todo], f30[todo]))

  # Default result for the non-special cases
  todo <- is.na(y)

  y[todo] <- ifelse(is_forest[todo],
    0.25 * f30[todo] + 0.75 * f150[todo],
    0.75 * f30[todo] + 0.25 * f150[todo]
  )

  y
}

# get_rooting_depth ------------------------------------------------------------
get_rooting_depth <- function(land_type, veg_class)
{
  n <- length(land_type)
  stopifnot(length(veg_class) == n)

  y <- rep(NA_real_, n)

  is_urban <- land_type_is_urban(land_type)
  y[is_urban] <- ifelse(veg_class[is_urban] <= 50, 0.6, 0.7)
  y[land_type_is_vegetationless(land_type)] <- 0.2
  y[land_type_is_horticultural(land_type)] <- 0.7
  y[land_type_is_forest(land_type)] <- 1.0

  # in any other case
  y[is.na(y)] <- 0.2

  y
}

# getRootingDepth_1 ------------------------------------------------------------
getRootingDepth_1 <- function(land_type, veg_class)
{
  stopifnot(length(land_type) == 1L)
  stopifnot(length(veg_class) == 1L)

  if (land_type_is_urban(land_type)) {
    return(ifelse(veg_class <= 50, 0.6, 0.7))
  }

  if (land_type_is_vegetationless(land_type)) {
    return(0.2)
  }

  if (land_type_is_horticultural(land_type)) {
    return(0.7)
  }

  if (land_type_is_forest(land_type)) {
    return(1.0)
  }

  # in any other case
  0.2
}

# get_mean_potential_capillary_rise_rate ---------------------------------------
get_mean_potential_capillary_rise_rate <- function(
    potential_capillary_rise,
    usable_field_capacity,
    days_of_growth,
    dbg = FALSE
)
{
  # potential_capillary_rise <- 0.39
  # usable_field_capacity <- 8.2

  n <- length(potential_capillary_rise)
  stopifnot(length(usable_field_capacity) == n)
  stopifnot(length(days_of_growth) == n)

  M <- MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX

  i <- helpers_index(usable_field_capacity, attr(M, "row_values"), dbg = dbg)
  j <- helpers_index(potential_capillary_rise, attr(M, "col_values"), dbg = dbg)

  indices <- cbind(i, j) + 1L

  kr <- ifelse(potential_capillary_rise <= 0.0, 7.0, M[indices])

  #as.integer(round(days_of_growth * kr))
  days_of_growth * kr
}

# MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX ----------------------------
MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX <- local({

  row_values <- c(
    8.0, 9.0, 14.0, 14.5, 15.5, 17.0, 20.5
  )

  col_values <- c(
    0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.7, 2.0, 2.3
  )

  result <- matrix(
    data = c(
      7.0, 6.0, 5.0, 1.5, 0.5, 0.2, 0.1, 0.0, 0.0, 0.0 , 0.0 , 0.0, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 5.0, 3.0, 1.2, 0.5, 0.2, 0.1, 0.0 , 0.0 , 0.0, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 3.0, 1.5, 0.7, 0.3, 0.15, 0.1 , 0.0, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 3.0, 2.0, 1.0, 0.7, 0.4 , 0.15, 0.1, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 4.5, 2.5, 1.5, 0.7, 0.4 , 0.15, 0.1, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 5.0, 3.5, 2.0, 1.5, 0.8 , 0.3 , 0.1, 0.05, 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 6.0, 5.0, 5.0, 5.0, 3.0, 2.0 , 1.0 , 0.5, 0.15, 0.0, 0.0
    ),
    ncol = length(col_values),
    byrow = TRUE,
    dimnames = list(
      usable_field_capacity = as.character(row_values),
      potential_rate_of_ascent = as.character(col_values)
    )
  )

  structure(
    result,
    row_values = row_values,
    col_values = col_values
  )
})

# estimate_days_of_growth ------------------------------------------------------
estimate_days_of_growth <- function(land_type, veg_class, default = 50L)
{
  n <- length(land_type)

  stopifnot(length(veg_class) == n)

  # Initialise result vector
  y <- rep(NA_integer_, n)

  # Special case for agricultural use
  is_urban <- land_type_is_urban(land_type)
  y[is_urban] <- ifelse(veg_class[is_urban] <= 50, 60L, 75L)

  # Constant estimates for other uses
  y[land_type_is_vegetationless(land_type)] <- 50L
  y[land_type_is_horticultural(land_type)] <- 100L
  y[land_type_is_forest(land_type)] <- 90L

  # Return default for any other use
  y[is.na(y)] <- default

  y
}

# estimate_days_of_growth_1 ----------------------------------------------------
estimate_days_of_growth_1 <- function(land_type, veg_class, default = 50)
{
  stopifnot(length(land_type) == 1L)
  stopifnot(length(veg_class) == 1L)

  # Special case for agricultural use
  if (land_type_is_urban(land_type)) {
    return(ifelse(veg_class <= 50, 60, 75))
  }

  # Constant estimates for other uses
  days_of_growth <- list(
    vegetationless = 50,
    horticultural = 100,
    forested = 90
  )

  # Lookup constant estimate. Return default if use is not in list
  default_if_null(days_of_growth[[land_type]], default)
}

# lookup_g02 -------------------------------------------------------------------
lookup_g02 <- function(usable_field_capacity)
{
  index <- as.integer(usable_field_capacity + 0.5) + 1L

  stopifnot(all(index %in% seq_along(LOOKUP_G02)))

  LOOKUP_G02[index]
}

# LOOKUP_G02 -------------------------------------------------------------------
LOOKUP_G02 <- c(
  0.0,   0.0,  0.0,  0.0,  0.3,  0.8,  1.4,  2.4,  3.7,  5.0,
  6.3,   7.7,  9.3, 11.0, 12.4, 14.7, 17.4, 21.0, 26.0, 32.0,
  39.4, 44.7, 48.0, 50.7, 52.7, 54.0, 55.0, 55.0, 55.0, 55.0,
  55.0
)
