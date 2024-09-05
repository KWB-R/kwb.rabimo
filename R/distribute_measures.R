# distribute_measures ----------------------------------------------------------
#' Distribute Rainwater Management Measures to Single Block Areas
#'
#' @param blocks data frame as being input to \code{\link{run_rabimo}}
#' @param targets numeric vector with elements named \code{green_roof},
#'   \code{unpaved}, \code{to_swale}, each of which is a value between 0 and 1,
#'   describing the extent to which the corresponding measures are to be
#'   installed on average over all \code{blocks}. The percentages refer to
#'   different base areas: green_roof = total area of green roofs divided by
#'   total area of roofs; unpaved = sum of unpaved (non-roof) area divided by
#'   total area; to_swale = sum of areas that are connected to a swale divided
#'   by sum of of areas that are sealed (roof area + paved area).
#' @param intermediates logical indicating whether or not to return tables with
#'   intermediate results in attributes. The default is \code{FALSE}, i.e. no
#'   attributes are attached to the result data frame.
#' @return data frame with the columns describing the measurements being
#'   updated. In case of \code{intermediates = TRUE}, the data frame has
#'   attributes \code{green_roof_table}, \code{unpaved_area_table},
#'   \code{swale_connection_table}, carrying intermediate results.
#' @export
distribute_measures <- function(blocks, targets, intermediates = FALSE)
{
  target_green_roof <- targets[["green_roof"]]
  target_unpaved <- targets[["unpaved"]]
  target_to_swale <- targets[["to_swale"]]

  # Apply target degrees of application of measures
  green_roof_table <- get_green_roof_table(blocks, target_green_roof)
  unpaved_area_table <- get_unpaved_area_table(blocks, target_unpaved)
  swale_connection_table <- get_swale_connection_table(
    unpaved_area_table,
    target_to_swale,
    total_area = sum(get_main_area(blocks))
  )

  # Update columns in blocks
  blocks$green_roof <- green_roof_table$green_roof
  blocks$pvd <- unpaved_area_table$pvd
  blocks$to_swale <- swale_connection_table$to_swale

  # Recalculate "sealed"
  blocks$sealed <- get_sealed(blocks)

  # Return the tables with intermediate values as attributes, if requested
  if (intermediates) {
    return(structure(
      blocks,
      green_roof_table = green_roof_table,
      unpaved_area_table = unpaved_area_table,
      swale_connection_table = swale_connection_table
    ))
  }

  blocks
}

# get_green_roof_table ---------------------------------------------------------
get_green_roof_table <- function(blocks, target)
{
  roof_areas <- get_roof_area(blocks)
  green_roof_areas <- get_green_roof_area(blocks)

  distributed <- distribute_shares(
    partial_areas = green_roof_areas,
    base_areas = roof_areas,
    complementary_areas = roof_areas - green_roof_areas,
    target
  )

  rename_and_select(distributed, list(
    "consider",
    "ref_area",
    area = "green_roof_area",
    fraction = "green_roof"
  ))
}

# get_roof_area ----------------------------------------------------------------
get_roof_area <- function(blocks)
{
  get_main_area(blocks) * select_columns(blocks, "roof")
}

# get_main_area ----------------------------------------------------------------
get_main_area <- function(blocks)
{
  select_columns(blocks, "total_area") *
    select_columns(blocks, "main_frac")
}

# get_green_roof_area ----------------------------------------------------------
get_green_roof_area <- function(blocks)
{
  select_columns(blocks, "green_roof") * get_roof_area(blocks)
}

# get_unpaved_area_table -------------------------------------------------------
get_unpaved_area_table <- function(blocks, target)
{
  unpaved_areas <- get_unpaved_area(blocks)
  main_areas <- get_main_area(blocks)
  paved_areas_old <- get_paved_area(blocks)
  sealed_areas_old <- get_sealed_area(blocks)
  to_swale_values <- select_columns(blocks, "to_swale")

  distributed_unpaved <- distribute_shares(
    partial_areas = unpaved_areas,
    base_areas = main_areas,
    complementary_areas = paved_areas_old,
    target_value = target
  )

  deltas_unpaved <- select_columns(distributed_unpaved, "delta")

  paved_areas_new <- pmax(0, paved_areas_old - deltas_unpaved)
  sealed_areas_new <- pmax(0, sealed_areas_old - deltas_unpaved)

  scaling_factors <- ifelse(
    sealed_areas_new > 0,
    sealed_areas_old / sealed_areas_new,
    1
  )

  to_swale_new <- pmin(1, to_swale_values * scaling_factors)

  data.frame(
    consider = select_columns(distributed_unpaved, "consider"),
    ref_area = select_columns(distributed_unpaved, "ref_area"),
    delta = deltas_unpaved,
    paved_area = paved_areas_new,
    pvd = paved_areas_new / main_areas,
    unpaved_area = select_columns(distributed_unpaved, "area"),
    unpaved = select_columns(distributed_unpaved, "fraction"),
    sealed_area = sealed_areas_new,
    sealed = sealed_areas_new / main_areas,
    corr_sca = to_swale_new * sealed_areas_new,
    corr_sca_frac = to_swale_new
  )
}

# get_unpaved_area -------------------------------------------------------------
get_unpaved_area <- function(blocks)
{
  get_main_area(blocks) - get_roof_area(blocks) - get_paved_area(blocks)
}

# get_paved_area ---------------------------------------------------------------
get_paved_area <- function(blocks)
{
  get_main_area(blocks) * select_columns(blocks, "pvd")
}

# get_sealed_area --------------------------------------------------------------
get_sealed_area <- function(blocks)
{
  select_columns(blocks, "sealed") * get_main_area(blocks)
}

# get_swale_connection_table ---------------------------------------------------
get_swale_connection_table <- function(unpaved_area_table, target, total_area)
{
  sealed_areas <- select_columns(unpaved_area_table, "sealed_area")
  corrected_scas <- select_columns(unpaved_area_table, "corr_sca")
  shares <- select_columns(unpaved_area_table, "corr_sca_frac")
  to_swale_areas <- select_columns(unpaved_area_table, "corr_sca") #???
  zeros <- rep(0, nrow(unpaved_area_table))

  sealed_mean <- sum(sealed_areas) / total_area

  to_swale_diff_rel <- target - sum(corrected_scas) / sum(sealed_areas)
  total_diff_area <- to_swale_diff_rel * sealed_mean * total_area

  to_increase <- total_diff_area > 0

  consider <- if (to_increase) {
    shares < target
  } else {
    shares > target
  }

  ref_areas <- zeros
  deltas <- zeros

  ref_areas[consider] <- if (to_increase) {
    sealed_areas[consider] - corrected_scas[consider]
  } else {
    to_swale_areas[consider]
  }

  #deltas <- get_weight(ref_areas) * total_diff_area
  if ((tot_area <- sum(ref_areas)) > 0) {
    deltas <- ref_areas / tot_area * total_diff_area
  }

  to_swale_areas <- corrected_scas + deltas

  data.frame(
    consider = consider,
    ref_area = ref_areas,
    delta = deltas,
    to_swale_area = to_swale_areas,
    to_swale = ifelse(to_swale_areas > 0, to_swale_areas / sealed_areas, 0)
  )
}

# get_to_swale_area ------------------------------------------------------------
get_to_swale_area <- function(blocks)
{
  select_columns(blocks, "to_swale") * get_sealed_area(blocks)
}

# get_sealed -------------------------------------------------------------------
get_sealed <- function(blocks)
{
  dividend <- get_roof_area(blocks) + get_paved_area(blocks)
  divisor <- kwb.utils::selectColumns(blocks, "total_area")
  ifelse(dividend > 0, dividend / divisor, 0)
}
