# calculate_delta_w ------------------------------------------------------------

#' Deviation from Natural Water Balance (Delta-W)
#'
#' Calculate the deviation from the natural water balance (delta-W) given
#' R-Abimo results as returned by \code{\link{run_rabimo}}.
#'
#' @param natural R-Abimo results for the "natural" scenario
#' @param urban R-Abimo results for the "urban" scenario
#' @param columns_water_balance names of columns in \code{natural} and
#'   \code{urban}, respectively, containing the water balance components runoff,
#'   infiltration, evaporation. Default: \code{c("runoff", "infiltr", "evapor")}
#' @param column_code name of column in \code{natural} and \code{urban},
#'   respectively, containing the block area identifiers.
#' @param digits integer indicating the number of decimal places in the result
#' @return a data frame with the area codes in column \code{code} and the
#'   delta-W values in column \code{delta_w}
#' @export
calculate_delta_w <- function(
    natural,
    urban,
    columns_water_balance = c("runoff", "infiltr", "evapor"),
    column_code = "code",
    digits = 1L
)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #columns_water_balance=c("runoff","infiltr","evapor");column_code="code";digits=1L

  urban <- remove_geo_column_if_required(urban)

  columns <- c(column_code, columns_water_balance)
  data_urban <- select_columns(urban, columns)
  data_natural <- select_columns(natural, columns)

  joined <- dplyr::inner_join(data_urban, data_natural, by = column_code)

  joined_urban <- joined[paste0(columns_water_balance, ".x")]
  joined_natural <- joined[paste0(columns_water_balance, ".y")]

  # Calculate delta-W. Precipitation = rowSums(m_natural)
  delta_ws <- rowSums(abs(joined_urban - joined_natural)) /
    rowSums(joined_natural) * 100 / 2

  delta_w <- data.frame(
    code = joined[[column_code]],
    delta_w = unname(round(delta_ws, digits)),
    stringsAsFactors = FALSE
  )
  
  if (is.null(geometry <- attr(urban, "geometry"))) {
    delta_w
  } else {
    restore_geo_column_if_required(
      delta_w,
      # unfortunately, the [] selection removes the attribute "sf_column"
      geometry = structure(
        geometry[match(delta_w$code, urban$code)],
        sf_column = attr(geometry, "sf_column")
      )
    )
  }
}
