# get_potential_evaporation ----------------------------------------------------

#' Provide Data on Potential Evaporation
#'
#' @param is_waterbody (vector of) logical indicating whether a block area is
#'   of type (from the type/yield/irrigation tuple) "waterbody"
#' @param district (vector of) integer indicating the district number of the
#'   plot area (from the original input column "BEZIRK")
#' @param lookup data frame with key columns \code{is_waterbody}, \code{district}
#'   and value columns \code{etp}, \code{etps}. A data frame of the required
#'   structure is returned by \code{\link{abimo_config_to_config}} in list
#'   element \code{"potential_evaporation"}
#' @export
#' @examples
#' \dontrun{
#' config <- abimo_config_to_config(kwb.abimo:::read_config())
#' get_potential_evaporation(
#'   is_waterbody = TRUE,
#'   district = 1,
#'   lookup = config$potential_evaporation
#' )
#'
#' get_potential_evaporation(
#'   is_waterbody = c(TRUE, TRUE, FALSE, FALSE),
#'   district = c(1, 99, 2, 99),
#'   lookup = config$potential_evaporation
#' )
#' }
#'
get_potential_evaporation <- function(is_waterbody, district, lookup)
{
  # Lookup values for "etp", "etps" in the lookup table based on the
  # value combinations in vectors "is_waterbody", "district"
  result <- data.frame(is_waterbody = is_waterbody, district = district) %>%
    multi_column_lookup(lookup, value = c("etp", "etps")) %>%
    set_names(c("year", "summer"))

  if (all(lengths(result) == 1L)) {
    result
  } else {
    as.data.frame(result)
  }
}
