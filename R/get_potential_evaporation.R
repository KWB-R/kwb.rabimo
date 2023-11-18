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
#' }
#'
get_potential_evaporation <- function(is_waterbody, district, lookup)
{
  # Prepare input data for multi_column_lookup
  data <- data.frame(
    is_waterbody = is_waterbody,
    district = district
  )

  # One after another, lookup values for "etp", "etps" in the lookup table
  result <- c(year = "etp", summer = "etps") %>%
    lapply(function(column) {
      multi_column_lookup(
        data = data,
        lookup = select_columns(lookup, c(names(data), column))
      )
    })

  if (all(lengths(result) == 1L)) {
    return(result)
  }

  as.data.frame(result)
}
