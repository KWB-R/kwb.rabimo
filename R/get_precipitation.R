# get_precipitation ------------------------------------------------------------

#' Provide Information on Precipitation
#'
#' @param precipitation_year precipitation per year in mm
#' @param precipitation_summer precipitation within summer period in mm
#' @param correction_factor correction factor
#' @return list with elements \code{year}, \code{summer}
#' @export
#' @examples
#' get_precipitation(600, 300, 0.8)
get_precipitation <- function(
    precipitation_year,
    precipitation_summer,
    correction_factor
)
{
  # - Correct the (non-summer) precipitation (at ground level)
  # - No correction for summer precipitation!

  data.frame(
    year = precipitation_year * correction_factor,
    summer = precipitation_summer
  )
}
