# get_measure_stats ------------------------------------------------------------

#' Get Mean/Max Statistics on Measures
#'
#' @param blocks data frame similar to \code{\link{rabimo_inputs_2020}$data},
#'   with each row representing a block area
#' @param reference_system indicator for the "reference system" in which the
#'   returned values are to be given. 1: all values refer to percentages of
#'   specific areas (green roof: roof area, unsealed: total area, to_swale:
#'   sealed area); 2: all values refer to percentages of the total area. The
#'   default is 2.
#' @returns list with elements "mean" and "max" each of which is a list with one
#'   element per measure
#' @importFrom kwb.utils createAccessor
#' @export
get_measure_stats <- function(blocks, reference_system = 2)
{
  stopifnot(reference_system %in% 1:2)

  refer_to_total <- reference_system == 2

  #blocks <- kwb.rabimo::rabimo_inputs_2020$data
  get <- kwb.utils::createAccessor(blocks)

  total_areas <- get("total_area")
  roofs <- get("roof")
  pvds <- get("pvd")

  area_total <- sum(total_areas)
  area_roof <- sum(total_areas * roofs)
  area_green_roof <- sum(total_areas * roofs * get("green_roof"))
  area_pvd <- sum(total_areas * pvds)
  area_unpvd <- sum(total_areas * (1 - roofs - pvds))
  area_sca <- sum(total_areas * (roofs + pvds) * get("to_swale"))
  area_sealed <- area_roof + area_pvd

  mean_roof <- area_roof / area_total
  mean_pvd <- area_pvd / area_total

  mean_green_roof <- if (refer_to_total) {
    area_green_roof / area_total
  } else {
    area_green_roof / area_roof
  }

  mean_unpvd <- area_unpvd / area_total

  mean_sca <- if (refer_to_total) {
    area_sca / area_total
  } else {
    area_sca / area_sealed
  }

  max_green_roof <- if (refer_to_total) mean_roof else 1
  max_unpvd <- 1 - mean_roof
  max_sca <- if (refer_to_total) {
    1 - mean_unpvd
  } else {
    1
  }

  list(
    green_roof = c(mean = mean_green_roof, max = max_green_roof),
    unpaved = c(mean = mean_unpvd, max = max_unpvd),
    to_swale = c(mean = mean_sca, max = max_sca)
  )
}

