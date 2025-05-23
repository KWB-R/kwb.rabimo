# rescale_target_values --------------------------------------------------------
rescale_target_values <- function(new_targets, blocks)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  total_area <- sum(get_main_area(blocks))
  total_roof_area <- sum(get_roof_area(blocks))
  total_sealed_area <- sum(get_sealed_area(blocks))

  green_roof_new <- select_elements(new_targets, "green_roof")
  unpaved_new <- select_elements(new_targets, "unpaved")
  to_swale_new <- select_elements(new_targets, "to_swale")

  green_roof <- if (total_roof_area > 0) {
    green_roof_new * total_area / total_roof_area
  } else {
    0
  }

  unpaved <- unpaved_new

  to_swale <- if (total_sealed_area > 0) {
    to_swale_new * total_area / total_sealed_area
  } else {
    0
  }

  max_allowed_unpaved <- (total_area - total_roof_area) / total_area

  stopifnot(green_roof <= 1)
  stopifnot(unpaved <= max_allowed_unpaved)
  stopifnot(to_swale <= 1)

  list(green_roof = green_roof, unpaved = unpaved, to_swale = to_swale)
}
