# distribute_shares ------------------------------------------------------------
distribute_shares <- function(
    partial_areas,
    base_areas,
    complementary_areas,
    target_value
)
{
  n_areas <- length(partial_areas)

  stopifnot(
    is.numeric(partial_areas),
    is.numeric(base_areas),
    is.numeric(complementary_areas),
    is.numeric(target_value),
    n_areas == length(base_areas),
    n_areas == length(complementary_areas),
    length(target_value) == 1L
  )

  total_diff_area <- round(sum(target_value * base_areas) - sum(partial_areas),2)

  to_increase <- total_diff_area > 0
  shares <- ifelse(base_areas > 0, partial_areas / base_areas, 0)

  consider <- if (to_increase) {
    round(shares,2) < round(target_value,2)
  } else {
    round(shares,2) > round(target_value,2)
  }

  ref_areas <- rep(0, n_areas)
  ref_areas[consider] <- if (to_increase) {
    complementary_areas[consider]
  } else {
    partial_areas[consider]
  }

  area_offsets <- total_diff_area * get_weight(ref_areas[consider])
  partial_areas[consider] <- partial_areas[consider] + area_offsets

  delta <- rep(0, n_areas)
  delta[consider] <- area_offsets

  data.frame(
    area = partial_areas,
    fraction = ifelse(base_areas > 0, partial_areas / base_areas, 0),
    consider = consider,
    ref_area = ref_areas,
    delta = delta
  )
}

# get_weight -------------------------------------------------------------------
get_weight <- function(x) kwb.utils::percentageOfSum(x)/100
