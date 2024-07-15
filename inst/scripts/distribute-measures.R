#
# - Source the whole script first to load the functions defined below
# - Then, interactively go through the lines within "if (FALSE) {...}"
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  block_areas <- get_example_block_areas()
  blocks <- get_example_blocks(block_areas)

  # Check if the blocks are accepted by R-Abimo
  kwb.rabimo:::stop_on_invalid_data(blocks)

  # Calculate absolute areas, the fractions of unpaved areas, and weights
  {
    blocks$roof_area <- get_roof_area(blocks)
    blocks$green_roof_area <- get_green_roof_area(blocks)
    blocks$paved_area <- get_paved_area(blocks)
    blocks$unpaved_area <- get_unpaved_area(blocks)
    blocks$sealed_area <- get_sealed_area(blocks)
    blocks$to_swale_area <- get_to_swale_area(blocks)

    blocks$unpaved <- get_unpaved(blocks)

    blocks$weight_of_block <- kwb.rabimo:::get_weight(blocks$total_area)
    blocks$weight_of_roof <- kwb.rabimo:::get_weight(blocks$roof_area)
    blocks$weight_of_sealed <- kwb.rabimo:::get_weight(blocks$sealed_area)
  }

  # Compare with numbers calculated in Excel file
  check_validity_of_inputs(blocks)

  # Current mean degrees of application of measures
  green_roof_mean(blocks)
  unpaved_mean(blocks)

  # Target degrees of application of measures
  TARGET_GREEN_ROOF <- 0.8
  TARGET_UNPAVED <- 0.4
  TARGET_TO_SWALE <- 0.3942

  green_roof_table <- get_green_roof_table(blocks, target = TARGET_GREEN_ROOF)
  unpaved_area_table <- get_unpaved_area_table(blocks, target = TARGET_UNPAVED)
  swale_connection_table <- get_swale_connection_table(blocks, unpaved_area_table, target = TARGET_TO_SWALE)

  round_numeric_columns(green_roof_table)
  round_numeric_columns(unpaved_area_table)
  round_numeric_columns(swale_connection_table)

  check_equality(round(green_roof_table$green_roof * 100), c(100, 60, 60, 0, 91, 0, 0, 0, 75, 0, 60))
  check_equality(round(unpaved_area_table$unpaved * 100), c(30, 29, 67, 41, 3, 71, 24, 100, 9, 64, 34))
  check_equality(round(swale_connection_table$to_swale * 100), c(31, 41, 0, 0, 66, 0, 91, 0, 0, 0, 54))

  #new_blocks <- ...
  #print_blocks(new_blocks)
  #green_roof_mean(new_blocks)
  #unpaved_mean(new_blocks)
}

# get_example_block_areas ------------------------------------------------------
get_example_block_areas <- function() {
  rbind(
    data.frame(total_area = 14,	roof_area =  6, green_roof_area =  6, pvd_area =  5, to_swale_area =  3),
    data.frame(total_area = 17,	roof_area =  6, green_roof_area =  0, pvd_area =  8, to_swale_area =  5),
    data.frame(total_area = 12,	roof_area =  4, green_roof_area =  0, pvd_area =  0, to_swale_area =  0),
    data.frame(total_area =  9, roof_area =  0, green_roof_area =  0, pvd_area =  7, to_swale_area =  0),
    data.frame(total_area = 16,	roof_area = 14, green_roof_area = 11, pvd_area =  2, to_swale_area = 11),
    data.frame(total_area = 14,	roof_area =  0, green_roof_area =  0, pvd_area =  4, to_swale_area =  0),
    data.frame(total_area = 16,	roof_area =  0, green_roof_area =  0, pvd_area = 16, to_swale_area = 15),
    data.frame(total_area = 11,	roof_area =  0, green_roof_area =  0, pvd_area =  0, to_swale_area =  0),
    data.frame(total_area = 13,	roof_area =  8, green_roof_area =  3, pvd_area =  5, to_swale_area =  0),
    data.frame(total_area = 11,	roof_area =  0, green_roof_area =  0, pvd_area =  4, to_swale_area =  0),
    data.frame(total_area = 18,	roof_area =  2, green_roof_area =  0, pvd_area = 13, to_swale_area =  7)
  )
}

# get_example_blocks -----------------------------------------------------------
get_example_blocks <- function(block_areas) {

  blocks <- data.frame(
    code = sprintf("a%02d", seq_len(nrow(block_areas))),
    prec_yr = 500L,
    prec_s = 300L,
    epot_yr = 400L,
    epot_s = 300L,
    district = "main_district",
    total_area = kwb.utils::selectColumns(block_areas, "total_area"),
    area_main = NA_real_,
    area_road = NA_real_,
    main_frac = 1,
    green_roof = get_green_roof(block_areas),
    swg_roof = 1,
    pvd = get_paved(block_areas),
    swg_pvd = 1,
    srf1_pvd = 1,
    srf2_pvd = 0,
    srf3_pvd = 0,
    srf4_pvd = 0,
    srf5_pvd = 0,
    road_frac = 0,
    pvd_r = 0,
    swg_pvd_r = 1,
    srf1_pvd_r = 1,
    srf2_pvd_r = 0,
    srf3_pvd_r = 0,
    srf4_pvd_r = 0,
    gw_dist = 10,
    ufc30 = 10,
    ufc150 = 15,
    land_type = "abc",
    veg_class = 5L,
    irrigation = 0L,
    block_type = "any_block",
    stringsAsFactors = FALSE
  )

  blocks$roof <- get_roof(cbind(blocks, block_areas))
  blocks$sealed <- get_sealed(blocks)
  blocks$to_swale = get_to_swale(cbind(blocks, block_areas))

  kwb.utils::selectColumns(blocks, names(kwb.rabimo::rabimo_inputs_2020$data))
}

# check_equality ---------------------------------------------------------------
check_equality <- function(a, b) stopifnot(all.equal(a, b))

# check_validity_of_inputs -----------------------------------------------------
check_validity_of_inputs <- function(blocks)
{
  with(blocks, {
    check_equality(total_area, block_areas$total_area)
    check_equality(round(weight_of_block, 2L), c(0.09, 0.11, 0.08, 0.06, 0.11, 0.09, 0.11, 0.07, 0.09, 0.07, 0.12))
    check_equality(roof_area, block_areas$roof_area)
    check_equality(round(roof * 100), c(43, 35, 33, 0, 88, 0, 0, 0, 62, 0, 11))
    check_equality(round(weight_of_roof, 2L), c(0.15, 0.15, 0.10, 0.00, 0.35, 0.00, 0.00, 0.00, 0.20, 0.00, 0.05))
    check_equality(green_roof_area, block_areas$green_roof_area)
    check_equality(round(green_roof * 100), c(100, 0, 0, 0, 79, 0, 0, 0, 38, 0, 0))
    check_equality(paved_area, block_areas$pvd_area)
    check_equality(round(pvd * 100, 1), c(35.7, 47.1, 0.0, 77.8, 12.5, 28.6, 100.0, 0.0, 38.5, 36.4, 72.2))
    check_equality(unpaved_area, c(3, 3, 8, 2, 0, 10, 0, 11, 0, 7, 3))
    check_equality(round(unpaved * 100), c(21, 18, 67, 22, 0, 71, 0, 100, 0, 64, 17))
    check_equality(round(sealed_area), c(11, 14, 4, 7, 16, 4, 16, 0, 13, 4, 15))
    check_equality(round(sealed * 100), c(79, 82, 33, 78, 100, 29, 100, 0, 100, 36, 83))
    check_equality(round(blocks$weight_of_sealed, 3L), c(0.106, 0.135, 0.038, 0.067, 0.154, 0.038, 0.154, 0.000, 0.125, 0.038, 0.144))
    check_equality(to_swale_area, c(3, 5, 0, 0, 11, 0, 15, 0, 0, 0, 7))
    check_equality(round(to_swale * 100), c(27, 36, 0, 0, 69, 0, 94, 0, 0, 0, 47))
  })
}

# green_roof_mean --------------------------------------------------------------
green_roof_mean <- function(blocks)
{
  sum(kwb.utils::selectColumns(blocks, "green_roof_area")) /
    sum(kwb.utils::selectColumns(blocks, "roof_area"))
}

# unpaved_mean -----------------------------------------------------------------
unpaved_mean <- function(blocks)
{
  sum(kwb.utils::selectColumns(blocks, "unpaved_area")) /
    sum(kwb.utils::selectColumns(blocks, "total_area"))
}

# print_blocks -----------------------------------------------------------------
print_blocks <- function(blocks)
{
  print(kwb.utils::selectColumns(blocks, c(
    "code",
    "total_area",
    "main_frac",
    "green_roof",
    "unpaved"
  )))
}

# get_main_area ----------------------------------------------------------------
get_main_area <- function(blocks) {
  kwb.utils::selectColumns(blocks, "total_area") *
    kwb.utils::selectColumns(blocks, "main_frac")
}

# get_roof_area ----------------------------------------------------------------
get_roof_area <- function(blocks) {
  if (!is.null(roof_area <- blocks$roof_area)) {
    return(roof_area)
  }
  get_main_area(blocks) * kwb.utils::selectColumns(blocks, "roof")
}

# get_green_roof_area ----------------------------------------------------------
get_green_roof_area <- function(blocks) {
  values <- blocks[["green_roof_area"]]
  if (!is.null(values)) {
    return(values)
  }
  kwb.utils::selectColumns(blocks, "green_roof") * get_roof_area(blocks)
}

# get_paved_area ---------------------------------------------------------------
get_paved_area <- function(blocks) {
  get_main_area(blocks) * kwb.utils::selectColumns(blocks, "pvd")
}

# get_unpaved_area -------------------------------------------------------------
get_unpaved_area <- function(blocks) {
  values <- blocks[["unpaved_area"]]
  if (!is.null(values)) {
    return(values)
  }
  get_main_area(blocks) - get_roof_area(blocks) - get_paved_area(blocks)
}

# get_sealed_area --------------------------------------------------------------
get_sealed_area <- function(blocks) {
  kwb.utils::selectColumns(blocks, "sealed") * get_main_area(blocks)
}

# get_to_swale_area ------------------------------------------------------------
get_to_swale_area <- function(blocks) {
  kwb.utils::selectColumns(blocks, "to_swale") * get_sealed_area(blocks)
}

# get_roof ---------------------------------------------------------------------
get_roof <- function(blocks) {
  quotient_or_zero(
    dividend = get_roof_area(blocks),
    divisor = get_main_area(blocks)
  )
}

# get_green_roof ---------------------------------------------------------------
get_green_roof <- function(blocks) {
  quotient_or_zero(
    dividend = kwb.utils::selectColumns(blocks, "green_roof_area"),
    divisor = get_roof_area(blocks)
  )
}

# get_paved --------------------------------------------------------------------
get_paved <- function(blocks) {
  values <- blocks[["pvd"]]
  if (!is.null(values)) {
    return(values)
  }
  quotient_or_zero(
    dividend = areas <- kwb.utils::selectColumns(blocks, "pvd_area"),
    divisor = kwb.utils::selectColumns(blocks, "total_area")
  )
}

# get_unpaved ------------------------------------------------------------------
get_unpaved <- function(blocks) {
  get_unpaved_area(blocks) / get_main_area(blocks)
}

# get_sealed -------------------------------------------------------------------
get_sealed <- function(blocks) {
  #new_sealed <- (new_blocks$roof_area + new_pvd) / get_main_area(new_blocks)
  quotient_or_zero(
    dividend = get_roof_area(blocks) + get_paved_area(blocks),
    divisor = get_main_area(blocks)
  )
}

# get_to_swale -----------------------------------------------------------------
get_to_swale <- function(blocks) {
  quotient_or_zero(
    dividend = kwb.utils::selectColumns(blocks, "to_swale_area"),
    divisor = get_roof_area(blocks) + get_paved_area(blocks)
  )
}

# quotient_or_zero -------------------------------------------------------------
quotient_or_zero <- function(dividend, divisor) {
  ifelse(dividend > 0, dividend / divisor, 0)
}

# get_green_roof_table ---------------------------------------------------------
get_green_roof_table <- function(blocks, target)
{
  get <- kwb.utils::selectColumns

  roof_areas <- get_roof_area(blocks)
  green_roof_areas <- get_green_roof_area(blocks)

  distributed_green_roof <- kwb.rabimo:::distribute_shares(
    partial_areas = green_roof_areas,
    base_areas = roof_areas,
    complementary_areas = roof_areas - green_roof_areas,
    target
  )

  data.frame(
    consider = get(distributed_green_roof, "consider"),
    ref_area = get(distributed_green_roof, "ref_area"),
    green_roof_area = get(distributed_green_roof, "area"),
    green_roof = get(distributed_green_roof, "fraction")
  )
}

# get_unpaved_area_table -------------------------------------------------------
get_unpaved_area_table <- function(blocks, target)
{
  get <- kwb.utils::selectColumns

  main_areas <- get_main_area(blocks)

  distributed_unpaved <- kwb.rabimo:::distribute_shares(
    partial_areas = get_unpaved_area(blocks),
    base_areas = main_areas,
    complementary_areas = get_paved_area(blocks),
    target_value = target
  )

  deltas_unpaved <- get(distributed_unpaved, "delta")

  paved_areas_old <- get(blocks, "paved_area")
  sealed_areas_old <- get(blocks, "sealed_area")

  paved_areas_new <- pmax(0, paved_areas_old - deltas_unpaved)
  sealed_areas_new <- pmax(0, sealed_areas_old - deltas_unpaved)

  scaling_factors <- ifelse(
    sealed_areas_new > 0,
    sealed_areas_old / sealed_areas_new,
    1
  )

  to_swale_new <- pmin(1, get(blocks, "to_swale") * scaling_factors)

  data.frame(
    consider = get(distributed_unpaved, "consider"),
    ref_area = get(distributed_unpaved, "ref_area"),
    delta = deltas_unpaved,
    paved_area = paved_areas_new,
    pvd = paved_areas_new / main_areas,
    unpaved_area = get(distributed_unpaved, "area"),
    unpaved = get(distributed_unpaved, "fraction"),
    sealed_area = sealed_areas_new,
    sealed = sealed_areas_new / main_areas,
    corr_sca = to_swale_new * sealed_areas_new,
    corr_sca_frac = to_swale_new
  )
}

# get_swale_connection_table ---------------------------------------------------
get_swale_connection_table <- function(blocks, unpaved_area_table, target)
{
  #target <- 0.39
  main_area <- sum(get_main_area(blocks))
  sealed_mean <- sum(unpaved_area_table$sealed_area) / main_area

  to_swale_mean <- sum(unpaved_area_table$corr_sca) / sum(unpaved_area_table$sealed_area)
  to_swale_diff_rel <- target - to_swale_mean
  total_diff_area <- to_swale_diff_rel * sealed_mean * main_area

  to_increase <- total_diff_area > 0
  shares <- blocks$to_swale

  consider <- if (to_increase) {
    shares < target
  } else {
    shares > target
  }

  ref_areas <- rep(0, nrow(blocks))
  ref_areas[consider] <- if (to_increase) {
    unpaved_area_table$sealed_area[consider] - unpaved_area_table$corr_sca[consider]
  } else {
    blocks$to_swale_area[consider]
  }

  deltas <- kwb.rabimo:::get_weight(ref_areas) * total_diff_area

  to_swale_areas <- unpaved_area_table$corr_sca + deltas

  data.frame(
    consider = consider,
    ref_area = ref_areas,
    delta = deltas,
    to_swale_area = to_swale_areas,
    to_swale = ifelse(to_swale_areas > 0, to_swale_areas / unpaved_area_table$sealed_area, 0)
  )
}

# round_numeric_columns --------------------------------------------------------
round_numeric_columns <- function(df, digits = 2L)
{
  is_numeric <- sapply(df, is.numeric)
  df[is_numeric] <- lapply(df[is_numeric], round, digits = digits)
  df
}
