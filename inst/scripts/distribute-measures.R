#
# - Source the whole script first to load the functions defined below
# - Then, interactively go through the lines within "if (FALSE) {...}"
#

source("inst/scripts/read-stormwatermeasures-xls.R")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  ref_file <- "~/Projekte/AMAREX/distribution_stormwatermeasures_playground.xlsx"
  #kwb.utils::hsOpenWindowsExplorer(path.expand(ref_file))

  ref_input_table <- read_input_table(xls_file = ref_file)
  ref_output_tables <- read_output_tables(ref_file)
  ref_targets <- read_targets(ref_file)

  # Read the columns representing absolute areas (coloured cells in Excel file)
  block_areas <- kwb.utils::renameAndSelect(ref_input_table, list(
    area = "total_area",
    roof = "roof_area",
    green_roof = "green_roof_area",
    paved = "pvd_area",
    sca = "to_swale_area"
  ))

  # Generate "full" block records as required by R-Abimo
  blocks <- get_example_blocks(block_areas)

  # Check if the blocks are accepted by R-Abimo
  kwb.rabimo:::stop_on_invalid_data(blocks)

  # Compare with numbers calculated in Excel file
  check_validity_of_inputs(blocks, block_areas, ref_input_table)

  # Current mean degrees of application of measures
  check_equality(get_measure_means(blocks), read_measure_means(ref_file))

  targets <- list(green_roof = 0.4, unpaved = 0.4, to_swale = 0.5)

  targets <- ref_targets

  new_blocks <- kwb.rabimo::distribute_measures(
    blocks,
    targets
  )

  new_blocks_with_tables <- kwb.rabimo::distribute_measures(
    blocks,
    targets,
    intermediates = TRUE
  )

  # Apply target degrees of application of measures
  green_roof_table <- attr(new_blocks_with_tables, "green_roof_table")
  unpaved_area_table <- attr(new_blocks_with_tables, "unpaved_area_table")
  swale_connection_table <- attr(new_blocks_with_tables, "swale_connection_table")

  round_numeric_columns(green_roof_table)
  round_numeric_columns(unpaved_area_table)
  round_numeric_columns(swale_connection_table)

  {
    check_percentage <- function(a, b, digits = 1L) {
      check_equality(
        round(a * 100, digits),
        round(b * 100, digits)
      )
    }
    check_percentage(
      green_roof_table$green_roof,
      ref_output_tables$green_roof_table$green_roof
    )
    check_percentage(
      unpaved_area_table$unpaved,
      ref_output_tables$unpaved_area_table$unpaved
    )
    check_percentage(
      swale_connection_table$to_swale,
      ref_output_tables$swale_connection_table$to_swale
    )
  }

  #new_blocks <- ...
  #print_blocks(new_blocks)
  #green_roof_mean(new_blocks)
  #unpaved_mean(new_blocks)
}

# get_example_blocks -----------------------------------------------------------
get_example_blocks <- function(block_areas)
{
  codes <- sprintf("a%02d", seq_len(nrow(block_areas)))
  total_areas <- kwb.utils::selectColumns(block_areas, "total_area")

  blocks <- do.call(rbind, lapply(codes, kwb.rabimo:::generate_rabimo_area))

  blocks$total_area <- total_areas
  blocks$area_main <- total_areas
  blocks$green_roof <- get_green_roof(block_areas)
  blocks$pvd <- get_paved(block_areas)

  blocks$swg_roof <- 1
  blocks$swg_pvd <- 1
  blocks$swg_pvd_r <- 1

  blocks$roof <- get_roof(cbind(blocks, block_areas))
  blocks$sealed <- get_sealed(blocks)
  blocks$to_swale <- get_to_swale(cbind(blocks, block_areas))

  columns_in_stored_data <- names(kwb.rabimo::rabimo_inputs_2020$data)
  expected_columns <- setdiff(columns_in_stored_data, "block_type")
  kwb.utils::selectColumns(blocks, expected_columns)
}

# check_equality ---------------------------------------------------------------
check_equality <- function(a, b) stopifnot(all.equal(a, b))

# check_validity_of_inputs -----------------------------------------------------
check_validity_of_inputs <- function(blocks, block_areas, ref_input_table)
{
  # Calculate absolute areas, the fractions of unpaved areas, and weights
  fetch <- kwb.utils::createAccessor(blocks)

  main_areas <- kwb.rabimo:::get_main_area(blocks)
  roof_areas <- kwb.rabimo:::get_roof_area(blocks)
  sealed_areas <- kwb.rabimo:::get_sealed_area(blocks)

  check_equality_rounded <- function(a, b, digits = 0L) {
    check_equality(round(a, digits), round(b, digits))
  }

  check_equality_percent <- function(a, b, digits = 0L) {
    check_equality_rounded(a * 100, b * 100, digits)
  }

  {
    check_equality(
      main_areas,
      block_areas$total_area
    )
    check_equality_rounded(
      kwb.rabimo:::get_weight(main_areas),
      ref_input_table$weight,
      digits = 2L
    )
    check_equality(
      roof_areas,
      block_areas$roof_area
    )
    check_equality_percent(
      fetch("roof"),
      ref_input_table$roof_1
    )
    check_equality_rounded(
      kwb.rabimo:::get_weight(roof_areas),
      ref_input_table$weight_of_roof,
      digits = 2L
    )
    check_equality(
      kwb.rabimo:::get_green_roof_area(blocks),
      block_areas$green_roof_area
    )
    check_equality_percent(
      fetch("green_roof"),
      ref_input_table$of_roof
    )
    check_equality(
      kwb.rabimo:::get_paved_area(blocks),
      block_areas$pvd_area
    )
    check_equality_percent(
      fetch("pvd"),
      ref_input_table$paved_1,
      digits = 1L
    )
    check_equality(
      kwb.rabimo:::get_unpaved_area(blocks),
      ref_input_table$unpaved
    )
    check_equality_percent(
      get_unpaved(blocks),
      ref_input_table$unpaved_1
    )
    check_equality_rounded(
      sealed_areas,
      ref_input_table$sealed
    )
    check_equality_percent(
      fetch("sealed"),
      ref_input_table$sealed_1
    )
    check_equality_rounded(
      kwb.rabimo:::get_weight(sealed_areas),
      ref_input_table$weight_of_sealed,
      digits = 3L
    )
    check_equality(
      kwb.rabimo:::get_to_swale_area(blocks),
      ref_input_table$sca
    )
    check_equality_percent(
      fetch("to_swale"),
      ref_input_table$of_sealed
    )
  }
}

# get_measure_means ------------------------------------------------------------
get_measure_means <- function(blocks)
{
  list(
    green_roof = green_roof_mean(blocks),
    unpaved = unpaved_mean(blocks),
    to_swale = to_swale_mean(blocks)
  )
}

# green_roof_mean --------------------------------------------------------------
green_roof_mean <- function(blocks)
{
  sum(kwb.rabimo:::get_green_roof_area(blocks)) /
    sum(kwb.rabimo:::get_roof_area(blocks))
}

# unpaved_mean -----------------------------------------------------------------
unpaved_mean <- function(blocks)
{
  sum(kwb.rabimo:::get_unpaved_area(blocks)) /
    sum(kwb.rabimo:::get_main_area(blocks))
}

# to_swale_mean ----------------------------------------------------------------
to_swale_mean <- function(blocks)
{
  sum(kwb.rabimo:::get_to_swale_area(blocks)) /
    sum(kwb.rabimo:::get_sealed_area(blocks))
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

# get_roof ---------------------------------------------------------------------
get_roof <- function(blocks) {
  quotient_or_zero(
    dividend = kwb.utils::selectColumns(blocks, "roof_area"), #kwb.rabimo:::get_roof_area(blocks),
    divisor = kwb.utils::selectColumns(blocks, "total_area")
  )
}

# get_green_roof ---------------------------------------------------------------
get_green_roof <- function(blocks) {
  quotient_or_zero(
    dividend = kwb.utils::selectColumns(blocks, "green_roof_area"),
    divisor = kwb.utils::selectColumns(blocks, "roof_area") #kwb.rabimo:::get_roof_area(blocks)
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
  kwb.rabimo:::get_unpaved_area(blocks) /
    kwb.rabimo:::get_main_area(blocks)
}

# get_sealed -------------------------------------------------------------------
get_sealed <- function(blocks) {
  quotient_or_zero(
    dividend = kwb.rabimo:::get_roof_area(blocks) +
      kwb.rabimo:::get_paved_area(blocks),
    divisor = kwb.utils::selectColumns(blocks, "total_area")
  )
}

# get_to_swale -----------------------------------------------------------------
get_to_swale <- function(blocks) {
  quotient_or_zero(
    dividend = kwb.utils::selectColumns(blocks, "to_swale_area"),
    divisor = kwb.rabimo:::get_roof_area(blocks) +
      kwb.rabimo:::get_paved_area(blocks)
  )
}

# quotient_or_zero -------------------------------------------------------------
quotient_or_zero <- function(dividend, divisor) {
  ifelse(dividend > 0, dividend / divisor, 0)
}

# round_numeric_columns --------------------------------------------------------
round_numeric_columns <- function(df, digits = 2L)
{
  is_numeric <- sapply(df, is.numeric)
  df[is_numeric] <- lapply(df[is_numeric], round, digits = digits)
  df
}
