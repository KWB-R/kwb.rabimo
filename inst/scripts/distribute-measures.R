#
# - Source the whole script first to load the functions defined below
# - Then, interactively go through the lines within "if (FALSE) {...}"
#

source("inst/scripts/read-stormwatermeasures-xls.R")

#remotes::install_github("omegahat/RDCOMClient")
#install.packages("kwb.db")

# Provide Excel file locally ---------------------------------------------------
if (FALSE)
{
  file_server <- "Y:/SUW_Department/Projects/AMAREX/Work-packages/AP_4/RWB-Integration/distribution_stormwatermeasures.xlsx"
  dir_local <- "~/Projekte/AMAREX"
  file_local <- file.path(dir_local, gsub("\\.xlsx$", "_playground.xlsx", basename(file_server)))
  file.copy(file_server, file_local, overwrite = TRUE)
  kwb.utils::hsOpenWindowsExplorer(path.expand(dir_local))
}

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  xls_file <- "~/Projekte/AMAREX/distribution_stormwatermeasures_playground.xlsx"
  sheet <- 2

  # Provide input data for R-Abimo, based on input table in the Excel file
  blocks <- provide_rabimo_input_based_on_excel_input(
    ref_input_table = read_input_table(xls_file, sheet = sheet)
  )

  # Just to double check: Read current target values from the Excel file
  get_or_set_target_values_in_xls_file(xls_file, targets = NULL,  sheet = sheet)

  # Set the target values in the old reference system,
  # e.g. 100% green roof = 100% of roofs are green
  (unpaved_max <- sum(blocks$total_area * (1 - blocks$roof)) / sum(blocks$total_area))

  target_combis <- expand.grid(
    green_roof = seq(0, 1, length.out = 10L),
    unpaved = seq(0, unpaved_max, length.out = 10L),
    to_swale = seq(0, 1, length.out = 10L)
  )

  # Current mean degrees of application of measures
  check_equality(get_measure_means(blocks), read_measure_means(xls_file, sheet = sheet))

  indices <- seq_len(n <- nrow(target_combis))
  indices <- sample(indices)
  #i <- 132L
  for (i in seq_along(indices)) {
    cat(sprintf("Checking target %d/%d\n", i, n))

    #new_targets <- c(green_roof = 1, unpaved = 0.75, to_swale = 1)
    new_targets <- unlist(target_combis[indices[i], ])
    print(new_targets)
    get_or_set_target_values_in_xls_file(xls_file, targets = new_targets)

    # Distribute the measures over the blocks (skip block that have already
    # implemented the measure with a higher value than the target value)
    new_blocks <- kwb.rabimo::distribute_measures(
      blocks, targets = new_targets, intermediates = TRUE
    )

    # Current mean degrees of application of measures
    check_equality(unlist(get_measure_means(new_blocks)), new_targets)

    # Check that the distribution in R works exactly as the distribution in Excel
    check_distribution(
      green_roof_table = attr(new_blocks, "green_roof_table"),
      unpaved_area_table = attr(new_blocks, "unpaved_area_table"),
      swale_connection_table = attr(new_blocks, "swale_connection_table"),
      ref_output_tables = read_output_tables(xls_file)
    )
  }
}

# Working with targets in the "new" reference system ---------------------------
if (FALSE)
{
  # Statistics on measure implementation in new reference system (share of total
  # area)
  get_measure_stats_new(blocks)

  # Convert new targets (all percentages referring to total area) to targets
  # that are expected by distribute_measures()
  new_targets <- list(green_roof = 0.259, unpaved = 0.5, to_swale = 0.67)

  targets <- rescale_target_values(
    new_targets,
    total_area = sum(kwb.rabimo:::get_main_area(blocks)),
    total_roof_area = sum(kwb.rabimo:::get_roof_area(blocks)),
    total_sealed_area = sum(kwb.rabimo:::get_sealed_area(blocks))
  )
}

# provide_rabimo_input_based_on_excel_input ------------------------------------
provide_rabimo_input_based_on_excel_input <- function(ref_input_table)
{
  # From that table, provide the columns representing absolute areas
  block_areas <- kwb.utils::renameAndSelect(ref_input_table, list(
    area = "total_area",
    roof = "roof_area",
    green_roof = "green_roof_area",
    paved = "pvd_area",
    sca = "to_swale_area"
  ))

  # From these columns, generate "full" block records as required by R-Abimo
  blocks <- create_example_blocks(block_areas)

  # Check if blocks can be processed by R-Abimo and compare block data with
  # what is calculated in the Excel file
  check_validity_of_inputs(blocks, block_areas, ref_input_table)

  blocks
}

# create_example_blocks --------------------------------------------------------
create_example_blocks <- function(block_areas)
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
  blocks$sealed <- kwb.rabimo:::get_sealed(blocks)
  blocks$to_swale <- get_to_swale(cbind(blocks, block_areas))

  # Provide columns in the same order as they are stored in the package
  kwb.utils::selectColumns(blocks, columns = setdiff(
    names(kwb.rabimo::rabimo_inputs_2020$data), "block_type"
  ))
}

# check_validity_of_inputs -----------------------------------------------------
check_validity_of_inputs <- function(blocks, block_areas, ref_input_table)
{
  # Check if the blocks are accepted by R-Abimo
  kwb.rabimo:::stop_on_invalid_data(blocks)

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

# check_equality ---------------------------------------------------------------
check_equality <- function(a, b)
{
  stopifnot(all.equal(a, b))
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

# rescale_target_values --------------------------------------------------------
rescale_target_values <- function(
    new_targets, total_area, total_roof_area, total_sealed_area
)
{
  select_elements <- kwb.utils::selectElements

  green_roof_new <- select_elements(new_targets, "green_roof")
  unpaved_new <- select_elements(new_targets, "unpaved")
  to_swale_new <- select_elements(new_targets, "to_swale")

  green_roof <- green_roof_new * total_area / total_roof_area
  unpaved <- unpaved_new
  to_swale <- to_swale_new * total_area / total_sealed_area

  # TODO: calculate
  max_allowed_unpaved <- 1

  stopifnot(green_roof <= 1)
  stopifnot(unpaved <= max_allowed_unpaved)
  stopifnot(to_swale <= 1)

  list(green_roof = green_roof, unpaved = unpaved, to_swale = to_swale)
}

# get_measure_stats_new --------------------------------------------------------
get_measure_stats_new <- function(blocks, refer_to_total = TRUE)
{
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

  # area_total <- sum(kwb.rabimo:::get_main_area(blocks))
  # area_green_roof <- sum(kwb.rabimo:::get_green_roof_area(blocks))
  # area_roof <- sum(kwb.rabimo:::get_roof_area(blocks))
  # area_unpvd <- sum(kwb.rabimo:::get_unpaved_area(blocks))
  # area_pvd <- sum(kwb.rabimo:::get_paved_area(blocks))
  # area_sca <- sum(kwb.rabimo:::get_to_swale_area(blocks))
  # area_sealed <- sum(kwb.rabimo:::get_sealed_area(blocks))
  #
  # mean_green_roof <- area_green_roof / area_total
  # max_green_roof <- area_roof / area_total
  # mean_unpvd <- area_unpvd / area_total
  # max_unpvd <- area_pvd / area_total
  # mean_sca <- area_sca / area_total
  # max_sca <- area_sealed / area_total

  cbind(
    mean = c(
      green_roof = mean_green_roof,
      unpaved = mean_unpvd,
      to_swale = mean_sca
    ),
    max = c(
      green_roof = max_green_roof,
      unpaved = max_unpvd,
      to_swale = max_sca
    )
  )
}

# check_distribution -----------------------------------------------------------
check_distribution <- function(
    green_roof_table,
    unpaved_area_table,
    swale_connection_table,
    ref_output_tables
)
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
