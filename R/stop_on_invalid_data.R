# stop_on_invalid_data ---------------------------------------------------------
stop_on_invalid_data <- function(input)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #input <- prepare_input_data(kwb.abimo::abimo_input_2019, abimo_config_to_config(kwb.abimo::read_config()))
  #input <- data

  # Read information on column names and types
  column_info <- read_column_info()

  # Helper function to lookup column names matching a property value
  columns_with <- function(property, value) {
    fetch <- create_accessor(column_info)
    fetch("rabimo_berlin")[fetch(property) == value]
  }

  # Stop if any required column is missing
  missing <- setdiff(columns_with("type", "required"), names(input))

  if (length(missing)) {
    column_info %>%
      dplyr::filter(.data[["rabimo_berlin"]] %in% missing) %>%
      rename_and_select(list(rabimo_berlin = "column", "meaning", "unit")) %>%
      reset_row_names() %>%
      print_if(condition = TRUE, caption = "Missing columns")
    clean_stop(
      "There are missing columns (see above). ",
      "Please make sure that you provide all required columns."
    )
  }

  # Stop if a column does not have the expected data type
  check_data_types(
    data = input,
    types = get_expected_data_type(names(input))
  )

  # Do not accept any NA
  check_columns(
    data = input,
    columns = names(input) %>%
      intersect(columns_with("data_type", "numeric")) %>%
      intersect(columns_with("type", "required")),
    check = function(x) !is.na(x),
    msg = paste(
      "Column '%s' must not contain missing values (NA, found %d times).",
      "Please give a value (may be 0) in each row."
    )
  )

  # Check precipitation and evapotranspiration for negative values
  check_columns(
    data = input,
    columns = c("prec_yr", "prec_s", "epot_yr", "epot_s"),
    check = function(x) x >= 0,
    msg = paste(
      "There are negative values in column '%s' (%d-times).",
      "Please make sure that all values are greater than or equal to zero."
    )
  )

  # Check fractions
  check_columns(
    data = input,
    columns = columns_with("unit", "0..1") %>%
      intersect(names(input)),
    check = function(x) in_range(x, 0, 1),
    msg = paste(
      "Not all values in column '%s' are between 0 and 1 as expected",
      "(%d failures)."
    )
  )

  pattern_no_roads <- "^srf.*pvd$"
  pattern_roads <- "^srf.*rd$"
  surface_cols_no_rd <- grep(pattern_no_roads, names(input), value = TRUE)
  surface_cols_rd <- grep(pattern_roads, names(input), value = TRUE)

  check_sum_up_to_1_or_0(input, surface_cols_no_rd)
  check_sum_up_to_1_or_0(input, surface_cols_rd)
}

# get_expected_data_type -------------------------------------------------------
get_expected_data_type <- function(x)
{
  info <- read_column_info() %>%
    dplyr::filter(.data[["rabimo_berlin"]] %in% x) %>%
    create_accessor()

  stats::setNames(info("data_type"), info("rabimo_berlin"))
}

# check_data_types -------------------------------------------------------------
check_data_types <- function(data, types)
{
  for (column in names(types)) {
    data_type <- mode(select_columns(data, column))
    if (data_type != types[column]) {
      stop_formatted(
        "Column '%s' (%s) does not have the expected data type (%s).",
        column, data_type, types[column]
      )
    }
  }
}

# check_sum_up_to_1_or_0 -------------------------------------------------------
check_sum_up_to_1_or_0 <- function(data, columns, tolerance = 0.005)
{
  # Helper function to check for equality allowing a tolerance
  equals <- function(a, b) abs(a - b) <= tolerance

  sums <- rowSums(select_columns(data, columns))
  ok <- equals(sums, 0) | equals(sums, 1)

  if (all(ok)) {
    return()
  }

  cat("(First) invalid rows:\n")

  select_columns(data, c("code", columns))[!ok, ] %>%
    utils::head() %>%
    print()

  stop_formatted(string_list(columns), tolerance, x = paste(
    "The sum of columns %s is not 1 or 0 in each row as expected",
    "(see above). The tolerance was: %f"
  ))
}
