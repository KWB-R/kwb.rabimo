# stop_on_invalid_data ---------------------------------------------------------
stop_on_invalid_data <- function(data)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #data <- prepare_input_data(kwb.abimo::abimo_input_2019, abimo_config_to_config(kwb.abimo::read_config()))

  # Read information on column names and types
  column_info <- read_column_info()

  # Helper function to lookup column names matching a property value
  columns_with <- function(property, value) {
    fetch <- create_accessor(column_info)
    fetch("rabimo_berlin")[fetch(property) == value]
  }

  # Stop if any required column is missing
  missing <- setdiff(columns_with("type", "required"), names(data))

  if (length(missing)) {
    info <- dplyr::filter(column_info, .data[["rabimo_berlin"]] %in% missing)
    clean_stop("There are missing columns:\n", paste(collapse = "\n", sprintf(
        "- %s (%s)", info$rabimo_berlin, info$meaning
    )))
  }

  # Stop if a column does not have the expected data type
  check_or_convert_data_types(
    data = data,
    types = get_expected_data_type(columns = names(data)),
    convert = FALSE
  )

  # Do not accept any NA
  check_columns(
    data = data,
    columns = names(data) %>%
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
    data = data,
    columns = c("prec_yr", "prec_s", "epot_yr", "epot_s"),
    check = function(x) x >= 0,
    msg = paste(
      "There are negative values in column '%s' (%d-times).",
      "Please make sure that all values are greater than or equal to zero."
    )
  )

  # Check fractions
  check_columns(
    data = data,
    columns = columns_with("unit", "0..1") %>%
      intersect(names(data)),
    check = function(x) in_range(x, 0, 1),
    msg = paste(
      "Not all values in column '%s' are between 0 and 1 as expected",
      "(%d failures)."
    )
  )

  surface_cols_no_rd <- matching_names(data, pattern_no_roads())
  surface_cols_rd <- matching_names(data, pattern_roads())

  check_sum_up_to_1_or_0(data, surface_cols_no_rd)
  check_sum_up_to_1_or_0(data, surface_cols_rd)
}

# get_expected_data_type -------------------------------------------------------
get_expected_data_type <- function(columns = NULL)
{
  type_info <- read_column_info() %>%
    columns_to_named_vector(
      key_column = "rabimo_berlin",
      value_column = "data_type"
    )

  if (is.null(columns)) {
    return(type_info)
  }

  type_info[intersect(names(type_info), columns)]
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
