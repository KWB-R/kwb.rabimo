# stop_on_invalid_data ---------------------------------------------------------
#' @importFrom rlang .data
stop_on_invalid_data <- function(data)
{
  # Read information on column names and types
  column_info <- read_column_info()

  # Helper function to lookup column names matching a property value
  columns_with <- function(property, value) {
    fetch <- create_accessor(column_info)
    fetch("rabimo_berlin")[fetch(property) == value]
  }

  # Helper function to check values in columns
  check_columns <- function(data, columns, check, msg) {
    for (column in columns) {
      stopifnot(is.function(check))
      failed <- !check(select_columns(data, column))
      if (any(failed)) {
        stop_formatted(msg, column, sum(failed))
      }
    }
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

  check_sum_up_to_1_or_0(data, matching_names(data, pattern_no_roads()))

  if (length(columns <- matching_names(data, pattern_roads()))) {
    check_sum_up_to_1_or_0(data, columns)
  }
}

# get_expected_data_type -------------------------------------------------------
get_expected_data_type <- function(columns = NULL)
{
  columns_to_named_vector <- function(data, key_column, value_column) {
    select_columns(data, value_column) %>%
      stats::setNames(select_columns(data, key_column))
  }

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
  select_columns <- kwb.utils::selectColumns
  
  # Helper function to check for equality allowing a tolerance
  equals <- function(a, b) abs(a - b) <= tolerance

  column_data <- select_columns(data, columns)
  
  # Check for non-numeric columns
  is_numeric <- sapply(column_data, is.numeric)
  if (any(!is_numeric)) {
    clean_stop(
      "There are non-numeric columns in check_sum_up_to_1_or_0(): ",
      paste0('"', columns[!is_numeric], '"', collapse = ", ")
    )
  }
  
  sums <- rowSums(column_data)
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
