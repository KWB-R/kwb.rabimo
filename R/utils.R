# cat_and_run ------------------------------------------------------------------
#' @importFrom kwb.utils catAndRun
cat_and_run <- kwb.utils::catAndRun

# cat_if -----------------------------------------------------------------------
#' @importFrom kwb.utils catIf
cat_if <- kwb.utils::catIf

# check_columns ----------------------------------------------------------------
check_columns <- function(
    data, columns, check, msg = "Column '%s' is invalid (%d-times)."
)
{
  #column <- columns[1L]
  for (column in columns) {
    check_column(data, column, check, msg)
  }
}

# check_column -----------------------------------------------------------------
check_column <- function(data, column, check, msg)
{
  stopifnot(is.function(check))

  failed <- !check(select_columns(data, column))

  if (any(failed)) {
    stop_formatted(msg, column, sum(failed))
  }
}

# check_for_missing_columns ----------------------------------------------------
#' @importFrom kwb.utils checkForMissingColumns
check_for_missing_columns <- kwb.utils::checkForMissingColumns

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# columns_to_named_vector ------------------------------------------------------
columns_to_named_vector <- function(data, key_column, value_column)
{
  select_columns(data, value_column) %>%
    stats::setNames(select_columns(data, key_column))
}

# check_or_convert_data_types --------------------------------------------------
check_or_convert_data_types <- function(
    data, types, convert = FALSE, dbg = TRUE
)
{
  columns <- intersect(names(data), names(types))

  for (column in columns) {
    #column <- columns[2L]
    new_type <- types[[column]]
    old_type <- class(data[[column]])[1L]
    if (old_type != new_type) {
      if (!convert) {
        stop_formatted(
          "Column '%s' (%s) does not have the expected data type (%s).",
          column, old_type, new_type
        )
      }
      cat_and_run(
        sprintf("Converting %s from %s to %s", column, old_type, new_type),
        dbg = dbg,
        data[[column]] <- do.call(paste0("as.", new_type), list(data[[column]]))
      )
    }
  }

  data
}

# create_accessor --------------------------------------------------------------
#' @importFrom kwb.utils createAccessor
create_accessor <- kwb.utils::createAccessor

# default_if_null --------------------------------------------------------------
#' @importFrom kwb.utils defaultIfNULL
default_if_null <- kwb.utils::defaultIfNULL

# expand_to_matrix -------------------------------------------------------------
expand_to_matrix <- function(x, nrow = NULL, ncol = NULL)
{
  if (is.null(nrow) && is.null(ncol) || !is.null(nrow) && !is.null(ncol)) {
    clean_stop(
      "Either nrow or ncol must be given but not both at the same time."
    )
  }

  if (!is.null(nrow)) {
    return(matrix(rep(x, nrow), nrow = nrow, byrow = TRUE))
  }

  if (!is.null(ncol)) {
    return(matrix(rep(x, ncol), ncol = ncol, byrow = FALSE))
  }
}

# filter_elements --------------------------------------------------------------
filter_elements <- function(x, pattern)
{
  x[grepl(pattern, names(x))]
}

# first_upper ------------------------------------------------------------------
first_upper <- function(x) {
  chars <- strsplit(x, "")
  paste0(
    toupper(sapply(chars, "[", 1L)),
    sapply(chars, function(x) paste(x[-1L], collapse = ""))
  )
}

# get_attribute ----------------------------------------------------------------
#' @importFrom kwb.utils getAttribute
get_attribute <- kwb.utils::getAttribute

# helpers_index ----------------------------------------------------------------
helpers_index <- function(x, values, epsilon = 0.0001, dbg = FALSE)
{
  if (length(x) > 1L) {
    return(sapply(x, helpers_index, values, epsilon, dbg))
  }

  stopifnot(length(x) == 1L)

  indices <- which(x <= values + epsilon)
  index <- ifelse(length(indices), min(indices), length(values)) - 1L

  print_if(dbg, x)
  print_if(dbg, values)
  print_if(dbg, indices)
  print_if(dbg, index)

  index
}

# int Calculation::index(float wert, float *feld, int anz)
# {
#   int i;
#   float eps = 0.0001;
#   for (i = 0; i < anz; i++)
#     if (wert <= feld[i] + eps) return(i);
#   return(anz - 1);
# }

# index_string_to_integers -----------------------------------------------------
#' Convert String of Integer Ranges to Vector of Integer
#'
#' Convert e.g. "1,4-6,10-11,20" to c(1L, 4L, 5L, 6L, 10L, 11L, 20L)
#'
#' @param x vector of character of length one representing a string
#'   of integer ranges, e.g. \code{"1,4-6,10-11,20"}
#' @param splits characters at which to 1. split \code{x} into range strings, 2.
#'  split the range strings into begin and end values of the ranges. Default:
#'  \code{c(",", "-")}
#' @return vector of integer
#' @export
#' @examples
#' index_string_to_integers("1,4-6,10-11,20")
#'
index_string_to_integers <- function(x, splits = c(",", "-"))
{
  do.call(c, lapply(strsplit(x, splits[1L])[[1L]], function(range_string) {
    from_to <- strsplit(range_string, splits[2L])[[1L]]
    seq.int(
      as.integer(from_to[1L]),
      as.integer(from_to[1L + (length(from_to) > 1L)]),
      by = 1L
    )
  }))
}

# in_range ---------------------------------------------------------------------
#' @importFrom kwb.utils inRange
in_range <- function(x, a, b, tolerance = 0.005)
{
  x + tolerance >= a & x - tolerance <= b
}

# interpolate ------------------------------------------------------------------
interpolate <- function(x, y, xout)
{
  yout <- rep(NA_real_, length(xout))

  nx <- length(x)

  yout[xout <= x[1L]] <- y[1L]
  yout[xout >= x[nx]] <- y[nx]

  if (any(is_na <- is.na(yout))) {
    yout[is_na] <- sapply(xout[is_na], function(xi) {
      i <- which(xi <= x[-1L])[1L] + 1L
      (y[i - 1L] + y[i]) / 2
    })
  }

  yout
}

interpolate_cpp <- function(xi, x, y)
{
  n <- length(x)
  stopifnot(n == length(y))

  if (xi <= x[1L]) {
    return(y[1L])
  }

  if (xi >= x[n]) {
    return(y[n])
  }

  for (i in seq_len(n)) {
    print(i)
    if (xi <= x[i + 1L]) {
      print(y[i])
      print(y[i+1])
      print ((y[i] + y[i+1]) / 2.0)
      return ((y[i] + y[i+1]) / 2.0)
    }
  }

  return(0.0)
}

# list_to_data_frame_with_keys -------------------------------------------------

#' Convert List of Similar Flat Sublists to a Data Frame
#'
#' @param x list of similar flat lists, i.e. lists that have list elements with
#'   the same names and list elements that all have length one
#' @param key_name name of column in the returned data frame that will contain
#'   the integer values that are constructed from the element names in \code{x}
#' @param key_pattern regular expression matching all element names in \code{x}.
#'   The expression must contain one pair of parentheses enclosing the part that
#'   is to be used as key, e.g. \code{"element_([0-9]+)"}
#' @param convert function to be applied to the (character) key. Set e.g.
#'   \code{convert = as.integer} to generate integer keys. Default:
#'   \code{\link{identity}}
#' @return data frame with keys in a column named according to \code{key_name}
#'   and value columns according to the list elements in the sublists of
#'   \code{x}
#' @export
#' @examples
#' list_to_data_frame_with_keys(
#'   x = list(
#'     element_1 = list(a = 100, b = 10),
#'     element_2 = list(a = 200, b = 20)
#'   ),
#'   key_name = "element",
#'   key_pattern = "element_([0-9]+)",
#'   convert = as.integer
#' )
list_to_data_frame_with_keys <- function(
    x, key_name, key_pattern, convert = identity
)
{
  elements <- names(x)

  stopifnot(is.list(x), all(grepl(key_pattern, elements)))

  result <- safe_row_bind_all(lapply(x, as.data.frame))

  result[[key_name]] <- convert(gsub(key_pattern, "\\1", elements))

  move_columns_to_front(result, key_name)
}

# matching_names ---------------------------------------------------------------
matching_names <- function(data, pattern)
{
  grep(pattern, names(data), value = TRUE)
}

# move_columns_to_front --------------------------------------------------------
#' @importFrom kwb.utils moveColumnsToFront
move_columns_to_front <- kwb.utils::moveColumnsToFront

# multi_column_lookup ----------------------------------------------------------
#' @importFrom kwb.utils multiColumnLookup
multi_column_lookup <- kwb.utils::multiColumnLookup

# multi_substitute -------------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute
multi_substitute <- kwb.utils::multiSubstitute

# paste_columns ----------------------------------------------------------------
#' @importFrom kwb.utils pasteColumns
paste_columns <- kwb.utils::pasteColumns

# prefix_names -----------------------------------------------------------------
prefix_names <- function(x, prefix)
{
  set_names(x, paste0(prefix, names(x)))
}

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

# range_to_seq -----------------------------------------------------------------

#' Sequence of Values Between the Range of Values in a Given Vector
#'
#' @param x vector of values from which to take the range
#' @param by increment of seqence
#' @return sequence of values between \code{min(x)} and \code{max(x)} with
#'   increment \code{by}
range_to_seq <- function(x, by = 1)
{
  do.call(seq, c(as.list(range(x)), list(by = by)))
}

# remove_columns ---------------------------------------------------------------
#' @importFrom kwb.utils removeColumns
remove_columns <- kwb.utils::removeColumns

# remove_elements --------------------------------------------------------------
#' @importFrom kwb.utils removeElements
remove_elements <- kwb.utils::removeElements

# remove_left ------------------------------------------------------------------
remove_left <- function(x, n)
{
  right(x, nchar(x) - n)
}

# rename_and_select ------------------------------------------------------------
#' @importFrom kwb.utils renameAndSelect
rename_and_select <- kwb.utils::renameAndSelect

# rename_columns ---------------------------------------------------------------
#' @importFrom kwb.utils renameColumns
rename_columns <- kwb.utils::renameColumns

# rescale_to_row_sum -----------------------------------------------------------
rescale_to_row_sum <- function(x, row_sum = 1)
{
  kwb.utils::stopIfNotMatrix(x)

  x / rowSums(x) * row_sum
}

# reset_row_names --------------------------------------------------------------
#' @importFrom kwb.utils resetRowNames
reset_row_names <- kwb.utils::resetRowNames

# right ------------------------------------------------------------------------
#' @importFrom kwb.utils right
right <- kwb.utils::right

# safe_path --------------------------------------------------------------------
#' @importFrom kwb.utils safePath
safe_path <- kwb.utils::safePath

# safe_row_bind_all ------------------------------------------------------------
#' @importFrom kwb.utils safeRowBindAll
safe_row_bind_all <- kwb.utils::safeRowBindAll

# select_columns ---------------------------------------------------------------
#' @importFrom kwb.utils selectColumns
select_columns <- kwb.utils::selectColumns

# select_elements --------------------------------------------------------------
#' @importFrom kwb.utils selectElements
select_elements <- kwb.utils::selectElements

# set_columns_to_zero ----------------------------------------------------------
set_columns_to_zero <- function(data, columns, check, text)
{
  stopifnot(is.data.frame(data))

  for (column in columns) {

    x <- data[[column]]

    stopifnot(is.numeric(x))

    meets_condition <- check(x)

    if (!any(meets_condition)) {
      next
    }

    data[[column]] <- cat_and_run(
      sprintf(
        "Setting %d value(s) in \"%s\" to 0 where %s",
        sum(meets_condition),
        column,
        text
      ),
      expr = {
        x[meets_condition] <- 0
        x
      }
    )
  }

  data
}

# set_columns_to_zero_where_almost_zero ----------------------------------------
set_columns_to_zero_where_almost_zero <- function(
    data, columns, threshold = 0.001
)
{
  set_columns_to_zero(
    data = data,
    columns = columns,
    check = function(x) abs(x) < threshold,
    text = paste("value <", threshold)
  )
}


# set_columns_to_zero_where_na -------------------------------------------------
set_columns_to_zero_where_na <- function(data, columns)
{
  set_columns_to_zero(
    data = data,
    columns = columns,
    check = is.na,
    text = "value is NA"
  )
}

# set_names --------------------------------------------------------------------
#' @importFrom stats setNames
set_names <- stats::setNames

# stop_formatted ---------------------------------------------------------------
#' @importFrom kwb.utils stopFormatted
stop_formatted <- kwb.utils::stopFormatted

# string_list ------------------------------------------------------------------
#' @importFrom kwb.utils stringList
string_list <- kwb.utils::stringList

# subst_special_chars ----------------------------------------------------------
#' @importFrom kwb.utils substSpecialChars
subst_special_chars <- kwb.utils::substSpecialChars

# to_lookup_list ---------------------------------------------------------------
#' @importFrom kwb.utils toLookupList
to_lookup_list <- kwb.utils::toLookupList
