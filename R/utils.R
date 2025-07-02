# cat_and_run ------------------------------------------------------------------
#' @importFrom kwb.utils catAndRun
cat_and_run <- kwb.utils::catAndRun

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# check_or_convert_data_types --------------------------------------------------
#' @importFrom kwb.utils stopFormatted
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
        kwb.utils::stopFormatted(
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

# in_range ---------------------------------------------------------------------
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

# matching_names ---------------------------------------------------------------
matching_names <- function(data, pattern)
{
  grep(pattern, names(data), value = TRUE)
}

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

# rename_and_select ------------------------------------------------------------
#' @importFrom kwb.utils renameAndSelect
rename_and_select <- kwb.utils::renameAndSelect

# rename_columns ---------------------------------------------------------------
#' @importFrom kwb.utils renameColumns
rename_columns <- kwb.utils::renameColumns

# select_columns ---------------------------------------------------------------
#' @importFrom kwb.utils selectColumns
select_columns <- kwb.utils::selectColumns

# select_elements --------------------------------------------------------------
#' @importFrom kwb.utils selectElements
select_elements <- kwb.utils::selectElements
