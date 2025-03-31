# Functions about the natural scenarios and the calculation of Delta-W

# data_to_natural --------------------------------------------------------------

#' Transform R-Abimo input Data into their natural scenario equivalent
#'
#' Three scenarios are possible:
#' 1) undeveloped: all paved or constructed areas are set to 0%. No connection
#'   to the sewer.
#' 2) forested: like undeveloped, but the land type is declared to be
#'  "forested".
#' 3) horticultural: like undeveloped, but the land type is declared to be
#'   "horticultural".
#'
#' @param data the input data in R-Abimo format
#' @param type a character object containing the name of natural scenario.
#'   Defaults to "undeveloped"
#' @return a dataframe with R-Abimo input data for the chosen natural scenario
#' @export
data_to_natural <- function(data, type = "undeveloped")
{
  # kwb.utils::assignPackageObjects("kwb.rabimo")
  # data <- kwb.rabimo::rabimo_inputs_2020$data; type = "undeveloped"

  # Check if data has R-Abimo format
  stop_on_invalid_data(data)

  # Columns related to urbanisation
  urban_columns <- grep("pv|swg|roof", names(data), value = TRUE)

  # non urbanized state: no building, no pavements
  nat_data <- data
  nat_data[urban_columns] <- 0

  # vegetation class 50
  nat_data["veg_class"] <- 50L

  if (type == "undeveloped") {
    return(nat_data)
  }

  land_types <- select_columns(data, "land_type")
  is_waterbody <- land_type_is_waterbody(land_types)

  nat_data[["land_type"]][!is_waterbody] <- if (type == "forested") {
    "forested"
  } else if (type == "horticultural") {
    "horticultural"
  } else {
    stop("please provide a known natural scenario type: undeveloped, horticultural or forested")
  }

  # Convert data types as required
  check_or_convert_data_types(
    nat_data,
    types = get_expected_data_type(),
    convert = TRUE
  )
}

# calculate_delta_w ------------------------------------------------------------

#' Deviation from Natural Water Balance (Delta-W)
#'
#' Calculate the deviation from the natural water balance (delta-W) given
#' R-Abimo results as returned by \code{\link{run_rabimo}}.
#'
#' @param natural R-Abimo results for the "natural" scenario
#' @param urban R-Abimo results for the "urban" scenario
#' @param \dots further arguments passed to the implementation function as
#'   specified in \code{implementation}
#' @param implementation one of 1, 2, 3, indicating a different implementation
#' @return a data frame with the area codes in column \code{code} and the
#'   delta-W values in column \code{delta_w}
#' @export
calculate_delta_w <- function(natural, urban, ..., implementation = 3L)
{
  FUN <- if (implementation == 1) {
    calculate_delta_w_1
  } else if (implementation == 2) {
    calculate_delta_w_2
  } else if (implementation == 3) {
    calculate_delta_w_3
  } else {
    clean_stop("implementation must be one of 1, 2, 3.")
  }

  FUN(natural, urban, ...)
}

# calculate_delta_w_1 ----------------------------------------------------------

#' Deviation from Natural Water Balance (Delta-W)
#'
#' Calculate the deviation from the natural water balance (delta-W) given
#'  R-Abimo results as returned by \code{\link{run_rabimo}}.
#'
#' @param natural R-Abimo results for the natural scenario
#' @param urban R-Abimo results for the "urban" scenario
#' @param cols_to_omit column names that not contain result data or code identifiers. Defaults to "total_area"
#' @param return_codes a logical value determining whether the codes should be returned along the delta-w values
#' @return a dataframe containing the delta-w values (and optionally the areas' codes)
calculate_delta_w_1 <- function(
    natural,
    urban,
    cols_to_omit = c("area"),
    return_codes = FALSE
)
{
  stopifnot("code" %in% names(natural))
  stopifnot("code" %in% names(urban))
  stopifnot(all(urban[["code"]] %in% natural[["code"]]))

  combined <- dplyr::left_join(
    urban,
    kwb.utils::removeColumns(natural, cols_to_omit),
    by = "code", suffix = c("_u","_n")
  )

  # variable columns
  pattern <- "_[un]$"
  column_names <- grep(pattern, names(combined), value = TRUE)
  unique_names <- unique(sub(pattern, "", column_names))

  precipitation <- rowSums(combined[,column_names])/2

  diff_list <- list()

  for (name in unique_names) {
    urban_name <- paste0(name, "_u")
    natural_name <- paste0(name, "_n")

    diff_list[[name]] <- abs(combined[[urban_name]] - combined[[natural_name]])
  }

  delta_w <- round(rowSums(as.data.frame(diff_list))/2*100/precipitation,1)

  if(return_codes){
    data.frame(code = urban[["code"]], delta_w = delta_w)
  } else {
    data.frame(delta_w = delta_w)
  }
}

# calculate_delta_w_2 ----------------------------------------------------------
calculate_delta_w_2 <- function(
    natural,
    urban,
    columns_water_balance = c("runoff", "infiltr", "evapor"),
    column_code = "code"
)
{
  stopifnot(column_code %in% names(natural))
  stopifnot(column_code %in% names(urban))
  stopifnot(all(urban[[column_code]] %in% natural[[column_code]]))

  urban_codes <- urban[[column_code]]

  natural_selection <- natural %>%
    dplyr::filter(.data[["code"]] %in% urban_codes) %>%
    dplyr::arrange(match(.data[["code"]], urban_codes)) %>%
    `rownames<-`(NULL)

  diff_matrix <- abs(
    natural_selection[columns_water_balance] - urban[columns_water_balance])

  precipitation <- rowSums(natural_selection[columns_water_balance])

  cbind(
    code = urban_codes,
    data.frame(delta_w = round(rowSums(diff_matrix)*100/precipitation/2, 1))
  )
}

# calculate_delta_w_3 ----------------------------------------------------------
calculate_delta_w_3 <- function(
    natural,
    urban,
    columns_water_balance = c("runoff", "infiltr", "evapor"),
    column_code = "code"
)
{
  columns <- c(column_code, columns_water_balance)
  data_urban <- select_columns(urban, columns)
  data_natural <- select_columns(natural, columns)

  codes <- data_urban[[1L]]
  matching_rows <- match(codes, data_natural[[1L]])
  code_not_found <- is.na(matching_rows)

  if (any(code_not_found)) {
    stop_formatted(
      "Cannot find %d 'urban' codes in 'natural': %s",
      sum(code_not_found),
      paste(codes[code_not_found], collapse = ", ")
    )
  }

  # Convert data frames to matrices of the same size
  m_urban <- as.matrix(data_urban[-1L])
  m_natural <- as.matrix(data_natural[matching_rows, -1L])

  # Calculate delta-W. Precipitation = rowSums(m_natural)
  delta_ws <- rowSums(abs(m_urban - m_natural)) / rowSums(m_natural) * 100 / 2

  data.frame(
    code = codes,
    delta_w = unname(round(delta_ws, digits = 1L)),
    stringsAsFactors = FALSE
  )
}
