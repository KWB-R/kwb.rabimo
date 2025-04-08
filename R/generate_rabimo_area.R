# generate_rabimo_area ---------------------------------------------------------

#' Generate an area in R-ABIMO format with default values
#'
#' All default values can be overridden by entering new key-value pairs.
#'
#' @param code identifier of area
#' @param \dots key = value pairs overriding the default column values
#' @param column_info data frame as returned by \code{\link{read_column_info}}
#' @export
generate_rabimo_area <- function(code, ..., column_info = read_column_info())
{
  #kwb.utils::assignPackageObjects("kwb.rabimo");column_info=read_column_info();`%>%`<-magrittr::`%>%`

  # Remove rows that describe Berlin-specific columns
  types <- select_columns(column_info, "type")

  dictionary <- column_info[types != "berlin-specific", ] %>%
    select_columns(c("rabimo_berlin", "default", "data_type"))

  # Create a list of key-value pairs
  key_value_pairs <- dictionary$default %>%
    as.list() %>%
    stats::setNames(dictionary$rabimo_berlin)

  # Convert numeric columns to numeric
  is_numeric <- dictionary$data_type == "numeric"
  key_value_pairs[is_numeric] <- lapply(key_value_pairs[is_numeric], as.numeric)

  # Convert integer columns to integer
  is_integer <- dictionary$data_type == "integer"
  key_value_pairs[is_integer] <- lapply(key_value_pairs[is_integer], as.integer)

  # Compose a one-row data frame from the key-value pairs
  further_args <- list(...)
  #further_args <- list()
  args <- key_value_pairs
  args[names(further_args)] <- further_args

  result <- do.call(data.frame, args)

  # Add column "code"
  result["code"] <- code
  result
}
