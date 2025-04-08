# read_column_info -------------------------------------------------------------

#' Provide Meta Information About Input Columns
#'
#' @returns data frame with columns "rabimo_berlin", "abimo_berlin", "by_100",
#'   "meaning", "unit", "type", "data_type", "default"
#' @export
read_column_info <- function()
{
  "extdata/column-names.csv" %>%
    system.file(package = "kwb.rabimo") %>%
    utils::read.table(
      header = TRUE,
      stringsAsFactors = FALSE,
      quote = "",
      sep = ",",
      dec = "."
    )
}
