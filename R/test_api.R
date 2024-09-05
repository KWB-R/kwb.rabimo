#' Test the API to main functions using the plumber package and swagger
#'
#' @export
test_api <- function()
{
  plumber_file <- system.file("scripts/plumber.R", package = "kwb.rabimo")
  plumber::pr_run(plumber::pr(plumber_file))
}
