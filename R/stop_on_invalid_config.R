# stop_on_invalid_config -------------------------------------------------------
stop_on_invalid_config <- function(config)
{
  stopifnot(is.list(config))

  check_values_for_surface_types <- function(x) {
    stopifnot(is.numeric(x))
    stopifnot("roof" %in% names(x))
    stopifnot(identical(
      grep("^surface", names(x), value = TRUE),
      paste0("surface", 1:5)
    ))
  }

  bagrov_values <- select_elements(config, "bagrov_values")
  runoff_factors <- select_elements(config, "runoff_factors")

  x <- config$bagrov_values
  check_values_for_surface_types(x)

  x <- config$runoff_factors
  check_values_for_surface_types(x)
}

