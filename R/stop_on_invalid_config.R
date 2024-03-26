# stop_on_invalid_config -------------------------------------------------------
stop_on_invalid_config <- function(config)
{
  stopifnot(is.list(config))

  check_values_for_surface_types <- function(x) {
    stopifnot(is.numeric(x))
    stopifnot(length(x) == 6L)
    stopifnot("roof" %in% names(x))
    stopifnot("surface1" %in% names(x))
    stopifnot("surface2" %in% names(x))
    stopifnot("surface3" %in% names(x))
    stopifnot("surface4" %in% names(x))
    stopifnot("surface5" %in% names(x))
  }

  bagrov_values <- select_elements(config, "bagrov_values")
  runoff_factors <- select_elements(config, "runoff_factors")

  check_values_for_surface_types(config$bagrov_values)
  check_values_for_surface_types(config$runoff_factors)
}

