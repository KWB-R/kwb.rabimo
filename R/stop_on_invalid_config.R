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

  check_values_for_surface_types(x = bagrov_values)
  check_values_for_surface_types(x = runoff_factors)
  
  if (is_new_format <- !is.null(config$measures)) {
    green_roof_configs <- select_elements(config$measures, "green_roof")
    infiltration_configs <- select_elements(config$measures, "infiltration")
    columns_green_roof <- sapply(
      green_roof_configs, 
      FUN = select_elements,
      elements = "input_column"
    )
    columns_infiltration <- sapply(
      infiltration_configs, 
      FUN = select_elements, 
      elements = "input_column"
    )
    if (length(columns_green_roof) != length(unique(columns_green_roof))) {
      kwb.utils::stopFormatted(
        "The <input_column>s in config$measures$green_roof (%s) are not unique as expected.", 
        kwb.utils::stringList(columns_green_roof)
      )
    }
    if (length(columns_infiltration) != length(unique(columns_infiltration))) {
      kwb.utils::stopFormatted(
        "The <input_column>s in config$measures$infiltration (%s) are not unique as expected.", 
        kwb.utils::stringList(columns_infiltration)
      )
    }
    
  } else {
    stopifnot("green_roof" %in% names(bagrov_values))
  }
  
}

