if (FALSE)
{
  config_2020 <- kwb.rabimo::rabimo_inputs_2020$config
  config_2025 <- kwb.rabimo::rabimo_inputs_2025$config
  
  str(config_2020)
  str(kwb.rabimo:::reconfigure(config = config_2020))
  
  str(config_2025)
  str(kwb.rabimo:::reconfigure(config = config_2025))
  
  config$green_roof <- list(
    list(input_column = "green_roof", bagrov_value = 0.5)
  )
  
  config$green_roof <- list(
    list(input_column = "green_roof_ext", bagrov_value = 0.5),
    list(input_column = "green_roof_int", bagrov_value = 0.5)
  )
}

# reconfigure ------------------------------------------------------------------
reconfigure <- function(config)
{
  # Provide vector of Bagrov values
  bagrov_values <- config$bagrov_values
  
  # Remove element "green_roof" from vector of Bagrov values
  config$bagrov_values <- bagrov_values[names(bagrov_values) != "green_roof"]
  
  config$measures <- list(
    green_roof = list(
      list(
        input_column = "green_roof", 
        bagrov_value = bagrov_values[["green_roof"]]
      )
    ),
    infiltration = list(
      list(
        input_column = "to_swale",
        # Use evaporation factor from given config
        evaporation_factor = config$swale[["swale_evaporation_factor"]], 
        overflow_factor = 0
      )
    ),
    retention = list(
      list(
        input_column = "to_storage",
        overflow_factor = 0.5
      )
    )
  )
  
  # Remove old config$swale
  config$swale <- NULL
  
  config
}
