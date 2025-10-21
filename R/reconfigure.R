if (FALSE)
{
  config_2020 <- kwb.rabimo::rabimo_inputs_2020$config
  config_2025 <- kwb.rabimo::rabimo_inputs_2025$config
  
  str(config_2020)
  str(kwb.rabimo:::reconfigure(config = config_2020))
  
  str(config_2025)
  str(kwb.rabimo:::reconfigure(config = config_2025))
  
  config$green_roof <- list(
    list(roof_fraction_column = "green_roof", bagrov_value = 0.5)
  )
  
  config$green_roof <- list(
    list(roof_fraction_column = "green_roof_ext", bagrov_value = 0.5),
    list(roof_fraction_column = "green_roof_int", bagrov_value = 0.5)
  )
}

# reconfigure ------------------------------------------------------------------
reconfigure <- function(config)
{
  #config <- config_2020
  
  # Provide vector of Bagrov values
  bagrov_values <- config$bagrov_values
  
  # Remove element "green_roof" from vector of Bagrov values
  config$bagrov_values <- bagrov_values[names(bagrov_values) != "green_roof"]
  
  # Provide evaporation factor for infiltration method
  evaporation_factor <- config$swale[["swale_evaporation_factor"]]
  
  # Remove config$swale
  config$swale <- NULL
  
  config$measures <- list(
    green_roof = list(
      list(
        roof_fraction_column = "green_roof", 
        bagrov_value = bagrov_values[["green_roof"]]
      )
    ),
    infiltration = list(
      list(
        area_fraction_column = "to_swale",
        evaporation_factor = evaporation_factor
        #, overflow_rate = 0 # not yet supported!
      )
      # , list(
      #   name = "rigole",
      #   area_fraction_column = "to_inf_rigole",
      #   evaporation_factor = 0,
      #   overflow_rate = 0
      #   #, rigole_specific_factor = 1
      # )
    )
  )
  
  config
}
