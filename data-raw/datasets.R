## code to prepare datasets goes here

# do you want to use georeferenced data?
use_shape <- TRUE

if(use_shape){
  rabimo_inputs_2025 <- readRDS("../abimo.scripts/assets/rabimo_inputs_2025_geo.rds")
  sf::st_as_sf(rabimo_inputs_2025$data)
} else {
  rabimo_inputs_2025 <- readRDS("../abimo.scripts/assets/rabimo_inputs_2025.rds")
}

usethis::use_data(rabimo_inputs_2025, overwrite = TRUE, compress = "xz")
