## code to prepare datasets goes here

# do you want to use georeferenced data?
use_shape <- TRUE

rabimo_inputs_2025 <- if (use_shape) {
  inputs <- readRDS("../abimo.scripts/assets/rabimo_inputs_2025_geo.rds")
  inputs$data <- sf::st_as_sf(inputs$data)
  Encoding(attr(inputs$data$Shape, "crs")$wkt) <- "UTF-8"
  inputs
} else {
  readRDS("../abimo.scripts/assets/rabimo_inputs_2025.rds")
}

usethis::use_data(rabimo_inputs_2025, overwrite = TRUE, compress = "xz")
