library(terra)
library(stringr)
library(R.utils)

# Set the directory where files are stored
data_dir <- "C:/Users/fdpunt/Downloads/dwd/grids_germany/monthly/evapo_p"
output_dir <- "C:/Users/fdpunt/Documents/Projekte/AMAREX/Daten/Raster/epot"
unzipped_dir <- file.path(data_dir, "unzipped")

# download and save data
if(FALSE) {
  from <- "199101"
  to <- "202312"
  pot_evaporation_monthly <- kwb.dwd::load_potential_evaporation_berlin(from, to)
}

# List all .asc.gz files
files <- list.files(data_dir, pattern = "grids_germany_monthly_evapo_p_\\d{6}\\.asc\\.gz$", full.names = TRUE)
file_names <- basename(files)

grid <- kwb.dwd::read_asc_gz_file(files[1])

# Extract year and month information
timestamp <- str_extract(file_names, "\\d{6}")
year <- as.numeric(substr(timestamp, 1, 4))
month <- as.numeric(substr(timestamp, 5, 6))

# Create the data frame
file_info <- data.frame(
  file = files,
  year = year,
  month = month
)

districts <- sf::read_sf("C:/Users/fdpunt/Documents/Projekte/AMAREX/Daten/Bezirke/Bezirke/ISU5_UA_FL_2020_Bezirke.shp")

# Function to check and handle raster values before saving
process_raster <- function(raster, scale = 0.1) {

  raster <- raster * scale
  return(raster)
}

# Process yearly total for all months
years <- 1991:2020
raster_list_year <- list()
raster_list_summer <- list()

# Loop over each year
for (i in seq_along(years)) {
  # Get files for the current year
  yr <- years[i]
  year_files <- file_info$file[file_info$year == yr]
  summer_files <- file_info$file[file_info$year == yr &
                                   file_info$month %in% 5:10]

  # Read all monthly rasters for the current year
  rasters_year <- lapply(year_files, kwb.dwd::read_asc_gz_file)
  rasters_year_stack <- raster::stack(rasters_year)

  # Read all summer rasters for the current year
  rasters_summer <- lapply(summer_files, kwb.dwd::read_asc_gz_file)
  rasters_summer_stack <- raster::stack(rasters_summer)

  # Sum rasters to get the yearly total
  yearly_sum <- sum(rasters_year_stack, na.rm = TRUE)
  summer_sum <- sum(rasters_summer_stack, na.rm = TRUE)

  # Process raster (data scaling)
  yearly_sum <- process_raster(yearly_sum)
  summer_sum <- process_raster(summer_sum)

  # Save raster to list
  raster_list_year[[as.character(yr)]] <- yearly_sum
  raster_list_summer[[as.character(yr)]] <- summer_sum
  print(paste("Stored rasters for year:", yr))

  # Define the output file path
  output_file_year <- file.path(output_dir, paste0("evapo_p_total_", yr, ".tif"))
  output_file_summer <- file.path(output_dir, paste0("evapo_p_summer_", yr, ".tif"))

  # Save the raster to a .tif file
  terra::writeRaster(yearly_sum, output_file_year, overwrite = TRUE)
  terra::writeRaster(summer_sum, output_file_summer, overwrite = TRUE)

  # visualize the result (for verification)
  plot(yearly_sum, main = paste("Yearly Sum of Potential Evaporation", yr))
  plot(summer_sum, main = paste("Summer Sum of Potential Evaporation", yr))
}

# Calculate average raster
years_stack <- raster::stack(raster_list_year)
average_epot_year <- mean(rast(years_stack), na.rm = TRUE)

summers_stack <- raster::stack(raster_list_summer)
average_epot_summer <- mean(rast(summers_stack), na.rm = TRUE)

# Reproject the shapefile to match the raster's CRS
average_epot_crs <- sf::st_crs(average_epot_year)$proj4string
berlin_reprojected <- sf::st_transform(districts, average_epot_crs)

# Save the rasters
output_file <- file.path(output_dir, paste0("yearly-average_", min(years), "-", max(years), ".tif"))
terra::writeRaster(average_epot_year, output_file, overwrite = TRUE)

output_file <- file.path(output_dir, paste0("summer-average_", min(years), "-", max(years), ".tif"))
terra::writeRaster(average_epot_summer, output_file, overwrite = TRUE)

# Visualize the raster with the reprojected shapefile
plot(average_epot_year, , main = paste0("average epot ", min(years), "-", max(years)))
plot(average_epot_summer, , main = paste0("average summer epot ", min(years), "-", max(years)))

# mask and crop Berlin
berlin_epot_avg_year <- (raster::mask(average_epot_year, berlin_reprojected[,1]))
berlin_epot_avg_year_crop <- raster::crop(berlin_epot_avg_year, berlin_reprojected[,1])
plot(berlin_epot_avg_year_crop,
     main = "mean yearly potential evaporation berlin", axes = NA)
plot(berlin_reprojected[,1], col = NA, border = "white", add = TRUE)
district_centroids <- sf::st_centroid(berlin_reprojected)
text(sf::st_coordinates(district_centroids),
     labels = berlin_reprojected$bezneu, col = 'white', cex = 0.4)

berlin_epot_avg_summer <- (raster::mask(average_epot_summer, berlin_reprojected[,1]))
berlin_epot_avg_summer_crop <- raster::crop(berlin_epot_avg_summer, berlin_reprojected[,1])
plot(berlin_epot_avg_summer_crop,
     main = "mean summer potential evaporation berlin", axes = NA)
plot(berlin_reprojected[,1], col = NA, border = "white", add = TRUE)
district_centroids <- sf::st_centroid(berlin_reprojected)
text(sf::st_coordinates(district_centroids),
     labels = berlin_reprojected$bezneu, col = 'white', cex = 0.4)

# Convert the sf object to a SpatVector
districts_spatvector <- terra::vect(berlin_reprojected)

# Extract mean raster values for each district
district_means_year <- terra::extract(berlin_epot_avg_year_crop,
                                 districts_spatvector,
                                 fun = mean,
                                 na.rm = TRUE)

district_means_summer <- terra::extract(berlin_epot_avg_summer_crop,
                                      districts_spatvector,
                                      fun = mean,
                                      na.rm = TRUE)

epot_berlin_1991_2020 <- berlin_reprojected %>%
  dplyr::select(ID = bezneu, district = bezirk) %>%
  dplyr::mutate(ID = as.integer(ID)) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(district_means_year, by = "ID") %>%
  dplyr::mutate(total_mean = as.integer(round(mean))) %>%
  dplyr::select(-mean) %>%
  dplyr::left_join(district_means_summer, by = "ID") %>%
  dplyr::mutate(summer_mean = as.integer(round(mean))) %>%
  dplyr::select(-mean)


