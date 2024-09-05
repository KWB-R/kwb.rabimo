# 2nd Script to calibrate RABIMO storm water management elements with WABILA.

# SCOPE: For each RABIMO element to calibrate, find the bagrov value that
# minimizes the difference between ABIMO and WABILA results (delta-mod) in most
# cases, i.e. for the most climate combinations P/EPot

library(ggplot2)
`%>%` <- magrittr::`%>%`

# rabimo input data and config
inputs <- kwb.rabimo::rabimo_inputs_2020
config <- inputs$config
data <- inputs$data

# Factor summer climate data
factor_prec <- round(mean(data$prec_s/data$prec_yr),1) # ~0.5
factor_epot <- round(mean(data$epot_s/data$epot_yr),1) # ~0.8

# Set of precipation and potential evaporation values
prec_yr <- as.integer(c(seq(350, 900, by = 20)))
epot_yr <- as.integer(c(seq(350, 900, by = 50)))
# IDEA: Maybe only allow realistic combinations for the Berlin climate zone

# combinations of climate data
climates <- kwb.utils::expandGrid(prec_yr = prec_yr, epot_yr = epot_yr)
climates$prec_s <- as.integer(round(climates$prec_yr * factor_prec))
climates$epot_s <- as.integer(round(climates$epot_yr * factor_epot))

# Closest typical combination for berlin
mean_prec_yr <- mean(data$prec_yr)
mean_epot_yr <- mean(data$epot_yr)

berlin_climate <- climates %>%
  dplyr::mutate(distance = sqrt((prec_yr - mean_prec_yr)^2 + (epot_yr - mean_epot_yr)^2)) %>%
  dplyr::slice_min(distance, n = 1)

areas_climate <- cbind(code = paste(
  "p", climates$prec_yr, "-e", climates$epot_yr, sep = ""
), climates)

# column names of RABIMO results
result_columns <- c("surface_runoff", "infiltration", "evaporation")

# path to plot folder
path_plot <-"~/Projekte/AMAREX/calibration-stormwater/plots"

# calibration roof ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROOF is not a storm water management measure, but we "calibrate" it to validate
# the method and to identify the right "Retention High" (Speicherhöhe) in the
# WABILA input.

# Bagrov Values for test --> can change depends on the test
bagrov_values_roof <- seq(0.00, 0.1, by = 0.001)[-1]

# Generate areas with only roof surface for each climate combination
areas_roof <- kwb.utils::callWith(kwb.rabimo::generate_rabimo_area,
                                  areas_climate,
                                  roof = 1,
                                  green_roof = 0,
                                  pvd = 0)

config_roof_calib <- config

# RABIMO water balance for the roof areas and different bagrov values
rabimo_roof <- lapply(bagrov_values_roof, FUN = function(bagrov){
  config_roof_calib$bagrov_values["roof"] <- bagrov
  cat("\nCalculating water balance for bagrov = ", bagrov, "\n\n")
  result <- kwb.rabimo::run_rabimo(areas_roof, config = config_roof_calib)
  cbind(bagrov = bagrov, result)
})

# data frame with water balance and climate parameters.
# RABIMO results are expressed as fraction to compare with wabila results
rabimo_roof_df <- dplyr::bind_rows(rabimo_roof) %>%
  dplyr::left_join(areas_climate[c("code", "prec_yr", "epot_yr")], by = "code") %>%
  dplyr::relocate(prec_yr, epot_yr, .after = "area") %>%
  dplyr::mutate(surface_runoff = surface_runoff / prec_yr,
                infiltration = infiltration / prec_yr,
                evaporation = evaporation / prec_yr)

# calculate WABILA water balance for the roof areas
wabila_roof <- kwb.rabimo:::calculate_wabila_roof(areas_roof, retention_height = 0.48)

# compare results
compare_roof <- dplyr::left_join(rabimo_roof_df, wabila_roof,
                                 by = "code", suffix = c("_r","_w"))

# calculate delta-mod
delta_mod_roof <- kwb.rabimo:::calculate_delta_mod(
  compare_roof %>% dplyr::select(code, ends_with("_r")),
  compare_roof %>% dplyr::select(code, ends_with("_w"))
)

if(identical(compare_roof$code, delta_mod_roof$code)){
  compare_roof <- cbind(compare_roof, delta_mod = delta_mod_roof$delta_mod)
}


# calculate ideal bagrov value to minimize delta_mod
(bagrov_roof <- compare_roof %>%
    dplyr::group_by(code) %>%
    dplyr::filter(delta_mod == min(delta_mod)) %>%
    dplyr::summarise(bagrov = dplyr::first(bagrov),
                     delta_mod = dplyr::first(delta_mod)
    ) %>%
    dplyr::summarise(
      median = median(bagrov),
      mean = mean(bagrov),
      standard_deviation = sd(bagrov)
    ) %>%
    as.data.frame())


# plot delta-mod over bagrov
plot_roof <- ggplot(data = compare_roof,
                    aes(x = bagrov, y = delta_mod, group = code)) +
  geom_line(aes(color = factor(prec_yr), alpha = epot_yr)) +
  theme_minimal() +
  labs(title = "'delta-mod' vs Bagrov for diffferent Climate Combinations",
       x = "Bagrov green roof",
       y = "Delta mod",
       color = "Precipitation",
       alpha = "Potential Evaporation") +
  scale_color_discrete() +
  geom_line(data = compare_roof %>%
              dplyr::filter(prec_yr == berlin_climate$prec_yr,
                            epot_yr == berlin_climate$epot_yr),
            linewidth = 1.0,  # Thicker line for visibility
            linetype = "dashed") +
  geom_segment(aes(x = bagrov_roof$median,
                   xend = bagrov_roof$median,
                   y = -Inf,
                   yend = Inf),
               linetype = "dashed")

# save plot
ggsave(paste(path_plot, "delta-mod-roof.png", sep = "/"),
       plot = plot_roof, device = "png", width = 10, height = 7)

# To obtain the standard bagrov value for roof to minimise delta-mod, the
# wabila parameter "retention height" must be set to approx. 0.48 mm
# (without selecting particularly relevant climate combinations)

# calibration green roofs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bagrov Values for test
bagrov_values_green_roof <- seq(0.01, 2, by = 0.01)

# Generate areas with only green roof surface for each climate combination
areas_green_roof <- kwb.utils::callWith(kwb.rabimo::generate_rabimo_area,
                                        areas_climate,
                                        roof = 1,
                                        green_roof = 1,
                                        pvd = 0)

config_green_roof_calib <- config

# RABIMO water balance for the green roof areas and different bagrov values
rabimo_green_roof <- lapply(bagrov_values_green_roof, FUN = function(bagrov){
  config_green_roof_calib$bagrov_values["green_roof"] <- bagrov
  cat("\nCalculating water balance for bagrov = ", bagrov, "\n\n")
  result <- kwb.rabimo::run_rabimo(areas_green_roof, config = config_green_roof_calib)
  cbind(bagrov = bagrov, result)
})

# data frame with water balance and climate parameters.
# RABIMO results are expressed as fraction to compare with wabila results
rabimo_green_roof_df <- dplyr::bind_rows(rabimo_green_roof) %>%
  dplyr::left_join(areas_climate[c("code", "prec_yr", "epot_yr")], by = "code") %>%
  dplyr::relocate(prec_yr, epot_yr, .after = "area") %>%
  dplyr::mutate(surface_runoff = surface_runoff / prec_yr,
                infiltration = infiltration / prec_yr,
                evaporation = evaporation / prec_yr)

# calculate WABILA water balance for the green roof areas
wabila_green_roof <- kwb.rabimo:::calculate_wabila_green_roof(area = areas_green_roof,
                                                              height = 100,
                                                              kf = 70,
                                                              w_diff = 0.5)

# compare results
compare_green_roof <- dplyr::left_join(rabimo_green_roof_df, wabila_green_roof,
                                       by = "code", suffix = c("_r","_w"))

# calculate delta-mod
delta_mod_green_roof <- kwb.rabimo:::calculate_delta_mod(
  compare_green_roof %>% dplyr::select(code, ends_with("_r")),
  compare_green_roof %>% dplyr::select(code, ends_with("_w"))
)

if(identical(compare_green_roof$code, delta_mod_green_roof$code)){
  compare_green_roof <- cbind(compare_green_roof,
                              delta_mod = delta_mod_green_roof$delta_mod)
}


# calculate ideal bagrov value to minimize delta_mod
(bagrov_green_roof <- compare_green_roof %>%
    dplyr::group_by(code) %>%
    dplyr::filter(delta_mod == min(delta_mod)) %>%
    dplyr::summarise(bagrov = dplyr::first(bagrov),
                     delta_mod = dplyr::first(delta_mod)
    ) %>%
    dplyr::summarise(
      median = median(bagrov),
      mean = mean(bagrov),
      standard_deviation = sd(bagrov)
    ) %>%
    as.data.frame())


# plot delta-mod over bagrov
plot_green_roof <- ggplot(data = compare_green_roof,
                          aes(x = bagrov, y = delta_mod, group = code)) +
  geom_line(aes(color = factor(prec_yr), alpha = epot_yr)) +
  theme_minimal() +
  labs(title = "'delta-mod' vs Bagrov for diffferent Climate Combinations",
       x = "Bagrov green roof",
       y = "Delta mod",
       color = "Precipitation",
       alpha = "Potential Evaporation") +
  scale_color_discrete() +
  geom_line(data = compare_green_roof %>%
              dplyr::filter(prec_yr == berlin_climate$prec_yr,
                            epot_yr == berlin_climate$epot_yr),
            linewidth = 1.0,  # Thicker line for visibility
            linetype = "dashed") +
  geom_segment(aes(x = bagrov_green_roof$median,
                   xend = bagrov_green_roof$median,
                   y = -Inf,
                   yend = Inf),
               linetype = "dashed")

# save plot
ggsave(paste(path_plot, "delta-mod_green-roof.png", sep = "/"),
       plot = plot_green_roof, device = "png", width = 10, height = 7)


# calibration infiltration swale -----------------------------------------------
# function to calculate a complex wabila area with swale
calculate_wabila_complex_area <- function(block_area, swale_factor = 0,
                                          runoff_factor_unpaved = 0.1,
                                          infiltration_factor_unpaved = 0.3,
                                          evaporation_factor_unpaved = 0.6,
                                          retention_height = 0.48,
                                          kf = 18) {

  climate_parameters <- list(
    prec_yr = block_area$prec_yr,
    prec_s = block_area$prec_s,
    epot_yr = block_area$epot_yr,
    epot_s = block_area$epot_s
  )

  # Calculate partial roof area
  roof_factor <- block_area$roof
  actual_roof_area <- block_area$total_area * block_area$roof

  partial_roof_area <- kwb.rabimo::generate_rabimo_area(
    code = "partial_roof",
    climate_parameters,
    total_area = actual_roof_area,
    area_main = actual_roof_area,
    roof = 1,
    pvd = 0
  )


  partial_wabila_roof <- kwb.rabimo:::calculate_wabila_roof(
    partial_roof_area,
    retention_height = retention_height
  )

  precipitation_roof <- climate_parameters$prec_yr * roof_factor

  partial_roof_runoff <- partial_wabila_roof$surface_runoff * precipitation_roof
  partial_roof_infiltration <- partial_wabila_roof$infiltration * precipitation_roof
  partial_roof_evaporation <- partial_wabila_roof$evaporation * precipitation_roof

  # Calculate partial unpaved area WITHOUT swale
  actual_unpaved_area <- block_area$total_area *
    ((1 - (block_area$roof - block_area$pvd)) - swale_factor)
  unpaved_factor <- actual_unpaved_area / block_area$total_area

  partial_wabila_unpaved <- as.data.frame(t(c(surface_runoff = runoff_factor_unpaved,
                                              infiltration = infiltration_factor_unpaved,
                                              evaporation = evaporation_factor_unpaved)))

  precipitation_unpaved <- climate_parameters$prec_yr * unpaved_factor

  partial_unpaved_runoff <- precipitation_unpaved * partial_wabila_unpaved$surface_runoff
  partial_unpaved_infiltration <- precipitation_unpaved * partial_wabila_unpaved$infiltration
  partial_unpaved_evaporation <- precipitation_unpaved * partial_wabila_unpaved$evaporation

  # Calculate partial swale area
  actual_swale_area <- block_area$total_area * swale_factor

  precipitation_swale <- climate_parameters$prec_yr * swale_factor
  swale_input <- precipitation_swale + partial_roof_runoff

  partial_swale_area <- kwb.rabimo::generate_rabimo_area(
    code = "partial_swale",
    climate_parameters,
    total_area = actual_swale_area,
    area_main = actual_swale_area,
    roof = 0,
    pvd = 0
  )

  partial_wabila_swale <- kwb.rabimo:::calculate_wabila_swale(
    input = swale_input,
    area = partial_swale_area,
    kf = kf
  )
  names(partial_wabila_swale) <- c("surface_runoff", "infiltration", "evaporation")

  partial_swale_runoff <- partial_wabila_swale["surface_runoff"] * swale_input
  partial_swale_infiltration <- partial_wabila_swale["infiltration"] * swale_input
  partial_swale_evaporation <- partial_wabila_swale["evaporation"] * swale_input

  # Calculate total results
  total_runoff <- partial_swale_runoff + partial_unpaved_runoff
  total_runoff_factor <- unname(total_runoff / climate_parameters$prec_yr)

  total_infiltration <-
    partial_roof_infiltration +
    partial_unpaved_infiltration +
    partial_swale_infiltration

  total_infiltration_factor <- unname(total_infiltration / climate_parameters$prec_yr)

  total_evaporation <-
    partial_roof_evaporation +
    partial_unpaved_evaporation +
    partial_swale_evaporation

  total_evaporation_factor <- unname(total_evaporation / climate_parameters$prec_yr)

  # Return results as a named vector
  total_wabila_results_complex_area <- c(
    surface_runoff = total_runoff_factor,
    infiltration = total_infiltration_factor,
    evaporation = total_evaporation_factor
  )

  total_wabila_results_complex_area
}

# Generate complex block area with swale
prec_yr <- 800L
epot_yr <- 600L

climate_parameters <- list(
  prec_yr = prec_yr,
  prec_s = as.integer(round(prec_yr * factor_prec)),
  epot_yr = epot_yr,
  epot_s = as.integer(round(epot_yr * factor_epot))
)


complex_area <- kwb.rabimo::generate_rabimo_area(
  code = "complex_area",
  climate_parameters,
  total_area = 1000,
  area_main = 1000,
  roof = 0.7,
  pvd = 0,
  to_swale = 1
)

# calculate wabila results for complex area

# wabila parameter
kf_swale <- 18
swale_area <- kwb.rabimo:::estimate_swale_area(kf = kf_swale)*complex_area$total_area/100
swale_factor <- swale_area / complex_area$total_area
retention_height_roof <- 0.48 #mm

wabila_result_complex_area <- calculate_wabila_complex_area(block_area = complex_area,
                                        swale_factor = swale_factor,
                                        retention_height = retention_height_roof,
                                        kf = kf_swale)


# calculate r-abimo results for complex area
test_config <- config
test_config$swale[["swale_evaporation_factor" ]] <- 0

rabimo_result_complex_area <- kwb.rabimo::run_rabimo(data = complex_area, config = test_config) %>%
  dplyr::mutate(surface_runoff = surface_runoff / prec_yr,
                infiltration = infiltration / prec_yr,
                evaporation = evaporation / prec_yr)

wabila_result_complex_area
as.vector(rabimo_result_complex_area[-c(1,2)])


