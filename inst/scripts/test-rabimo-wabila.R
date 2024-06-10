# # Install package "remotes" from CRAN
# if (! require("remotes")) {
#   install.packages("remotes", repos = "https://cloud.r-project.org")
# }

# Install KWB package 'kwb.rabimo' from GitHub
#Sys.unsetenv("GITHUB_PAT")
# remotes::install_github("KWB-R/kwb.utils@dev")
# remotes::install_github("KWB-R/kwb.rabimo@add-functions-for-calibration", force = TRUE, upgrade = FALSE)
# remotes::install_github("pmoevius/kwb.rabimo@add-functions-for-calibration", force = TRUE, upgrade = FALSE)

# load pakets

library(magrittr)
library(ggplot2)

# Preparations for running R-ABIMO & WaBiLa
inputs <- kwb.rabimo::rabimo_inputs_2020
config <- inputs$config
data <- inputs$data

# mean(data$prec_s/data$prec_yr) # ~0.5
# mean(data$epot_s/data$epot_yr) # ~0.8

# Precepation and "Climate", plus every possibility
epot_yr <- as.integer(c(400, 450, 500, 550, 600, 650, 700, 750, 800))
prec_yr <- epot_yr + 25L

climates <- kwb.utils::expandGrid(epot_yr = epot_yr, prec_yr = prec_yr)
climates$epot_s <- as.integer(round(climates$epot_yr * 0.8))
climates$prec_s <- as.integer(round(climates$prec_yr * 0.5))
areas_climate <- cbind(code = sprintf("%03d", seq_len(nrow(climates))), climates)

# generate SimAreas

areas <- kwb.utils::callWith(kwb.rabimo::generate_rabimo_area, areas_climate, roof = 1, green_roof = 1, pvd = 0)

# Bagrov_Value Data Frame

bagrov_values <- seq(0.2, 5, by = 0.15)

# Calculate R-ABIMO and correct Format for comparrision with WaBiLa

results_rabimo <- lapply(bagrov_values, FUN = function(bagrov){
  config$bagrov_values["green_roof"] <- bagrov
  result <- kwb.rabimo::run_rabimo(areas, config = config)
  cbind(bagrov = bagrov, result)
})

results_rabimo_df <- dplyr::bind_rows(results_rabimo)

result_columns <- c("surface_runoff", "infiltration", "evaporation")

results_rabimo_df_shares <- results_rabimo_df[, result_columns] %>%
  as.matrix() %>%
  apply(MARGIN = 1L, FUN = kwb.utils::percentageOfSum, simplify = FALSE) %>%
  do.call(what = rbind) %>%
  `/`(100) %>%
  as.data.frame()

# Calculate WaBiLa
# Prepare WaBiLa inputs and connect them to climate

wabila_inputs_df <- dplyr::left_join(results_rabimo_df, areas_climate, by = "code")
wabila_inputs <- split(wabila_inputs_df, seq_len(nrow(wabila_inputs_df)))

# actual calculation

results_wabila <- lapply(wabila_inputs, FUN = kwb.rabimo:::calculate_wabila_green_roof, height = 100, kf = 70, w_diff = 0.5)
results_wabila_df <- as.data.frame(do.call(rbind, results_wabila))
names(results_wabila_df) <- result_columns

# Calculate Delta-mod

delta_mod <- kwb.rabimo:::calculate_delta_mod(results_rabimo_df_shares, results_wabila_df, has_codes = FALSE)

deviation_table <- cbind(
  kwb.utils::addSuffixToColumns(results_rabimo_df_shares, ".rabimo") ,
  kwb.utils::addSuffixToColumns(results_wabila_df, ".wabila") ,
  delta_mod ,
  wabila_inputs_df[c("bagrov", "epot_yr", "prec_yr", "code")]
)

deviation_table["epot_by_prec"] <- deviation_table["epot_yr"]/deviation_table["prec_yr"]

# Hauke Analysis method
min_bagrov_values <- deviation_table %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(bagrov = bagrov[which.min(delta_mod)])

sort(table(min_bagrov_values$bagrov))

# Analysis with ggplot2

ggplot(deviation_table, mapping = aes(
  x = bagrov,
  y = delta_mod,
  #color = code
  group = bagrov
)) +
  #geom_line()+
  geom_boxplot() +
  scale_x_continuous(n.breaks = 50)
  #geom_point()
  #facet_grid(vars(epot_yr), vars(prec_yr))
  #theme(legend.position = "none")

#kwb.utils::hsOpenWindowsExplorer(system.file("scripts", package = "kwb.rabimo"))
