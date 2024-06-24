# Validierung Pilotgebiet Bln I über Infoworks
`%>%` <- magrittr::`%>%`
library(ggplot2)
library(reshape2)
#install.packages("extrafont")
library(extrafont)

get_path <- kwb.utils::createAccessor(kwb.utils::resolve(list(
  amarex_ap4 = "Y:/SUW_Department/Projects/AMAREX/Work-packages/AP_4",
  abimo_daten = "<amarex_ap4>/ABIMO_Daten",
  pilotgebiete = "<abimo_daten>/Pilotgebiete",
  iw = "<pilotgebiete>/Infoworks_Validierung_Pilotgebiete/ABIMO_validierung_15_21_infoworks_von_dominik/",
  berlin_1_2020 = "<abimo_daten>/ISU5_2020_datengrundlage/finaler_eingang_von_sensbw/2020_bln1/2020_bln1.dbf",
  extdata = paste0(getwd(),"/inst/extdata"),
  climate = "<extdata>/climate_bln1_2015-2021.RDS",
  validation = "C:/Users/fdpunt/Documents/Projekte/AMAREX/Validation/plots"
)))

# load infoworks runoff for the years 2015 to 2021
path_iw <- get_path("iw")

keyword <- 'totrun.csv'
csv_files <- list.files(path_iw, pattern = keyword, full.names = TRUE)

iw_runoff <- data.frame(year = numeric(), infoworks_runoff = numeric())
row <- 1
year <- 2015

for (file in  csv_files){
  print(file)
  data_iw <- read.csv(file, col.names = c("seconds", "outflow"))
  iw_runoff[row, "year"] <- year
  iw_runoff[row, "infoworks_runoff"] <- data_iw[nrow(data_iw),]$outflow / 10^6
  row <- row + 1
  year <- year + 1
}

iw_runoff

# input data for Berlin1
data_bln1_2020 <- foreign::read.dbf(get_path("berlin_1_2020"))
bln1_codes <- data_bln1_2020$code
bln1_area <- 3275271 #m²

inputs <- kwb.rabimo::rabimo_inputs_2020
data <- inputs$data %>%
  dplyr::filter(code %in% bln1_codes)
inputs$data <- data

# load climate data for berlin1 from 2015 to 2021
climate_bln1 <- readRDS(get_path("climate"))

# run rabimo with climate data

rabimo_runoff <- data.frame(year = numeric(), abimo_runoff = numeric())

for (year in climate_bln1$year){

  data <- inputs$data
  data$prec_yr <- climate_bln1[which(climate_bln1["year"] == year),
                               "precipitation"]

  results <- kwb.rabimo::run_rabimo(data = data,
                                    config = inputs$config)

  rabimo_runoff_year <- sum(kwb.rabimo::yearly_height_to_volume_flow(
    results["surface_runoff"],
    results["area"]))/10^6*86400*365/10^6 #m³/year

  year_result <- data.frame(year = year, rabimo_runoff = rabimo_runoff_year)

  rabimo_runoff <- rbind(rabimo_runoff, year_result)

}

result_table <- rabimo_runoff %>%
  dplyr::left_join(iw_runoff, by = "year")

# Melt the data for ggplot
melted_data <- reshape2::melt(result_table,
                              id.vars = 'year',
                              variable.name = 'runoff_type',
                              value.name = 'runoff')

# Create the plot
plot_runoff <- function(table, fontsize){

  ggplot(melted_data, aes(x = year, y = runoff, color = runoff_type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    #labs(x = 'Year', y = 'Runoff in million m³') +#, title = 'Comparison of Rabimo and Infoworks Runoff') +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    scale_color_manual(values = c("rabimo_runoff" = "dodgerblue4", "infoworks_runoff" = "firebrick"),
                       labels = c("Abimo", "Infoworks")) +
    scale_x_continuous(breaks = seq(2015, 2021, by = 1)) +
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.85),
          legend.key.height = unit(2, 'lines'),
          #legend.spacing.y = unit(4, "cm"),
          text = element_text(family = "Calibri", size = fontsize),
          axis.title = element_text(family = "Calibri", size = fontsize),
          axis.text = element_text(family = "Calibri", size = fontsize),
          legend.text = element_text(family = "Calibri", size = fontsize+5)) +
    ylim(0, 2)

}


p <- plot_runoff(melted_data, 28)
ggsave(filename = paste0(
  get_path("validation"),
  "/runoff_comparison_plot_nolables.png"),
  plot = p, width = 20.5, height = 14.2, units = "cm", dpi = 300)

