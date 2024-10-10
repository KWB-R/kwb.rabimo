# dependencies
# install.packages("readxl")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("kwb.rabimo")
# remotes::install_github("kwb-r/kwb.dwd@get-rid-of-rgdal", build_vignettes = TRUE, force = TRUE)

# pipe operator
`%>%` <- magrittr::`%>%`

# read xls table

# INSERT HERE PATH TO TABLE
path <- "~/../Downloads/Entwaesserungsgebiete.xls"
table <- readxl::read_excel(path)

# rename select relevant columns
waterbody_df <- as.data.frame(kwb.utils::renameAndSelect(table, list(
  "Schlüssel" = "code",
  "Nummer des erstaufnehmenden Gewässers" = "wb_num",
  "Erstaufnehmendes Gewässer" = "wb_name"
)))

# test waterbody_df for duplicates
combis <- unique(waterbody_df[-1])
(dupl_num <- kwb.utils::findPartialDuplicates(combis, "wb_num"))
(dupl_name <- kwb.utils::findPartialDuplicates(combis, "wb_name"))

# double occurring waterbodies in berlin
is_fenngraben <- waterbody_df$wb_name == "Fenngraben"
is_froschteich <- waterbody_df$wb_name == "Froschteich"
is_waldsee <- waterbody_df$wb_name == "Waldsee"

table(waterbody_df$wb_num[is_fenngraben])
table(waterbody_df$wb_num[is_froschteich])
table(waterbody_df$wb_num[is_waldsee])

# corrrect missing number
waterbody_df[
  waterbody_df$wb_num == "-" & waterbody_df$wb_name != "-", "wb_num"
] <- "100b"

# rename double occurring waterbodies with number suffix
waterbody_df$wb_name[
  is_fenngraben | is_waldsee | is_froschteich
] <- kwb.utils::pasteColumns(
  waterbody_df[is_fenngraben | is_waldsee | is_froschteich, ],
  columns = c("wb_name", "wb_num"),
  sep = "_"
)

# filter out areas with no receiving waterbody
waterbody_df <- waterbody_df %>%
  dplyr::filter(wb_name != "-")

# split waterbody_df by waterbody
split_waterbody_df <- split(waterbody_df, waterbody_df$wb_name)

# download monthly precipitation and potential evaporation data from dwd
from <- "201401"
to <- "202312"

precipitation_monthly <- kwb.dwd::load_precipitation_berlin(from, to)
pot_evaporation_monthly <- kwb.dwd::load_potential_evaporation_berlin(from, to)

# sum up prec and epot by year for rabimo input
precipitation_year <- precipitation_monthly %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(year_prec = sum(mean))

precipitation_summer <- precipitation_monthly %>%
  dplyr::filter(month > 4 & month < 11) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(summer_prec = sum(mean))

pot_evaporation_year <- pot_evaporation_monthly %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(year_epot = sum(mean))

pot_evaporation_summer <- pot_evaporation_monthly %>%
  dplyr::filter(month > 4 & month < 11) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(summer_epot = sum(mean))

climate_data_yearly <- precipitation_year %>%
  dplyr::left_join(precipitation_summer, by = "year") %>%
  dplyr::left_join(pot_evaporation_year, by = "year") %>%
  dplyr::left_join(pot_evaporation_summer, by = "year") %>%
  as.data.frame()

# load rabimo input data and config
rabimo_inputs <- kwb.rabimo::rabimo_inputs_2020

# correct climate data for currently calculated year
year <- 2014

values <- climate_data_yearly %>%
  dplyr::filter(year == !!year) %>%
  lapply(function(x) as.integer(round(x)))

rabimo_inputs_year <- rabimo_inputs$data %>%
  dplyr::mutate(
    prec_yr = values$year_prec,
    prec_s = values$summer_prec,
    epot_yr = values$year_epot,
    epot_s = values$summer_epot
  )

config <- rabimo_inputs$config

# calculate rabimo results (water balance) for whole Berlin
rabimo_results <- kwb.rabimo::run_rabimo(rabimo_inputs_year, config)

# calculate runoff volume for each block area l/m²/a/ -> m³/a
runoff_vols <- rabimo_results %>%
  dplyr::select(code, area, surface_runoff) %>%
  dplyr::mutate(runoff_vol = surface_runoff * 0.001 * area)

# match areas connected to receiving water body with their runoff value
waterbody_runoff_df <- waterbody_df %>%
  dplyr::left_join(runoff_vols, by = "code") %>%
  kwb.utils::selectColumns(c("code", "area", "runoff_vol", "wb_num", "wb_name"))

# sum up volumes by receiving water body
runoff_by_waterbody <- waterbody_runoff_df %>%
  dplyr::group_by(wb_num, wb_name) %>%
  dplyr::summarize(total_runoff_year = sum(runoff_vol, na.rm = TRUE),
                   .groups = "drop") %>%
  as.data.frame()


