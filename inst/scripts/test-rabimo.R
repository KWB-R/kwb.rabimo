#
# Test kwb.rabimo
#
# - Source the whole script first to load variables and functions defined below
# - Manually go through the MAIN sections within "if (FALSE) {...}"
#

# MAIN: Convert Berlin data (2019) to new structure ----------------------------
if (FALSE)
{
  old_inputs <- list(
    data = kwb.abimo::abimo_input_2019,
    config = kwb.abimo::read_config()
  )

  new_inputs <- kwb.rabimo::prepare_berlin_inputs(
    data = old_inputs$data,
    config = old_inputs$config
  )

  # Run R-Abimo with the new data structures
  result <- kwb.rabimo::run_rabimo(
    data = new_inputs$data,
    config = new_inputs$config,
    controls = kwb.rabimo::define_controls(output_format = "abimo")
  )

  old_abimo_results <- kwb.abimo::run_abimo(
    input_data = old_inputs$data,
    config = old_inputs$config
  )

  plot_differences(
    abimo_result = old_abimo_results,
    rabimo_result = result
  )
}

# MAIN: Convert raw 2020 data to R-Abimo format --------------------------------
if (FALSE)
{
  # Read data from cache if there is no access to KWB server
  berlin_2020_data <- kwb.utils:::get_cached("berlin_2020_data")

  # Read dbf file
  if (is.null(berlin_2020_data)) {
    berlin_2020_data <- get_path("berlin_2020_combined") %>%
      foreign::read.dbf(as.is = TRUE) %>%
      kwb.utils:::cache_and_return(name = "berlin_2020_data")
  }


  # Set all NAs to zero (test)
  #berlin_2020_data <- kwb.utils::defaultIfNA(berlin_2020_data, 0)

  rabimo_inputs_2020 <- kwb.rabimo::prepare_berlin_inputs(
    data = berlin_2020_data,
    config = kwb.abimo::read_config()
  )

  # Fehler: Column 'gw_dist' must not contain missing values (NA, found 4
  # times). Please give a value (may be 0) in each row.
  # MANUAL CORRECTION
  data <- rabimo_inputs_2020$data
  data <- data[kwb.utils::matchesCriteria(data, "!is.na(gw_dist)"), ]

  rabimo_inputs_2020$data <- data

  cached <- kwb.utils:::cache_and_return(
    x = rabimo_inputs_2020,
    name = "rabimo_inputs_2020"
  )

  # Calculate R-ABIMO results
  results <- kwb.rabimo::run_rabimo(
    data = data,
    config = rabimo_inputs_2020$config
  ) %>%
    kwb.utils:::cache_and_return(name = "rabimo_results_2020")

  # save inputs list in package
  if (FALSE) {
    usethis::use_data(rabimo_inputs_2020, overwrite = TRUE)
  }

  # Convert column names (max 10 chars) in input data
  if (FALSE) {
    inputs <- kwb.rabimo::rabimo_inputs_2020
    inputs$data <- kwb.utils::renameColumns(inputs$data, list(
      area_rd = "area_road",
      main_fraction = "main_frac",
      road_fraction = "road_frac",
      pvd_rd = "pvd_r",
      swg_pvd_rd = "swg_pvd_r",
      srf1_pvd_rd = "srf1_pvd_r",
      srf2_pvd_rd = "srf2_pvd_r",
      srf3_pvd_rd = "srf3_pvd_r",
      srf4_pvd_rd = "srf4_pvd_r"
    ))

    names_1 <- names(inputs$data)
    names_1 <- setdiff(names_1, "block_type")

    names_2 <- kwb.rabimo::read_column_info()$rabimo_berlin
    names_2 <- setdiff(names_2, c("berlin_usage", "berlin_type"))

    stopifnot(all(names_1 %in% names_2))

    rabimo_inputs_2020 <- inputs
    usethis::use_data(rabimo_inputs_2020, overwrite = TRUE)
  }

  # export 2020 results
  if (FALSE){
    foreign::write.dbf(
      results,
      file = file.path(get_path("results"), "berlin_2020_v2.dbf")
    )
  }

  #kwb.utils::hsOpenWindowsExplorer(get_path("results"))
}

# MAIN: Convert Berlin data (clean 2020) to new structure ----------------------
if (FALSE)
{
  # Read dbf file. Do not convert character to factor (as.is = TRUE)
  old_inputs <- list(
    data = foreign::read.dbf(get_path("berlin_2020_clean"), as.is = TRUE),
    config = kwb.abimo::read_config()
  )

  # Handle errors that would occur below when running run_rabimo()

  # Fehler: Column 'district' must not contain missing values (NA, found 1181
  # times).
  data <- old_inputs$data
  data$BEZIRK <- kwb.utils::defaultIfNA(data$BEZIRK, -99L)

  # Fehler: Column 'gw_dist' must not contain missing values (NA, found 4
  # times). Please give a value (may be 0) in each row.
  data <- data[kwb.utils::matchesCriteria(data, "!is.na(FLUR)"), ]

  # Fehler: The sum of columns 'srf1_pvd', 'srf2_pvd', 'srf3_pvd', 'srf4_pvd' is
  # not 1 or 0 in each row as expected (see above). The tolerance was: 0.005000
  columns <- c("BELAG1", "BELAG2", "BELAG3", "BELAG4")
  data[, columns] <- kwb.rabimo:::rescale_to_row_sum(
    as.matrix(kwb.utils::selectColumns(data, columns)),
    row_sum = 100
  )

  new_inputs <- kwb.rabimo::prepare_berlin_inputs(
    data = data,
    config = old_inputs$config
  )

  # For comparison: Run Abimo with the old data structures
  data_old <- data
  data_old[, columns] <- lapply(data_old[, columns], as.integer)

  result_old <- kwb.abimo::run_abimo(
    input_data = data_old,
    config = old_inputs$config
  )

  # Run R-Abimo with the new data structures
  result <- kwb.rabimo::run_rabimo(
    data = new_inputs$data,
    config = new_inputs$config,
    controls = kwb.rabimo::define_controls(
      output_format = "abimo"
    )
  )

  # Compare new results with old results
  compare_results(data = result, old = result_old)

  # Plot the differences between Abimo and R-Abimo, per variable
  plot_differences(abimo_result = result_old, rabimo_result = result)
}

# MAIN: Provide function arguments for run_rabimo(), prepare_input_data() ------
if (FALSE)
{
  # Read dbf file. Do not convert character to factor (as.is = TRUE)
  berlin_2020_data <- get_path("berlin_2020_clean") %>%
    foreign::read.dbf(as.is = TRUE) %>%
    # Clean column "STR_FLGES"
    kwb.rabimo:::set_columns_to_zero_where_almost_zero(columns = "STR_FLGES")

  # Check for NA
  table_with_na(berlin_2020_data$STR_FLGES)

  # Provide Abimo default configuration
  config <- kwb.rabimo::abimo_config_to_config(kwb.abimo::read_config())

  # Convert input data to the new format
  input_backup <- kwb.rabimo::prepare_input_data(berlin_2020_data, config)

  # Read NDVI data
  ndvi_matrix <- foreign::read.dbf(get_path("ndvi"))

  # Merge the NDVI value to the input data (by the codes of the blocks)
  input_ndvi <- dplyr::left_join(
    input_backup,
    kwb.utils::selectColumns(ndvi_matrix, c("code", "ndvi_value")),
    by = "code"
  )

  # Do we have an NDVI value for each block?
  table_with_na(input_ndvi$ndvi_value)

  #  30    35    40    45    50  <NA>
  #   1  1583 24192 29384  3371     0

  # Save the original vegetation class (as assigned by hard-coded rules in
  # original ABIMO source code)
  old_veg_class <- input_ndvi$veg_class

  # Use the NDVI value as vegetation class
  input_ndvi[["veg_class"]] <- input_ndvi[["ndvi_value"]]

  # Remove the NDVI column
  input_ndvi[["ndvi_value"]] <- NULL

  # Remove rows where groundwater distance is NA
  input <- input_ndvi[has_gw_dist(input_ndvi), ]

  # use for old vegetation classes
  input <- input_backup[has_gw_dist(input_backup), ]
  input <- head(input)
}

# MAIN: Test comparison with abimo results -------------------------------------
if (FALSE)
{
  # Provide Abimo's default configuration (for kwb.abimo)
  abimo_config <- kwb.abimo::read_config()

  # Prepare a configuration for R-Abimo, based on the configuration for Abimo
  rabimo_config <- kwb.rabimo::abimo_config_to_config(abimo_config)

  # test with 2020 data
  # //////////////////
  input_abimo <- head(berlin_2020_data) %>%
    # Set STR_BELAG[1-4] to 0
    kwb.rabimo:::set_columns_to_zero_where_na(columns = c(
      "STR_BELAG1",
      "STR_BELAG2",
      "STR_BELAG3",
      "STR_BELAG4"
    ))

  # Call Abimo and R-Abimo
  results_2020 <- call_abimo_and_rabimo(
    abimo_data = input_abimo,
    abimo_config = abimo_config,
    rabimo_config = rabimo_config
  )

  # Have a look at the first lines of the result data frames
  lapply(results_2020, head)

  # test with 2019 data, as stored in R-wrapper package kwb.abimo
  # //////////////////

  # Call Abimo and R-Abimo
  results_2019 <- call_abimo_and_rabimo(
    abimo_data = head(kwb.abimo::abimo_input_2019),
    abimo_config = abimo_config,
    rabimo_config = rabimo_config
  )

  # Have a look at the first lines of the result data frames
  lapply(results_2019, head)

  # fictive areas for testing
  # /////////////////////////

  # Call Abimo and R-Abimo
  results_fictive <- call_abimo_and_rabimo(
    abimo_data = provide_fictive_data(),
    abimo_config = abimo_config,
    rabimo_config = rabimo_config
  )

  # Have a look at the first lines of the result data frames
  lapply(results_fictive, head)
}

# Load the "pipe" operator -----------------------------------------------------
`%>%` <- magrittr::`%>%`

# Define a path "dictionary" ---------------------------------------------------
get_path <- kwb.utils::createAccessor(kwb.utils::resolve(list(
  amarex_ap4 = "Y:/SUW_Department/Projects/AMAREX/Work-packages/AP_4",
  isu5_2020 = "<amarex_ap4>/ABIMO_Daten/ISU5_2020_datengrundlage",
  data_2020 = "<isu5_2020>/isu5_2020_berlin/cleaned",
  berlin_2020_clean = "<data_2020>/isu5_2020_abimo_cleaned.dbf",
  berlin_2020_raw = "<isu5_2020>/finaler_eingang_von_sensbw/isu5_2020_combined",
  berlin_2020_combined = "<berlin_2020_raw>/isu5_2020_abimo_hyras9120_amarex.dbf",
  ndvi = "Y:/Z-Exchange/Philipp/Amarex/NDVI R/combined_data_NDVI.dbf",
  results = "<amarex_ap4>/ABIMO_Daten/results/"
)))

# Define function: table_with_na() ---------------------------------------------
table_with_na <- function(x)
{
  table(x, useNA = "always")
}

# Define function: has_gw_dist() -----------------------------------------------
has_gw_dist <- function(data)
{
  kwb.utils::matchesCriteria(data, "!is.na(gw_dist)")
}

# Define function: call_abimo_and_rabimo ---------------------------------------
call_abimo_and_rabimo <- function(abimo_data, abimo_config, rabimo_config)
{
  # Prepare input data in "new" format, as expected by kwb.rabimo
  rabimo_data <- kwb.rabimo::prepare_input_data(abimo_data, rabimo_config)

  # Use the R-wrapper package kwb.abimo to run Abimo.exe (written in C++)
  abimo_result <- kwb.abimo::run_abimo(
    input_data = abimo_data,
    config = abimo_config
  )

  # Run R-Abimo, the R-implementation of Abimo in this package
  rabimo_result <- kwb.rabimo::run_rabimo(
    data = rabimo_data,
    config = rabimo_config
  )

  # Return both result data frames
  list(
    abimo = abimo_result,
    rabimo = rabimo_result
  )
}

# Define function: provide_fictive_data() --------------------------------------
provide_fictive_data <- function()
{
  data <- head(kwb.abimo::abimo_input_2019)

  btf1 <- data[3,]
  btf1$CODE <- "mixed-surfaces"
  btf1$FLGES <- 100
  btf1$PROBAU <- 40
  btf1$PROVGU <- 20
  btf1$STR_FLGES <- 0
  btf1$VGSTRASSE <- 0
  btf1$VG <- btf1$PROBAU + btf1$PROVGU
  btf1[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf1[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf1$BELAG1 <- 100L
  btf1$KAN_VGU <- 60L
  btf1$KAN_STR <- 100L

  btf2 <- btf1
  btf2$CODE <- "no-green"
  btf2$FLGES <- 100
  btf2$PROBAU <- 40
  btf2$PROVGU <- 60
  btf2$STR_FLGES <- 0
  btf2$VGSTRASSE <- 0
  btf2$VG <- btf2$PROBAU + btf2$PROVGU
  btf2[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf2[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf2$BELAG1 <- 100L
  btf2$KAN_VGU <- 60L
  btf2$KAN_STR <- 100L

  btf3 <- btf2
  btf3$CODE <- "only-roof"
  btf3$FLGES <- 100
  btf3$PROBAU <- 100
  btf3$PROVGU <- 0
  btf3$STR_FLGES <- 0
  btf3$VGSTRASSE <- 0
  btf3$VG <- btf3$PROBAU + btf3$PROVGU
  btf3[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf3[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf3$BELAG1 <- 100L
  btf3$KAN_VGU <- 60L
  btf3$KAN_STR <- 100L

  btf4 <- btf2
  btf4$CODE <- "only-paved"
  btf4$FLGES <- 100
  btf4$PROBAU <- 0
  btf4$PROVGU <- 100
  btf4$STR_FLGES <- 0
  btf4$VGSTRASSE <- 0
  btf4$VG <- btf4$PROBAU + btf4$PROVGU
  btf4[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf4[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf4$BELAG1 <- 100L
  btf4$KAN_VGU <- 60L
  btf4$KAN_STR <- 100L

  btf5 <- btf2
  btf5$CODE <- "only-green"
  btf5$FLGES <- 100
  btf5$PROBAU <- 0
  btf5$PROVGU <- 0
  btf5$STR_FLGES <- 0
  btf5$VGSTRASSE <- 0
  btf5$VG <- btf5$PROBAU + btf5$PROVGU
  btf5[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf5[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf5$BELAG1 <- 100L
  btf5$KAN_VGU <- 60L
  btf5$KAN_STR <- 100L

  btf6 <- btf1
  btf6$CODE <- "mixed-w-roads"
  btf6$FLGES <- 100
  btf6$PROBAU <- 40
  btf6$PROVGU <- 20
  btf6$STR_FLGES <- 10
  btf6$VGSTRASSE <- 0
  btf6$VG <- btf6$PROBAU + btf6$PROVGU
  btf6[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf6[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf6$BELAG1 <- 100L
  btf6$KAN_VGU <- 60L
  btf6$KAN_STR <- 100L

  data <- rbind(btf1, btf2, btf3, btf4, btf5, btf6)
  data$TYP <- 10L
  data$NUTZUNG <- 10L

  data
}

# compare_results --------------------------------------------------------------
compare_results <- function(data, old, codes = NULL)
{
  #data = result; old = result_old; codes = NULL
  #str(data)
  #str(old)

  codes <- kwb.utils::defaultIfNULL(codes, intersect(data$CODE, old$CODE))

  given_codes_only <- function(data) {
    data[data$CODE %in% codes, ]
  }

  data <- given_codes_only(data)

  old <- given_codes_only(old)
  old$CODE <- as.character(old$CODE)

  stopifnot(identical(data$CODE, old$CODE))

  diffs <- lapply(stats::setNames(nm = names(data)[-1L]), function(column) {
    x <- data[[column]]
    y <- old[[column]]
    x/y - 1
  })

  lapply(diffs, range)
}

# plot_differences -------------------------------------------------------------
plot_differences <- function(abimo_result, rabimo_result)
{
  for (name in names(abimo_result)[-1L]) {
    x <- abimo_result[[name]]
    y <- rabimo_result[[name]]
    plot(x, y, xlab = "Abimo", ylab = "Rabimo", main = name, asp = 1)
  }
}
