#library(plumber); pr("R/plumber.R") %>% pr_run()

# /example_data ----------------------------------------------------------------

#* Example data for Abimo (Berlin, 2019)
#* @param n_records:int number of records (= input rows = "Blockteilflaechen").
#* @param seed seed:int value for the random number generator used to randomly select rows
#* @param output_only:logical whether to return only the data frame with example data (true) or a list with inputs and output (false).
#* @get /example_data
function(n_records = 3L, seed = as.integer(Sys.time()), output_only = TRUE)
{
  n_records <- as.integer(n_records)
  seed <- as.integer(seed)

  stopifnot(length(n_records) == 1L, !is.na(n_records))
  stopifnot(length(seed) == 1L, !is.na(seed))

  data <- kwb.utils::selectElements(kwb.rabimo::rabimo_inputs_2020, "data")

  n_rows <- nrow(data)
  set.seed(seed)

  size <- min(c(n_rows, n_records))
  rows <- sample(n_rows, size = size)

  output <- data[rows, ]

  if (output_only) {
    return(output)
  }

  list(
    inputs = list(n_records = n_records, seed = seed),
    output = output
  )
}

# /get_measure_stats -----------------------------------------------------------

#* Statistics (mean, max) on measures within selected blocks
#* @param blocks:data.frame Selected blocks
#* @param max_only:logical whether or not to return only the "max" (and not also the "mean") values
#* @param reference_system:int "Reference system" (1:old, 2:new = percentages of total area)
#* @param safety_factor:float Factor that the "max" values are multiplied with to make them a bit smaller
#* @serializer unboxedJSON
#* @post get_measure_stats
function(blocks = data.frame(), max_only = TRUE, reference_system = 2L, safety_factor = 0.999)
{
  stats <- kwb.rabimo::get_measure_stats(blocks, reference_system)

  if (max_only) {
    lapply(lapply(stats, `[[`, "max"), `*`, as.numeric(safety_factor))
  } else {
    stats
  }
}

# /run_rabimo ------------------------------------------------------------------

#' Run R-Abimo with data and config (optional)
#* @param data:data.frame input data as json string, as returned by /example_data in "output"
#* @param measures_json Optional. Target values of measures, as json string, e.g. '{"green_roof":0.1, "unpaved":0.2, "to_swale":0.3}'
#* @param config_json Optional. Configuration as json string, as returned by /default_config
#* @serializer unboxedJSON
#* @post /run_rabimo
function(data = data.frame(), measures_json = "", config_json = "")
{
  # Convert json string to data frame and set all columns to expected types
  data <- kwb.rabimo:::check_or_convert_data_types(
    data = data,
    types = kwb.rabimo:::get_expected_data_type(names(data)),
    convert = TRUE
  )

  measures <- if (measures_json != "") {
    jsonlite::fromJSON(measures_json)
  } # else NULL implicitly

  if (config_json == "") {
    config <- kwb.rabimo::rabimo_inputs_2020$config
  } else {
    # Convert json string to list
    config <- jsonlite::fromJSON(config_json)
    # Convert elements that are lists to named vectors
    elements <- names(which(sapply(config, is.list)))
    config[elements] <- lapply(config[elements], unlist)
  }

  output <- try(
    if (is.null(measures)) {
      kwb.rabimo::run_rabimo(data, config)
    } else {
      kwb.rabimo::run_rabimo_with_measures(data, measures, config)
    }
  )

  failed <- kwb.utils::isTryError(output)

  list(
    data = if (failed) NULL else output,
    measures = measures,
    weighted_means = if (failed) NULL else {
      areas <- output$area
      cols <- 3:5
      x <- colSums(areas * as.matrix(output[, cols])) / sum(areas)
      stats::setNames(as.list(x), names(output)[cols])
    },
    error = if (failed) as.character(output) else ""
  )
}

# /data_to_natural -------------------------------------------------------------
#* Transform R-Abimo input data into their natural scenario equivalent
#* @param data_json input data as json string, as returned by /example_data in "output"
#* @param type one of "undeveloped": all paved or constructed areas are set to 0%. No connection to the sewer; "forested": like undeveloped, but the land type is declared to be "forested"; "horticultural": like undeveloped, but the land type is declared to be "horticultural".
#* @post data_to_natural
function(data_json, type = "undeveloped")
{
  data <- kwb.rabimo:::check_or_convert_data_types(
    data = jsonlite::fromJSON(data_json),
    types = kwb.rabimo:::get_expected_data_type(),
    convert = TRUE
  )

  kwb.rabimo::data_to_natural(data = data, type = type)
}

# /calculate_delta_w

#* Calculate deviation from natural water balance (delta-W)
#* @param natural_json R-Abimo results for the "natural" scenario, as returned by /run_rabimo, as a json string
#* @param urban_json R-Abimo results for the "urban" scenario, as returned by /run_rabimo, as a json string
#* @post calculate_delta_w
function(natural_json, urban_json)
{
  natural <- jsonlite::fromJSON(natural_json)
  urban <- jsonlite::fromJSON(urban_json)

  kwb.rabimo::calculate_delta_w(natural = natural, urban = urban)
}

# /default_config --------------------------------------------------------------

#* Example configuration for Abimo
#* @post /default_config
function()
{
  config <- kwb.utils::selectElements(kwb.rabimo::rabimo_inputs_2020, "config")

  str(config)

  # Convert named vectors to lists so that names do not get lost
  elements <- names(which(
    sapply(config, function(x) !is.list(x) && !is.null(names(x)))
  ))

  config[elements] <- lapply(config[elements], as.list)

  config
}

# /triangle --------------------------------------------------------------------

#* Plot Hydrological Triangle
#* @get /triangle
#* @param components_json json string with elements "evaporation", "runoff", "infiltration", given inmm
#* @param components_2_json optional. Json string similar to components_json specifying a second point in the triangle plot. If given, the difference between the two water balances is also calculated and represented in the plot.
#* @param size_cm image size in cm
# @serializer png
#* @serializer contentType list(type="image/png")
function(
  components_json = '{"evaporation": 100, "runoff": 60, "infiltration": 40}',
  components_2_json = "",
  size_cm = 10
)
{
  size_cm <- as.numeric(size_cm)

  # Helper functions
  to_vector <- function(x) unlist(jsonlite::fromJSON(x))
  to_fractions <- function(x) x / sum(x)

  # Create triangle plot
  p <- kwb.rabimo::triangle_of_fractions(
    fractions = to_fractions(to_vector(components_json)),
    fractions_2 = if (components_2_json != "") {
      to_fractions(to_vector(components_2_json))
    }
  )

  file <- file.path(tempdir(), "triangle.png")
  ggplot2::ggsave(file, plot = p, width = size_cm, height = size_cm, units = "cm")
  readBin(file, "raw", n = file.info(file)$size)
}
