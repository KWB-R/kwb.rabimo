#library(plumber); pr("R/plumber.R") %>% pr_run()

# /run_rabimo ------------------------------------------------------------------

#' Run R-Abimo with data and config (optional)
#* @param data_json input data as json string, as returned by /example_data in "output"
#* @param config_json Optional. Configuration as json string, as returned by /default_config
#* @post /run_rabimo
function(req, data_json, config_json = "")
{
  data <- jsonlite::fromJSON(data_json)

  # Convert json string to data frame and set all columns to expected types
  data <- kwb.rabimo:::check_or_convert_data_types(
    data = data,
    types = kwb.rabimo:::get_expected_data_type(names(data)),
    convert = TRUE
  )

  if (config_json == "") {

    config <- kwb.rabimo::rabimo_inputs_2020$config

  } else {

    # Convert json string to list
    config <- jsonlite::fromJSON(config_json)

    # Convert elements that are lists to named vectors
    elements <- names(which(sapply(config, is.list)))
    config[elements] <- lapply(config[elements], unlist)
  }

  output <- try(kwb.rabimo::run_rabimo(data, config))

  return(output)

  list(
    inputs = list(
      data_json = data_json,
      config_json = config_json
    ),
    output = output
  )
}

# /example_data ----------------------------------------------------------------

#* Example data for Abimo (Berlin, 2019)
#* @param n_records number of records (= input rows = "Blockteilflaechen").
#* @param seed seed value for the random number generator used to randomly select rows
#* @param output_only whether to return only the data frame with example data (true, the default) or a list with inputs and output (false).
#* @post /example_data
function(req, n_records = 3L, seed = as.integer(Sys.time()), output_only = TRUE)
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

# /get_measure_stats -----------------------------------------------------------

#* Statistics (mean, max) on measures within selected blocks
#* @param blocks_json Selected blocks
#* @param reference_system "Reference system" (1:old, 2:new = percentages of total area)
#* @post get_measure_stats
function(req, blocks_json, reference_system = 2)
{
  blocks <- jsonlite::fromJSON(blocks_json)
  kwb.rabimo::get_measure_stats(blocks, reference_system)
}
