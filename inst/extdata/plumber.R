
#library(plumber); pr("R/plumber.R") %>% pr_run()

#' Run R-Abimo with uploaded input.dbf and config.xml
#* @param input_dbf:binary
#* @param config_xml:binary
#* @post /run_rabimo
function(req, input_dbf, config_xml)
{
  #input_file <- "~/../Downloads/A/abimo/ABIMO_fuer_Hauke/abimo_2019_mitstrassen.dbf"
  #config_file <- "~/../Downloads/A/abimo/ABIMO_fuer_Hauke/config.xml"

  download <- function(x) {
    file <- tempfile(fileext = paste0(".", tools::file_ext(names(x)[1L])))
    writeBin(x[[1L]], file)
    file
  }

  input_file <- download(input_dbf)
  config_file <- download(config_xml)

  old_data <- foreign::read.dbf(input_file)
  old_config <- kwb.abimo::read_config(config_file)

  new_config <- kwb.rabimo::abimo_config_to_config(old_config)
  new_data <- kwb.rabimo::prepare_input_data(old_data, new_config)

  kwb.rabimo::run_rabimo(new_data, new_config)
}

#* Example data for Abimo (Berlin, 2019)
#* @param n_records number of records (= input rows = "Blockteilflaechen").
#* @param seed seed value for the random number generator used to randomly select rows
#* @post /example_data
function(n_records = 3L, seed = 12345L)
{
  data_old <- kwb.abimo::abimo_input_2019
  config_old <- kwb.abimo::read_config()

  n_max <- nrow(data_old)

  set.seed(seed)
  rows <- sample(n_max, size = min(c(n_max, n_records)))

  kwb.rabimo::prepare_input_data(
    data = data_old[rows, ],
    config = kwb.rabimo::abimo_config_to_config(config_old)
  )
}

#* Example configuration for Abimo
#* @post /example_config
function()
{
  config <- kwb.rabimo::abimo_config_to_config(kwb.abimo::read_config())

  # Convert named vectors to lists so that names do not get lost
  elements <- c("runoff_factors", "bagrov_values", "result_digits")
  config[elements] <- lapply(config[elements], as.list)

  config
}
