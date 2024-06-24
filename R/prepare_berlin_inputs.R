# prepare_berlin_inputs ----------------------------------------------------------

#' Function to be called when using ABIMO with the 'old' Berlin-data structure
#'
#' It calls the functions \code{\link{prepare_input_data}} and
#' \code{\link{abimo_config_to_config}} and modifies the config-object to the
#' required structure.
#'
#' @param data_file a path for the .dbf file containing the input data
#' @param config_file a path for the .xml config file
#' @param data optional. Data frame containing the input data, as e.g. provided
#'   in \code{\link[kwb.abimo]{abimo_input_2019}}.
#' @param config optional. List representing an Abimo configuration, as e.g.
#'   provided by \code{\link[kwb.abimo]{read_config}}.
#' @param dbg logical indicating whether or not to show debug messages
#' @return a list object containing elements \code{data},
#'   the prepared input data and \code{config}, the prepared config object,
#'   both in a form readable by \code{\link{run_rabimo}}
#' @export
prepare_berlin_inputs <- function(
    data_file,
    config_file = kwb.abimo::default_config(),
    data = NULL,
    config = NULL,
    dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  # Read config file if config is NULL
  if (is.null(config)) {
    config <- kwb.abimo::read_config(file = safe_path(config_file))
  }

  # Read database file if data is NULL
  if (is.null(data)) {
    data <- foreign::read.dbf(safe_path(data_file))
  }

  # Convert configuration object into a more convenient structure
  new_config <- abimo_config_to_config(config, add_class_5 = TRUE)

  # Return a list with input data and configuration as required by Rabimo, e.g.
  # - input data: with corrected precipitation and evaporation
  # - configuration: without correction factor or information on evaporation
  list(
    data = prepare_input_data(data, config = new_config, dbg = dbg),
    config = prepare_config(new_config)
  )
}

# prepare_config ---------------------------------------------------------------
prepare_config <- function(config, dbg = TRUE)
{
  # add evaporation effect of swales (percentage of water that does not infiltrate)
  config[["swale"]] <- c(swale_evaporation_factor = 0.1)

  # add bagrov value for green roofs
  current_bagrov <-  config[["bagrov_values"]]

  config[["bagrov_values"]] <- c(
    current_bagrov[1L],
    "green_roof" = 0.65,
    current_bagrov[-1L]
  )

  expected <- c("precipitation_correction_factor", "potential_evaporation")

  # Try to get the expected elements (for clear error message, if applicable)
  select_elements(config, expected)

  remove_elements(config, expected)
}
