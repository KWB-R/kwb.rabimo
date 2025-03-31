#' Distribute Rainwater Management Measures and run R-Abimo
#'
#' @param blocks data frame of selected blocks (same columns as in
#'   \code{\link{rabimo_inputs_2020}$data})
#' @param measures list with elements \code{green_roof}, \code{unpaved},
#'   \code{to_swale} representing the target percentages of the total areas
#'   corresponding to each measure
#' @param config configuration object, default:
#'   \code{\link{rabimo_inputs_2020}$config}
#' @export
run_rabimo_with_measures <- function(
    blocks,
    measures,
    config = kwb.rabimo::rabimo_inputs_2020$config
)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  rescaled_targets <- rescale_target_values(
    new_targets = measures,
    blocks = blocks
  )

  run_rabimo(
    distribute_measures(blocks = blocks, targets = rescaled_targets),
    config = config
  )
}

# config_file = kwb.abimo::default_config()
# config <- kwb.abimo:::read_config(file = safe_path(config_file))
# new_config <- abimo_config_to_config(config, add_class_5 = TRUE)
# config = prepare_config(new_config)
