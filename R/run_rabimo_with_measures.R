#' Distribute Rainwater Management Measures and run R-Abimo
#'
#' @param blocks data frame of selected blocks (same columns as in
#'   \code{\link{rabimo_inputs_2020}$data})
#' @param measures list with elements \code{green_roof}, \code{unpaved},
#'   \code{to_swale} representing the target percentages of the total areas
#'   corresponding to each measure
#' @param config configuration object, default:
#'   \code{\link{rabimo_inputs_2020}$config}
#' @param old_version if \code{TRUE} the old, erroneous version of this function 
#'   is used (not correctly considering the updated pvd value before calculating 
#'   the new to_swale values). The default is \code{FALSE}.
#' @param \dots further arguments passed to \code{\link{run_rabimo}}, such as
#'   \code{silent = TRUE}
#' @export
run_rabimo_with_measures <- function(
    blocks,
    measures,
    config = kwb.rabimo::rabimo_inputs_2020$config,
    old_version = FALSE,
    ...
)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  
  new_blocks <- if (old_version) {
    distribute_measures(blocks, rescale_target_values(measures, blocks))
  } else {
    apply_measures_to_blocks(blocks, measures)
  }
  
  run_rabimo(new_blocks, config = config, ...)
}
