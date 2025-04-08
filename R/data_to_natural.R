# Functions about the natural scenarios and the calculation of Delta-W

# data_to_natural --------------------------------------------------------------

#' Transform R-Abimo input Data into their natural scenario equivalent
#'
#' Three scenarios are possible:
#' 1) undeveloped: all paved or constructed areas are set to 0%. No connection
#'   to the sewer.
#' 2) forested: like undeveloped, but the land type is declared to be
#'  "forested".
#' 3) horticultural: like undeveloped, but the land type is declared to be
#'   "horticultural".
#'
#' @param data the input data in R-Abimo format
#' @param type a character object containing the name of natural scenario.
#'   Defaults to "undeveloped"
#' @param veg_class vegetation class to assign to each row in \code{data}.
#'   Default: 50
#' @return a dataframe with R-Abimo input data for the chosen natural scenario
#' @export
data_to_natural <- function(data, type = "undeveloped", veg_class = 50)
{
  # kwb.utils::assignPackageObjects("kwb.rabimo")
  # data <- kwb.rabimo::rabimo_inputs_2020$data; type = "undeveloped"
  # data <- kwb.rabimo::rabimo_inputs_2025$data; type = "undeveloped"

  # Check whether data look as expected
  stop_on_invalid_data(data)

  # Columns related to urbanisation
  urban_columns <- grep("pv|swg|roof", names(data), value = TRUE)

  # non urbanized state: no building, no pavements
  data[urban_columns] <- 0

  # set vegetation class
  data["veg_class"] <- veg_class

  if (type != "undeveloped") {
    land_types <- select_columns(data, "land_type")
    is_waterbody <- land_type_is_waterbody(land_types)
    data[["land_type"]][!is_waterbody] <- if (type == "forested") {
      "forested"
    } else if (type == "horticultural") {
      "horticultural"
    } else {
      stop("please provide a known natural scenario type: undeveloped, horticultural or forested")
    }
  }

  check_or_convert_data_types(
    data,
    types = get_expected_data_type(),
    convert = TRUE
  )
}
