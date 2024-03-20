# BERLIN_TYPES_TO_LAND_TYPE_VEG_CLASS_IRRIGATION -------------------------------

#' Assignment between (NUTZUNG, TYP) and (land_type, veg_class, irrigation)
#'
#' The following tables are read from csv files and then merged:
#'
#' berlin_tuples.csv
#'   table of different occurring (land_type, veg_class, irrigation) tuples
#'
#' berlin_type_tuple_groups.csv
#'   assignments between berlin_type (input column "TYP") and tuples
#'
#' berlin_usage_to_type_tuple_groups.csv
#'   assignments between berlin_usage (input column "NUTZUNG") and
#'   assignments between berlin_type (input column "TYP") and tuple
#'
#' @importFrom dplyr left_join
#' @export
BERLIN_TYPES_TO_LAND_TYPE_VEG_CLASS_IRRIGATION <- local({

  # Helper function to read csv file from extdata/
  read_csv <- function(name, colClasses = NA) {
    base_name <- sprintf("berlin_%s.csv", name)
    file <- system.file(package = "kwb.rabimo", "extdata", base_name)
    utils::read.csv(file, colClasses = colClasses)
  }

  # Merge the tables
  read_csv("usage_to_type_tuple_groups") %>%
    dplyr::left_join(
      read_csv("type_tuple_groups"),
      by = "group",
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      y = read_csv("tuples", c("integer", "character", "numeric", "numeric")),
      by = "tuple_id"
    ) %>%
    remove_columns(c("group", "tuple_id"))
})
