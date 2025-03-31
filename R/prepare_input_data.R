# prepare_input_data -----------------------------------------------------------

#' Prepare Input Data: Rename, Add Columns
#'
#' Rename columns from ABIMO 3.2 original names to ABIMO 3.3 internal names
#'
#' @param data data frame with columns CODE, REGENJA, REGENSO, NUTZUNG,
#'   TYP, BEZIRK, FLGES, STR_FLGES, PROBAU, PROVGU, VGSTRASSE, KAN_BEB, BELAG1,
#'   BELAG2, BELAG3, BELAG4, KAN_VGU, STR_BELAG1, STR_BELAG2, STR_BELAG3,
#'   STR_BELAG4, KAN_STR, FLUR, FELD_30, FELD_150
#' @param config configuration object (list) as returned by the function
#'   \code{abimo_config_to_config()} used on \code{kwb.abimo::read_config()}
#' @param dbg logical indicating whether or not to show debug messages
#' @return \code{data} with columns renamed and additional columns
#'  (e.g. ratios calculated from percentages, land type, vegetation class,
#'  irrigation)
prepare_input_data <- function(data, config, dbg = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  #data <- sf::read_sf("~/../Downloads/raw_combined.gpkg")
  #data$geom <- NULL
  #data <- as.data.frame(data)

  #data <- kwb.abimo::abimo_input_2019
  #data <- berlin_2020_data
  #data <- kwb.utils:::get_cached("berlin_2020_data")
  #data <- new_data
  #config <- abimo_config_to_config(kwb.abimo::read_config())
  #`%>%` <- magrittr::`%>%`

  #
  # See inst/scripts/test-rabimo.R for test data assignments
  #

  # Try to identify the "format" of the data frame
  data_format <- identify_data_format_or_stop(data)

  # Set column names to upper case to match 2019 data when using raw 2020 data
  names(data) <- toupper(names(data))

  # Rename columns from ABIMO 3.2 names to new ABIMO-internal names
  data <- rename_columns(data, get_column_renamings())

  # If area fractions or area main or area road are missing (NA) set them to 0
  data <- set_columns_to_zero_where_na(
    data = data,
    columns = matching_names(data, pattern = "roof|pvd|srf|area_")
  )

  if (data_format == "format_2020") {

    # Identify roads
    is_road <- grepl("Stra.e", select_columns(data, "ART"))

    # Check that there is no "usage" type for roads (NAs or zero)
    road_usages <- select_columns(data, "berlin_usage")[is_road]

    stopifnot(all(ifelse(is.na(road_usages), 0L, road_usages) == 0L))

    # Set "berlin_usage" for roads to 300
    data$berlin_usage[is_road] <- 300L

    # Copy district information into the correct column (not needed anymore)
    if ("BEZIRK_1" %in% names(data)) {
      stopifnot(identical(data$BEZIRK_1, data$district))
    }
    # data$district[is_road] <- select_columns(data, "BEZIRK_1")[is_road]

    surface_class_columns <- sprintf("srf%d_pvd", 1:5)

    delta_to_100 <- 100 - rowSums(data[surface_class_columns])
    has_delta <- delta_to_100 > 0

    # Assign missing surface class partition to surface class 5
    data[["srf5_pvd"]][has_delta] <-
      data[["srf5_pvd"]][has_delta] + delta_to_100[has_delta]

    # if for some areas the sum of all surface classes exceeds 1 correct it
    # by reducing proportionally all surface classes
    data[, surface_class_columns] <- rescale_to_row_sum(
      as.matrix(select_columns(data, surface_class_columns)),
      row_sum = 100
    )
  } else if (data_format == "format_2019") {

    # add en empty column for surface class 5 for format consistency
    data <- kwb.utils::insertColumns(
      data, srf5_pvd = rep(0,nrow(data)), after = "srf4_pvd")
  }

  # Create column accessor function
  fetch_data <- create_accessor(data)
  fetch_config <- create_accessor(config)

  # correct precipitation with correction factor from the config file
  data[["prec_yr"]] <- fetch_data("prec_yr") *
    fetch_config("precipitation_correction_factor")

  # Convert percentages to fractions
  data[["total_area"]] <- fetch_data("FLGES") + fetch_data("STR_FLGES")

  # Transform percentage to fractions
  data[["main_frac"]] <- fetch_data("FLGES") / data[["total_area"]]
  data[["road_frac"]] <- fetch_data("STR_FLGES") / data[["total_area"]]

  # Determine names of columns that need to be divided by 100
  columns_to_divide_by_100 <- read_column_info() %>%
    dplyr::filter(.data[["by_100"]] == "x") %>%
    select_columns("rabimo_berlin") %>%
    intersect(names(data))

  data[columns_to_divide_by_100] <- lapply(data[columns_to_divide_by_100], `/`, 100)

  # insert empty to_swale column (fraction of the area connected to a swale)
  data[["to_swale"]] <- 0

  # insert empty green-roof column (fraction of roof)
  data[["green_roof"]] <- 0

  # Get (land_type, veg_class, irrigation) tuples based on Berlin-specific codes
  usage_types <- fetch_data(c("berlin_usage", "berlin_type"))

  usages <- get_usage_tuple(
    usage = usage_types[[1L]],
    type = usage_types[[2L]]
  )

  # Calculate potential evaporation for all areas and column-bind everything
  # together. Roads have no district: etp 775
  data <- cbind(data, usages,
    get_potential_evaporation(
    is_waterbody = land_type_is_waterbody(usages[["land_type"]]),
    district = fetch_data("district"),
    lookup = fetch_config("potential_evaporation")
  ))

  # Add a text column describing the type of block (usage)
  data[["block_type"]] <- get_block_type(usage_types)

  # Write road specification into "block_type"
  data[["block_type"]][grepl("300", data[["block_type"]])] <- "300_road"

  # Set roof area that are NAs to 0 for water bodies
  data$roof[land_type_is_waterbody(data$land_type) & is.na(data$roof)] <- 0

  # Read information about the expected data types
  data_types <- get_expected_data_type()

  # Select only the required columns and convert data types as required
  data %>%
    select_columns(intersect(get_column_selection(), names(data))) %>%
    check_or_convert_data_types(data_types, convert = TRUE, dbg = dbg)
}

# identify_data_format_or_stop -------------------------------------------------
identify_data_format_or_stop <- function(data)
{
  columns <- names(data)

  expected <- list(
    format_2020 = "art",
    format_2019 = "CODE"
  )

  for (format_name in names(expected)) {
    if (expected[[format_name]] %in% columns) {
      return(format_name)
    }
  }

  stop_formatted(
    paste0(
      "Unknown format. I was looking for one of these columns: %s.\n",
      "I found these: %s"
    ),
    string_list(unlist(expected)),
    string_list(columns)
  )
}


# get_column_renamings ---------------------------------------------------------
get_column_renamings <- function()
{
  renamings <- read_column_info() %>%
    dplyr::filter(nzchar(.data[["abimo_berlin"]])) %>%
    to_lookup_list(data = select_columns(., c("abimo_berlin", "rabimo_berlin")))
}

# read_column_info -------------------------------------------------------------

#' Provide Meta Information About Input Columns
#'
#' @returns data frame with columns "rabimo_berlin", "abimo_berlin", "by_100",
#'   "meaning", "unit", "type", "data_type", "default"
#' @export
read_column_info <- function()
{
  "extdata/column-names.csv" %>%
    system.file(package = "kwb.rabimo") %>%
    utils::read.table(
      header = TRUE,
      stringsAsFactors = FALSE,
      quote = "",
      sep = ",",
      dec = "."
    )
}

# get_usage_tuple --------------------------------------------------------------

#' Get Usage Tuple (Land_type, Veg_class, Irrigation) from NUTZUNG and TYP
#'
#' @param usage value of column NUTZUNG in input data frame
#' @param type value of column TYP in input data frame
#' @param include_inputs logical indicating whether or not to include the
#'   input values in the output
#' @return list with elements \code{land_type}, \code{veg_class}, \code{irrigation}
#' @export
#' @examples
#' get_usage_tuple(10, 10)
#' get_usage_tuple(10, 10, TRUE)
#' get_usage_tuple(10, 1:3)
#' get_usage_tuple(10, 1:3, TRUE)
get_usage_tuple <- function(usage, type, include_inputs = FALSE)
{
  #usage = 10L; type = 10L
  #usage = 10L; type = 333L
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  # Prepare data for which to lookup value combinations in the lookup table
  data <- data.frame(
    berlin_usage = usage,
    berlin_type = type
  )

  result <- as.data.frame(multi_column_lookup(
    data = data,
    lookup = BERLIN_TYPES_TO_LAND_TYPE_VEG_CLASS_IRRIGATION,
    value = c("land_type", "veg_class", "irrigation"),
    includeKeys = include_inputs
  ))

  if (any(is_missing <- is.na(result[["land_type"]]))) {
    stop_formatted(
      "Could not find a (land_type, veg_class, irrigation) tuple for %s",
      paste(collapse = ", ", sprintf(
        "(NUTZUNG = %d, TYP = %d)",
        usage[is_missing],
        type[is_missing]
      ))
    )
  }

  result
}

# get_potential_evaporation ----------------------------------------------------

#' Provide Data on Potential Evaporation
#'
#' @param is_waterbody (vector of) logical indicating whether a block area is
#'   of type (from the land_type/veg_class/irrigation tuple) "waterbody"
#' @param district (vector of) integer indicating the district number of the
#'   plot area (from the original input column "BEZIRK")
#' @param lookup data frame with key columns \code{is_waterbody}, \code{district}
#'   and value columns \code{etp}, \code{etps}. A data frame of the required
#'   structure is returned by \code{\link{abimo_config_to_config}} in list
#'   element \code{"potential_evaporation"}
#' @export
#' @examples
#' \dontrun{
#' config <- abimo_config_to_config(kwb.abimo::read_config())
#' get_potential_evaporation(
#'   is_waterbody = TRUE,
#'   district = 1,
#'   lookup = config$potential_evaporation
#' )
#'
#' get_potential_evaporation(
#'   is_waterbody = c(TRUE, TRUE, FALSE, FALSE),
#'   district = c(1, 99, 2, 99),
#'   lookup = config$potential_evaporation
#' )
#' }
#'
get_potential_evaporation <- function(is_waterbody, district, lookup)
{
  # Lookup values for "etp", "etps" in the lookup table based on the
  # value combinations in vectors "is_waterbody", "district"
  result <- data.frame(is_waterbody = is_waterbody, district = district) %>%
    multi_column_lookup(lookup, value = c("etp", "etps")) %>%
    set_names(c("epot_yr", "epot_s"))

  if (all(lengths(result) == 1L)) {
    result
  } else {
    as.data.frame(result)
  }
}

# get_block_type ---------------------------------------------------------------
get_block_type <- function(usage_types)
{
  merge_metadata <- function(data, name, by) {
    dplyr::left_join(
      x = data,
      y = utils::read.table(
        file = kwb.abimo::extdata_file(paste0(name, ".csv")),
        sep = ";",
        header = TRUE,
        fileEncoding = "WINDOWS-1252"
      ),
      by = by
    )
  }

  usage_types %>%
    merge_metadata("nutzungstypen_berlin", c(berlin_usage = "Use_ID")) %>%
    merge_metadata("strukturtypen_berlin", c(berlin_type = "Type_ID")) %>%
    remove_columns(pattern = "_GER$") %>%
    paste_columns(sep = ": ") %>%
    subst_special_chars()

}

# get_column_selection ---------------------------------------------------------
get_column_selection <- function()
{
  read_column_info() %>%
    select_columns("rabimo_berlin") %>%
    setdiff(c("berlin_usage", "berlin_type")) %>%
    c("block_type")
}
