#' Convert Abimo Configuration to List
#'
#' @param abimo_config as returned by \code{kwb.abimo:::read_config}
#' @return list with elements \code{"potential_evaporation"},
#'   \code{"runoff_factors"}, \code{"bagrov_values"}, \code{"diverse"},
#'   \code{"result_digits"}
#' @export
abimo_config_to_config <- function(abimo_config)
{
  # abimo_config <- kwb.abimo:::read_config()
  #`%>%` <- magrittr::`%>%`
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  section_pattern <- "^section_"

  result <- abimo_config %>%
    filter_elements(section_pattern) %>%
    lapply(get_all_item_data)

  names(result) <- gsub(section_pattern, "", names(result))

  element_water <- "Gewaesserverdunstung"
  element_else <- "PotentielleVerdunstung"

  all_columns_to_int <- function(x) as.data.frame(lapply(x, as.integer))

  evap_water <- result %>%
    select_elements(element_water) %>%
    expand_district_ranges() %>%
    all_columns_to_int() %>%
    rename_columns(list(eg = "etp")) %>%
    cbind(etps = 0, is_waterbody = TRUE)

  evap_else <- result %>%
    select_elements(element_else) %>%
    expand_district_ranges() %>%
    all_columns_to_int() %>%
    cbind(is_waterbody = FALSE)

  result[["potential_evaporation"]] <- rbind(evap_water, evap_else) %>%
    move_columns_to_front("is_waterbody")

  convert_element <- function(config, from, to, convert = identity) {
    x <- select_elements(config, from)
    config[[to]] <- stats::setNames(
      convert(select_columns(x, "value")),
      select_columns(x, "key")
    )
    remove_elements(config, from)
  }

  result <- result %>%
    remove_elements(c(
      element_water,
      element_else
    )) %>%
    convert_element(
      from = "Infiltrationsfaktoren",
      to = "runoff_factors",
      convert = function(x) 1 - as.numeric(x)
    ) %>%
    convert_element(
      from = "Bagrovwerte",
      to = "bagrov_values",
      convert = as.numeric
    ) %>%
    convert_element(
      from = "Diverse",
      to = "diverse"
    ) %>%
    convert_element(
      from = "ErgebnisNachkommaStellen",
      to = "result_digits",
      convert = as.numeric
    )

  diverse <- select_elements(result, "diverse")

  result[["precipitation_correction_factor"]] <- as.numeric(
    diverse[["NIEDKORRF"]]
  )

  result[["irrigation_to_zero"]] <- as.logical(
    diverse[["BERtoZero"]]
  )

  result <- remove_elements(result, "diverse")

  clean_names <- function(x) {
    stats::setNames(x, multi_substitute(names(x), list(
      Dachflaechen = "roof",
      Belaglsklasse = "surface"
    )))
  }

  elements <- c("runoff_factors", "bagrov_values")

  result[elements] <- lapply(result[elements], clean_names)

  result
}

# get_all_item_data ------------------------------------------------------------
get_all_item_data <- function(x)
{
  get_attribute_data <- function(x) {
    values <- xml2::xml_attrs(x)
    do.call(data.frame, as.list(values))
  }

  get_item_data <- function(x) {
    list_to_data_frame_with_keys(x, "item", "item_(.*)")
  }

  filter_elements(x, "^item_") %>%
    lapply(get_attribute_data) %>%
    get_item_data()
}

# expand_district_ranges -------------------------------------------------------
expand_district_ranges <- function(
    x,
    range_column = "bezirke",
    district_column = "district"
)
{
  x <- x %>%
    expand_ranges_in_column(range_column, district_column) %>%
    reset_row_names() %>%
    remove_columns(c("item", range_column)) %>%
    move_columns_to_front(district_column)

  stopifnot(
    !anyDuplicated(select_columns(x, district_column))
  )

  x
}
