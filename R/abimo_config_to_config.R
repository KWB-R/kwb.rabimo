#' Convert Abimo Configuration to List
#'
#' @param abimo_config as returned by \code{kwb.abimo:::read_config}
#' @return list with elements \code{"potential_evaporation"},
#'   \code{"runoff_factors"}, \code{"bagrov_values"}, \code{"diverse"},
#'   \code{"result_digits"}
#' @export
abimo_config_to_config <- function(abimo_config)
{
  #abimo_config <- kwb.abimo:::read_config()
  #`%>%` <- magrittr::`%>%`
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  prefix <- "section_"
  result <- abimo_config[startsWith(names(abimo_config), prefix)] %>%
    set_names(remove_left(names(.), nchar(prefix))) %>%
    lapply(function(x) {
      filter_elements(x, "^item_") %>%
        lapply(function(y) {
          do.call(data.frame, as.list(xml2::xml_attrs(y)))
        }) %>%
        list_to_data_frame_with_keys("item", "item_(.*)")
    })

  epot_water <- "Gewaesserverdunstung"
  epot_else <- "PotentielleVerdunstung"

  extract_epot <- function(element, is_waterbody, renamings = NULL, ...) {
    result %>%
      select_elements(element) %>%
      expand_district_ranges() %>%
      lapply(as.integer) %>%
      as.data.frame() %>%
      rename_columns(renamings) %>%
      {cbind.data.frame(is_waterbody = is_waterbody, ., ...)}
  }

  result[["potential_evaporation"]] <- rbind(
    extract_epot(epot_water, TRUE, list(eg = "etp"), etps = 0),
    extract_epot(epot_else, FALSE)
  )

  convert_element <- function(config, from, to, convert = identity) {
    x <- select_elements(config, from)
    value <- convert(select_columns(x, "value"))
    key <- select_columns(x, "key")
    config[[to]] <- set_names(value, key)
    remove_elements(config, from)
  }

  complement <- function(x) 1 - as.numeric(x)

  result <- result %>%
    remove_elements(c(epot_water, epot_else)) %>%
    convert_element("Infiltrationsfaktoren", "runoff_factors", complement) %>%
    convert_element("Bagrovwerte", "bagrov_values", as.numeric) %>%
    convert_element("Diverse", "diverse") %>%
    convert_element("ErgebnisNachkommaStellen", "result_digits", as.numeric)

  # Convert types of "diverse" entries and shift them up to the main level
  diverse <- "diverse"
  x <- select_elements(result, diverse)
  result[["precipitation_correction_factor"]] <- as.numeric(x[["NIEDKORRF"]])
  result[["irrigation_to_zero"]] <- as.logical(x[["BERtoZero"]])
  result <- remove_elements(result, diverse)

  # Clean names for two sections
  keys <- c("runoff_factors", "bagrov_values")
  result[keys] <- lapply(result[keys], function(x) {
    set_names(x, multi_substitute(names(x), list(
      Dachflaechen = "roof",
      Belaglsklasse = "surface"
    )))
  })

  result
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

  stopifnot(!anyDuplicated(select_columns(x, district_column)))

  x
}

# expand_ranges_in_column ------------------------------------------------------
expand_ranges_in_column <- function(x, range_column, index_column)
{
  # Split data frame into a list of rows
  data_rows <- unname(split(x, seq_len(nrow(x))))

  # Expand each row with a range string in column according to the ranges
  do.call(rbind, unname(lapply(data_rows, function(data_row) {

    range_string <- select_columns(data_row, range_column)

    # Determine the indices that are described by the range string
    indices <- if (is.na(range_string)) {
      NA
    } else {
      index_string_to_integers(range_string)
    }

    # Repeat the current row for each of these indices
    result <- data_row[rep.int(1L, length(indices)), ]

    # Append a column with these indices
    result[[index_column]] <- indices

    # Return the data frame
    result
  })))
}
