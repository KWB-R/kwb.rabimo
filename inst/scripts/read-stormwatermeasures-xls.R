# read_input_table -------------------------------------------------------------
read_input_table <- function(xls_file, ...)
{
  na_to_empty <- function(x) ifelse(is.na(x), "", x)
  empty_to_na <- function(x) ifelse(x == "", NA, x)

  captions <- paste(
    na_to_empty(read_strings(xls_file, range = "Y1:AN2")),
    na_to_empty(read_strings(xls_file, "Y2:AN3")),
    sep = "_"
  )

  captions <- kwb.utils::naToLastNonNa(empty_to_na(gsub("^_|_$", "", captions)))
  captions <- kwb.utils::makeUnique(captions, sep = "_", warn = FALSE)

  stats::setNames(read_range(xls_file, "Y3:AN14", ...), captions)
}

# read_range -------------------------------------------------------------------
read_range <- function(xls_file, range, sheet = 1)
{
  suppressMessages(data <- readxl::read_excel(xls_file, range = range, sheet = sheet))
  as.data.frame(data)
}

# read_strings -----------------------------------------------------------------
read_strings <- function(xls_file, range, ...)
{
  data <- read_range(xls_file, range, ...)
  gsub(" ", "_", trimws(unname(unlist(data))))
}

# read_output_tables -----------------------------------------------------------
read_output_tables <- function(xls_file, ...)
{
  list(
    green_roof_table = kwb.utils::renameColumns(
      read_range(xls_file, "Z21:AC32", ...),
      list(
        "ref a" = "ref_area",
        "new gr" = "green_roof_area",
        "...4" = "green_roof"
      )
    ),
    unpaved_area_table = kwb.utils::renameColumns(
      read_range(xls_file, "AE21:AO32", ...),
      list(
        "ref a" = "ref_area",
        "new pvd" = "paved_area",
        "...5" = "pvd",
        "new unpvd" = "unpaved_area",
        "...7" = "unpaved",
        "new sealed" = "sealed_area",
        "...9" = "sealed",
        "corr. sca" = "corr_to_swale_area",
        "...11" = "corr_to_swale"
      )
    ),
    swale_connection_table = kwb.utils::renameColumns(
      read_range(xls_file, "AQ21:AU32", ...),
      list(
        "ref a" = "ref_area",
        "new sca" = "to_swale_area",
        "...5" = "to_swale"
      )
    )
  )
}

# read_targets -----------------------------------------------------------------
read_targets <- function(ref_file, ...)
{
  values <- read_range(ref_file, "K28:K31", ...)[[1L]]
  stats::setNames(values, c("green_roof", "unpaved", "to_swale"))
}

# read_measure_means -----------------------------------------------------------
read_measure_means <- function(ref_file, ...)
{
  values <- read_range(ref_file, "K20:K26", ...)[[1L]]
  list(
    green_roof = values[2L],
    unpaved = values[4L],
    to_swale = values[6L]
  )
}

# get_or_set_target_values_in_xls_file -----------------------------------------
get_or_set_target_values_in_xls_file <- function(xls_file, targets = NULL, ...)
{
  xls_file <- kwb.utils::safePath(path.expand(xls_file))

  if (is.null(targets)) {
    return(read_targets(xls_file, ...))
  }

  require("RDCOMClient", quietly = TRUE)

  # The target values are in cells
  CELL_TARGETS <- list(
    green_roof = c(29L, 11L), # column 11 = "K"
    unpaved = c(30L, 11L),
    to_swale = c(31L, 11L)
  )

  xls_app <- RDCOMClient::COMCreate("Excel.Application")
  on.exit(xls_app$Quit())

  workbook <- xls_app[["Workbooks"]]$Open(xls_file)
  sheet <- workbook$Worksheets("Tabelle1")

  replaced_values <- numeric()

  for (measure in names(CELL_TARGETS)) {
    cell_coords <- kwb.utils::selectElements(CELL_TARGETS, measure)
    cell <- sheet$Cells(cell_coords[1L], cell_coords[2L])
    replaced_values[measure] <- cell[["Value"]]
    cell[["Value"]] <- targets[[measure]]
  }

  workbook$Save()

  # Check that the changes have "arrived" in the Excel file
  stopifnot(identical(read_targets(xls_file, ...), targets))

  # Return the old values
  invisible(replaced_values)
}
