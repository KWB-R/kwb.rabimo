# prepare_input_data -----------------------------------------------------------

#' Prepare Input Data: Rename, Add Columns
#'
#' Rename columns from ABIMO 3.2 original names to ABIMO 3.3 internal names
#'
#' @param input_data data frame with columns CODE, REGENJA, REGENSO, NUTZUNG,
#'   TYP, BEZIRK, FLGES, STR_FLGES, PROBAU, PROVGU, VGSTRASSE, KAN_BEB, BELAG1,
#'   BELAG2, BELAG3, BELAG4, KAN_VGU, STR_BELAG1, STR_BELAG2, STR_BELAG3,
#'   STR_BELAG4, KAN_STR, FLUR, FELD_30, FELD_150
#' @return \code{input_data} with columns renamed and additional columns
#'  (e.g. ratios calculated from percentages, (main) usage, yield, irrigation)
#' @export
prepare_input_data <- function(input_data)
{
  # 1. Rename columns from ABIMO 3.2 names to ABIMO 3.3 internal names
  # 2. Select only the columns that are required
  input <- rename_and_select(input_data, INPUT_COLUMN_RENAMINGS)

  # Create column accessor function
  fetch <- create_accessor(input)

  # Calculate total area
  input[["totalArea"]] <- fetch("mainArea") + fetch("roadArea")

  # Convert percentages to fractions
  fractions <- calculate_fractions(input)

  fractions[["mainFractionSealed"]] <- calculate_main_fraction_sealed(
    fetch("mainPercentageBuiltSealed"),
    fetch("mainPercentageUnbuiltSealed")
  )

  # Get (usage, yield, irrigation) tuples based on Berlin-specific codes
  usages <- get_usage_tuple(
    usage = fetch("berlin_usage"),
    type = fetch("berlin_type")
  )

  # Column-bind everything together
  cbind(input, fractions, usages)
}

# INPUT_COLUMN_RENAMINGS -------------------------------------------------------
INPUT_COLUMN_RENAMINGS <- list(
  CODE = "code",
  REGENJA = "precipitationYear",
  REGENSO = "precipitationSummer",
  NUTZUNG = "berlin_usage",
  TYP = "berlin_type",
  BEZIRK = "district",
  FLGES = "mainArea",
  STR_FLGES = "roadArea",
  PROBAU = "mainPercentageBuiltSealed",
  PROVGU = "mainPercentageUnbuiltSealed",
  VGSTRASSE = "roadPercentageSealed",
  KAN_BEB = "builtSealedPercentageConnected",
  BELAG1 = "unbuiltSealedPercentageSurface1",
  BELAG2 = "unbuiltSealedPercentageSurface2",
  BELAG3 = "unbuiltSealedPercentageSurface3",
  BELAG4 = "unbuiltSealedPercentageSurface4",
  KAN_VGU = "unbuiltSealedPercentageConnected",
  STR_BELAG1 = "roadSealedPercentageSurface1",
  STR_BELAG2 = "roadSealedPercentageSurface2",
  STR_BELAG3 = "roadSealedPercentageSurface3",
  STR_BELAG4 = "roadSealedPercentageSurface4",
  KAN_STR = "roadSealedPercentageConnected",
  FLUR = "depthToWaterTable",
  FELD_30 = "fieldCapacity_30",
  FELD_150 = "fieldCapacity_150"
)

# INPUT_COLUMNS_NEEDED ---------------------------------------------------------
INPUT_COLUMNS_NEEDED <- names(INPUT_COLUMN_RENAMINGS)

# INPUT_COLUMNS_NOT_NEEDED -----------------------------------------------------
INPUT_COLUMNS_NOT_NEEDED <- setdiff(
  names(kwb.abimo::abimo_input_2019),
  INPUT_COLUMNS_NEEDED
)

# calculate_fractions ----------------------------------------------------------
calculate_fractions <- function(input)
{
  # Column accessor
  fetch <- create_accessor(input)

  # Helper function to select column and divide by 100
  by_100 <- function(x) fetch(x) / 100

  total_area <- fetch("totalArea")

  data.frame(
    areaFractionMain = fetch("mainArea") / total_area,
    areaFractionRoad = fetch("roadArea") / total_area,
    mainFractionBuiltSealed = by_100("mainPercentageBuiltSealed"),
    mainFractionUnbuiltSealed = by_100("mainPercentageUnbuiltSealed"),
    roadFractionRoadSealed = by_100("roadPercentageSealed"),
    builtSealedFractionConnected = by_100("builtSealedPercentageConnected"),
    unbuiltSealedFractionConnected = by_100("unbuiltSealedPercentageConnected"),
    unbuiltSealedFractionSurface1 = by_100("unbuiltSealedPercentageSurface1"),
    unbuiltSealedFractionSurface2 = by_100("unbuiltSealedPercentageSurface2"),
    unbuiltSealedFractionSurface3 = by_100("unbuiltSealedPercentageSurface3"),
    unbuiltSealedFractionSurface4 = by_100("unbuiltSealedPercentageSurface4"),
    roadSealedFractionConnected = by_100("roadSealedPercentageConnected"),
    roadSealedFractionSurface1 = by_100("roadSealedPercentageSurface1"),
    roadSealedFractionSurface2 = by_100("roadSealedPercentageSurface2"),
    roadSealedFractionSurface3 = by_100("roadSealedPercentageSurface3"),
    roadSealedFractionSurface4 = by_100("roadSealedPercentageSurface4")
  )
}

# calculate_main_fraction_sealed -----------------------------------------------
calculate_main_fraction_sealed <- function(
    mainPercentageBuiltSealed,
    mainPercentageUnbuiltSealed
)
{
  # Calculate the percentage of built and unbuilt sealed areas. Add a small
  # value to round .5 "up" not "down":
  # round(98.5) -> 98
  # round(98.5 + 1e-12) -> 99

  # === Code in C++:
  # vgd = (dbReader.getRecord(k, "PROBAU")).toFloat() / 100.0F; // Dachflaechen
  # vgb = (dbReader.getRecord(k, "PROVGU")).toFloat() / 100.0F; // Hofflaechen
  # ptrDA.VER = (int)round((vgd * 100) + (vgb * 100));

  as.integer(round(
    1e-12 + mainPercentageBuiltSealed + mainPercentageUnbuiltSealed
  )) / 100
}
