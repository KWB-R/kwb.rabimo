`%>%` <- magrittr::`%>%`

config <- list(
  precipitation_correction_factor = 1,
  potential_evaporation = rbind(
    data.frame(is_waterbody = TRUE, district = NA, etp = 800, etps = 0),
    data.frame(is_waterbody = FALSE, district = NA, etp = 650, etps = 515)
  ),
  bagrov_values = c(
    roof = 0.05,
    surface1 = 0.11,
    surface2 = 0.11,
    surface3 = 0.25,
    surface4 = 0.40
  ),
  runoff_factors = c(
    roof = 1,
    surface1 = 1,
    surface2 = 1,
    surface3 = 1,
    surface4 = 1
  )
)

usages <- kwb.abimo::abimo_input_2019 %>%
  kwb.rabimo::prepare_input_data() %>%
  kwb.utils::selectColumns("usage") %>%
  unique()

data <- data.frame(
  CODE = seq_along(usages),
  district = 1,
  yield = 10,
  usage = usages,
  precipitationYear = 600,
  precipitationSummer = 200,
  fieldCapacity_30 = 10,
  fieldCapacity_150 = 10,
  depthToWaterTable = 5,
  irrigation = 0,
  areaFractionMain = 1,
  areaFractionRoad = 0,
  mainFractionBuiltSealed = 1,
  mainFractionUnbuiltSealed = 0,
  builtSealedFractionConnected = 1,
  unbuiltSealedFractionConnected = 1,
  unbuiltSealedFractionSurface1 = 1,
  unbuiltSealedFractionSurface2 = 0,
  unbuiltSealedFractionSurface3 = 0,
  unbuiltSealedFractionSurface4 = 0,
  roadFractionRoadSealed = 1,
  roadSealedFractionConnected = 1,
  roadSealedFractionSurface1 = 1,
  roadSealedFractionSurface2 = 0,
  roadSealedFractionSurface3 = 0,
  roadSealedFractionSurface4 = 0,
  totalArea = 1000
)

kwb.rabimo::run_rabimo(data, config)
