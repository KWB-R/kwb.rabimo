#' R-Abimo Inputs (Data and Config) for Berlin, 2025.
#'
#' Data frame and configuration object that are required by the R-Abimo main
#' function \code{\link{run_rabimo}}.
#' The data have been provided by
#' Senate Department for Urban Development, Building and Housing
#' III D Spatial Data Infrastructure, Environmental Atlas.
#'
#' @format ## `rabimo_inputs_2025`
#' A list containing two elements:
#' \describe{
#'   \item{data}{a data frame with the input data in r-abimo format ...(number of vars)}
#'   \item{config}{a list object with configuration data}
#' }
#' @format ## `rabimo_inputs_2025$data`
#' A data.frame with 58531 observations of 25 variables:
#' \describe{
#'   \item{`code`}{Unique block area identifier (character)}
#'   \item{`prec_yr`}{Long-term average of annual precipitation in mm (integer)}
#'   \item{`prec_s`}{Long-term average of annual precipitation within summer months (May to October) in mm (integer)}
#'   \item{`epot_yr`}{Long-term average of annual potential evapotranspiration in mm (integer)}
#'   \item{`epot_s`}{Long-term average of annual potential evapotranspiration within summer months (May to October) in mm (integer)}
#'   \item{`district`}{Number of Berlin "Bezirk" (district) in which the block area is located (character)}
#'   \item{`total_area`}{Total block area in square metres (numeric)}
#'   \item{`roof`}{Fraction of the total area that is considered as "roof" area (numeric value between 0.0 and 1.0)}
#'   \item{`green_roof`}{Fraction of the roof area that belongs to green roofs (numeric value between 0.0 and 1.0). A value of 1.0 means that all roofs in the block area are green roofs.}
#'   \item{`swg_roof`}{Fraction of the roof area that is connected to the drainage system (numeric value between 0.0 and 1.0)}
#'   \item{`pvd`}{Fraction of the total block area that is paved (numeric value between 0.0 and 1.0)}
#'   \item{`swg_pvd`}{Fraction of the paved area that is connected to the drainage system (numeric value between 0.0 and 1.0)}
#'   \item{`srf1_pvd`}{Fraction of the paved area that belongs to surface class 1 (numeric value between 0.0 and 1.0, see note 1 below)}
#'   \item{`srf2_pvd`}{Fraction of the paved area that belongs to surface class 2 (numeric value between 0.0 and 1.0, see note 1 below)}
#'   \item{`srf3_pvd`}{Fraction of the paved area that belongs to surface class 3 (numeric value between 0.0 and 1.0, see note 1 below)}
#'   \item{`srf4_pvd`}{Fraction of the paved area that belongs to surface class 4 (numeric value between 0.0 and 1.0, see note 1 below)}
#'   \item{`srf5_pvd`}{Fraction of the paved area that belongs to surface class 5 (numeric value between 0.0 and 1.0, see note 1 below)}
#'   \item{`to_swale`}{Fraction of sealed area (roof area + paved area) that is connected to an infiltration swale (numeric)}
#'   \item{`gw_dist`}{Distance between groundwater table and surface in metres (numeric)}
#'   \item{`ufc30`}{field capacity in 30 cm depth (numeric)}
#'   \item{`ufc150`}{field capacity in 150 cm depth (numeric)}
#'   \item{`land_type`}{land type, one of `forested`, `horticultural`, `urban`, `vegetationless`, `waterbody` (character)}
#'   \item{`veg_class`}{vegetation class index (numeric), derived from an analysis tree volumes}
#'   \item{`irrigation`}{irrigation in mm per year (integer)}
#'   \item{`block_type`}{Block type identifier of the form "usage-type-id_block-type-id_usage-type-description_block-type-description" (character)}
#' }
#' @format ## `rabimo_inputs_2025$config`
#' A list with 3 named elements:
#' \describe{
#'   \item{runoff_factors}{Runoff factors, vector of numeric with names `roof`, `surface1`, `surface2`, `surface3`, `surface4`, `surface5`}
#'   \item{bagrov_values}{Bagrov values for sealed surfaces, vector of numeric with names `roof`, `green_roof`, `surface1`, `surface2`, `surface3`, `surface4`, `surface5`}
#'   \item{swale}{Model parameter(s) related to the 'swale' measure, vector of numeric with currently one value, named `swale_evaporation_factor`}
#' }
#' @source <https://www.berlin.de/umweltatlas/en/general/contact/>
#' @source <https://gdi.berlin.de/services/wfs/ua_gruendaecher_2020?REQUEST=GetCapabilities&SERVICE=wfs>
#'
"rabimo_inputs_2025"
