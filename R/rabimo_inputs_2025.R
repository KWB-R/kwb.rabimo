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
#'   \item{data}{a data frame with the input data in R-Abimo format (see below)}
#'   \item{config}{a list object with configuration data (see below)}
#' }
#' @format ## `rabimo_inputs_2025$data`
#' A data.frame with 58531 observations of 26 variables:
#' \describe{
#'   \item{`code`}{Unique block area identifier (character)}
#'   \item{`prec_yr`}{Long-term average of annual precipitation in mm (integer)}
#'   \item{`prec_s`}{Long-term average of annual precipitation within summer months (May to October) in mm (integer)}
#'   \item{`epot_yr`}{Long-term average of annual potential evapotranspiration in mm (integer)}
#'   \item{`epot_s`}{Long-term average of annual potential evapotranspiration within summer months (May to October) in mm (integer)}
#'   \item{`district`}{Number of Berlin "Bezirk" (district) in which the block area is located (character). This column is Berlin-specific and optional, i.e. not required by the model.}
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
#'   \item{`to_swale`}{Fraction of sealed area (roof area + paved area) that is connected to an infiltration swale (numeric value between 0.0 and 1.0)}
#'   \item{`gw_dist`}{Distance between groundwater table and surface in metres (numeric)}
#'   \item{`ufc30`}{field capacity in 30 cm depth (numeric)}
#'   \item{`ufc150`}{field capacity in 150 cm depth (numeric)}
#'   \item{`land_type`}{land type, one of `forested`, `horticultural`, `urban`, `vegetationless`, `waterbody` (character)}
#'   \item{`veg_class`}{vegetation class index (numeric), derived from an analysis of tree volumes}
#'   \item{`irrigation`}{irrigation in mm per year (integer)}
#'   \item{`block_type`}{Block type identifier of the form "usage-type-id_block-type-id_usage-type-description_block-type-description" (character). This column is Berlin-specific and optional, i.e. not required by the model.}
#'   \item{`Shape`}{List structure containing geometry information on the different block areas. This column is optional. If provided, it will be appended to the model output so that model results can be plotted in the form of maps.}
#' }
#' 
#' Note 1: The sum of surface class fractions `srf1_pvd`, `srf2_pvd`, `srf3_pvd`, `srf4_pvd`, `srf5_pvd` should be 1.0 within each block area. For the meaning of the five different surface classes in Berlin, see the table in the Details section below.
#' 
#' @format ## `rabimo_inputs_2025$config`
#' A list with 3 named elements:
#' \describe{
#'   \item{runoff_factors}{Runoff factors for roofs and five different surface 
#'   types, given as a vector of numeric with element names `roof`, `surface1`, 
#'   `surface2`, `surface3`, `surface4`, `surface5`. A runoff factor determines 
#'   the proportion of precipitation that, after subtraction of 
#'   evapotranspiration, becomes surface runoff from a paved area. The higher 
#'   the factor, the less permeable is the surface. See the table in the Details section below.}
#'   \item{bagrov_values}{Bagrov values to calculate evapotranspiration from 
#'   paved surfaces, given as a vector of numeric with element names `roof`, 
#'   `green_roof`, `surface1`, `surface2`, `surface3`, `surface4`, `surface5`.
#'   The higher the Bagrov value, the more evapotranspiration is generated by 
#'   the model, see the table in the Details section below. For a description of the evapotranspiration model and for a 
#'   figure that shows the influence of the Bagrov values (n) on the 
#'   evapotranspiration (in German), see \url{https://www.berlin.de/umweltatlas/wasser/wasserhaushalt/2001/methode/}}
#'   \item{swale}{Model parameter(s) related to the 'swale' measure, given as a 
#'   vector of numeric with currently one value, named 
#'   `swale_evaporation_factor`. The swale evaporation factor determines which 
#'   fraction of the water going into a swale becomes evapotranspiration (the 
#'   rest becomes infiltration).}
#' }
#' @source <https://www.berlin.de/umweltatlas/en/general/contact/>
#' @source <https://gdi.berlin.de/services/wfs/ua_gruendaecher_2020?REQUEST=GetCapabilities&SERVICE=wfs>
#' @details
#' See the following table for the meaning of the surface classes and their Bagrov values and runoff factors used for Berlin.
#' |**Surface Class**|**Surface Type**                                                                                                      |**Bagrov Value**|**Runoff Factor**|
#' |:------------:|:------------------------------------------------------------------------------------------------------------------------|------------:|-------------:|
#' |-             |Roof surfaces                                                                                                            |         0.05|          1.00|
#' |-             |Extensive vegetated roofs                                                                                                |         0.65|          1.00|
#' |1             |Asphalt, concrete, paving with joint sealing or concrete base, synthetic surfaces                                        |         0.11|          0.90|
#' |2             |Artificial stone and slab coverings (side length > 8 cm), concrete interlocking pavers, clinker, medium and large pavers |         0.11|          0.70|
#' |3             |Small and mosaic pavers (side length < 8 cm)                                                                             |         0.25|          0.40|
#' |4             |Grass pavers, bound gravel surfaces, gravel lawns                                                                        |         0.40|          0.10|
#' |5             |Unknown type (average parameters)                                                                                        |         0.25|          0.48|
#' @md
"rabimo_inputs_2025"
