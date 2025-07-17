#' R-Abimo Inputs (Data and Config) for Berlin, 2020
#'
#' Data frame and configuration object that are required by the R-Abimo main
#' function \code{\link{run_rabimo}}.
#'
#' @format ## `rabimo_inputs_2020`
#' A list containing two elements:
#' \describe{
#'   \item{`data`}{a data frame with the input data to R-Abimo (see below)}
#'   \item{`config`}{a list of model parameters (see below)}
#' }
#' @format ## `rabimo_inputs_2020$data`
#' A data.frame with 58531 observations of 33 variables:
#' \describe{
#'   \item{`code`}{Unique block area identifier (character)}
#'   \item{`prec_yr`}{Long-term average of annual precipitation in mm (integer)}
#'   \item{`prec_s`}{Long-term average of annual precipitation within summer months (May to October) in mm (integer)}
#'   \item{`epot_yr`}{Long-term average of annual potential evapotranspiration in mm (integer)}
#'   \item{`epot_s`}{Long-term average of annual potential evapotranspiration within summer months (May to October) in mm (integer)}
#'   \item{`district`}{Number of Berlin "Bezirk" (district) in which the block area is located (character). This column is Berlin-specific and optional, i.e. not required by the model.}
#'   \item{`total_area`}{Total block area in square metres (numeric)}
#'   \item{`main_frac`}{Fraction of the total area that is NOT considered as "road" area (numeric value between 0.0 and 1.0). This value should be 0.0 if roads are modelled separately, i.e. as block areas on their own.}
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
#'   \item{`road_frac`}{Fraction of the total area that is considered as "road" area (numeric value between 0.0 and 1.0)}
#'   \item{`pvd_r`}{Fraction of road area (within the block area) that is paved (numeric, see note 2 below)}
#'   \item{`swg_pvd_r`}{Fraction of paved road area (within the block area) that is connected to the drainage system (numeric, see note 2 below))}
#'   \item{`srf1_pvd_r`}{Fraction of paved road area (within the block area) that belongs to surface class 1 (numeric, see note 2 below))}
#'   \item{`srf2_pvd_r`}{Fraction of paved road area (within the block area) that belongs to surface class 2 (numeric, see note 2 below))}
#'   \item{`srf3_pvd_r`}{Fraction of paved road area (within the block area) that belongs to surface class 3 (numeric, see note 2 below))}
#'   \item{`srf4_pvd_r`}{Fraction of paved road area (within the block area) that belongs to surface class 4 (numeric, see note 2 below))}
#'   \item{`to_swale`}{Fraction of sealed area (roof area + paved area) that is connected to an infiltration swale (numeric)}
#'   \item{`gw_dist`}{Distance between groundwater table and surface in metres (numeric)}
#'   \item{`ufc30`}{field capacity in 30 cm depth (numeric)}
#'   \item{`ufc150`}{field capacity in 150 cm depth (numeric)}
#'   \item{`land_type`}{land type, one of `forested`, `horticultural`, `urban`, `vegetationless`, `waterbody` (character)}
#'   \item{`veg_class`}{vegetation class (numeric values each being one of 0, 1, 10, 25, 30, 35, 40, 45, 50, 55). Each combination of block and land use type is assigned to a distinct vegetation class value.}
#'   \item{`irrigation`}{irrigation in mm per year (integer)}
#'   \item{`block_type`}{Block type identifier of the form "usage-type-id_block-type-id_usage-type-description_block-type-description" (character)}
#' }
#'
#' Note 1: The sum of surface class fractions `srf1_pvd`, `srf2_pvd`, `srf3_pvd`, `srf4_pvd`, `srf5_pvd` should be 1.0 within each block area.
#'
#' Note 2: The fields with suffix "_r" are all zero because rows are modelled as their own blocks. In an earlier version of the dataset, roads were modelled as parts of the block area.
#'
#' @format ## `rabimo_inputs_2020$config`
#' A list with 5 named elements:
#' \describe{
#'   \item{`runoff_factors`}{vector of num with names `roof`, `surface1`, `surface2`, `surface3`, `surface4`, `surface5`}
#'   \item{`bagrov_values`}{vector of num with names `roof`, `green_roof`, `surface1`, `surface2`, `surface3`, `surface4`, `surface5`}
#'   \item{`result_digits`}{vector of num with names `R`, `ROW`, `RI`, `RVOL`, `ROWVOL`, `RIVOL`, `FLAECHE`, `VERDUNSTUNG`}
#'   \item{`irrigation_to_zero`}{not used!}
#'   \item{`swale`}{vector of num with names `swale_evaporation_factor`}
#' }
"rabimo_inputs_2020"
