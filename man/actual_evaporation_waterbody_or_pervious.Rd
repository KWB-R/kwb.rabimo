% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/actual_evaporation_waterbody_or_pervious.R
\name{actual_evaporation_waterbody_or_pervious}
\alias{actual_evaporation_waterbody_or_pervious}
\title{Calculate Actual Evapotranspiration for Waterbodies or Pervious Areas}
\usage{
actual_evaporation_waterbody_or_pervious(
  usage_tuple,
  climate,
  soil_properties,
  dbg = TRUE,
  ...,
  digits = NULL
)
}
\arguments{
\item{usage_tuple}{list with elements \code{land_type}, \code{veg_class},
\code{irrigation}}

\item{climate}{list with elements \code{epot.year}, \code{epot.summer}
(potential evaporation in mm per year and in the summer period,
respecively), \code{prec.year}, \code{prec.summer} (precipitation in mm
per year and in the summer period, respectively).}

\item{soil_properties}{list as returned by \code{\link{get_soil_properties}},
with elements \code{mean_potential_capillary_rise_rate}, \code{g02},
\code{potential_capillary_rise}, \code{depth_to_water_table}}

\item{dbg}{logical indicating whether or not to show debug messages}

\item{\dots}{further arguments passed to \code{\link{real_evapo_transpiration}}
such as \code{run_parallel}, \code{blocksize}}

\item{digits}{optional. If given, the BAGROV parameter values are rounded to
this number of digits. This reduces the number of BAGROV curves that need
to be calculated and thus improves the performance (by reducing the
precision of the output)}
}
\description{
Calculate Actual Evapotranspiration for Waterbodies or Pervious Areas
}
\keyword{internal}
