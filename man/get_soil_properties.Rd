% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_soil_properties.R
\name{get_soil_properties}
\alias{get_soil_properties}
\title{Calculate Soil Properties}
\usage{
get_soil_properties(
  land_type,
  veg_class,
  depth_to_water_table,
  field_capacity_30,
  field_capacity_150,
  dbg = FALSE
)
}
\arguments{
\item{land_type}{land_type string, one of "vegetationless", "waterbody",
"horticultural", "urban", "forested"}

\item{veg_class}{vegetation class}

\item{depth_to_water_table}{depth to water table}

\item{field_capacity_30}{field capacity in 30 cm depth}

\item{field_capacity_150}{field capacity in 150 cm depth}

\item{dbg}{logical indicating whether or not to show debug messages}
}
\description{
Provide variables that are relevant to calculate the actual evaporation for
unsealed areas
}
