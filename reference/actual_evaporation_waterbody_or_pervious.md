# Calculate Actual Evapotranspiration for Waterbodies or Pervious Areas

Calculate Actual Evapotranspiration for Waterbodies or Pervious Areas

## Usage

``` r
actual_evaporation_waterbody_or_pervious(
  usage_tuple,
  climate,
  soil_properties,
  dbg = TRUE,
  ...,
  digits = NULL
)
```

## Arguments

- usage_tuple:

  list with elements `land_type`, `veg_class`, `irrigation`

- climate:

  list with elements `epot.year`, `epot.summer` (potential evaporation
  in mm per year and in the summer period, respecively), `prec.year`,
  `prec.summer` (precipitation in mm per year and in the summer period,
  respectively).

- soil_properties:

  list as returned by
  [`get_soil_properties`](https://kwb-r.github.io/kwb.rabimo/reference/get_soil_properties.md),
  with elements `mean_potential_capillary_rise_rate`, `g02`,
  `potential_capillary_rise`, `depth_to_water_table`

- dbg:

  logical indicating whether or not to show debug messages

- ...:

  further arguments passed to
  [`real_evapo_transpiration`](https://kwb-r.github.io/kwb.rabimo/reference/real_evapo_transpiration.md)
  such as `run_parallel`, `blocksize`

- digits:

  optional. If given, the BAGROV parameter values are rounded to this
  number of digits. This reduces the number of BAGROV curves that need
  to be calculated and thus improves the performance (by reducing the
  precision of the output)
