# Calculate Actual Evapotranspiration with Bagrov

Calculate Actual Evapotranspiration with Bagrov

## Usage

``` r
real_evapo_transpiration(
  precipitation,
  potential_evaporation,
  bagrov_parameter,
  x_ratio = NULL,
  FUN_y_ratio = y_ratio_3,
  ...
)
```

## Arguments

- precipitation:

  precipitation in mm

- potential_evaporation:

  potential evaporation in mm

- bagrov_parameter:

  Bagrov parameter (n-value)

- x_ratio:

  optional. Instead of `precipitation` and `potential_evaporation` the
  quotient of both may be passed to this function. The idea is to
  calculate the quotient out of the function and to reuse the quotient
  instead of recalculating it.

- FUN_y_ratio:

  function to be called to calculate the y_ratio(s) from the given
  x_ratio(s). Default: `kwb.rabimo:::y_ratio_3`

- ...:

  further arguments passed to `FUN_y_ratio`

## Value

estimated actual evapotranspiration in mm
