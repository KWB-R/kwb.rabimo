# Deviation from Natural Water Balance (Delta-W)

Calculate the deviation from the natural water balance (delta-W) given
R-Abimo results as returned by
[`run_rabimo`](https://kwb-r.github.io/kwb.rabimo/dev/reference/run_rabimo.md).

## Usage

``` r
calculate_delta_w(
  natural,
  urban,
  columns_water_balance = c("runoff", "infiltr", "evapor"),
  column_code = "code",
  digits = 1L
)
```

## Arguments

- natural:

  R-Abimo results for the "natural" scenario

- urban:

  R-Abimo results for the "urban" scenario

- columns_water_balance:

  names of columns in `natural` and `urban`, respectively, containing
  the water balance components runoff, infiltration, evaporation.
  Default: `c("runoff", "infiltr", "evapor")`

- column_code:

  name of column in `natural` and `urban`, respectively, containing the
  block area identifiers.

- digits:

  integer indicating the number of decimal places in the result

## Value

a data frame with the area codes in column `code` and the delta-W values
in column `delta_w`
