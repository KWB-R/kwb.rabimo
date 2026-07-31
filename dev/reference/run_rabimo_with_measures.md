# Distribute Rainwater Management Measures and run R-Abimo

Distribute Rainwater Management Measures and run R-Abimo

## Usage

``` r
run_rabimo_with_measures(
  blocks,
  measures,
  config = kwb.rabimo::rabimo_inputs_2020$config,
  old_version = FALSE,
  ...
)
```

## Arguments

- blocks:

  data frame of selected blocks (same columns as in
  [`rabimo_inputs_2020`](https://kwb-r.github.io/kwb.rabimo/dev/reference/rabimo_inputs_2020.md)`$data`)

- measures:

  list with elements `green_roof`, `unpaved`, `to_swale` representing
  the target percentages of the total areas corresponding to each
  measure

- config:

  configuration object, default:
  [`rabimo_inputs_2020`](https://kwb-r.github.io/kwb.rabimo/dev/reference/rabimo_inputs_2020.md)`$config`

- old_version:

  if `TRUE` the old, erroneous version of this function is used (not
  correctly considering the updated pvd value before calculating the new
  to_swale values). The default is `FALSE`.

- ...:

  further arguments passed to
  [`run_rabimo`](https://kwb-r.github.io/kwb.rabimo/dev/reference/run_rabimo.md),
  such as `silent = TRUE`
