# Distribute Rainwater Management Measures to Single Block Areas

Distribute Rainwater Management Measures to Single Block Areas

## Usage

``` r
distribute_measures(blocks, targets, intermediates = FALSE)
```

## Arguments

- blocks:

  data frame as being input to
  [`run_rabimo`](https://kwb-r.github.io/kwb.rabimo/dev/reference/run_rabimo.md)

- targets:

  numeric vector with elements named `green_roof`, `unpaved`,
  `to_swale`, each of which is a value between 0 and 1, describing the
  extent to which the corresponding measures are to be installed on
  average over all `blocks`. The percentages refer to different base
  areas: green_roof = total area of green roofs divided by total area of
  roofs; unpaved = sum of unpaved (non-roof) area divided by total area;
  to_swale = sum of areas that are connected to a swale divided by sum
  of of areas that are sealed (roof area + paved area).

- intermediates:

  logical indicating whether or not to return tables with intermediate
  results in attributes. The default is `FALSE`, i.e. no attributes are
  attached to the result data frame.

## Value

data frame with the columns describing the measurements being updated.
In case of `intermediates = TRUE`, the data frame has attributes
`green_roof_table`, `unpaved_area_table`, `swale_connection_table`,
carrying intermediate results.
