# Get Mean/Max Statistics on Measures

Get Mean/Max Statistics on Measures

## Usage

``` r
get_measure_stats(blocks, reference_system = 2)
```

## Arguments

- blocks:

  data frame similar to
  [`rabimo_inputs_2020`](https://kwb-r.github.io/kwb.rabimo/reference/rabimo_inputs_2020.md)`$data`,
  with each row representing a block area

- reference_system:

  indicator for the "reference system" in which the returned values are
  to be given. 1: all values refer to percentages of specific areas
  (green roof: roof area, unsealed: total area, to_swale: sealed area);
  2: all values refer to percentages of the total area. The default is
  2.

## Value

list with elements "mean" and "max" each of which is a list with one
element per measure
