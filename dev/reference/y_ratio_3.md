# Lookup y_ratio for given x_ratio on a BAGROV curve

Lookup y_ratio for given x_ratio on a BAGROV curve

## Usage

``` r
y_ratio_3(
  bagrov_parameter,
  x_ratio,
  min_size_for_parallel = 10L,
  use_abimo_algorithm = FALSE
)
```

## Arguments

- bagrov_parameter:

  (vector of) BAGROV parameter(s)

- x_ratio:

  (vector of) x-ratio(s) (between precipitation and potential
  evaporation) for which to look up the corresponding y-ratio(s)
  (between actual evaporation and potential evaporation) on the BAGROV
  curve(s)

- min_size_for_parallel:

  minimum number of BAGROV curves to start parallel processing

- use_abimo_algorithm:

  whether or not to use the original algorithm that is implemented in
  the C++ code (converted to R: `kwb.rabimo:::yratio_cpp`). Default:
  `FALSE`
