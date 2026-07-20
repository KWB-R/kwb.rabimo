# Run R-Abimo, the R-implementation of Water Balance Model Abimo

Run R-Abimo, the R-implementation of Water Balance Model Abimo

## Usage

``` r
run_rabimo(data, config, controls = define_controls(), silent = TRUE)
```

## Arguments

- data:

  data frame similar to
  [`rabimo_inputs_2025`](https://kwb-r.github.io/kwb.rabimo/reference/rabimo_inputs_2025.md)`$data`

- config:

  configuration object (list) similar to
  [`rabimo_inputs_2025`](https://kwb-r.github.io/kwb.rabimo/reference/rabimo_inputs_2025.md)`$config`

- controls:

  list of settings that control how the function should behave. Use
  [`define_controls`](https://kwb-r.github.io/kwb.rabimo/reference/define_controls.md)
  to define such a list. The default is the list returned by
  [`define_controls()`](https://kwb-r.github.io/kwb.rabimo/reference/define_controls.md).

- silent:

  logical indicating whether to suppress console outputs, the default is
  `FALSE`

## Value

data frame with columns as returned by Abimo

## Examples

``` r
# Get input data and config for Berlin (version 2020)
inputs_2020 <- kwb.rabimo::rabimo_inputs_2020

# Randomly select 1000 blocks (to reduce runtime)
data <- inputs_2020$data
data <- data[sample(seq_len(nrow(data)), size = 1000L), ]

# Run R-Abimo
results_2020 <- kwb.rabimo::run_rabimo(data, inputs_2020$config)
#> You are using an old configuration. No problem, I convert it.

# Get input data and config for Berlin (version 2025)
inputs_2025 <- kwb.rabimo::rabimo_inputs_2025

# Crop a box (to reduce runtime)
data <- crop_box(inputs_2025$data)
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries

# Run R-Abimo
results_2025 <- kwb.rabimo::run_rabimo(data, inputs_2025$config)
#> You are using an old configuration. No problem, I convert it.
  
plot(results_2025[, -1L])
```
