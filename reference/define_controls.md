# Define List of "Controls"

Define a list of settings that control how the main function
[`run_rabimo`](https://kwb-r.github.io/kwb.rabimo/reference/run_rabimo.md)
should behave.

## Usage

``` r
define_controls(
  check = TRUE,
  use_abimo_bagrov_solver = TRUE,
  reproduce_abimo_error = FALSE,
  output_format = "rabimo",
  intermediates = FALSE
)
```

## Arguments

- check:

  logical indicating whether the check functions are executed. Default:
  `TRUE`.

- use_abimo_bagrov_solver:

  logical indicating whether or not to use the (fast!) algorithm
  implemented in Abimo to solve the Bagrov equations. Default: `TRUE`.

- reproduce_abimo_error:

  logical indicating whether or not to reproduce the error that is
  contained in Abimo (missing area fraction factor). Default: `FALSE`.

- output_format:

  one of "abimo" (upper case columns: CODE, R, ROW, RI, RVOL, ROWVOL,
  RIVOL, FLAECHE, VERDUNSTUN), "rabimo" (lower case columns: code,
  surface_runoff, infiltration, evaporation).

- intermediates:

  logical indicating whether the intermediate results are returned as
  attributes. Default: `FALSE`.

## Value

list with the arguments of this function as list elements

## Examples

``` r
inputs <- kwb.rabimo::rabimo_inputs_2020
test_data <- inputs$data[sample(seq_len(nrow(inputs$data)), size = 1000L), ]
controls_default <- define_controls()
controls_no_check <- define_controls(check = FALSE)
controls_no_solver <- define_controls(use_abimo_bagrov_solver = FALSE)
system.time(result_default <- kwb.rabimo::run_rabimo(
  test_data, inputs$config, controls_default
))
#> You are using an old configuration. No problem, I convert it.
#>    user  system elapsed 
#>    0.20    0.00    0.21 
system.time(result_no_check <- kwb.rabimo::run_rabimo(
  test_data, inputs$config, controls_no_check
))
#> You are using an old configuration. No problem, I convert it.
#>    user  system elapsed 
#>    0.19    0.00    0.19 
identical(result_default, result_no_check)
#> [1] TRUE
if (FALSE) { # \dontrun{
system.time(result_no_solver <- kwb.rabimo::run_rabimo(
  test_data, inputs$config, controls_no_solver
))
} # }
```
