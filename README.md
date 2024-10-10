[![R-CMD-check](https://github.com/KWB-R/kwb.rabimo/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.rabimo/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.rabimo/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.rabimo/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.rabimo/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.rabimo)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.rabimo)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.rabimo)](https://kwb-r.r-universe.dev/)

# kwb.rabimo

The code in this package has been transferred from the C++
code of ABIMO 3.3: Water Balance Model for Urban Areas
(https://github.com/KWB-R/abimo/).

## Installation

```r
# Install package "remotes" from CRAN
install.packages("remotes", repos = "https://cloud.r-project.org")

# Install package "kwb.rabimo" (latest "release") from GitHub
remotes::install_github("KWB-R/kwb.rabimo")

# Install package "kwb.rabimo" (development version) from GitHub
remotes::install_github("KWB-R/kwb.rabimo@dev")

# Install package "kwb.abimo" (wrapper around C++ version of Abimo) from GitHub
remotes::install_github("KWB-R/kwb.abimo@dev")
```

## Basic Usage

### Provide input data and configuration

Compared to the original C++ version of Abimo we have modified the structures
of input data, output data and configuration.

You may still use data that has been prepared for the usage with Abimo as
described below.

```r
# Load Berlin data in the original Abimo format
old_data <- kwb.abimo::abimo_input_2019

# Convert the original Abimo inputs (data frame in old format, path to XML 
# file containing configuration details) to corresponding inputs that are 
# required by R-Abimo. The new inputs are intended to be more general than the 
# original, Berlin-specific, inputs. Feel free to provide your own data in this
# new format!
new_inputs <- kwb.rabimo::prepare_berlin_inputs(
  data = old_data,
  config_file = kwb.abimo::default_config()
)
```

### Run R-Abimo for the status quo

```r
# Run R-Abimo, the R-implementation of Abimo
rabimo_result <- kwb.rabimo::run_rabimo(
  data = new_inputs$data, 
  config = new_inputs$config
)

# Have a look at the first lines of the result data frame
head(rabimo_result)
```

### Run R-Abimo for a natural state scenario

```r
rabimo_result_natural <- kwb.rabimo::run_rabimo(
  data = kwb.rabimo::data_to_natural(new_inputs$data), 
  config = new_inputs$config
)
```

### Calculate "Delta-W"

For the first ten blocks, calculate the deviation from the natural state:

```r
kwb.rabimo::calculate_delta_w(
  urban = rabimo_result[1:10, ],
  natural = rabimo_result_natural
)
```

## Compare with what Abimo (C++ version) returns

```r
# Use the R-wrapper to run Abimo.exe (with a default configuration)
abimo_result <- kwb.abimo::run_abimo(
  input_data = old_data, 
  config = kwb.abimo::read_config()
)

# Have a look at the first lines of the result data frames
head(abimo_result)
head(rabimo_result)

# Rename the columns of the Abimo result to match the column names of the 
# R-Abimo result
abimo_result_renamed <- kwb.utils::renameColumns(abimo_result, list(
  FLAECHE = "area",
  ROW = "surface_runoff",
  RI = "infiltration",
  VERDUNSTUN = "evaporation"
))

# Plot the differences between Abimo and R-Abimo, per variable
columns <- setdiff(names(rabimo_result), "code")
for (column in columns) {
  x <- abimo_result_renamed[[column]]
  y <- rabimo_result[[column]]
  plot(x, y, xlab = "Abimo", ylab = "Rabimo", main = column, asp = 1)
}
```

## Documentation

Release: [https://kwb-r.github.io/kwb.rabimo](https://kwb-r.github.io/kwb.rabimo)

Development: [https://kwb-r.github.io/kwb.rabimo/dev](https://kwb-r.github.io/kwb.rabimo/dev)
