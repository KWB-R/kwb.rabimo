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

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.rabimo' from GitHub
remotes::install_github("KWB-R/kwb.rabimo")
```

## Basic usage

```r
# Load Berlin data from the R-wrapper package kwb.abimo
data <- kwb.abimo::abimo_input_2019

# Provide Abimo's default configuration 
abimo_config <- kwb.abimo:::read_config()

# Use the R-wrapper to run Abimo.exe
abimo_result <- kwb.abimo::run_abimo(input_data = data, config = abimo_config)

# Prepare a configuration for R-Abimo, based on the default Abimo configuration
config <- kwb.rabimo::abimo_config_to_config(abimo_config)

# Run R-Abimo, the R-implementation of Abimo in this package
rabimo_result <- kwb.rabimo::run_rabimo(data, config)

# Have a look at the first lines of the result data frames
head(abimo_result)
head(rabimo_result)

# Plot the differences between Abimo and R-Abimo, per variable
for (name in names(abimo_result)[-1L]) {
  x <- abimo_result[[name]]
  y <- rabimo_result[[name]]
  plot(x, y, xlab = "Abimo", ylab = "Rabimo", main = name, asp = 1)
}
```

## Usage with data being independent from the Berlin case

```r
# Load Berlin data from the R-wrapper package kwb.abimo
data <- kwb.abimo::abimo_input_2019

# Convert the data format used for Berlin into a more general format
# Feel free to provide your own data in this new format!
generalised_data <- kwb.rabimo::prepare_input_data(data)

# Prepare a configuration for R-Abimo, based on the default Abimo configuration
config <- kwb.rabimo::abimo_config_to_config(kwb.abimo:::read_config())

# Run R-Abimo, the R-implementation of Abimo in this package
rabimo_result <- kwb.rabimo::run_rabimo(generalised_data, config)

# Have a look at the first lines of the result data frame
head(rabimo_result)
```

## Documentation

Release: [https://kwb-r.github.io/kwb.rabimo](https://kwb-r.github.io/kwb.rabimo)

Development: [https://kwb-r.github.io/kwb.rabimo/dev](https://kwb-r.github.io/kwb.rabimo/dev)
