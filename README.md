[![R-CMD-check](https://github.com/KWB-R/kwb.rabimo/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.rabimo/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.rabimo/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.rabimo/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.rabimo/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.rabimo)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.rabimo)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.rabimo)](https://kwb-r.r-universe.dev/)

# kwb.rabimo

R-implementation of a simple water balance model for urban areas,

- based on "Wasserhaushaltsmodell Berlin ABIMO 3.2" (see Documentation below) and
- further developed by [KWB](https://kompetenz-wasser.de) 
  within [BMBF](https://www.bmbf.de/EN/Home/home_node.html)-funded 
  research project [AMAREX](https://amarex-projekt.de/en).

For our Tutorial, click [here](https://kwb-r.github.io/kwb.rabimo/dev/articles/tutorial.html) (see also Documentation below).

## Installation

```r
# Install package "remotes" from CRAN
install.packages("remotes", repos = "https://cloud.r-project.org")

# Install package "kwb.rabimo" (latest "release") from GitHub
remotes::install_github("KWB-R/kwb.rabimo", build_vignettes = TRUE)

# Install package "kwb.rabimo" (development version) from GitHub
remotes::install_github("KWB-R/kwb.rabimo@dev", build_vignettes = TRUE)
```

## Basic Usage

### Provide input data and configuration

Compared to the original C++ version of Abimo we have modified the structures
of input data, output data and configuration.

For the German city of Berlin, we provide data in the new structures in the 
package:

```r
# Load Berlin data in the original Abimo format
abimo_inputs <- kwb.rabimo::rabimo_inputs_2025
```

### Run R-Abimo for the status quo

```r
# Run R-Abimo, the R-implementation of Abimo
rabimo_result <- kwb.rabimo::run_rabimo(
  data = abimo_inputs$data, 
  config = abimo_inputs$config
)

# Have a look at the first lines of the result data frame
head(rabimo_result)
```

### Run R-Abimo for a natural state scenario

```r
rabimo_result_natural <- kwb.rabimo::run_rabimo(
  data = kwb.rabimo::data_to_natural(abimo_inputs$data), 
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

## Documentation

### R-package kwb.rabimo

- Release: https://kwb-r.github.io/kwb.rabimo
- Development: https://kwb-r.github.io/kwb.rabimo/dev
- Tutorial: https://kwb-r.github.io/kwb.rabimo/dev/articles/tutorial.html

### Original software "Wasserhaushaltsmodell Berlin ABIMO 3.2"

- Source code (C++): https://github.com/umweltatlas/abimo, 
- User manual (in German): https://www.berlin.de/umweltatlas/_assets/literatur/goedecke_et_al_abimo2019_doku.pdf
