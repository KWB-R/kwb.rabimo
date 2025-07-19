[![R-CMD-check](https://github.com/KWB-R/kwb.rabimo/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.rabimo/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.rabimo/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.rabimo/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.rabimo/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.rabimo)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.rabimo)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.rabimo)](https://kwb-r.r-universe.dev/)

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
