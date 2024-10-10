# Set the name of your (!) new package
package <- "kwb.rabimo"

# Set the path to the package directory
pkg_dir <- getwd()

# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)

# kwb.orcid::get_kwb_orcids()
author <- list(name = "Hauke Sonnenberg", orcid = "0000-0001-9134-2871")

description <- list(
  name = package,
  title = "R Implementation of Water Balance Model Abimo",
  desc  = paste(
    "The code in this package has been transferred from the C++ code of",
    "ABIMO 3.3: Water Balance Model for Urban Areas",
    "(https://github.com/KWB-R/abimo/)."
  )
)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)
