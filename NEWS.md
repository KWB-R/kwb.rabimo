# kwb.rabimo 2.1.0 (2025-07-21)

Contains a tutorial in the form of a vignette. To view the vignette, install
the package with

`remotes::install_github("kwb-r/kwb.rabimo@dev", build_vignettes = TRUE)`

and open the vignette with

`vignette("tutorial", package = "kwb.rabimo")`

You can read the tutorial also on our GitHub page: 
https://kwb-r.github.io/kwb.rabimo/articles/tutorial.html

# kwb.rabimo 2.0.0 (2025-05-20)

- contains new data for Berlin in which road areas do not belong to blocks 
  anymore but are blocks themselves
- change in input format
  - new (English) column names,
  - partial areas are given as fractions instead of percentages or absolute 
    areas
  - columns with redundant information are removed
  - potential evaporation is now part of the input data (was originally 
    configured in the configuration file)
  - model-internal parameters "land_type", "yield" (renamed to: "veg_class"), 
    and "irrigation", originally determined from Berlin-specific block area 
    usage and structure, are now explicitly given in the input data
- supports stormwater management measures

# kwb.rabimo 1.0.0 (2023-11-09)

First release of kwb.rabimo. This version tries to simulate exactly what
Abimo 3.3.0 (https://github.com/KWB-R/abimo/releases/tag/v3.3.0) does.
When being applied to the Berlin data (kwb.abimo::abimo_input_2019) there
are remaining maximum differences between intermediate variables calculated
by Abimo and kwb.rabimo, respectively, between about -6% and + 2%. They are
most probably the result of differences in the precision of fractional numbers
and rounding behaviour in C++ and R, respectively.

# kwb.rabimo 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`
