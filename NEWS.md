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
