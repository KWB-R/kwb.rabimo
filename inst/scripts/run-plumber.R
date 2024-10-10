#install.packages("remotes")
#remotes::install_github("kwb-r/kwb.rabimo@dev")

plumber_file <- system.file(
  "scripts/plumber.R",
  package = "kwb.rabimo",
  mustWork = TRUE
)

#plumber_file <- "./inst/scripts/plumber.R"

pr <- plumber::pr(plumber_file)
plumber::pr_run(pr)
