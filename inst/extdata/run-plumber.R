#install.packages("remotes")
#remotes::install_github("kwb-r/kwb.rabimo@dev")

plumber_file <- system.file(
  "extdata/plumber.R",
  package = "kwb.rabimo",
  mustWork = TRUE
)

writeLines(readLines(plumber_file))

plumber_router <- plumber::pr(plumber_file)
plumber::pr_run(plumber_router)
