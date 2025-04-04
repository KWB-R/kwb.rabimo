## code to prepare datasets goes here

rabimo_inputs_2025 <- readRDS("../../A/abimo.scripts/assets/rabimo_inputs_2025.rds")
usethis::use_data(rabimo_inputs_2025, overwrite = TRUE, compress = "xz")
