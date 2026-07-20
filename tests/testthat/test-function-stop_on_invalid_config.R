#library(testthat)
test_that("stop_on_invalid_config() works", {
  
  f <- kwb.rabimo:::stop_on_invalid_config
  
  expect_error(f())
  expect_error(f(list()))
  
  base_config <- list(
    bagrov_values = c(
      roof = 1, 
      surface1 = 1, 
      surface2 = 1, 
      surface3 = 1, 
      surface4 = 1,
      surface5 = 1
    ),
    runoff_factors = c(
      roof = 1,
      surface1 = 1, 
      surface2 = 1, 
      surface3 = 1, 
      surface4 = 1,
      surface5 = 1
    )
  )
  
  expect_error(
    f(c(base_config, list(
      measures = list(
        green_roof = list(
          list(input_column = "column-1"),
          list(input_column = "column-1")
        ),
        infiltration = list()
      )
    ))), 
    "input_column.*are not unique as expected"
  )
  
  expect_error(
    f(c(base_config, list(
      measures = list(
        green_roof = list(
          list(input_column = "column-1"),
          list(input_column = "column-2")
        ),
        infiltration = list(
          list(input_column = "column-1"),
          list(input_column = "column-1")
        )
      )
    ))), 
    "input_column.*are not unique as expected"
  )
  
})
