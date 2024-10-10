#library(testthat)

test_that("data_to_natural() works", {

  f <- kwb.rabimo::data_to_natural

  expect_error(f())
  expect_error(f(data.frame(), "make sure .*all required columns"))

  data <- kwb.rabimo::rabimo_inputs_2020$data

  data_new <- data %>%
    kwb.rabimo:::check_or_convert_data_types(
      types = kwb.rabimo:::get_expected_data_type(),
      convert = TRUE,
      dbg = FALSE
    )

  result <- f(data_new)
})
