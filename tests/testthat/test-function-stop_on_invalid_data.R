#library(testthat)

test_that("stop_on_invalid_data() works", {
  
  f <- kwb.rabimo:::stop_on_invalid_data
  
  expect_error(f())
  
  data_base <- data.frame(
    code = "a",
    prec_yr = 400L,
    prec_s = 100L,
    epot_yr = 200L,
    epot_s = 100L
  )
  
  expect_error(f(data_base[1L, ]), "There are missing columns")
  
  data <- cbind(
    data_base, 
    total_area = 100,
    roof = 0.3,
    swg_roof = 1,
    pvd = 0.9,
    swg_pvd = 1,
    srf1_pvd = 0.5,
    srf2_pvd = 0.5,
    srf3_pvd = 0,
    srf4_pvd = 0,
    srf5_pvd = 0,
    gw_dist = 3,
    ufc30 = 1,
    ufc150 = 3,
    land_type = "abc",
    veg_class = 10,
    irrigation = 0L
  )
  
  expect_no_error(f(data))
  
  expect_output(expect_error(
    f(dplyr::mutate(data, srf1_pvd = 1)), 
    "is not 1 or 0"
  ))
  
  expect_no_error(f(data, measures = list()))
  expect_error(regexp = "No such element.*'input_column'", f(
    data, measures = list(green_roof = list(list(a = 1)))
  ))
  expect_error(regexp = "No such column.*'a'", f(
    data, measures = list(green_roof = list(list(input_column = "a")))
  ))
  expect_no_error(f(
    cbind(data, green_roof_int = 0),
    measures = list(green_roof = list(list(input_column = "green_roof_int")))
  ))
  expect_output(expect_error(regexp = "sum of columns.*is not less than or equal to 1", f(
    cbind(data, green_roof_int = 1.1),
    measures = list(green_roof = list(list(input_column = "green_roof_int")))
  )))
  expect_error(regexp = "No such column.*'green_roof_ext", f(
    cbind(data, green_roof_int = 0.1),
    measures = list(green_roof = list(
      list(input_column = "green_roof_int"),
      list(input_column = "green_roof_ext")
    ))
  ))
  expect_no_error(
    f(
      cbind(
        data, 
        green_roof_int = 0.1, 
        green_roof_ext = 0.9
      ),
      measures = list(green_roof = list(
        list(input_column = "green_roof_int"),
        list(input_column = "green_roof_ext")
      ))
    )
  )
  expect_no_error(
    f(
      cbind(
        data, 
        infiltration_1 = 0.1, 
        infiltration_2 = 0.9
      ),
      measures = list(infiltration = list(
        list(input_column = "infiltration_1"),
        list(input_column = "infiltration_2")
      ))
    )
  )
  expect_error(regexp = "No such column.*'retention_1'", f(
    cbind(
      data, 
      infiltration_1 = 0.1, 
      infiltration_2 = 0.9
    ),
    measures = list(
      infiltration = list(
        list(input_column = "infiltration_1"),
        list(input_column = "infiltration_2")
      ),
      retention = list(
        list(input_column = "retention_1")
      )
    )
  ))
  expect_no_error(f(
    cbind(
      data, 
      infiltration_1 = 0.1, 
      infiltration_2 = 0.9,
      retention_1 = 0
    ),
    measures = list(
      infiltration = list(
        list(input_column = "infiltration_1"),
        list(input_column = "infiltration_2")
      ),
      retention = list(
        list(input_column = "retention_1")
      )
    )
  ))

  expect_output(expect_error(f(
    cbind(
      data, 
      infiltration_1 = 0.1, 
      infiltration_2 = 0.9,
      retention_1 = 0.1
    ),
    measures = list(
      infiltration = list(
        list(input_column = "infiltration_1"),
        list(input_column = "infiltration_2")
      ),
      retention = list(
        list(input_column = "retention_1")
      )
    )
  ), regexp = "The sum of columns.*is not less than or equal to 1"))
  
})
