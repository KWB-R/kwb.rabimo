#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-02-16 08:26:28.470892.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("get_mean_potential_capillary_rise_rate() works", {

  f <- kwb.rabimo:::get_mean_potential_capillary_rise_rate

  expect_error(
    f()
    # Argument "potential_capillary_rise" fehlt (ohne Standardwert)
  )

})
