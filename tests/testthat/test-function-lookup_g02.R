#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-02-16 08:26:28.470892.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("lookup_g02() works", {

  f <- kwb.rabimo:::lookup_g02

  expect_error(
    f()
    # Argument "usable_field_capacity" fehlt (ohne Standardwert)
  )

})
