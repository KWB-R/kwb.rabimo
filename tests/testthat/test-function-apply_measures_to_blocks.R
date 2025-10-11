#library(testthat)
apply_measures_to_blocks <- kwb.rabimo:::apply_measures_to_blocks

test_that("apply_measures_to_blocks() sets green_roof (alone) correctly", {
  
  # Make all roofs green roofs
  blocks <- data.frame(total_area = 100, roof = 0.5, green_roof = 0)
  measures <- list(green_roof = 0.5, unpaved = NA, to_swale = NA)
  expected <- blocks
  expected$green_roof <- 1
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)

  # Remove all green roofs
  blocks <- data.frame(total_area = 100, roof = 0.5, green_roof = 0.4)
  measures <- list(green_roof = 0, unpaved = NA, to_swale = NA)
  expected <- blocks
  expected$green_roof <- 0
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)

})

test_that("apply_measures_to_blocks() sets pvd (alone) correctly", {
  
  # Remove all pavement
  
  # roof = 0 -> max. unpaved = 1
  blocks <- data.frame(total_area = 100, roof = 0, pvd = seq(0, 1, 0.1))
  measures <- list(green_roof = NA, unpaved = 1, to_swale = NA)
  expected <- blocks
  expected$pvd <- 0
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)

  # roof = 0.2 -> max. unpaved = 0.8
  blocks <- data.frame(total_area = 100, roof = 0.2, pvd = seq(0, 0.8, 0.1))
  measures <- list(green_roof = NA, unpaved = 0.8, to_swale = NA)
  expected <- blocks
  expected$pvd <- 0
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)
  
  # Pave everything

  # roof = 0 -> max. paved = 1
  blocks <- data.frame(total_area = 100, roof = 0, pvd = seq(0, 1, 0.1))
  measures <- list(green_roof = NA, unpaved = 0, to_swale = NA)
  expected <- blocks
  expected$pvd <- 1
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)

  # roof = 0.5 -> max. paved = 0.5
  blocks <- data.frame(total_area = 100, roof = 0.5, pvd = seq(0, 0.5, 0.1))
  measures <- list(green_roof = NA, unpaved = 0, to_swale = NA)
  expected <- blocks
  expected$pvd <- 0.5
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)
  
})

test_that("apply_measures_to_blocks() sets to_swale (alone) correctly", {
  
  # Connect everything to swales
  
  blocks <- rbind(
    data.frame(total_area = 100, roof = 0.1, pvd = 0.4, to_swale = 0),
    data.frame(total_area = 100, roof = 0.2, pvd = 0.3, to_swale = 0),
    data.frame(total_area = 100, roof = 0.3, pvd = 0.2, to_swale = 0)
  )
  # max. to_swale = roof + pvd = 0.5
  measures <- list(green_roof = NA, unpaved = NA, to_swale = 0.5)
  expected <- blocks
  expected$to_swale <- 1
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)

  # Disconnect everything from swales
  
  blocks <- rbind(
    data.frame(total_area = 100, roof = 0.1, pvd = 0.4, to_swale = 0.0),
    data.frame(total_area = 100, roof = 0.2, pvd = 0.3, to_swale = 0.2),
    data.frame(total_area = 100, roof = 0.3, pvd = 0.2, to_swale = 0.4)
  )
  measures <- list(green_roof = NA, unpaved = NA, to_swale = 0)
  expected <- blocks
  expected$to_swale <- 0
  expect_equal(apply_measures_to_blocks(blocks, measures), expected)
  
})
