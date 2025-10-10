#library(testthat)
test_that("apply_measures_to_blocks() works", {
  
  apply_measures_to_blocks <- kwb.rabimo:::apply_measures_to_blocks
  
  test_me <- function(data) {
    
    blocks <- data[sample(seq_len(nrow(data)), 10L), ]
    stats <- kwb.rabimo:::get_measure_stats(blocks)
    safety_factor <- 0.999
    
    measures_max <- list(
      green_roof = safety_factor * stats$green_roof$max,
      unpaved_max = safety_factor * stats$unpaved$max,
      to_swale = NA
    )
    
    measures_too_big_green_roof <- list(
      green_roof = measures_max$green_roof + 0.01,
      unpaved = measures_max$unpaved,
      to_swale = NA
    )
    
    measures_too_big_unpaved <- list(
      green_roof = measures_max$green_roof,
      unpaved = measures_max$unpaved + 0.01,
      to_swale = NA
    )
    
    expect_silent(b1 <- apply_measures_to_blocks(
      blocks, 
      measures = measures_max
    ))
    
    expect_warning(b2 <- apply_measures_to_blocks(
      blocks, 
      measures = measures_too_big_green_roof
    ))
    
    expect_warning(b3 <- apply_measures_to_blocks(
      blocks, 
      measures = measures_too_big_unpaved
    ))
  }
  
  test_me(data = kwb.rabimo::rabimo_inputs_2020$data)
  test_me(data = kwb.rabimo::rabimo_inputs_2025$data)
  
})

if (FALSE) {
  
  blocks <- rbind(
    data.frame(total_area = 100, roof = 0.1, green_roof = 0, pvd = 0.7, to_swale = 1)
    , data.frame(total_area = 200, roof = 0.2, green_roof = 0.8, pvd = 0.5, to_swale = 1)
    , data.frame(total_area =  50, roof = 0.9, green_roof = 0.2, pvd = 0.1, to_swale = 1)
  )
  
  stats <- kwb.rabimo::get_measure_stats(blocks)
  str(stats)
  
  n <- 10L
  
  m <- as.data.frame(matrix(
    runif(3*n), 
    ncol = 3L, 
    dimnames = list(NULL, c("green_roof", "unpaved", "to_swale"))
  ))
  
  combinations <- split(m, seq_len(n))
  
  result <- lapply(combinations, function(measures) {
    apply_measures_to_blocks(blocks, measures, dbg = TRUE, check = TRUE)
  })
  
  measures <- list(
    green_roof = 0, 
    unpaved = 0,
    to_swale = 0.1
  )
  
  new_blocks <- apply_measures_to_blocks(blocks, measures)
  
  new_stats <- kwb.rabimo::get_measure_stats(new_blocks)
  new_stats$green_roof$mean
  new_stats$unpaved$mean
  new_stats$to_swale$mean
}
