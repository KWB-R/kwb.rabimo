# @param measures list with elements green_roof, unpaved, to_swale representing 
#   the target percentages of the total areas corresponding to each measure.
#   A value of NA means that the corresponding measure column is not touched.
apply_measures_to_blocks <- function(blocks, measures, dbg = FALSE, check = FALSE)
{
  #dbg = FALSE; check = FALSE
  
  # Define helper functions
  {
    report_problem <- function(...) {
      problem <- paste(unlist(list(...)), collapse = "")
      warning(problem, call. = FALSE)
    }
    
    debug <- function(...) {
      if (dbg) {
        writeLines(...)
      }
    }
    
    share_of_sum <- function(x) {
      # Do not divide by zero, return vector of zeros instead.
      if ((s <- sum(x)) == 0) {
        rep(0, length(x))
      } else {
        x/s  
      }
    }
    
    check_if_target_was_reached <- function(blocks, measure) {
      obtained <- kwb.rabimo::get_measure_stats(blocks)[[measure]]$mean
      target <- measures[[measure]]
      if (!isTRUE(all.equal(obtained, target))) {
        warning(
          sprintf("Target value %0.2f for '%s' ", target, measure),
          sprintf("could not be achieved. Actual value: %0.2f", obtained), 
          call. = FALSE
        )
      }
    }
    
    check_for_negative_values <- function(blocks, measure) {
      is_negative <- blocks[[measure]] < 0
      if (any(is_negative)) {
        warning(call. = FALSE, sprintf(
          "There are %d negative values in column '%s'", 
          sum(is_negative), measure
        ))
      }
    }
  }
  
  # The prefix "a_" refers to absolute area (in square metres)
  
  # Provide the total areas and roof areas in advance. They are not changed by 
  # the measures.
  a_total <- blocks$total_area
  a_roof <- blocks$total_area * blocks$roof
  a_total_sum <- sum(a_total)
  
  # 1. Handle measure "green roof"
  if (!is.na(measures$green_roof)) {
    
    a_green_roof <- a_roof * blocks$green_roof
    
    # Total green roof area to add (if value >= 0) or to remove (if value < 0)
    a_green_roof_change <- measures$green_roof * a_total_sum - sum(a_green_roof)
    
    # Roof area that can be converted to green roof area
    if (a_green_roof_change >= 0) {
      
      # increase green roof area
      a_potential <- a_roof - a_green_roof
      
      if (a_green_roof_change > sum(a_potential)) {
        report_problem(sprintf(
          "Not enough (non-green) roof area available (%0.2f m2 missing)",
          a_green_roof_change - sum(a_potential)
        ))
      }
      
    } else {
      
      # decrease green roof area
      a_potential <- a_green_roof
      
      if (- a_green_roof_change > sum(a_potential)) {
        report_problem(sprintf(
          "Not enough green roof area available (%0.2f m2 missing)",
          - a_green_roof_change - sum(a_potential)
        ))
      }    
    }
    
    # Distribute change in green roof area to the different blocks    
    a_green_roof_new <- a_green_roof + share_of_sum(a_potential) * a_green_roof_change
    
    # Update column "green_roof" (as fraction of roof area)
    blocks$green_roof <- ifelse(a_roof == 0, 0, a_green_roof_new / a_roof)
  }
  
  # 2. Handle measure "Unsealing"
  if (!is.na(measures$unpaved)) {
    
    # current paved/unpaved areas
    a_paved <- a_total * blocks$pvd
    a_unpaved <- a_total - a_roof - a_paved

    # Required increase/decrease in unpaved area
    a_unpaved_change <- measures$unpaved * a_total_sum - sum(a_unpaved)
    unpave <- a_unpaved_change >= 0
    
    debug(sprintf(
      "%s area to be %s: %0.2f m2", 
      ifelse(unpave, "Paved", "Unpaved"),
      ifelse(unpave, "unpaved", "paved"),
      abs(a_unpaved_change)
    ))
    
    if (unpave) {
      
      a_potential <- a_paved
      
      if (a_unpaved_change > sum(a_potential)) {
        report_problem(sprintf(
          "Not enough paved area available to be unpaved (%0.2f m2 missing)",
          a_unpaved_change - sum(a_potential)
        ))
      }
      
    } else {
      
      # actually pave instead of unpave!
      a_potential <- a_unpaved
      
      # a_unpaved_change is negative here
      if (- a_unpaved_change > sum(a_potential)) {
        report_problem(sprintf(
          "Not enough unpaved area available to be paved (%0.2f m2 missing)",
          - a_unpaved_change - sum(a_potential)
        ))
      }
    }
    
    # Distribute change in paved/unpaved area to the different blocks
    a_paved_new <- a_paved - share_of_sum(a_potential) * a_unpaved_change
    
    blocks$pvd <- ifelse(a_total == 0, 0, a_paved_new / a_total)
  }
  
  # 3. Handle measure "Connection to swales"
  if (!is.na(measures$to_swale)) {
    a_sealed <- a_roof + a_total * blocks$pvd
    a_to_swale <- blocks$to_swale * a_sealed
    a_to_swale_change <- measures$to_swale * a_total_sum - sum(a_to_swale)
    
    increase <- a_to_swale_change >= 0
    
    debug(sprintf(
      "Sealed area to be %s swales: %0.2f m2",
      ifelse(increase, "connected to", "disconnected from"),
      abs(a_to_swale_change)
    ))
    
    if (increase) {
      
      a_potential <- a_sealed - a_to_swale
      
      if (a_to_swale_change > sum(a_potential)) {
        report_problem(sprintf(
          "Not enough sealed area available to be connected to swales (%0.2f m2 missing)",
          a_to_swale_change - sum(a_potential)
        ))
      }
      
    } else {
      
      # a_to_swale_change is negative here
      a_potential <- a_to_swale
      
      if (- a_to_swale_change > sum(a_potential)) {
        report_problem(sprintf(
          "Not enough swale-connected sealed area available to be disconnected (%0.2f m2 missing)",
          abs(a_to_swale_change) - sum(a_potential)
        ))
      }
    }
    
    # distribute
    a_to_swale_new <- a_to_swale + share_of_sum(a_potential) * a_to_swale_change
    
    # Update column "to_swale"
    blocks$to_swale <- ifelse(a_sealed == 0, 0, a_to_swale_new / a_sealed)
  }
  
  # Targets reached?
  if (check) {
    for (measure in names(measures)[!is.na(measures)]) {
      check_if_target_was_reached(blocks, measure)  
      check_for_negative_values(blocks, measure)
    }
  }
  
  blocks
}
