# real_evapo_transpiration -----------------------------------------------------

#' Calculate Actual Evapotranspiration with Bagrov
#'
#' @param precipitation precipitation in mm
#' @param potential_evaporation potential evaporation in mm
#' @param bagrov_parameter Bagrov parameter (n-value)
#' @param x_ratio optional. Instead of \code{precipitation} and
#'   \code{potential_evaporation} the quotient of both may be passed to this
#'   function. The idea is to calculate the quotient out of the function and to
#'   reuse the quotient instead of recalculating it.
#' @param FUN_y_ratio function to be called to calculate the y_ratio(s) from the
#'   given x_ratio(s). Default: \code{kwb.rabimo:::y_ratio_3}
#' @param \dots further arguments passed to \code{FUN_y_ratio}
#' @return estimated actual evapotranspiration in mm
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#' @export
real_evapo_transpiration <- function(
    precipitation, # P or P + KR + BER
    potential_evaporation, # ETP
    bagrov_parameter, # n
    x_ratio = NULL,
    FUN_y_ratio = y_ratio_3,
    ...
)
{

  if(FALSE)
  {
    potential_evaporation = ep_year[i]
    x_ratio = available_water / ep_year[i]
    bagrov_parameter = bagrov_values
    FUN_y_ratio = y_ratio_3
  }

  # Calculate the x-factor of the Bagrov relation
  if (is.null(x_ratio)) {
    x_ratio <- precipitation / potential_evaporation
  }

  # Estimate the y-ratio (of real evaporation to potential evaporation)...

  # ... and calculate the real evapotransporation using the estimated y-ratio
  FUN_y_ratio(bagrov_parameter, x_ratio, ...) * potential_evaporation
  #FUN_y_ratio(bagrov_parameter, x_ratio, use_abimo_algorithm = TRUE) * potential_evaporation
}

# y_ratio ----------------------------------------------------------------------
y_ratio <- function(bagrov_parameter, x_ratio)
{
  df <- calculate_bagrov_table(n_values = bagrov_parameter)

  # print_if(TRUE, nrow(df), caption = "chatty's nrow(df)")

  abs_diffs <- abs(df$P_over_E_p - x_ratio)

  index <- which.min(abs_diffs)

  #cat("min(abs_diffs): ", abs_diffs[index], "\n")

  df$E_a_over_E_p[index]
}

# y_ratio_2 --------------------------------------------------------------------
y_ratio_2 <- function(bagrov_parameter, x_ratio)
{
  df <- calculate_bagrov_curve(
    effectivity = bagrov_parameter,
    P_over_Ep_max = x_ratio + 0.1,
    delta_Ea = 1
  )

  # print_if(TRUE, nrow(df))

  abs_diffs <- abs(df$P_over_Ep - x_ratio)

  index <- which.min(abs_diffs)

  #cat("min(abs_diffs): ", abs_diffs[index], "\n")

  df$Ea_over_Ep[index]
}

# y_ratio_3 --------------------------------------------------------------------

#' Lookup y_ratio for given x_ratio on a BAGROV curve
#'
#' @param bagrov_parameter (vector of) BAGROV parameter(s)
#' @param x_ratio (vector of) x-ratio(s) (between precipitation and potential
#'   evaporation) for which to look up the corresponding y-ratio(s) (between
#'  actual evaporation and potential evaporation) on the BAGROV curve(s)
#' @param min_size_for_parallel minimum number of BAGROV curves to start
#'   parallel processing
#' @param use_abimo_algorithm whether or not to use the original algorithm that
#'   is implemented in the C++ code (converted to R:
#'   \code{kwb.rabimo:::yratio_cpp}). Default: \code{FALSE}
#' @keywords internal
y_ratio_3 <- function(
    bagrov_parameter,
    x_ratio,
    min_size_for_parallel = 10L,
    use_abimo_algorithm = FALSE
)
{
  if (length(bagrov_parameter) == 1L) {
    stopifnot(length(x_ratio) == 1L)
    return(
      if (use_abimo_algorithm) {
        y_ratio_2(bagrov_parameter, x_ratio)
      } else {
        yratio_cpp(bagrov_parameter, x_ratio)
      }
    )
  }

  # Split combinations of bagrov_parameter and x_ratio into blocks with
  # the same bagrov_parameter
  combis <- data.frame(
    bagrov_parameter = bagrov_parameter,
    x_ratio = x_ratio,
    index = seq_along(bagrov_parameter)
  )

  # TODO: Can we use kwb.utils::callWithData() here?
  combisets <- split(combis, combis$bagrov_parameter)

  if (use_abimo_algorithm) {

    result <- combisets %>%
      lapply(function(df) {
        df[["y_ratio"]] <- yratio_cpp(
          bag = df[["bagrov_parameter"]][1L],
          x = df[["x_ratio"]]
        )
        df
      }) %>%
      do.call(what = rbind)

  } else {

    P_over_Ep_max <- max(x_ratio) + 0.1

    # Define function to be called for each combiset
    getBagrovCurve <- function(combiset) {
      bagrov <- combiset$bagrov_parameter[1L]
      cat_and_run(
        paste("Calculating BAGROV curve for BAGROV parameter =", bagrov),
        calculate_bagrov_curve(
          effectivity = bagrov,
          P_over_Ep_max = P_over_Ep_max,
          delta_Ea = 1
        )
      )
    }

    # Calculate the BAGROV curves for the different bagrov_parameter values

    # Should we do parallel processing?
    doParallel <- length(combisets) >= min_size_for_parallel &&
      (ncores <- parallel::detectCores() - 1L) > 1L

    curves <- if (doParallel) {

      # Prepare parallel processing
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))

      cat_and_run(
        sprintf(
          "Calculating %d BAGROV curves in parallel on %d cores",
          length(combisets),
          ncores
        ),
        expr = {
          parallel::parLapply(cl, combisets, fun = getBagrovCurve)
        }
      )

    } else {

      cat_and_run(
        sprintf("Calculating %d BAGROV curves", length(combisets)),
        newLine = 3L,
        expr = {
          # Calculate the Bagrov curves for the different bagrov_parameter values
          lapply(combisets, FUN = getBagrovCurve)
        }
      )
    }

    result <- do.call(rbind, mapply(
      FUN = function(curve, combiset) {
        cat_and_run(
          sprintf(
            "Reading %d y_ratio values from the BAGROV curve (n = %f)",
            nrow(combiset),
            combiset$bagrov_parameter[1L]
          ),
          expr = {
            n <- nrow(curve)
            M <- matrix(rep(combiset$x_ratio, each = n), nrow = n)
            indices <- apply(abs(curve$P_over_Ep - M), 2L, which.min)
            cbind(
              index = combiset$index,
              y_ratio = curve$Ea_over_Ep[indices]
            )
          }
        )
      },
      curves,
      combisets,
      SIMPLIFY = FALSE
    ))
  }

  # initialise result vector
  yratios <- numeric(length(bagrov_parameter))

  # set the y_ratio values at the indices of the related x_ratio input values
  yratios[result[, "index"]] <- result[, "y_ratio"]

  yratios
}

# calculate_bagrov_table -------------------------------------------------------
calculate_bagrov_table <- function(n_values) {

  # test
  # n_values <- bagrov_parameter

  # Initialize values
  E_a <- 0
  P <- 0
  E_p <- 650  # mm/a
  P_limit <- 4 * E_p  # Si arriva fino a 4 * E_p
  Delta_E_a <- 1
  tolerance <- 0.01

  # empty dataframe for results
  results <- data.frame(
    E_a_over_E_p = numeric(),
    P_over_E_p = numeric(),
    n_value = numeric()
  )

  # loop over all given n_values
  for (n in n_values) {

    #test
    #n <- n_values

    # reset E_a and P for every new n_value
    E_a <- 0
    P <- 0

    # loop until the difference between P and P_limit is either lower than the
    # tolerance or E_a/E_p > 1
    while(abs(P - P_limit) > tolerance & E_a / E_p <= 1) {

      Delta_P <- Delta_E_a / (1 - (E_a / E_p)^n)
      P <- P + Delta_P
      E_a <- E_a + Delta_E_a

      # normalize results and add to result dataframe
      results <- rbind(results, data.frame(
        E_a_over_E_p = E_a / E_p,
        P_over_E_p = P / E_p,
        n_value = n
      ))
    }
  }

  results
}
