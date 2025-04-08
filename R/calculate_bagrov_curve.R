# This is a copy of kwb.abimo::calculate_bagrov_curve
calculate_bagrov_curve <- function (
    effectivity, P_over_Ep_max = 4, Ep = 650, delta_Ea = 0.1, dbg = FALSE
)
{
  stopifnot(is.numeric(effectivity))
  if (length(effectivity) > 1L) {
    return(do.call(rbind, lapply(
      X = effectivity,
      FUN = calculate_bagrov_curve,
      P_over_Ep_max = P_over_Ep_max,
      Ep = Ep,
      delta_Ea = delta_Ea,
      dbg = dbg
    )))
  }
  stopifnot(length(effectivity) == 1L)
  kwb.utils::catAndRun(
    paste("Calculating Bagrov curve for effectivity =", effectivity),
    dbg = dbg,
    expr = {
      Ea <- 0
      P <- 0
      Ea_over_Ep <- 0
      results <- list()
      while (P <= P_over_Ep_max * Ep && Ea_over_Ep <= 1) {
        delta_P <- delta_Ea/(1 - Ea_over_Ep^effectivity)
        P <- P + delta_P
        Ea <- Ea + delta_Ea
        P_over_Ep <- P/Ep
        Ea_over_Ep <- Ea/Ep
        results[[length(results) + 1L]] <- data.frame(
          P_over_Ep = P_over_Ep,
          Ea_over_Ep = Ea_over_Ep,
          effectivity = effectivity
        )
      }
      do.call(rbind, results)
    }
  )
}
