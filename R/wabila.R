# calculate_wabila_roof --------------------------------------------------------
calculate_wabila_roof <- function(area, retention_height, return_code = TRUE) {
  # get climatic parameters from area
  P <- area["prec_yr"]
  ET_p <- area["epot_yr"]
  code <- area["code"]

  # runoff component
  a <- 0.9115 + 0.00007063 * P - 0.000007498 * ET_p - 0.2063 * log(retention_height + 1)

  # infiltration component
  g <- rep(0, length(P))  # if P is a vector, g will be a vector of zeros with the same length

  # evaporation component
  v <- 1 - a

  result <- setNames(c(a, g, v),
                     c("surface_runoff", "infiltration", "evaporation"))

  if (return_code){
    return(cbind(code, result))
  }

  result

}

# calculate_wabila_green_roof --------------------------------------------------
calculate_wabila_green_roof <- function(area, height, kf, w_diff, return_code = TRUE)
{
  # h: depth, in cm
  # kf: permeability of soil, in mm/h
  # w_diff: difference between water holding capacity and wilting point, unitless

  # get climatic parameters from area
  P <- area["prec_yr"]
  ET_p <- area["epot_yr"]
  code <- area["code"]

  # runoff component
  a = -2.182 + 0.4293 * log(P) - 0.0001092 * P +  236.1/ET_p +
    0.0001142 * height + 0.0002297 * kf + 0.01628 * log(w_diff) - 0.1214 *
    log((w_diff) * height)

  # infiltration component
  g <- 0

  #evaporation component
  v <- 1 - a

  result <- setNames(c(a, g, v),
                     c("surface_runoff", "infiltration", "evaporation"))

  if (return_code){
    return(cbind(code, result))
  }

  result
}

# calculate_wabila_paved -------------------------------------------------------
calculate_wabila_paved <- function(area, joint_perc, retention, kf, w_diff)
{
  # joint_perc: percentage of joints
  # retention: retention_heigth, in mm
  # kf: permeability of joints, in mm/h
  # w_diff: difference between water holding capacity and wilting point, unitless

  # get climatic parameters from area
  P <- area$prec_yr
  ET_p <- area$epot_yr

  if (2 <= joint_perc & 5.5 >= joint_perc)
  {
    a = 0.800734 * log(P) - 0.0582828 * joint_perc - 0.0501693 *
      retention - 0.385767 * (w_diff) + 8.7040284 / (11.9086896 + kf)
    g = -0.2006 - 0.000253 * ET_p + 0.05615 * joint_perc - 0.0636 *
      log(1 + retention) + 0.1596 * log(1 + kf) + 0.2778 * w_diff
    v = 0.8529 - 0.1248 * log(P) + 0.00005057 * ET_p + 0.002372 *
      joint_perc + 0.1583 * log(1 + retention)

  } else if (5.5 < joint_perc & 10 >= joint_perc) {

    a = 0.05912 * log(P) - 0.02749 * joint_perc - 0.03671 *
      retention - 0.30514 * (w_diff) + 4.97687 / (4.7975 + kf)
    g = 0.00004941 * P - 0.0002817 * ET_p + 0.02566 *
      joint_perc - 0.03823 * retention + 0.691 * exp(-6.465 / kf)
    v = 0.9012 - 0.1325 * log(P) + 0.00006661 * ET_p + 0.002302 * joint_perc +
      0.1489 * log(1 + retention)
  }

  c(a, g, v)
}

# calculate_wabila_swale -------------------------------------------------------
calculate_wabila_swale <- function(input = NULL, area, kf, BA = NULL)
{

  # kf: permeability, in mm/h
  # input: the discharge to the swale in mm
  # BA: percentage of swale area (what is it?)

  # get climatic parameters from area
  P <- area$prec_yr
  ET_p <- area$epot_yr

  if (is.null(input)){
    input <- P
  }

  if (is.null(BA)){
    #BA = 42.323 * kf^(-0.314)
    BA <- estimate_swale_area(kf)
  }

  g = 0.8608 + 0.02385 * log(input) - 0.00005331 * ET_p - 0.002827 * BA -
    0.000002493 * kf + 0.0009514 * log(kf/BA)
  v = 0.000008562 * ET_p + 2.611/(-64.35 + input) * BA ^ 0.9425 -
    0.000001211 * kf
  a = 1 - g - v

  result <- c(a, g, v)
  attr(result, "estimated swale area") <- BA/100

  return(result)
}

# estimate_swale_area ----------------------------------------------------------
estimate_swale_area <- function(kf)
{
  stopifnot(kf>=14 && kf<= 3600)

  k_min <- 14
  k_max <- 3600
  A_min <- 27.14
  A_max <- 62.414
  E_min <- -0.303
  E_max <- -0.328


  A_value <- A_min + ((kf - k_min) * (A_max - A_min)) / (k_max - k_min)
  E_value <- E_min + ((kf - k_min) * (E_max - E_min)) / (k_max - k_min)

  A_value * kf ^ E_value
}


# calculate_delta_mod ----------------------------------------------------------
calculate_delta_mod_v0 <- function(results_mod_1, results_mod_2,
                                has_codes = TRUE,
                                var_names = c("surface_runoff",
                                              "infiltration",
                                              "evaporation"),
                                codes_name = "code"){

  #   results_mod_1 <- test_res_3a
  #   results_mod_2 <- test_res_3b

  stopifnot(identical(dim(results_mod_1), dim(results_mod_2)))

  # force result objects to be data.frames
  if(is.vector(results_mod_1)){
    results_mod_1 <- as.data.frame(t(results_mod_1))
    result_mod_2 <- as.data.frame(t(results_mod_2))
  } else {
    results_mod_1 <- as.data.frame(results_mod_1)
    results_mod_2 <- as.data.frame(results_mod_2)
  }

  precipitation <- rowSums(results_mod_1[var_names])

  delta_mod <- data.frame(
    delta_mod = rowSums(
      abs(results_mod_1[var_names] - results_mod_2[var_names])
    ) * 0.5 / precipitation
  )

  if(has_codes){
    stopifnot(
      identical(results_mod_1[[codes_name]], results_mod_2[[codes_name]])
    )
    codes <- results_mod_1[[codes_name]]
    delta_mod <- cbind(code = codes, delta_mod = delta_mod)
  }

  return(delta_mod)

}

calculate_delta_mod <- function(results_mod_1, results_mod_2,
                                has_codes = TRUE,
                                col_patterns = c("runoff|infiltration|evaporation"),
                                codes_name = "code"){

  # force result objects to be data.frames
  if(is.vector(results_mod_1)){
    results_mod_1 <- as.data.frame(t(results_mod_1))
    result_mod_2 <- as.data.frame(t(results_mod_2))
  } else {
    results_mod_1 <- as.data.frame(results_mod_1)
    results_mod_2 <- as.data.frame(results_mod_2)
  }

  stopifnot(nrow(results_mod_1) == nrow(results_mod_2))

  var_names_1 <- grep(pattern = col_patterns, x = names(results_mod_1), value = TRUE)
  var_names_2 <- grep(pattern = col_patterns, x = names(results_mod_2), value = TRUE)

  precipitation_1 <- rowSums(results_mod_1[var_names_1])
  precipitation_2 <- rowSums(results_mod_2[var_names_2])
  tolerance <- 1e-6
  stopifnot(all(abs(precipitation_1 - precipitation_2) < tolerance))

  delta_mod <- data.frame(
    delta_mod = rowSums(
      abs(results_mod_1[var_names_1] - results_mod_2[var_names_2])
    ) * 0.5 / precipitation_1
  )

  if(has_codes){
    stopifnot(
      identical(results_mod_1[[codes_name]], results_mod_2[[codes_name]])
    )
    codes <- results_mod_1[[codes_name]]
    return(cbind(code = codes, delta_mod = delta_mod))
  }

  delta_mod

}
