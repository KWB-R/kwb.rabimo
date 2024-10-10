# Test script for implementing stormwater management in RABIMO

kwb.utils::assignPackageObjects("kwb.rabimo")
`%>%` <- magrittr::`%>%`

# get 2020 input data and config in R-Abimo format
inputs <- kwb.rabimo::rabimo_inputs_2020

input_data <- inputs$data
input_config <- inputs$config

# subset input for testing
codes <- c("0000000004000011","0000000004000017","0000000004000018",
           "0000000004000034","0000000004000876","0000000004000882",
           "0000000004000895","0000000004001658","0000000004001680",
           "0000000004000896","0000000004002242","0000000007000617",
           "0000000007000630","0000000007000631","0000000007000642",
           "0000000007001516","0000000007001529","0000000007002218",
           "0000000007002226","0000000007001539","0000000007002227",
           "0000000007002402","0900452811000000","0900452941000000",
           "0900453141000000","0900453151000000","0900453171000000",
           "0900453181000000","0900453191000000","0900453201000000",
           "1100612741000000","1100612751000000","1100612861000000",
           "1100612871000000","1100612881000000","1100613031000000",
           "1100613041000000")

input_data <- input_data[which(input_data$code %in% codes),]

if (FALSE)
{
  ## RESULTS WITHOUT WSUD
  results_no_wsud <- kwb.rabimo::run_rabimo(data = input_data,
                                          config = input_config,
                                          check = FALSE)

  ## GREEN-ROOF ----------------------------------------------------------------

  # fake storm-water management input from user: green_roof
  set.seed(123)
  green_roof_input <- data.frame(code = codes) %>%
    dplyr::mutate(green_roof = round(runif(length(codes)),2)) %>%
    dplyr::sample_frac(0.6)

  # update green roof column in input_data
  updated_input_data <- update_input(original_data = input_data,
                                      update_data = green_roof_input,
                                      value_column = "green_roof")

  # calculate water balance results with green-roof component
  results_only_gr <- kwb.rabimo::run_rabimo(data = updated_input_data,
                                       config = input_config,
                                       check = FALSE)



  compare_gr <- updated_input_data %>%
    select_columns(c("code", "roof", "green_roof")) %>%
    cbind(results_only_gr[-1] - results_no_wsud[-1])

  input_data_with_greenroof <- updated_input_data

  ## INFILTRATION TRENCH -------------------------------------------------------

  # vector with total percentage of the runoff-relevant area (sealed area)
  # that is connected to an infiltration swale
  area_to_swale <- data.frame(code = codes) %>%
    dplyr::mutate(to_swale = round(runif(length(codes)),2)) %>%
    dplyr::sample_frac(0.4)

  # the to_swale input vector must be merged with the input-data
  updated_input_data <- update_input(original_data = input_data,
                                     update_data = area_to_swale,
                                     value_column = "to_swale")

  results_only_swale <- kwb.rabimo::run_rabimo(data = updated_input_data,
                                               config = input_config,
                                               check = FALSE)

  compare_swale <- updated_input_data %>%
    select_columns(c("code", "to_swale", "pvd")) %>%
    cbind(results_only_swale[-1] - results_no_wsud[-1])

}


# update_input ------------------------------------------------------------
update_input <- function(original_data, update_data, value_column)
{
  stopifnot(all(update_data[["code"]] %in% original_data[["code"]]))

  original_data[match(update_data[["code"]],original_data[["code"]]),
                value_column] <- update_data[[value_column]]

  original_data

}





