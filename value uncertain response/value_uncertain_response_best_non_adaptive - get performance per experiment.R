library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
## performance data ####
voi_data_uncertain_response <- read.csv(
  paste0("res/value_uncertain_response_deployment_best_non_adaptive_gamma_",gamma,".csv"),
  header = TRUE)

res_file <-
  paste0("res/value_uncertain_response_deployment_best_non_adaptive_solution_gamma_",gamma,".csv")


experiments <- list(filtered_scenarios_2_models,
                    filtered_scenarios_4_models,
                    filtered_scenarios_16_models)
names_test <-  names(voi_data_uncertain_response)[grep("_test$", names(voi_data_uncertain_response))]

#create a results data frame for each experiment####
voi_data <- data.frame()
for (index_exp in seq_along(experiments)){

  filtered_scenarios_now <- experiments[[index_exp]]
  names(filtered_scenarios_now) <- c("delta_t_crit_r","delta_t_crit_K",
                                     "sigmoid_bool_r" ,"sigmoid_bool_K" ,
                                     "DEP_EFFECT" , "scen")
  cols_to_match <- names(filtered_scenarios_now)
  #filter responses
  voi_data_uncertain_response_exp <- voi_data_uncertain_response %>%
    semi_join(filtered_scenarios_now, by = cols_to_match) %>%
    filter(delta_t_crit_r_test %in% filtered_scenarios_now$delta_t_crit_r,
           delta_t_crit_K_test %in% filtered_scenarios_now$delta_t_crit_K,
           sigmoid_bool_r_test %in% filtered_scenarios_now$sigmoid_bool_r,
           sigmoid_bool_K_test %in% filtered_scenarios_now$sigmoid_bool_K
    )

  #best perf scenario
  best_perf_scenario <- voi_data_uncertain_response_exp %>%
    group_by(across(all_of(names_test)))%>%
    summarise(mean_test_value=mean(test_value),
              sd_test_value=sd(test_value))%>%
    ungroup()%>%
    group_by(scen_test)%>%
    slice_max(order_by = mean_test_value, with_ties = FALSE)

  # Filter voi_data_uncertain_response to keep rows that match any row in best_perf_scenario
  filtered_voi_data_uncertain_response <- voi_data_uncertain_response_exp %>%
    semi_join(best_perf_scenario, by = names_test)

  #select relevant columns
  filtered_voi_data_uncertain_response <- filtered_voi_data_uncertain_response%>%
    select(c("delta_t_crit_r","delta_t_crit_K",
              "sigmoid_bool_r" ,"sigmoid_bool_K" ,
              "DEP_EFFECT" , "scen","max_value","test_value"))
  filtered_voi_data_uncertain_response$experiment <- paste0("experiment",index_exp)
  filtered_voi_data_uncertain_response$scen_test <- "best non-adaptive"

  voi_data <- rbind(voi_data,
                    filtered_voi_data_uncertain_response)
}

write.csv(voi_data,
          res_file,row.names = FALSE)

