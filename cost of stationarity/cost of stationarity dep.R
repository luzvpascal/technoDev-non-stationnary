library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
## performance data ####
voi_data_uncertain_response <- read.csv(paste0("res_onlyK/value_uncertain_response_deployment_stationary_strategy_gamma_",gamma,".csv"),
                                        header = TRUE)
res_file <- paste0("res_onlyK/cost_of_stationarity_deployment_gamma_",gamma,".csv")
# voi_data_uncertain_response <- read.csv(paste0("res/value_uncertain_response_deployment_stationary_strategy_gamma_",gamma,".csv"),
#                                         header = TRUE)
# res_file <- paste0("res/cost_of_stationarity_deployment_gamma_",gamma,".csv")

voi_data_uncertain_response <- voi_data_uncertain_response %>%
  mutate(r_EVPI=(max_value-test_value)/max_value)

names_test <-  names(voi_data_uncertain_response)[grep("_test$", names(voi_data_uncertain_response))]

#create a results data frame for each experiment####
names(filtered_scenarios) <- c("delta_t_crit_r","delta_t_crit_K",
                                   "sigmoid_bool_r" ,"sigmoid_bool_K" ,
                                   "DEP_EFFECT" , "scen")
cols_to_match <- names(filtered_scenarios)
#filter responses
voi_data_uncertain_response_exp <- voi_data_uncertain_response %>%
  filter(delta_t_crit_r_test == delta_t_crit_r,
         delta_t_crit_K_test == delta_t_crit_K,
         sigmoid_bool_r_test == sigmoid_bool_r,
         sigmoid_bool_K_test == sigmoid_bool_K,
         scen == scen_test
  )

#best perf scenario
best_perf_scenario <- voi_data_uncertain_response_exp %>%
  group_by(across(all_of(names_test[-7])))%>%
  slice_min(order_by = r_EVPI, with_ties = FALSE)

#select relevant columns
best_perf_scenario <- best_perf_scenario%>%
  select(c("delta_t_crit_r","delta_t_crit_K",
           "sigmoid_bool_r" ,"sigmoid_bool_K" ,
           "DEP_EFFECT" , "scen","max_value","r_EVPI","stationary_test"))

write.csv(best_perf_scenario,
          res_file, row.names = FALSE)

