# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
## load global variables ####
source("global variables.R")
source("helper functions/read_solutions.R")
source("helper functions/simulations.R")
source("helper functions/write_POMDPx.R")
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/simulations success failure.R")


#build transition function for each tested value of delta_t_crit_K
climate_scenarios <- unique(all_scenarios$scenario)

# for (index_climate in seq(length(climate_scenarios))){
transition_matrix_list <- list()
#build transition function
index_climate <- 3
scen <- climate_scenarios[index_climate]
temperature_data_now <- temperature_data %>% filter(scenario == scen)

index_temp <- 3
delta_t_crit_K_now <- tested_delta_t_crit_K[index_temp]

#stationary with todays temperature
temperature_data_now_start <- temperature_data_now[rep(1,nrow(temperature_data_now)),]
temperature_data_now_start$Year <- temperature_data_now$Year
temperature_data_now_start$scenario <- "start"

#stationary with end temperature
temperature_data_now_end <-
  temperature_data_now[rep(nrow(temperature_data_now),nrow(temperature_data_now)),]
temperature_data_now_end$Year <- temperature_data_now$Year
temperature_data_now_end$scenario <- "end"

#stationray with average temperature
temperature_data_now_avg <- summarise(temperature_data_now,
                                      X5.=mean(X5.),
                                      Mean=mean(Mean),
                                      X95.=mean(X95.))
temperature_data_now_avg <- temperature_data_now_avg[rep(1,nrow(temperature_data_now)),]
temperature_data_now_avg$Year <- temperature_data_now$Year
temperature_data_now_avg$scenario <- "average"

temperature_data_now <- rbind(temperature_data_now,
                              temperature_data_now_start,
                              temperature_data_now_end,
                              temperature_data_now_avg)


## build transition function ####
transition_matrix_list <- list()
scenarios_temp <- unique(temperature_data_now$scenario)

for (index_temp in seq(length(scenarios_temp))){
  #build transition function
  scen <- scenarios_temp[index_temp]
  temperature_data_filter <- temperature_data_now %>%
    filter(scenario == scen)

  combined_transition_matrix <- transition_function(ecosystem_states,
                                                    temperature_states,
                                                    temperature_data_filter,
                                                    DEP_EFFECT,
                                                    r_min,
                                                    r_max,
                                                    delta_t_crit_r,
                                                    sigmoid_bool_r,
                                                    K_min,
                                                    K_max,
                                                    delta_t_crit_K_now,
                                                    sigmoid_bool_K,
                                                    time_step,
                                                    sigma_eco)

  transition_matrix_list[[index_temp]] <- combined_transition_matrix
}

## common reward function ####
Reward <- reward_function(ecosystem_states,
                          seq(max(temperature_data_now$Year)),
                          DEP_EFFECT,
                          cost_deploy)
