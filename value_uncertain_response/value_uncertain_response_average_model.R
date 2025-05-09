# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
source("helper functions/read_solutions.R")
source("helper functions/simulations.R")
source("helper functions/write_POMDPx.R")
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/simulations success failure.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")

## Set up constants and parameters for technology
res_file <- "res/value_uncertain_response_deployment_average_model.csv"

## Loop over climate and temperature scenarios
voi_data <- data.frame()
for (scen in climate_scenarios){
  filtered_scenarios_now <- filtered_scenarios %>% filter(scenario==scen)

  temperature_data_filter <- temperature_data %>% filter(scenario == scen)

  transition_matrix_list <- list()
  solution_list <- list()
  for (index_config in seq(nrow(filtered_scenarios_now))) {
    print(index_config)
    #set scenarios variables ####
    config <- filtered_scenarios_now[index_config,]
    delta_t_crit_r <- config$delta_t_crit_r
    delta_t_crit_K <- config$delta_t_crit_K
    sigmoid_bool_r <- config$sigmoid_bool_r
    sigmoid_bool_K <- config$sigmoid_bool_K
    DEP_EFFECT <- c(0, config$dep_effect,config$dep_effect*2)
    scen <- config$scenario

    ## Call the function to generate transition matrices and rewards
    transition_matrix <- transition_function(
      ecosystem_states, temperature_states, temperature_data_filter, DEP_EFFECT,
      r_min, r_max, delta_t_crit_r, sigmoid_bool_r, K_min, K_max, delta_t_crit_K,
      sigmoid_bool_K, time_step, sigma_eco
    )

    transition_matrix_list[[index_config]] <- transition_matrix
    Reward <- reward_function(
      ecosystem_states, seq(max(temperature_data_filter$Year)),
      DEP_EFFECT, cost_deploy
    )

    solution <- mdp_value_iteration(transition_matrix,
                                    Reward,
                                    gamma)
    solution_list[[index_config]] <- solution
  }

  ## calculate average transition function
  avg_transition_matrix <- transition_matrix_list[[1]] * 0

  # Loop through each transition matrix and add it to the avg_transition_matrix
  for (transition_matrix in transition_matrix_list) {
    avg_transition_matrix <- avg_transition_matrix + transition_matrix
  }

  # Divide by the number of transition matrices to get the average
  avg_transition_matrix <- avg_transition_matrix / length(transition_matrix_list)

  ## solve agerage matrix ####
  Reward <- reward_function(
    ecosystem_states, seq(max(temperature_data_filter$Year)),
    DEP_EFFECT, cost_deploy
  )

  solution <- mdp_value_iteration(avg_transition_matrix,
                                  Reward,
                                  gamma)

  for (index_MDP_true in seq(length(transition_matrix_list))){
    print(index_MDP_true)
    config_true <- filtered_scenarios_now[index_MDP_true,]
    #apply policy of index_MDP_test to index_MDP_true
    solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                            Reward,
                                            gamma,
                                            solution$policy)

    voi_data_current <- data.frame(max_value=solution_list[[index_MDP_true]]$V[length(ecosystem_states)],
                                   test_value=solution_test[length(ecosystem_states)]
    )

    voi_data_current$delta_t_crit_r <- config_true$delta_t_crit_r
    voi_data_current$delta_t_crit_K <- config_true$delta_t_crit_K
    voi_data_current$sigmoid_bool_r <- config_true$sigmoid_bool_r
    voi_data_current$sigmoid_bool_K <- config_true$sigmoid_bool_K
    voi_data_current$DEP_EFFECT <- config_true$dep_effect
    voi_data_current$scen <- config_true$scenario

    voi_data_current$delta_t_crit_r_test <- 1
    voi_data_current$delta_t_crit_K_test <- 1
    voi_data_current$sigmoid_bool_r_test <- TRUE
    voi_data_current$sigmoid_bool_K_test <- TRUE
    voi_data_current$DEP_EFFECT_test <- config_true$dep_effect
    voi_data_current$scen_test <- "average"


    voi_data <- rbind(voi_data,
                      voi_data_current)
  }
}
write.csv(voi_data, res_file, row.names = FALSE)
