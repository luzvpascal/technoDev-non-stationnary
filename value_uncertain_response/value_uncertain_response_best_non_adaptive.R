# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")

## Set up constants and parameters for technology

res_file <- paste0("res_onlyK/value_uncertain_response_deployment_best_non_adaptive_gamma_",gamma,".csv")
# res_file <- paste0("res/value_uncertain_response_deployment_best_non_adaptive_gamma_",gamma,".csv")

start <- Sys.time()
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


  for (index_MDP_true in seq(length(transition_matrix_list))){
    config_true <- filtered_scenarios_now[index_MDP_true,]
    for (index_MDP_test in seq(length(solution_list))){
      config_test <- filtered_scenarios_now[index_MDP_test,]
      #apply policy of index_MDP_test to index_MDP_true
      solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                              Reward,
                                              gamma,
                                              solution_list[[index_MDP_test]]$policy)

      voi_data_current <- data.frame(max_value=solution_list[[index_MDP_true]]$V[length(ecosystem_states)],
                                     test_value=solution_test[length(ecosystem_states)]
                                     )

      voi_data_current$delta_t_crit_r <- config_true$delta_t_crit_r
      voi_data_current$delta_t_crit_K <- config_true$delta_t_crit_K
      voi_data_current$sigmoid_bool_r <- config_true$sigmoid_bool_r
      voi_data_current$sigmoid_bool_K <- config_true$sigmoid_bool_K
      voi_data_current$DEP_EFFECT <- config_true$dep_effect
      voi_data_current$scen <- config_true$scenario

      voi_data_current$delta_t_crit_r_test <- config_test$delta_t_crit_r
      voi_data_current$delta_t_crit_K_test <- config_test$delta_t_crit_K
      voi_data_current$sigmoid_bool_r_test <- config_test$sigmoid_bool_r
      voi_data_current$sigmoid_bool_K_test <- config_test$sigmoid_bool_K
      voi_data_current$DEP_EFFECT_test <- config_test$dep_effect
      voi_data_current$scen_test <- config_test$scenario


      voi_data <- rbind(voi_data,
                        voi_data_current)
    }
  }
}
write.csv(voi_data, res_file, row.names = FALSE)
end <- Sys.time()

print(end-start)
