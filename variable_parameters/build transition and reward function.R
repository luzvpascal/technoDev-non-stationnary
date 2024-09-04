#build transition function for each tested value of delta_t_crit_K
transition_matrix_list <- list()

for (index_MDP in seq(nrow(all_scenarios))){

  #build transition function
  scen <- all_scenarios$scenario[index_MDP]
  temperature_data_filter <- temperature_data %>%
    filter(scenario == scen)

  delta_t_crit_K_now <- all_scenarios$delta_t_crit_K[index_MDP]

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

  transition_matrix_list[[index_MDP]] <- combined_transition_matrix
}

## common reward function ####
Reward <- reward_function(ecosystem_states,
                          seq(max(temperature_data$Year)),
                          DEP_EFFECT,
                          cost_deploy)
