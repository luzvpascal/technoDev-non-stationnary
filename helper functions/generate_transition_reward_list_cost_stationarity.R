generate_transition_reward_list_cost_stationarity <- function(scen,
                                                              climate_scenarios,
                                                              temp_scenarios,
                                                              temperature_data,
                                                              ecosystem_states,
                                                              temperature_states,
                                                              DEP_EFFECT,
                                                              r_min,
                                                              r_max,
                                                              delta_t_crit_r,
                                                              sigmoid_bool_r,
                                                              K_min,
                                                              K_max,
                                                              delta_t_crit_K,
                                                              sigmoid_bool_K,
                                                              time_step,
                                                              sigma_eco,
                                                              cost_deploy) {

  # Initialize an empty list to store transition matrices
  transition_matrix_list <- list()

  # Select the current scenario based on index_climate
  temperature_data_now <- temperature_data %>% filter(scenario == scen)

  # Generate stationary datasets with start, end, and average temperatures
  temperature_data_now_start <- temperature_data_now[rep(1, nrow(temperature_data_now)),]
  temperature_data_now_start$Year <- temperature_data_now$Year
  temperature_data_now_start$scenario <- "start"

  temperature_data_now_end <- temperature_data_now[rep(nrow(temperature_data_now), nrow(temperature_data_now)),]
  temperature_data_now_end$Year <- temperature_data_now$Year
  temperature_data_now_end$scenario <- "end"

  temperature_data_now_avg <- temperature_data_now %>%
    summarise(X5. = mean(X5.), Mean = mean(Mean), X95. = mean(X95.))
  temperature_data_now_avg <- temperature_data_now_avg[rep(1, nrow(temperature_data_now)),]
  temperature_data_now_avg$Year <- temperature_data_now$Year
  temperature_data_now_avg$scenario <- "average"

  # Combine temperature data for different scenarios
  temperature_data_now <- rbind(temperature_data_now,
                                temperature_data_now_start,
                                temperature_data_now_end,
                                temperature_data_now_avg)

  # Get unique scenarios from the current temperature data
  scenarios_temp <- unique(temperature_data_now$scenario)

  # Loop over each temperature scenario to build transition matrices
  for (i in seq_along(scenarios_temp)) {
    scen <- scenarios_temp[i]
    temperature_data_filter <- temperature_data_now %>% filter(scenario == scen)

    combined_transition_matrix <- transition_function(
      ecosystem_states, temperature_states, temperature_data_filter, DEP_EFFECT,
      r_min, r_max, delta_t_crit_r, sigmoid_bool_r, K_min, K_max, delta_t_crit_K,
      sigmoid_bool_K, time_step, sigma_eco
    )

    transition_matrix_list[[i]] <- combined_transition_matrix
  }

  # Calculate reward using the reward function
  Reward <- reward_function(
    ecosystem_states, seq(max(temperature_data_now$Year)),
    DEP_EFFECT, cost_deploy
  )

  # Return the list of transition matrices and the reward
  list(transition_matrix_list = transition_matrix_list, Reward = Reward)
}
