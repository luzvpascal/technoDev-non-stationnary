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
source("helper functions/generate_transition_reward_list_cost_stationarity.R")

## Set up constants and parameters for technology
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2), matrix(c(1 - p_dev, 0, p_dev, 1), ncol = 2))
transition_failure <- list(diag(2), diag(2))
TR_FUNCTION_TECH <- list(transition_success, transition_failure)
res_file <- "res/cost_of_stationarity.csv"

## Loop over climate and temperature scenarios
for (index_config in seq(nrow(filtered_scenarios))) {

  #set scenarios variables ####
  config <- filtered_scenarios[index_config,]
  delta_t_crit_r <- config$delta_t_crit_r
  delta_t_crit_K <- config$delta_t_crit_K
  sigmoid_bool_r <- config$sigmoid_bool_r
  sigmoid_bool_K <- config$sigmoid_bool_K
  DEP_EFFECT <- c(0, config$dep_effect)
  scen <- config$scenario

  ## Call the function to generate transition matrices and rewards
  result <- generate_transition_reward_list_cost_stationarity(
    scen = scen,
    climate_scenarios = climate_scenarios,
    temperature_data = temperature_data,
    ecosystem_states = ecosystem_states,
    temperature_states = temperature_states,
    DEP_EFFECT = DEP_EFFECT,
    r_min = r_min,
    r_max = r_max,
    delta_t_crit_r = delta_t_crit_r,
    sigmoid_bool_r = sigmoid_bool_r,
    K_min = K_min,
    K_max = K_max,
    delta_t_crit_K = delta_t_crit_K,
    sigmoid_bool_K = sigmoid_bool_K,
    time_step = time_step,
    sigma_eco = sigma_eco,
    cost_deploy = cost_deploy
  )

  transition_matrix_list <- result$transition_matrix_list
  REW <- result$Reward
  B_FULL_ECO <- c(rep(0, nrow(REW)))
  B_FULL_ECO[N_ecosystem + 1] <- 1
  B_FULL_TECH <- c(1, rep(0, length(tech_states) - 1))
  B_PAR <- 1
  B_PAR_TECH <- rep(1, length(TR_FUNCTION_TECH)) / length(TR_FUNCTION_TECH)

  # Define file names with indices for clarity
  file_name <- paste0("pomdpx/cost_of_stationarity_",
                      "delta_t_crit_r_", delta_t_crit_r,
                      "_delta_t_crit_K_", delta_t_crit_K,
                      "_sigmoid_bool_r_", ifelse(sigmoid_bool_r, "TRUE", "FALSE"),
                      "_sigmoid_bool_K_", ifelse(sigmoid_bool_K, "TRUE", "FALSE"),
                      "_DEP_EFFECT_", paste(DEP_EFFECT, collapse = "_"),
                      "_scenario_", scen)

  res_file_sim <- paste0("res/cost_of_stationarity_",
                         "delta_t_crit_r_", delta_t_crit_r,
                         "_delta_t_crit_K_", delta_t_crit_K,
                         "_sigmoid_bool_r_", ifelse(sigmoid_bool_r, "TRUE", "FALSE"),
                         "_sigmoid_bool_K_", ifelse(sigmoid_bool_K, "TRUE", "FALSE"),
                         "_DEP_EFFECT_", paste(DEP_EFFECT, collapse = "_"),
                         "_scenario_", scen)

  ## Run simulations over all test scenarios in transition_matrix_list
  for (test_scenario in seq_along(transition_matrix_list)) {
    ## POMDP file setup
    FILE <- paste0(file_name, "_test_", test_scenario, ".pomdpx")
    OUTPUT_FILE <- paste0(file_name, "_test_", test_scenario, ".policyx")

    # Write the full POMDP model
    write_full_POMDP(list(transition_matrix_list[[test_scenario]]),
                     TR_FUNCTION_TECH,
                     B_FULL_ECO,
                     B_FULL_TECH,
                     B_PAR,
                     B_PAR_TECH,
                     REW,
                     GAMMA,
                     FILE)

    # Run POMDP solver
    path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package = "sarsop")
    cmd <- paste(path_to_sarsop, "--precision", 0.0000001, "--timeout", 60, "--output", OUTPUT_FILE, FILE, sep = " ")
    system(cmd)
    alphas <- read_policyx2(OUTPUT_FILE)

    # Set up for the simulation
    true_scenario <- 1
    TRUE_MODEL <- 1
    print(paste("Running scenario:", true_scenario, "-", test_scenario))

    TR_FUNCTION_ECO <- list(transition_matrix_list[[true_scenario]])

    # Set up parallel processing
    cl <- makeCluster(ncores)
    clusterExport(cl, c("N_ecosystem", "B_PAR", "B_PAR_TECH",
                        "TR_FUNCTION_ECO", "TR_FUNCTION_TECH",
                        "REW", "TRUE_MODEL", "alphas", "GAMMA", "tuple_to_index",
                        "trajectory", "belief_tech", "belief_mod", "belief",
                        "update_belief_mod", "update_belief_tech",
                        "update_belief", "factored_state", "interp_policy2"))

    # Run simulations
    results_failure <- parLapply(cl, 1:1000, run_simulation_failure)
    results_success <- parLapply(cl, 1:1000, run_simulation_success)

    # Stop the cluster
    stopCluster(cl)

    # Combine and save results
    results_failure <- bind_rows(results_failure, .id = "sim")
    results_failure$tech_model <- 2
    results_success <- bind_rows(results_success, .id = "sim")
    results_success$tech_model <- 1
    results <- rbind(results_success, results_failure)

    # Save individual simulation results
    write.table(results, paste0(res_file_sim, "_test_", test_scenario, "_on_", true_scenario, ".csv"),
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)

    # Summarize results
    summ_results <- results %>%
      rowwise() %>%
      mutate(state_year = index_to_year(state_eco, N_ecosystem + 1),
             state_ecosystem = index_to_eco(state_eco, N_ecosystem + 1)) %>%
      mutate(state_ecosystem = ecosystem_states[state_ecosystem],
             action_dev = ifelse(action == 1, 0, ifelse(state_tech == 1, 1, 0)),
             action_deploy = ifelse(action == 1, 0, ifelse(state_tech == 2, 1, 0))) %>%
      group_by(time, tech_model) %>%
      summarise(mean_ecosystem = mean(state_ecosystem),
                sd_ecosystem = sd(state_ecosystem),
                mean_tech = mean(state_tech),
                sd_tech = sd(state_tech),
                mean_action_dev = mean(action_dev),
                sd_action_dev = sd(action_dev),
                mean_action_deploy = mean(action_deploy),
                sd_action_deploy = sd(action_deploy),
                mean_value = mean(value),
                sd_value = sd(value))

    summ_results$delta_t_crit_r <- config$delta_t_crit_r
    summ_results$delta_t_crit_K <- config$delta_t_crit_K
    summ_results$sigmoid_bool_r <- config$sigmoid_bool_r
    summ_results$sigmoid_bool_K <- config$sigmoid_bool_K
    summ_results$DEP_EFFECT <- config$dep_effect
    summ_results$scen <- config$scenario
    summ_results$test_scenario <- test_scenario

    # Append summarized results to main result file
    write.table(summ_results, res_file,
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
}
