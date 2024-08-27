# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
source("deployment POMDP/read_solutions.R")
source("variable_parameters/simulations.R")
alphas <- read_policyx2("POMDPtest.policyx")

# Define the function to run a single simulation
run_simulation_failure <- function(i) {
  trajectory(
    state_prior_eco = tuple_to_index(1, N_ecosystem/2 + 1, N_ecosystem + 1),
    state_prior_tech = 1,
    Tmax = 85,
    initial_belief_state = B_PAR,
    initial_belief_state_tech = B_PAR_TECH,
    transition_ecosystem = TR_FUNCTION_ECO,
    transition_tech = TR_FUNCTION_TECH,
    reward = REW,
    true_model = TRUE_MODEL,
    true_model_tech = 2,
    alpha_momdp = alphas,
    disc = GAMMA,
    optimal_policy = TRUE,
    naive_policy = NA,
    alpha_indexes = FALSE
  )$data_output[-86,]
}
run_simulation_success <- function(i) {
  trajectory(
    state_prior_eco = tuple_to_index(1, N_ecosystem/2 + 1, N_ecosystem + 1),
    state_prior_tech = 1,
    Tmax = 85,
    initial_belief_state = B_PAR,
    initial_belief_state_tech = B_PAR_TECH,
    transition_ecosystem = TR_FUNCTION_ECO,
    transition_tech = TR_FUNCTION_TECH,
    reward = REW,
    true_model = TRUE_MODEL,
    true_model_tech = 1,
    alpha_momdp = alphas,
    disc = GAMMA,
    optimal_policy = TRUE,
    naive_policy = NA,
    alpha_indexes = FALSE
  )$data_output[-86,]
}

# Set the number of cores
ncores <-detectCores()-2
res_file <- "voi_POMDP_pars_climate.csv"
test_scenario <- length(TR_FUNCTION_ECO)+1

for (true_scenario  in seq(length(TR_FUNCTION_ECO))){
  TRUE_MODEL <- true_scenario
   print(paste(TRUE_MODEL))

  # Create a cluster
  cl <- makeCluster(ncores)

  # Export necessary variables to the cluster
  clusterExport(cl, c("N_ecosystem", "B_PAR", "B_PAR_TECH",
                      "TR_FUNCTION_ECO", "TR_FUNCTION_TECH",
                      "REW", "TRUE_MODEL","alphas", "GAMMA", "tuple_to_index",
                      "trajectory", "belief_tech","belief_mod","belief",
                      "update_belief_mod","update_belief_tech",
                      "update_belief","factored_state","interp_policy2"
  ))

  # Run the simulations in parallel
  results_failure <- parLapply(cl, 1:500, run_simulation_failure)
  results_success <- parLapply(cl, 1:500, run_simulation_success)

  # Stop the cluster
  stopCluster(cl)

  # Combine the results if needed
  # For example, if results are lists, you can combine them with do.call(rbind, results)
  results_failure <- bind_rows(results_failure, .id = "sim")
  results_failure$tech_model <- 2
  results_success <- bind_rows(results_success, .id = "sim")
  results_success$tech_model <- 1

  results <- rbind(results_success,
                   results_failure)
  summ_results <- results %>%
    rowwise() %>%
    mutate(state_year=index_to_year(state_eco, N_ecosystem+1),
           state_ecosystem=index_to_eco(state_eco,N_ecosystem+1))%>%
    mutate(state_ecosystem=ecosystem_states[state_ecosystem],
           action_dev=ifelse(action==1, 0,
                             ifelse(state_tech==1, 1,0)),
           action_deploy=ifelse(action==1, 0,
                             ifelse(state_tech==2, 1,0)))%>%
    group_by(time, tech_model)%>%
    summarise(mean_ecosystem = mean(state_ecosystem),
              sd_ecosystem = sd(state_ecosystem),
              mean_tech = mean(state_tech),
              sd_tech = sd(state_tech),
              mean_action_dev=mean(action_dev),
              sd_action_dev=sd(action_dev),
              mean_action_deploy=mean(action_deploy),
              sd_action_deploy=sd(action_deploy),
              mean_value=mean(value),
              sd_value=sd(value)
    )

  summ_results$true_scenario <- true_scenario
  summ_results$test_scenario <- test_scenario

  write.table(summ_results, res_file,
              append = TRUE,
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)
}


