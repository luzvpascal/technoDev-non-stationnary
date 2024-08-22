# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
source("deployment POMDP/read_solutions.R")
source("variable parameters/simulations.R")
alphas <- read_policyx2(OUTPUT_FILE)

# Define the function to run a single simulation
run_simulation <- function(i) {
  trajectory(
    state_prior_eco = tuple_to_index(1, N_ecosystem/2 + 1, N_ecosystem + 1),
    state_prior_tech = 1,
    Tmax = 85,
    initial_belief_state = B_PAR_TEST,
    initial_belief_state_tech = B_PAR_TECH,
    transition_ecosystem = TR_FUNCTION_ECO,
    transition_tech = TR_FUNCTION_TECH,
    reward = REW,
    true_model = TRUE_MODEL,
    true_model_tech = sample(1:2,1),
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

for (true_scenario  in seq(length(TR_FUNCTION_ECO))){
  TRUE_MODEL <- true_scenario
  for (test_scenario  in seq(length(TR_FUNCTION_ECO))){
    print(paste(TRUE_MODEL, "-", test_scenario))

    B_PAR_TEST <- rep(0, length(TR_FUNCTION_ECO))
    B_PAR_TEST[test_scenario] <- 1

    # Create a cluster
    cl <- makeCluster(ncores)

    # Export necessary variables to the cluster
    clusterExport(cl, c("N_ecosystem", "B_PAR_TEST", "B_PAR_TECH",
                        "TR_FUNCTION_ECO", "TR_FUNCTION_TECH",
                        "REW", "TRUE_MODEL","alphas", "GAMMA", "tuple_to_index",
                        "trajectory", "belief_tech","belief_mod","belief",
                        "update_belief_mod","update_belief_tech",
                        "update_belief","factored_state","interp_policy2"
    ))

    # Run the simulations in parallel
    results <- parLapply(cl, 1:1000, run_simulation)

    # Stop the cluster
    stopCluster(cl)

    # Combine the results if needed
    # For example, if results are lists, you can combine them with do.call(rbind, results)
    results <- bind_rows(results, .id = "sim")

    summ_results <- results %>%
      rowwise() %>%
      mutate(state_year=index_to_year(state_eco, N_ecosystem+1),
             state_ecosystem=index_to_eco(state_eco,N_ecosystem+1))%>%
      mutate(state_ecosystem=ecosystem_states[state_ecosystem],
             action=action-1)%>%
      group_by(time)%>%
      summarise(mean_ecosystem = mean(state_ecosystem),
                sd_ecosystem = sd(state_ecosystem),
                mean_action=mean(action),
                sd_action=sd(action),
                upper_action=min(mean_action+sd_action, 1),
                lower_action=max(mean_action-sd_action, 0),
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
}



