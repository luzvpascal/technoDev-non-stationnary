# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
source("helper functions/read_solutions.R")
source("helper functions/simulations.R")
source("helper functions/simulations success failure.R")
source("helper functions/write_POMDPx.R")

##technology####
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2),
                           matrix(c(1-p_dev, 0, p_dev, 1), ncol=2))
transition_failure <- list(diag(2),
                           diag(2))
transition_tech <- list(transition_success, transition_failure)

##
TR_FUNCTION_ECO <- transition_matrix_list
TR_FUNCTION_TECH <- transition_tech
REW <- Reward
GAMMA <- gamma

B_FULL_ECO <- c(rep(0, nrow(Reward)))
B_FULL_ECO[N_ecosystem+1] <- 1
B_FULL_TECH <- c(1, rep(0, length(tech_states)-1))

B_PAR <- rep(1, length(transition_matrix_list))/length(transition_matrix_list)
B_PAR_TECH <- rep(1, length(transition_tech))/length(transition_tech)


file_name <- paste0("pomdpx/fullPOMDP")

FILE <- paste0(file_name, ".pomdpx")

write_full_POMDP(TR_FUNCTION_ECO,
                 TR_FUNCTION_TECH,
                 B_FULL_ECO,
                 B_FULL_TECH,
                 B_PAR,
                 B_PAR_TECH,
                 REW,
                 GAMMA,
                 FILE)

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

OUTPUT_FILE <- paste0(file_name, ".policyx")

cmd <- paste(path_to_sarsop,
             "--precision", 0.0000001,
             "--timeout",600,
             "--output", OUTPUT_FILE,
             FILE,
             sep=" ")
system(cmd)

## read solutions ####
alphas <- read_policyx2(OUTPUT_FILE)

# Set the number of cores
ncores <-detectCores()-2
res_file <- "res/voi_POMDP_pars_climate3.csv"
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
  results_failure <- parLapply(cl, 1:1000, run_simulation_failure)
  results_success <- parLapply(cl, 1:1000, run_simulation_success)

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


