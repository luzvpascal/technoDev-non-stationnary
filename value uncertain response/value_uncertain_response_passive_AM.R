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
source("helper functions/simulations value uncertain response passive AM.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")
source("helper functions/write_hmMDP_any_AM_transition_between_models.R")

#initialise res_file , REW
res_file <- paste0("res/value_uncertain_response_deployment_passive_AM_gamma_",gamma,".csv")

## simulations ####
experiments <- list(filtered_scenarios_2_models
                    # ,
                    # filtered_scenarios_4_models
                    # , filtered_scenarios_16_models
                    )

for (index_exp in seq_along(experiments)){
  filtered_scenarios_now <- experiments[[index_exp]]
  for (scen in climate_scenarios){
    print(scen)
    start <- Sys.time()
    ## run simulations ####
    filtered_scenarios_now <- filtered_scenarios_now %>% filter(scenario==scen)
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

      ## Call the function to generate transition matrices and rewards
      transition_matrix <- transition_function(
        ecosystem_states, temperature_states, temperature_data_filter, DEP_EFFECT,
        r_min, r_max, delta_t_crit_r, sigmoid_bool_r, K_min, K_max, delta_t_crit_K,
        sigmoid_bool_K, time_step, sigma_eco
      )

      transition_matrix_list[[index_config]] <- transition_matrix

      ## rewards
      Reward <- reward_function(
        ecosystem_states, seq(max(temperature_data$Year)),
        DEP_EFFECT, cost_deploy
      )

      solution <- mdp_value_iteration(transition_matrix,
                                      Reward,
                                      gamma)

      solution_list[[index_config]] <- solution

    }

    #for passive AM simulations####
    REW <- Reward
    B_PAR <- rep(1, length(transition_matrix_list))/length(transition_matrix_list)
    for (index_config in seq(nrow(filtered_scenarios_now))){
      print(index_config)
      ## apply AM strategy ####
      true_transition_ecosystem_now <- transition_matrix_list[[index_config]]
      # Create a cluster
      cl <- makeCluster(ncores)

      # Export necessary variables to the cluster
      clusterExport(cl, c("N_ecosystem", "B_PAR",
                          "transition_matrix_list",
                          "true_transition_ecosystem_now",
                          "REW", "GAMMA","weighted_average_model",
                          "trajectory","mdp_value_iteration",
                          "update_belief","index_to_tuple",
                          "tuple_to_index",
                          "index_to_eco"
      ))
      # Run the simulations in parallel
      results_sim <- parLapply(cl, 1:100, run_simulation_passive_AM)

      # Stop the cluster
      stopCluster(cl)
      end <- Sys.time()
      # Combine the results if needed
      # For example, if results are lists, you can combine them with do.call(rbind, results)
      results_sim <- bind_rows(results_sim)

      summ_results <- results_sim %>%
        rowwise() %>%
        mutate(state_ecosystem=ecosystem_states[state_eco])%>%
        group_by(time)%>%
        summarise(mean_ecosystem = mean(state_ecosystem),
                  sd_ecosystem = sd(state_ecosystem),
                  mean_action=mean(action),
                  sd_action_deploy=sd(action),
                  mean_value=mean(value),
                  sd_value=sd(value)
        )
      ##write results###
      config <- filtered_scenarios_now[index_config,]
      summ_results$max_value=solution_list[[index_config]]$V[N_ecosystem+1]
      summ_results$delta_t_crit_r <- config$delta_t_crit_r
      summ_results$delta_t_crit_K <- config$delta_t_crit_K
      summ_results$sigmoid_bool_r <- config$sigmoid_bool_r
      summ_results$sigmoid_bool_K <- config$sigmoid_bool_K
      summ_results$DEP_EFFECT <- config$dep_effect
      summ_results$scen <- config$scenario

      summ_results$scen_test <- "passive_AM"
      summ_results$experiment <- paste0("experiment",index_exp)

      write.table(summ_results, res_file,
                  append = TRUE,
                  sep = ",",
                  col.names = FALSE,
                  row.names = FALSE,
                  quote = FALSE)
    }
    end <- Sys.time()
    print(end-start)
  }
}

