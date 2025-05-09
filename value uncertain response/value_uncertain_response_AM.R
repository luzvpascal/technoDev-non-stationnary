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
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/simulations transition between models.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")
source("helper functions/simulations AM.R")
source("helper functions/write_hmMDP_any_AM.R")
source("helper functions/build transition functions MOMDP.R")
source("helper functions/perron_lower_bound.R")

## Set up constants and parameters for technology
res_file <- paste0("res/value_uncertain_response_deployment_AM_gamma_",gamma,".csv")
res_file_individual <- paste0("res/value_uncertain_response_deployment_AM_individual_gamma_",gamma,".csv")

write_model <- TRUE
solve_POMDP <- FALSE
run_simulations <- FALSE

experiments <- list(
      filtered_scenarios_2_models
      # ,
      # filtered_scenarios_4_models
      # , filtered_scenarios_16_models
)

N_models <- 85

for (index_exp in seq_along(experiments)){

  for (scen in climate_scenarios){
    filtered_scenarios_now <- experiments[[index_exp]]
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
      Reward <- reward_function(
        ecosystem_states, seq(max(temperature_data_filter$Year)),
        DEP_EFFECT, cost_deploy
      )

      solution <- mdp_value_iteration(transition_matrix,
                                      Reward,
                                      gamma)

      solution_list[[index_config]] <- solution
    }

    ## solve POMDP ####
    file_name <- paste0("pomdpx_gamma",gamma,
                        "/value_uncertain_response_deployment_AM_",scen,
                        "_",N_models,"_experiment",index_exp)
    FILE <- paste0(file_name, ".pomdpx")
    OUTPUT_FILE <- paste0(file_name, ".policyx")
    B_FULL <- rep(0, nrow(transition_matrix_list[[index_config]][,,1]))
    B_FULL[length(ecosystem_states)] <- 1

    B_PAR <- rep(1, length(transition_matrix_list))/length(transition_matrix_list)

    REW <- reward_function(
      ecosystem_states, seq(max(temperature_data_filter$Year)),
      DEP_EFFECT, cost_deploy
    )

    if (write_model){
      start <- Sys.time()
      write_hmMDP(TR_FUNCTION = transition_matrix_list,
                  B_FULL = B_FULL,
                  B_PAR = B_PAR,
                  REW = REW,
                  GAMMA=GAMMA,
                  FILE)
      end <- Sys.time()
      print("Time to write model")
      print(end-start)
    }

    if (solve_POMDP){
      path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

      cmd <- paste(path_to_sarsop,
                   "--precision", 0.0000001,
                   "--timeout",7200,
                   "--output", OUTPUT_FILE,
                   FILE,
                   sep=" ")
      system(cmd)
    }


    if (run_simulations){
      alphas <- read_policyx2(OUTPUT_FILE)

      alphas <- perron_lower_bound(transition_matrix_list,
                                   solution_list,
                                   Reward,
                                   gamma,
                                   alphas)
      tr_momdp <- transition_hmMDP(transition_matrix_list)
      obs_momdp <- obs_hmMDP(transition_matrix_list)

      for (index_config in seq(nrow(filtered_scenarios_now))) {
      # for (index_config in c(3,4)) {

        print(index_config)
        config <- filtered_scenarios_now[index_config,]
        ## apply AM strategy ####
        # Create a cluster
        start <- Sys.time()
        cl <- makeCluster(ncores)
        clusterExport(cl, c("N_ecosystem", "ecosystem_states", "B_PAR",
                            "transition_matrix_list",'index_config',
                            "REW", "tr_momdp","obs_momdp", "alphas", "GAMMA",
                            "trajectory",  "belief_state_function", "update_belief",
                            "sum_Nstate_by_Nstate",
                            "interp_policy2"))

        # Run simulations
        results_sim <- parLapply(cl, 1:1000, run_simulation_AM)

        # Stop the cluster
        stopCluster(cl)

        # Combine and save results
        # results_sim <- list()
        # for (i in seq(1000)){
        #   results_sim[[i]] <- trajectory(state_prior = length(ecosystem_states),
        #                                  Tmax = 85,
        #                                  initial_belief_state = B_PAR,
        #                                  tr_mdp = transition_matrix_list[[index_config]],
        #                                  rew_mdp = REW,
        #                                  tr_momdp = tr_momdp,
        #                                  obs_momdp = obs_momdp,
        #                                  alpha_momdp = alphas,
        #                                  disc = GAMMA,
        #                                  optimal_policy = TRUE,
        #                                  naive_policy = NA,
        #                                  alpha_indexes=FALSE)$data_output[84,]
        # }
        end <- Sys.time()
        print("Time to generate 1000 simulations:")
        print(end-start)
        # Combine the results if needed
        # For example, if results are lists, you can combine them with do.call(rbind, results)

        results_sim <- bind_rows(results_sim, .id = "sim")
        #save results at tmax only ####
        results_sim_tmax <- results_sim %>%
          filter(time==max(time))

        results_sim_tmax$max_value=solution_list[[index_config]]$V[N_ecosystem+1]
        results_sim_tmax$delta_t_crit_r <- config$delta_t_crit_r
        results_sim_tmax$delta_t_crit_K <- config$delta_t_crit_K
        results_sim_tmax$sigmoid_bool_r <- config$sigmoid_bool_r
        results_sim_tmax$sigmoid_bool_K <- config$sigmoid_bool_K
        results_sim_tmax$DEP_EFFECT <- config$dep_effect
        results_sim_tmax$scen <- config$scenario

        results_sim_tmax$scen_test <- paste0("AM_", 85)
        results_sim_tmax$experiment <- paste0("experiment",index_exp)

        write.table(results_sim_tmax,
                    res_file_individual,
                    append = TRUE,
                    sep = ",",
                    col.names = FALSE,
                    row.names = FALSE,
                    quote = FALSE)
        #save overall results ####
        summ_results <- results_sim %>%
          rowwise() %>%
          mutate(state_eco_index=index_to_eco(state, N_ecosystem+1))%>%
          mutate(state_ecosystem=ecosystem_states[state_eco_index])%>%
          group_by(time)%>%
          summarise(mean_ecosystem = mean(state_ecosystem),
                    sd_ecosystem = sd(state_ecosystem),
                    mean_action=mean(action),
                    sd_action_deploy=sd(action),
                    mean_value=mean(value),
                    sd_value=sd(value)
          )

        summ_results$max_value=solution_list[[index_config]]$V[N_ecosystem+1]
        summ_results$delta_t_crit_r <- config$delta_t_crit_r
        summ_results$delta_t_crit_K <- config$delta_t_crit_K
        summ_results$sigmoid_bool_r <- config$sigmoid_bool_r
        summ_results$sigmoid_bool_K <- config$sigmoid_bool_K
        summ_results$DEP_EFFECT <- config$dep_effect
        summ_results$scen <- config$scenario

        summ_results$scen_test <- paste0("AM_", 85)
        summ_results$experiment <- paste0("experiment",index_exp)

        write.table(summ_results, res_file,
                    append = TRUE,
                    sep = ",",
                    col.names = FALSE,
                    row.names = FALSE,
                    quote = FALSE)
      }
    }
  }
}
