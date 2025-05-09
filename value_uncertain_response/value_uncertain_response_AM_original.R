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
source("helper functions/simulations transition between models.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")
source("helper functions/write_hmMDP_any_AM_transition_between_models.R")

## Set up constants and parameters for technology
res_file <- paste0("res/value_uncertain_response_deployment_AM_gamma_",gamma,".csv")
res_file_individual <- paste0("res/value_uncertain_response_deployment_AM_individual_gamma_",gamma,".csv")

create_matrix <- function(n, alpha) {
  mat <- matrix(0, n, n)  # Start with an n x n matrix of zeros
  # Set first and last rows
  mat[n, n] <- 1

  # Set the main diagonal and the elements directly above and below it for middle rows
  for (i in 1:(n - 1)) {
    mat[i, i] <- 1 - alpha           # Main diagonal
    mat[i, i + 1] <- alpha    # Above main diagonal
  }

  return(mat)
}

run_simulations <- TRUE
write_model <- TRUE
solve_POMDP <- TRUE
seq_N_models <- c(85)
# seq_N_models <- c(10,85)

experiments <- list(filtered_scenarios_2_models,
                    filtered_scenarios_4_models
                    # , filtered_scenarios_16_models
)

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

    ## For the set of discrete time steps ####

    for (N_models in seq_N_models){

      transition_matrix_list_AM <- list()
      transition_between_models_list_AM <- list()

      start <- Sys.time()
      B_PAR <- c()
      for (index_config in seq(nrow(filtered_scenarios_now))){
        config <- filtered_scenarios_now[index_config,]
        delta_t_crit_r <- config$delta_t_crit_r
        delta_t_crit_K <- config$delta_t_crit_K
        sigmoid_bool_r <- config$sigmoid_bool_r
        sigmoid_bool_K <- config$sigmoid_bool_K
        DEP_EFFECT <- c(0, config$dep_effect,config$dep_effect*2)

        ## finding max time for each model ####
        # tested_delta <- temperature_data_filter$Mean
        # K_eff <- c()
        # for (delta_t in tested_delta){
        #   K_eff <- c(K_eff, K_function(K_min, K_max, delta_t_crit_K,
        #                                delta_t-2,sigmoid_bool_K))
        # }
        # indexes_below <- which(K_eff<0.01)
        #
        # if (length(indexes_below)==0){
        #   Tmax_time <- max(temperature_data_filter$Year)
        # } else {
        #   Tmax_time <- indexes_below[1]
        # }
        # discretize times with max time
        # discrete_times <- unique(round(seq(1, Tmax_time,length.out=N_models)))
        discrete_times <- temperature_data_filter$Year
        time_step_models <- discrete_times[2]-discrete_times[1]

        #select the data temperature
        temperature_data_exp <- temperature_data_filter %>%
          filter(Year %in% discrete_times)

        B_PAR_NOW <- rep(0, length(discrete_times))
        B_PAR_NOW[1] <- 1
        B_PAR <- c(B_PAR, B_PAR_NOW)
        for (index_time in temperature_data_exp$Year) {
          temperature_data_exp_now <- temperature_data_exp %>%
            filter(Year == index_time)

          ## Call the function to generate transition matrices and rewards
          transition_matrix <- transition_function(
            ecosystem_states, temperature_states, temperature_data_exp_now, DEP_EFFECT,
            r_min, r_max, delta_t_crit_r, sigmoid_bool_r, K_min, K_max, delta_t_crit_K,
            sigmoid_bool_K, time_step, sigma_eco
          )

          transition_matrix_list_AM[[length(transition_matrix_list_AM)+1]] <- transition_matrix
        }

        transition_between_models_list_AM[[length(transition_between_models_list_AM)+1]] <- create_matrix(length(temperature_data_exp$Year), 1/time_step_models)

        print(index_config)
      }
      end <- Sys.time()
      print("Time to generate AM model")
      print(end-start)
      ## solve POMDP ####
      file_name <- paste0("pomdpx_gamma",gamma,"/value_uncertain_response_deployment_AM_",scen,"_",N_models,"_experiment",index_exp)
      FILE <- paste0(file_name, ".pomdpx")
      OUTPUT_FILE <- paste0(file_name, ".policyx")
      B_FULL <- rep(0, length(ecosystem_states))
      B_FULL[length(ecosystem_states)] <- 1

      B_PAR <- B_PAR/sum(B_PAR)

      transition_between_models <- bdiag(transition_between_models_list_AM)

      REW <- reward_function(
        ecosystem_states, seq(length(discrete_times)),
        DEP_EFFECT, cost_deploy
      )
      # REW <- rep(REW,nrow(filtered_scenarios_now))
      REW <- REW[rep(seq(nrow(REW)),nrow(filtered_scenarios_now)),]

      if (write_model){
        start <- Sys.time()
        write_hmMDP(TR_FUNCTION = transition_matrix_list_AM,
                    TR_FUNCTION_MODELS = transition_between_models,
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
                 "--timeout",1000,
                 "--output", OUTPUT_FILE,
                 FILE,
                 sep=" ")
        system(cmd)
      }


      if (run_simulations){
        alphas <- read_policyx2(OUTPUT_FILE)

        for (index_config in seq(nrow(filtered_scenarios_now))) {

          print(index_config)
          config <- filtered_scenarios_now[index_config,]
          ## apply AM strategy ####
          true_transition_ecosystem_now <- transition_matrix_list[[index_config]]

          # Create a cluster
          start <- Sys.time()

          results_sim <- list()
          for (i in seq(1000)){
            results_sim[[i]] <- run_simulation_transition_between_models(i)
          }
          end <- Sys.time()
          print(end-start)
          # Combine the results if needed
          # For example, if results are lists, you can combine them with do.call(rbind, results)
          results_sim <- bind_rows(results_sim)

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

          results_sim_tmax$scen_test <- paste0("AM_", N_models)
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
            mutate(state_ecosystem=ecosystem_states[state_eco])%>%
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

          summ_results$scen_test <- paste0("AM_", N_models)
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
}
