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
res_file <- paste0("res/value_uncertain_response_deployment_AM_perron_LB_gamma_",gamma,".csv")

experiments <- list(filtered_scenarios_2_models
                    ,
                    filtered_scenarios_4_models
                    , filtered_scenarios_16_models
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
    B_PAR <- rep(1, length(transition_matrix_list))/length(transition_matrix_list)

    #alphas LB with Perron et al 2017####
    alphas <- list(vectors=matrix(rep(0,length(B_PAR)),nrow=length(B_PAR)),
                   action=c(1),
                   obs=c(1),
                   index=c(1))
    alphas <- perron_lower_bound(transition_matrix_list,
                                 solution_list,
                                 Reward,
                                 gamma,
                                 alphas)

    output <- interp_policy2(B_PAR,
                              obs = length(ecosystem_states),
                              alpha = alphas$vectors,
                              alpha_action = alphas$action,
                              alpha_obs = alphas$obs,
                              alpha_index = alphas$index)

      for (index_config in seq(nrow(filtered_scenarios_now))) {
        config <- filtered_scenarios_now[index_config,]
        #save results at tmax only ####
        summ_results <- data.frame(mean_value = output[[1]])

        summ_results$sd_value = 0

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
