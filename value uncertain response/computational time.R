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
source("helper functions/generate_transition_reward_list_cost_stationarity.R")

## Set up constants and parameters for technology
res_file <- paste0("res/computational_time_gamma_",gamma,".csv")


## Loop over climate and temperature scenarios
voi_data <- data.frame()
# for (scen in climate_scenarios){
scen <- climate_scenarios[3]
filtered_scenarios_now <- filtered_scenarios %>% filter(scenario==scen)
temperature_data_filter <- temperature_data %>% filter(scenario == scen)

transition_matrix_list_of_true_transitions <- list()
solution_list_of_optimal_strategies <- list()
solution_list_of_tested_strategies <- list()
index_tested_strategies <- 1
# for (index_config in seq(nrow(filtered_scenarios_now))) {
index_config  <- 1
#set scenarios variables ####
config <- filtered_scenarios_now[index_config,]
delta_t_crit_r <- config$delta_t_crit_r
delta_t_crit_K <- config$delta_t_crit_K
sigmoid_bool_r <- config$sigmoid_bool_r
sigmoid_bool_K <- config$sigmoid_bool_K
DEP_EFFECT <- c(0, config$dep_effect,config$dep_effect*2)
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
Reward <- result$Reward

#compute max_value for this configuration
start <- Sys.time()
solution <- mdp_finite_horizon(transition_matrix_list[[1]],
                                Reward,
                               gamma,
                               N=85,
                               h=rep(0,11*85))
end <- Sys.time()
print(end-start)


start <- Sys.time()
solution2 <- mdp_finite_horizon(transition_matrix_list[[4]][seq(11),seq(12,22),],
                                Reward[seq(11),],
                                gamma,
                               N=85,
                               h=rep(0,11))
end <- Sys.time()
print(end-start)

policy <- solution$policy[,1]
policy2 <- c(solution2$policy)

policy-policy2
