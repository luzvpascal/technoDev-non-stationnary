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
source("helper functions/generate_transition_reward_list_cost_stationarity.R")

## Set up constants and parameters for technology

res_file <- paste0("res/value_uncertain_climate_best_non_adaptive_gamma_",gamma,".csv")

start <- Sys.time()
## Loop over climate and temperature scenarios
voi_data <- data.frame()

transition_matrix_list <- list()
solution_list <- list()
for (index_config in seq(nrow(scenarios_uncertain_climate))) {
    print(index_config)
    #set scenarios variables ####
    config <- scenarios_uncertain_climate[index_config,]
    delta_t_crit_r <- config$delta_t_crit_r
    delta_t_crit_K <- config$delta_t_crit_K
    sigmoid_bool_r <- config$sigmoid_bool_r
    sigmoid_bool_K <- config$sigmoid_bool_K
    DEP_EFFECT <- c(0, config$dep_effect,config$dep_effect*2)
    scen <- config$scenario
    temperature_data_filter <- temperature_data %>% filter(scenario == scen)

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


for (index_MDP_true in seq(length(transition_matrix_list))){
  print(index_MDP_true)

  config_true <- scenarios_uncertain_climate[index_MDP_true,]
  for (index_MDP_test in seq(length(solution_list))){
    config_test <- scenarios_uncertain_climate[index_MDP_test,]
    #apply policy of index_MDP_test to index_MDP_true
    solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                            Reward,
                                            gamma,
                                            solution_list[[index_MDP_test]]$policy)

    voi_data_current <- data.frame(max_value=solution_list[[index_MDP_true]]$V[length(ecosystem_states)],
                                   test_value=solution_test[length(ecosystem_states)]
    )

    voi_data_current$delta_t_crit_r <- config_true$delta_t_crit_r
    voi_data_current$delta_t_crit_K <- config_true$delta_t_crit_K
    voi_data_current$sigmoid_bool_r <- config_true$sigmoid_bool_r
    voi_data_current$sigmoid_bool_K <- config_true$sigmoid_bool_K
    voi_data_current$DEP_EFFECT <- config_true$dep_effect
    voi_data_current$scen <- config_true$scenario

    voi_data_current$scen_test <- config_test$scenario


    voi_data <- rbind(voi_data,
                      voi_data_current)
  }
}
write.csv(voi_data, res_file, row.names = FALSE)
end <- Sys.time()

print(end-start)

###############################################################################
#create a results data frame for each experiment####
###############################################################################
voi_solution <- data.frame()
experiments_list <- list()
for (k in seq_along(climate_scenarios)){
  experiments <- combn(climate_scenarios,k)
  for (index_exp in seq(ncol(experiments))){

    scenanarios_now <- experiments[,index_exp]
    experiments_list[[length(experiments_list)+1]] <- scenanarios_now
    #filter responses
    voi_data_uncertain_climate_exp <- voi_data %>%
      filter(scen %in% scenanarios_now,
             scen_test %in% scenanarios_now
      )

    #best perf scenario
    best_perf_scenario <- voi_data_uncertain_climate_exp %>%
      group_by(scen_test)%>%
      summarise(mean_test_value=mean(test_value),
                sd_test_value=sd(test_value))%>%
      ungroup()%>%
      slice_max(order_by = mean_test_value, with_ties = FALSE)

    # Filter voi_data_uncertain_response to keep rows that match any row in best_perf_scenario
    filtered_voi_data_climate <- voi_data_uncertain_climate_exp %>%
      semi_join(best_perf_scenario)

    filtered_voi_data_climate <- filtered_voi_data_climate%>%
      summarise(rEVPI=(mean(max_value)-mean(test_value ))/mean(max_value))
    filtered_voi_data_climate$k <- k

    voi_solution <- rbind(voi_solution,
                          filtered_voi_data_climate)
  }

}

mean_values <- voi_solution %>%
  group_by(k) %>%
  summarise(rEVPI = mean(rEVPI))

voi_solution%>%
  ggplot()+
  geom_violin(aes(y=rEVPI*100, x="",))+
  facet_wrap(~k, ncol = 5)+
  geom_point(data=mean_values,
             aes(x = "", y = rEVPI * 100),
             shape=23, size = 3, fill = "black")

