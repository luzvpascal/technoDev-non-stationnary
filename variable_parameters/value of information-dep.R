library(ggplot2)
library(tidyverse)
library(dplyr)
library(latex2exp)
library(MDPtoolbox)

#load necessary functions ####
source("helper functions/functions variable parameters - MDP.R")

## load global variables ####
source("global variables.R")

#build transition and reward function ####
source("variable_parameters/build transition and reward function.R")

#solve corresponding MDP####
solution_list <- list()
for (index_MDP in seq(length(transition_matrix_list))){
  #solve current MDP
  solution <- mdp_value_iteration(transition_matrix_list[[index_MDP]],
                                  Reward,
                                  gamma)
  solution_list[[index_MDP]] <- solution
}

#test the deploy all the time strategy
solution$policy <- rep(2,length(solution$policy))
solution_list[[index_MDP+1]] <- solution
##################
# VOI analysis####
##################
voi_data <- data.frame()

for (index_MDP_true in seq(length(transition_matrix_list))){
  print(index_MDP_true)
  for (index_MDP_test in seq(length(solution_list))){
    #apply policy of index_MDP_test to index_MDP_true
    solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                            Reward,
                                            gamma,
                                            solution_list[[index_MDP_test]]$policy)
    voi_data_current <- data.frame(index_MDP_true=index_MDP_true,
                                   index_MDP_test=index_MDP_test,
                                   max_value=solution_list[[index_MDP_true]]$V[tuple_to_index(1,8, N_ecosystem + 1)],
                                   test_value=solution_test[tuple_to_index(1,8, N_ecosystem + 1)]
    )

    voi_data <- rbind(voi_data,
                      voi_data_current)
  }
}

all_scenarios_dep <- rbind(all_scenarios,
                       data.frame(delta_t_crit_K=0, scenario="all"))
voi_data_analysis <- voi_data %>%
  mutate(true_delta_crit = all_scenarios_dep$delta_t_crit_K[index_MDP_true],
         test_delta_crit = all_scenarios_dep$delta_t_crit_K[index_MDP_test],
         true_IPCC = all_scenarios_dep$scenario[index_MDP_true],
         test_IPCC = all_scenarios_dep$scenario[index_MDP_test],
  )%>%
  group_by(index_MDP_true, index_MDP_test)%>%
  summarize(max_value = mean(max_value),
            test_value = mean(test_value)) %>%
  mutate(r_EVPI=((max_value-test_value)/max_value))

voi_plot <- voi_data_analysis %>%
  ggplot(aes(x=index_MDP_true , y = index_MDP_test))+
  geom_tile(aes(fill=r_EVPI*100))+
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  coord_equal()+
  labs(title = "",
       x = "True scenario",
       y = "Assumed scenario",
       fill = "rEVPI (%)")+
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.ticks = element_blank(),    # Remove axis ticks
    axis.line = element_blank(),      # Remove axis lines
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()
  )

for (k in seq(0,length(unique(temperature_data$scenario)))){
  voi_plot <- voi_plot+
  geom_vline(xintercept = length(tested_delta_t_crit_K)*(k)+0.5)+
  geom_hline(yintercept = length(tested_delta_t_crit_K)*(k)+0.5)
}
for (k in seq(length(unique(all_scenarios_dep$scenario)))){
  scenario_text <- unique(all_scenarios_dep$scenario)[k]
  if (scenario_text!="all"){
    position_text <- length(tested_delta_t_crit_K)*(k-1/2)+0.5
    voi_plot <- voi_plot+
      annotate("text", x=position_text, y=-1,label=scenario_text)+
      annotate("text", y=position_text, x=-1,label=scenario_text, angle=90)
  }
}
voi_plot

voi_data_best_policy <- voi_data_analysis %>%
  ungroup()%>%
  group_by(index_MDP_test)%>%
  summarize(meanEVPI=mean(r_EVPI))%>%
  arrange(meanEVPI)
voi_data_best_policy
