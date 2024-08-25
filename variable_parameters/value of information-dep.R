library(ggplot2)
library(tidyverse)
library(dplyr)
library(latex2exp)
library(MDPtoolbox)

#load necessary functions ####
source("variable_parameters/functions variable parameters - MDP.R")

#######################################
# define common parameters of model####
#######################################

## discrete ecosystem states ####
N_ecosystem <- 10
ecosystem_states <- seq(0,1,1/N_ecosystem)
sigma_eco <- 0.2

## transition temperatures ####
temperature_data <- read.csv("data IPCC/summarized_data.csv")
temperature_data <- filter(temperature_data,
                           scenario != "Historical")
temperature_data$Year <- temperature_data$Year-min(temperature_data$Year)+1
## discrete temperature variations####
N_temperatures <-100
Temp_max <- round(max(temperature_data$X95.))
temperature_states <- seq(0,Temp_max,
                          Temp_max/N_temperatures)
##time steps####
time_step <- 1

## growth rate parameters ####
r_min <- -0.2
r_max <- 0.2
delta_t_crit_r <- 1
sigmoid_bool_r <- FALSE
## capacity parameters ####
K_min <- 0
K_max <- 1
sigmoid_bool_K <- TRUE

## deployment effects ####
DEP_EFFECT <- c(0,1)
cost_deploy <- 0.05

## common reward function ####
Reward <- reward_function(ecosystem_states,
                          seq(max(temperature_data$Year)),
                          DEP_EFFECT,
                          cost_deploy)

gamma <- 0.9999
#################################################
# define grid of variable parameters of model####
#################################################
tested_delta_t_crit_K <- seq(1.5,4,0.5) #tested values of delta_t_crit_K
# tested_delta_t_crit_K <- c(1,2) #tested values of delta_t_crit_K
tested_delta <- seq(-1, 6, 0.01)
K_eff_data <- data.frame()
for (tested_delta_t_crit in tested_delta_t_crit_K){
  K_eff <- c()
  for (delta_t in tested_delta){
    K_eff <- c(K_eff, K_function(K_min, K_max, tested_delta_t_crit,
                                 delta_t,sigmoid_bool_K))
  }

  K_eff_data <- rbind(K_eff_data,
                      data.frame(temp=tested_delta,
                                 K_eff=K_eff,
                                 tested_delta_t_crit=tested_delta_t_crit)
  )
}

K_eff_plot <- ggplot(K_eff_data)+
  geom_line(aes(x=temp, y = K_eff,
                group=tested_delta_t_crit,
                col=tested_delta_t_crit))+
  theme_bw()+
  labs(x=TeX("$\\Delta T$"),
       y=TeX("Maximum capacity ($K$)"),
       col=TeX("$\\Delta T_{crit}$"))


#build transition function for each tested value of delta_t_crit_K
transition_matrix_list <- list()
#solve corresponding MDP
solution_list <- list()
all_scenarios <- expand.grid(delta_t_crit_K = tested_delta_t_crit_K,
                             scenario = unique(temperature_data$scenario))
for (index_MDP in seq(nrow(all_scenarios))){

  #build transition function
  scen <- all_scenarios$scenario[index_MDP]
  temperature_data_filter <- temperature_data %>%
    filter(scenario == scen)

  delta_t_crit_K_now <- all_scenarios$delta_t_crit_K[index_MDP]

  combined_transition_matrix <- transition_function(ecosystem_states,
                                                    temperature_states,
                                                    temperature_data_filter,
                                                    DEP_EFFECT,
                                                    r_min,
                                                    r_max,
                                                    delta_t_crit_r,
                                                    sigmoid_bool_r,
                                                    K_min,
                                                    K_max,
                                                    delta_t_crit_K_now,
                                                    sigmoid_bool_K,
                                                    time_step,
                                                    sigma_eco)

  transition_matrix_list[[index_MDP]] <- combined_transition_matrix

  #solve current MDP
  solution <- mdp_value_iteration(combined_transition_matrix,
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

for (index_MDP_true in seq(nrow(all_scenarios))){
  print(index_MDP_true)
  for (index_MDP_test in seq(nrow(all_scenarios)+1)){
    #apply policy of index_MDP_test to index_MDP_true
    solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                            Reward,
                                            gamma,
                                            solution_list[[index_MDP_test]]$policy)
    voi_data_current <- data.frame(index_MDP_true=index_MDP_true,
                                   index_MDP_test=index_MDP_test,
                                   # max_value=solution_list[[index_MDP_true]]$V[seq(length(ecosystem_states))],
                                   # test_value=solution_test[seq(length(ecosystem_states))]
                                   max_value=solution_list[[index_MDP_true]]$V[length(ecosystem_states)],
                                   test_value=solution_test[length(ecosystem_states)]
    )

    voi_data <- rbind(voi_data,
                      voi_data_current)

  }
}

all_scenarios <- rbind(all_scenarios,
                       data.frame(delta_t_crit_K=0, scenario="all"))
voi_data_analysis <- voi_data %>%
  mutate(true_delta_crit = all_scenarios$delta_t_crit_K[index_MDP_true],
         test_delta_crit = all_scenarios$delta_t_crit_K[index_MDP_test],
         true_IPCC = all_scenarios$scenario[index_MDP_true],
         test_IPCC = all_scenarios$scenario[index_MDP_test],
  )%>%
  group_by(index_MDP_true, index_MDP_test)%>%
  summarize(max_value = mean(max_value),
            test_value = mean(test_value)) %>%
  mutate(r_EVPI=((max_value-test_value)/max_value))
  # group_by(true_IPCC, test_IPCC)%>%
  # group_by(true_delta_crit, test_delta_crit)%>%
  # filter(max_value>0)%>%

breaks_countour_r_EVPI <- seq(0.1,1,0.1)
voi_plot <- voi_data_analysis %>%
  ggplot(aes(x=index_MDP_true , y = index_MDP_test))+
  # ggplot(aes(x=true_IPCC , y = test_IPCC))+
  # ggplot(aes(x=true_delta_crit , y = test_delta_crit))+
  geom_tile(aes(fill=r_EVPI*100))+
  scale_fill_gradient(low = "lightyellow", high = "red") +
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
for (k in seq(length(unique(all_scenarios$scenario)))){
  scenario_text <- unique(all_scenarios$scenario)[k]
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
  # group_by(test_IPCC)%>%
  # group_by(test_delta_crit)%>%
  summarize(meanEVPI=mean(r_EVPI))%>%
  arrange(meanEVPI)
voi_data_best_policy

density_plot <- voi_data_analysis %>%
  filter(index_MDP_test %in% voi_data_best_policy$index_MDP_test[c(1,5)]
         # index_MDP_test %in% tail(voi_data_best_policy,n=2)$index_MDP_test
         )%>%
  ggplot(aes(x = r_EVPI, color = factor(index_MDP_test), fill = factor(index_MDP_test))) +
  geom_density(alpha = 0.4) +  # Adjust alpha for transparency
  labs(
    title = "Density Plot of r_EVPI for each index_MDP_test",
    x = "r_EVPI",
    y = "Density",
    color = "Index MDP Test",
    fill = "Index MDP Test"
  ) +
  facet_wrap(~ index_MDP_test, ncol = 2) +  # Each density plot on a different row
  theme_minimal()

density_plot
## policies ####
values_df <- data.frame()
names_df <- expand.grid(states=ecosystem_states,
                        year=temperature_data_filter$Year)
for (index_MDP in seq(nrow(all_scenarios))){
  values_df_index <- data.frame(V=solution_list[[index_MDP]]$V,
                                pol=solution_list[[index_MDP]]$policy)
  values_df_index$X <- names_df$year
  values_df_index$Y <- names_df$states
  values_df_index$index_MDP <- index_MDP

  values_df <- rbind(values_df, values_df_index)
}

policy <- ggplot(values_df, aes(y = Y, x = X,
                                fill = as.factor(pol))) +
  facet_wrap(~index_MDP)+
  geom_tile() +
  theme_minimal() +
  scale_fill_manual(values = c("white", "palegreen", "darkgreen")) +
  labs(title = "",
       y = "State",
       x = "Time",
       fill = "policy")
policy

## simulate one trajectory ####
MDP_index <- 2
trajectory <- simulate_mdp_trajectory(transition_matrix_list[[MDP_index]],
                                      Reward,
                                      solution_list[[2]],
                                      N_ecosystem/2,
                                      max(temperature_data$Year))
trajectory <- trajectory %>%
  rowwise() %>%
  mutate(state_year=index_to_year(state, N_ecosystem+1),
         state_ecosystem=index_to_eco(state,N_ecosystem+1))%>%
  mutate(state_ecosystem=ecosystem_states[state_ecosystem])


ggplot(trajectory, aes(x = state_year, y = state_ecosystem))+
  geom_line()+
  lims(y=c(0,1))

## density estimates ####
time_states <-  temperature_data_filter$Year[-length( temperature_data_filter$Year)]
data_priors_total <- data.frame()
MDP_index <- 1
for (action_index in c(1,2)){
  prior <- rep(1, N_ecosystem+1)/(N_ecosystem+1)
  data_priors <- data.frame(prior=prior, states=ecosystem_states, t=0,action_index=action_index)
  for (t in time_states){
    states_index <- seq(tuple_to_index(t, 1, N_ecosystem+1), tuple_to_index(t, N_ecosystem+1, N_ecosystem+1))
    next_states_index <- seq(tuple_to_index(t+1, 1, N_ecosystem+1), tuple_to_index(t+1, N_ecosystem+1, N_ecosystem+1))


    mat_t <- transition_matrix_list[[MDP_index]][states_index,next_states_index,action_index]

    prior <- prior %*% mat_t
    data_priors_new <- data.frame(prior=c(prior), states=ecosystem_states, t=t, action_index=action_index)
    data_priors <- rbind(data_priors,
                         data_priors_new)
  }
  data_priors_total <- rbind(data_priors_total,
                             data_priors)
}

ggplot(
  data_priors_total,
  aes(x = states, y = prior, group = t, col=t))+
  geom_line()+
  facet_wrap(~action_index)
