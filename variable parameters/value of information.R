library(ggplot2)
library(tidyverse)
library(dplyr)
library(latex2exp)
library(MDPtoolbox)

#load necessary functions ####
source("variable parameters/functions variable parameters - MDP.R")

#######################################
# define common parameters of model####
#######################################

## discrete ecosystem states ####
N_ecosystem <- 20
ecosystem_states <- seq(0,1,1/N_ecosystem)
sigma_eco <- 0.2

## transition temperatures ####
temperature_data <- read.csv("data IPCC/summarized_data.csv")
temperature_data <- filter(temperature_data,
                                  scenario == "Historical"|scenario=="SSP2_4_5")
                                  # scenario == "Historical"|scenario=="SSP5_8_5")
                                  # scenario=="SSP5_8_5")
temperature_data$Year <- temperature_data$Year-min(temperature_data$Year)+1
temperature_data_filter <- temperature_data
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
                          temperature_data_filter$Year,
                          DEP_EFFECT,
                          cost_deploy)

gamma <- 0.9999
#################################################
# define grid of variable parameters of model####
#################################################
# tested_delta_t_crit_K <- seq(1,4,1) #tested values of delta_t_crit_K
tested_delta_t_crit_K <- c(1,2) #tested values of delta_t_crit_K
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
for (index_MDP in seq(length(tested_delta_t_crit_K))){
  #build transition function
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
                                                    tested_delta_t_crit_K[index_MDP],
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

##################
# VOI analysis####
##################
voi_data <- data.frame()

for (index_MDP_true in seq(length(tested_delta_t_crit_K))){
  for (index_MDP_test in seq(length(tested_delta_t_crit_K))){
    #apply policy of index_MDP_test to index_MDP_true
    solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                            Reward,
                                            gamma,
                                            solution_list[[index_MDP_test]]$policy)
    voi_data_current <- data.frame(index_MDP_true=index_MDP_true,
                                   index_MDP_test=index_MDP_test,
                                   max_value=solution_list[[index_MDP_true]]$V[seq(length(ecosystem_states))],
                                   test_value=solution_test[seq(length(ecosystem_states))]
                                   )

    voi_data <- rbind(voi_data,
                      voi_data_current)

  }
}

voi_data_analysis <- voi_data %>%
  group_by(index_MDP_true, index_MDP_test)%>%
  filter(max_value>0)%>%
  summarize(r_EVPI=mean((max_value-test_value)/max_value))%>%
  mutate(true_delta_crit = tested_delta_t_crit_K[index_MDP_true],
         test_delta_crit = tested_delta_t_crit_K[index_MDP_test],
  )

breaks_countour_r_EVPI <- seq(0.1,1,0.1)
voi_data_analysis %>%
  ggplot(aes(x=true_delta_crit , y = test_delta_crit))+
  geom_tile(aes(fill=r_EVPI*100))+
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_equal()+
  labs(title = "",
       x = TeX("True $\\Delta T_{crit}$"),
       y = TeX("Assumed $\\Delta T_{crit}$"),
       fill = "rEVPI (%)")
  #+
  # geom_contour(aes(z = r_EVPI),
  #              breaks = breaks_countour_r_EVPI,
  #              colour="black") +
  # metR::geom_text_contour(aes(z = r_EVPI),
  #                         breaks = breaks_countour_r_EVPI,
  #                         min.size = 0,
  #                         skip=0,
  #                         stroke = 0.1,
  #                         rotate = FALSE,
  #                         size = 5,
  #                         fontface = "bold",
  #                         label.placer = metR::label_placer_fraction(frac = 0.3))

voi_data_best_policy <- voi_data_analysis %>%
  ungroup()%>%
  group_by(index_MDP_test)%>%
  summarize(meanEVPI=mean(r_EVPI))%>%
  arrange(meanEVPI)

## policies ####
values_df <- data.frame()
names_df <- expand.grid(states=ecosystem_states,
                       year=temperature_data_filter$Year)
for (index_MDP in seq(length(transition_matrix_list))){
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
