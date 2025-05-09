library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")


#select the analyzed scenarios parameters####
delta_t_crit_r_now <- 2.5
delta_t_crit_K_now <- 1
sigmoid_bool_r_now <- TRUE
sigmoid_bool_K_now <- TRUE
DEP_EFFECT <- c(0, 1,2)
# scen_tested <- c("SSP1_1_9", "SSP2_4_5","SSP5_8_5")
scen_tested <- c("SSP1_1_9", "SSP5_8_5")

actions_names <- c("0","1","2")
tested_scenarios_names <- factor(c("Non-stationary (optimal)",
                                   "Best stationary strategy (at 2015)",
                                   "Best stationary strategy (at 2100)",
                                   "Best stationary strategy (average temperature)"),
                                 levels=c("Non-stationary (optimal)",
                                          "Best stationary strategy (at 2015)",
                                          "Best stationary strategy (average temperature)",
                                          "Best stationary strategy (at 2100)"))

## read best stationary strategies #####
voi_data_full <- read.csv(paste0("res/cost_of_stationarity_deployment_gamma_",gamma,".csv"),
                          header = TRUE)

voi_data_full <- voi_data_full %>%
  filter(delta_t_crit_r == delta_t_crit_r_now,
         delta_t_crit_K == delta_t_crit_K_now,
         sigmoid_bool_r == sigmoid_bool_r_now,
         sigmoid_bool_K == sigmoid_bool_K_now,
         scen %in% scen_tested
  )

data_temp <- read.csv( "data IPCC/summarized_data.csv")

## make a list of plots ####
list_plots <- list()
for (index_scen in seq_along(scen_tested)){
  # r and K plots ####
  data <- data_temp %>%
    filter(scenario ==scen_tested[index_scen])%>%
    mutate(K_eff = K_function(K_min, K_max, delta_t_crit_K_now,
                              Mean,sigmoid_bool_K_now),
           K_eff_low = K_function(K_min, K_max, delta_t_crit_K_now,
                                  X5.,sigmoid_bool_K_now),
           K_eff_up = K_function(K_min, K_max, delta_t_crit_K_now,
                                 X95.,sigmoid_bool_K_now),
           r_eff = K_function(r_min, r_max, delta_t_crit_r_now,
                              Mean,sigmoid_bool_r_now),
           r_eff_low = K_function(r_min, r_max, delta_t_crit_r_now,
                                  X5.,sigmoid_bool_r_now),
           r_eff_up = K_function(r_min, r_max, delta_t_crit_r_now,
                                 X95.,sigmoid_bool_r_now)
    )

  coeff <- 0.2
  K_R_plot <- ggplot(data)+
    geom_ribbon(aes(x =Year,
                    ymin=K_eff_low, ymax=K_eff_up),
                    fill="salmon",
                alpha=0.1)+
    geom_line(aes(x=Year, y=K_eff), col="salmon",
              linewidth = 1.1)+
    # geom_ribbon(aes(x =Year,
    #                 ymin=r_eff_low/coeff, ymax=r_eff_up/coeff),
    #                 fill="blue4",
    #             alpha=0.1)+
    # geom_line(aes(x=Year, y=r_eff/coeff), col="blue4",
    #           linewidth = 1.1)+
    labs(y=TeX("Carrying capacity $(K)$"),
         col="")+
    guides(fill = "none")+
    theme_classic()+
    theme(legend.position = "none")+
    scale_y_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(
        TeX("$K_{min} = 0$"),
        # TeX("$\\frac{K_{max} + K_{min}}{2} = 0.5$"),
        TeX("0.5"),
        TeX("$K_{max} = 1$")
      ),
      limits= c(0,1)
      # , sec.axis = sec_axis(~.*coeff, name=TeX("Growth rate $(r)$"))
    )


  ## Call the function to generate transition matrices and rewards
  scen <- scen_tested[index_scen]
  result <- generate_transition_reward_list_cost_stationarity(
    scen = scen,
    climate_scenarios = climate_scenarios,
    temperature_data = temperature_data,
    ecosystem_states = ecosystem_states,
    temperature_states = temperature_states,
    DEP_EFFECT = DEP_EFFECT,
    r_min = r_min,
    r_max = r_max,
    delta_t_crit_r = delta_t_crit_r_now,
    sigmoid_bool_r = sigmoid_bool_r_now,
    K_min = K_min,
    K_max = K_max,
    delta_t_crit_K = delta_t_crit_K_now,
    sigmoid_bool_K = sigmoid_bool_K_now,
    time_step = time_step,
    sigma_eco = sigma_eco,
    cost_deploy = cost_deploy
  )

  transition_matrix_list <- result$transition_matrix_list
  Reward <- result$Reward

  ids_tested <- c(1, voi_data_full$stationary_test[index_scen])
  #compute max_value for this configuration
  solution_list <- list()
  for (index in seq_along(ids_tested)){
    solution <- mdp_value_iteration(transition_matrix_list[[ids_tested[index]]],
                                    Reward,
                                    gamma)

    solution_list[[index]] <- data.frame(state=seq(length(solution$policy)),
                                         action=solution$policy,
                                         id=ids_tested[index])
  }

  solution_list_data <- bind_rows(solution_list)
  solution_list_data <- solution_list_data %>%
    rowwise()%>%
    mutate(eco=index_to_eco(state, N_ecosystem+1),
           eco = ecosystem_states[eco],
           time=index_to_year(state, N_ecosystem+1),
           action=actions_names[action],
           id = tested_scenarios_names[id])

  strategies <- solution_list_data%>%
    # filter(id=="Non-stationary (optimal)")%>%
    ggplot()+
    geom_tile(aes(x=time+2014, y=eco, fill=action))+
    scale_fill_manual(values=c("grey","lightgreen","darkgreen"))+
    facet_wrap(~id, nrow=2)+
    theme_classic() +
    labs(
      title = "",
      x = TeX("Time"),
      y = TeX("Coral cover (s)"),
      fill = TeX("Local temperature mitigation ($\\Delta T^{techno}$ °C)   ")
    ) +
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  ## simulate to obtain ecosystem state ####
  # Define the number of time steps and simulations
  num_time_steps <- 85  # Set the desired number of time steps
  num_simulations <- 1000  # Set the number of simulations to get an average

  # Initialize a list to store the average state for each MDP
  average_state_over_time <- list()
  average_value_over_time <- list()

  for (index in seq_along(solution_list)) {
    # Extract the policy for the current MDP
    policy <- solution_list[[index]]$action

    # Initialize a matrix to store state occurrences at each time step
    state_counts <- matrix(0, nrow = num_time_steps, ncol = (N_ecosystem + 1))
    value_counts <- matrix(0, nrow = num_time_steps, ncol = num_simulations)

    for (sim in 1:num_simulations) {
      current_state <- tuple_to_index(1,8, N_ecosystem + 1)

      for (t in 1:num_time_steps) {
        # Record the current state
        eco_state <- index_to_eco(current_state,N_ecosystem+1)
        state_counts[t, eco_state] <- state_counts[t, eco_state] + 1

        # Determine the action to take from the policy
        action <- policy[current_state]

        if (t > 1){
          value_counts[t,sim] <- value_counts[t-1,sim] + gamma**(t-1)*Reward[current_state, action]
        } else {
          value_counts[t,sim] <- Reward[current_state, action]
        }
        # Get the next state based on the non-stationary transition matrix
        next_state_probs <- transition_matrix_list[[1]][current_state, , action]
        current_state <- sample(seq_along(next_state_probs), 1, prob = next_state_probs)
      }
    }
    state_counts <- state_counts/num_simulations
    state_counts <- as.data.frame(state_counts)
    names(state_counts) = ecosystem_states
    state_counts$time <- seq(nrow(state_counts))


    value_counts <- as.data.frame(value_counts)
    value_counts$time <- seq(nrow(value_counts))

    # Calculate the average state by normalizing state_counts at each time step
    state_counts <- pivot_longer(state_counts, !time, names_to = "eco_state",values_to = "freq")
    state_counts_summary <- state_counts %>%
      rowwise()%>%
      filter(freq>0)%>%
      mutate(eco_state=as.numeric(eco_state))%>%
      group_by(time) %>%
      summarize(
        mean_eco_state = weighted.mean(eco_state, freq),
        sd_eco_state = sqrt(sum(freq * (eco_state - weighted.mean(eco_state, freq))^2) / sum(freq))
      ) %>%
      rowwise()%>%
      mutate(lower_eco_state=max(0, mean_eco_state - sd_eco_state*1.96/sqrt(num_simulations)),
             upper_eco_state=min(1,mean_eco_state + sd_eco_state*1.96/sqrt(num_simulations)),
             id =  ids_tested[index])


    #calculate the mean and sd value at each time step
    value_counts_summary <- pivot_longer(value_counts, !time, names_to = "col_name",values_to = "value")
    value_counts_summary <- value_counts_summary %>%
      group_by(time) %>%
      summarize(
        mean_value = mean(value),
        sd_value = sd(value)
      ) %>%
      rowwise()%>%
      mutate(lower_value = mean_value - sd_value,
             upper_value = mean_value + sd_value,
             id = ids_tested[index])


    average_state_over_time[[index]] <- state_counts_summary
    average_value_over_time[[index]] <- value_counts_summary
  }
  average_state_over_time <- bind_rows(average_state_over_time)
  average_value_over_time <- bind_rows(average_value_over_time)

  #average state plot ####
  average_state_plot <- average_state_over_time %>%
    rowwise()%>%
    mutate(id = tested_scenarios_names[id],
           time=time+2014)%>%
    ggplot()+
    geom_ribbon(aes(x=time, ymin=lower_eco_state, ymax=upper_eco_state,
                    group=id,fill=id), alpha=0.1)+
    geom_line(aes(x=time, y=mean_eco_state,group=id,col=id))+
    theme_classic() +
    labs(
      title = "",
      x = TeX("Time"),
      y = TeX("Coral cover (s)"),
      fill="Strategy",
      col="Strategy"
    ) +
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()
    )+
    lims(y=c(0,1))

  #average value plot ####
  average_value_plot <- average_value_over_time %>%
    rowwise()%>%
    mutate(id = tested_scenarios_names[id],
           time=time+2014)%>%
    ggplot()+
    geom_ribbon(aes(x=time, ymin=lower_value, ymax=upper_value,
                    group=id,fill=id), alpha=0.1)+
    geom_line(aes(x=time, y=mean_value,group=id,col=id))+
    theme_classic() +
    labs(
      title = "",
      x = TeX("Time"),
      y = TeX("Cumulated rewards (V)"),
      fill="Strategy",
      col="Strategy"
    ) +
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()
    )

  state_value_plot <- ggpubr::ggarrange(
    average_state_plot,
    average_value_plot,
    ncol=2,
    align="hv",
    common.legend = TRUE,
    legend = "none")

  list_plots[[index_scen]] <- ggpubr::ggarrange(
                    # K_R_plot+theme(legend.position = "none"),
                    strategies +theme(legend.position = "none"),
                    average_state_plot+theme(legend.position = "none"),
                    # state_value_plot+theme(legend.position = "none"),
                    average_value_plot+theme(legend.position = "none"),
                    nrow=1,
                    # ,
                    align="hv"
                    # widths = c(1,2)
                    )
}


voi_strategies <- ggpubr::ggarrange(list_plots[[1]] ,
                              list_plots[[2]],
                              # list_plots[[3]],
                              ncol=1)
# ,
#                               align="hv")
voi_strategies
ggsave(plot = voi_strategies,
       filename = "figures/voi_cost_of_stationarity_strategies.svg",
       width = 24,
       height = 16,
       units = "cm")
#
# ## save legends ####
# l1 <- get_legend(strategies)
# l2 <- get_legend(average_state_plot)
#
# legends <- ggpubr::ggarrange(l1 , l2, ncol = 1)
#
# ggsave(plot = legends,
#        filename = "figures/voi_cost_of_stationarity_legends.svg",
#        width = 15,
#        height = 15,
#        units = "cm")
