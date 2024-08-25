library(ggplot2)
library(tidyverse)
library(dplyr)
library(latex2exp)
library(MDPtoolbox)
source("variable parameters/functions variable parameters - MDP.R")
## discrete ecosystem states ####
N_ecosystem <- 20
ecosystem_states <- seq(0,1,1/N_ecosystem)

## discrete temperature variations####
N_temperatures <-100
Temp_max <- 4
temperature_states <- seq(0,Temp_max,
                          Temp_max/N_temperatures)

##time steps####
# horizon <- 200
# N_times <- 100
time_step <- 1
# time_states <- seq(0, horizon, time_step)

## growth rate parameters ####
r_min <- -0.2
r_max <- 0.2
delta_t_crit_r <- 1
sigmoid_bool_r <- FALSE

## capacity parameters ####
K_min <- 0
K_max <- 1
delta_t_crit_K <- 2
sigmoid_bool_K <- TRUE

## actions parameters ####
DEP_EFFECT <- c(0,1)

sigma_eco <- 0.2

## growth rate plot ####
r_eff <- c()
r_deploy <- c()
for (delta_t in temperature_states){
  r_eff <- c(r_eff, r_function(r_min, r_max, delta_t_crit_r,
                               delta_t-DEP_EFFECT[1], sigmoid_bool_r))
  r_deploy<- c(r_deploy, r_function(r_min, r_max, delta_t_crit_r,
                                    delta_t-DEP_EFFECT[2], sigmoid_bool_r))
}

r_eff_data <- data.frame(temp=temperature_states,
                         BAU=r_eff
                         , Deploy=r_deploy
)
r_eff_data <- pivot_longer(r_eff_data,!temp,
                           names_to=c("strategy"),
                           values_to = "values")
r_eff_plot <- ggplot(r_eff_data)+
  geom_line(aes(x=temp, y = values,
                col=strategy,
                linetype = strategy))+
  theme_bw()+
  geom_hline(yintercept = 0,
             col="grey")+
  labs(x=TeX("$\\Delta T$"),
       y=TeX("Growth rate ($r$)"))
r_eff_plot
## capacity plot ####
K_eff <- c()
K_deploy <- c()
for (delta_t in temperature_states){
  K_eff <- c(K_eff, K_function(K_min, K_max, delta_t_crit_K,
                               delta_t-DEP_EFFECT[1], sigmoid_bool_K))
  K_deploy<- c(K_deploy, K_function(K_min, K_max, delta_t_crit_K,
                                    delta_t-DEP_EFFECT[2], sigmoid_bool_K))
}

K_eff_data <- data.frame(temp=temperature_states,
                         BAU=K_eff
                         , Deploy=K_deploy
)
K_eff_data <- pivot_longer(K_eff_data,!temp,
                           names_to=c("strategy"),
                           values_to = "values")
K_eff_plot <- ggplot(K_eff_data)+
  geom_line(aes(x=temp, y = values,
                col=strategy,
                linetype = strategy))+
  theme_bw()+
  labs(x=TeX("$\\Delta T$"),
       y=TeX("Maximum capacity ($K$)"))
K_eff_plot
#test functions
sik_bar <- sik_bar_function(ecosystem_states,
                 temperature_states,
                 DEP_EFFECT,
                 r_min,
                 r_max,
                 delta_t_crit_r,
                 sigmoid_bool_r,
                 K_min,
                 K_max,
                 delta_t_crit_K,
                 sigmoid_bool_K,
                 time_step)

# Convert the array into a data frame
sik_bar_df <- as.data.frame.table(sik_bar)
names(sik_bar_df) <- c("X", "Y", "Z", "value")

names_df <- expand.grid(states=ecosystem_states, temps=temperature_states, actions=DEP_EFFECT)

# Convert factors to numeric using as.numeric(levels(f))[f]
sik_bar_df$X <- names_df$states
sik_bar_df$Y <- names_df$temps
sik_bar_df$Z <- names_df$actions

# Create the ggplot
ggplot(sik_bar_df, aes(x = X, y = Y, fill = value)) +
  geom_tile() +
  facet_wrap(~ Z) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "",
       x = "Coral cover",
       y = "Temperature",
       fill = "Value")

## test transitions function of ecosystem states ####
transition_ecosystem <- transition_function_ecosystem(ecosystem_states,
                                          temperature_states,
                                          DEP_EFFECT,
                                          sik_bar,
                                          sigma_eco)

transition_ecosystem_df_total <- data.frame()
for (index_action in seq(DEP_EFFECT)){
  for (index_temp in seq(1,length(temperature_states),20)){
    transition_ecosystem_df <- as.data.frame.table(transition_ecosystem[[index_action]][,,index_temp])
    names(transition_ecosystem_df) <- c("X", "Y", "value")
    names_df <- expand.grid(states=ecosystem_states, states2=ecosystem_states)

    # Convert factors to numeric using as.numeric(levels(f))[f]
    transition_ecosystem_df$X <- names_df$states
    transition_ecosystem_df$Y <- names_df$states2
    transition_ecosystem_df$index_temp <- index_temp
    transition_ecosystem_df$index_action <- index_action

    transition_ecosystem_df_total <- rbind(transition_ecosystem_df_total,
                                           transition_ecosystem_df)
  }
}

# Create the ggplot
ggplot(transition_ecosystem_df_total, aes(y = X, x = Y, fill = value)) +
  geom_tile() +
  facet_wrap(~index_action+index_temp)+
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_equal()+
  labs(title = "",
       y = "Coral cover",
       x = "Coral cover next",
       fill = "Value")

## transition temperatures ####
temperature_data <- read.csv("data IPCC/summarized_data.csv")
temperature_data$Year <- temperature_data$Year-min(temperature_data$Year)+1
temperature_data_filter <- filter(temperature_data,
                                  # scenario == "Historical"|scenario=="SSP1_1_9")
                                  scenario == "Historical"|scenario=="SSP5_8_5")
                                  # scenario == "Historical"|scenario=="SSP1_1_9")

transition_temperatures <- transition_function_temperatures(temperature_states,
                                                            temperature_data_filter$Year,
                                                            temperature_data_filter)
transition_temperatures_df <- as.data.frame.table(transition_temperatures)
names(transition_temperatures_df) <- c("X", "Y", "value")
names_df <- expand.grid(time_states=temperature_data_filter$Year,
                        temp_states=temperature_states)

transition_temperatures_df$X <- names_df$time_states
transition_temperatures_df$Y <- names_df$temp_states

# Create the ggplot
ggplot(transition_temperatures_df, aes(x = X, y = Y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "",
       y = "Temperature",
       x = "Time",
       fill = "Value")

## build transition ecosystem over time ####
transition_ecosystem_time <- build_transition_ecosystem_time(transition_ecosystem, transition_temperatures)

transition_ecosystem_df_total <- data.frame()
for (index_action in seq(DEP_EFFECT)){
  for (index_time in  seq(1, length(temperature_data_filter$Year), 20)){
    transition_ecosystem_df <- as.data.frame.table(transition_ecosystem_time[[index_action]][,,index_time])
    names(transition_ecosystem_df) <- c("X", "Y", "value")
    names_df <- expand.grid(states=ecosystem_states, states2=ecosystem_states)

    # Convert factors to numeric using as.numeric(levels(f))[f]
    transition_ecosystem_df$X <- names_df$states
    transition_ecosystem_df$Y <- names_df$states2
    transition_ecosystem_df$index_time <- index_time
    transition_ecosystem_df$index_action <- index_action

    transition_ecosystem_df_total <- rbind(transition_ecosystem_df_total,
                                           transition_ecosystem_df)
  }
}

ggplot(transition_ecosystem_df_total, aes(y = X, x = Y, fill = value)) +
  geom_tile() +
  facet_wrap(~index_action+index_time)+
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  # coord_equal()+
  labs(title = "",
       y = "Coral cover",
       x = "Coral cover next",
       fill = "Value")

## transition function time states ####
transition_times <- transition_function_times(temperature_data_filter$Year)
transition_times_df <- as.data.frame.table(transition_times)
names_df <- expand.grid(states=temperature_data_filter$Year, states2=temperature_data_filter$Year)
names(transition_times_df) <- c("X", "Y", "value")
transition_times_df$X <- names_df$states
transition_times_df$Y <- names_df$states2

ggplot(transition_times_df, aes(y = X, x = Y, fill = as.factor(value))) +
  geom_tile() +
  theme_minimal() +
  coord_equal()+
  labs(title = "",
       y = "Time",
       x = "Time next",
       fill = "Value")

## combined_transition_matrix: times ecosystem state ####
combined_transition_matrix <- transition_function_ecosystem_time(transition_ecosystem_time, transition_times)

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
                                       delta_t_crit_K,
                                       sigmoid_bool_K,
                                       time_step,
                                       sigma_eco)
#define reward function
# Define parameters
cost_deploy <- 0.05

Reward <- reward_function(ecosystem_states,
                          temperature_data_filter$Year,
                          DEP_EFFECT,
                          cost_deploy)

gamma <- 0.9999
solution <- mdp_value_iteration(combined_transition_matrix,
                               Reward,
                               gamma)


values_df <-data.frame(V=solution$V,
                       pol=solution$policy)
names_df <- expand.grid(states=ecosystem_states,
                        states2=temperature_data_filter$Year)
values_df$X <- names_df$states
values_df$Y <- names_df$states2

breaks_countour_v <- seq(0,100,10)
values <- ggplot(values_df, aes(y = X, x = Y)) +
  geom_raster(aes(fill= V),interpolate = TRUE) +
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "",
       y = "State",
       x = "Time",
       fill = "Value") +
  geom_contour(aes(z = V),
               breaks = breaks_countour_v,
               colour="black") +
  metR::geom_text_contour(aes(z = V),
                          breaks = breaks_countour_v,
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 5,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.3))
values

policy <- ggplot(values_df, aes(y = X, x = Y,
                                fill = as.factor(pol))) +
  geom_tile() +
  theme_minimal() +
  scale_fill_manual(values = c("white", "palegreen", "darkgreen")) +
  labs(title = "",
       y = "State",
       x = "Time",
       fill = "policy")
policy


