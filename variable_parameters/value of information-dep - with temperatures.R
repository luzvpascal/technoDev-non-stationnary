library(ggplot2)
library(tidyverse)
library(dplyr)
library(latex2exp)
library(MDPtoolbox)

#load necessary functions ####
source("variable_parameters/functions variable parameters - MDP.R")
source("variable_parameters/write_POMDPx.R")
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
N_temperatures <- 10
Temp_max <- round(max(temperature_data$X95.))
Temp_min <- round(min(temperature_data$X5.))
temperature_states <- seq(Temp_min,Temp_max,
                          (Temp_max-Temp_min)/N_temperatures)
##time steps####
time_step <- 1
time_states <- unique(temperature_data$Year)
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
                col=factor(tested_delta_t_crit)))+
  theme_bw()+
  labs(x=TeX("$\\Delta T$"),
       y=TeX("Maximum capacity ($K$)"),
       col=TeX("$\\Delta T_{crit}$"))

###################################################################
#build transition function for each tested value of delta_t_crit_K#
###################################################################
transition_ecosystem_list <- list()
for (index_MDP in seq(length(tested_delta_t_crit_K))){
  delta_t_crit_K_now <- tested_delta_t_crit_K[index_MDP]
  sik_bar_now <- sik_bar_function(ecosystem_states,
                                  temperature_states,
                                  DEP_EFFECT,
                                  r_min,
                                  r_max,
                                  delta_t_crit_r,
                                  sigmoid_bool_r,
                                  K_min,
                                  K_max,
                                  delta_t_crit_K_now,
                                  sigmoid_bool_K,
                                  time_step)
  transition_ecosystem_now <- transition_function_ecosystem(ecosystem_states,
                                                             temperature_states,
                                                             DEP_EFFECT,
                                                             sik_bar_now,
                                                             sigma_eco)
  transition_ecosystem_list[[index_MDP]] <- transition_ecosystem_now
}


#######################################################
#build transition function for each climate trajectory#
#######################################################
transition_temp_list <- list()
tested_scenarios <- unique(temperature_data$scenario)
for (index_MDP in seq(length(tested_scenarios))){
  scen <- tested_scenarios[index_MDP]

  temperature_data_filter <- temperature_data %>%
    filter(scenario == scen)

  transition_temperatures_now <- transition_function_temperatures(temperature_states,
                                                                  time_states,
                                                                  temperature_data_filter)
  transition_temp_list[[index_MDP]] <- transition_temperatures_now
}

#################################
#build transition function time #
#################################
transition_time_list <- transition_function_times(time_states)

############################################
## build transitions function technology####
############################################
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2),
                           matrix(c(1-p_dev, 0, p_dev, 1), ncol=2))
transition_failure <- list(diag(2),
                           diag(2))
transition_tech <- list(transition_success, transition_failure)

#############
# reward ####
#############
cost_deploy <- 0.1
reward_BAU <- ecosystem_states
reward <- matrix(ncol = 0, nrow = length(reward_BAU))
for (dep_effect in DEP_EFFECT){
  reward_action <- matrix(reward_BAU - dep_effect**2*cost_deploy, ncol=1)
  reward <- cbind(reward, reward_action)
}

reward_time_list <- list()
for (t in seq(length(time_states)-1)){
  reward_time_list[[t]] <- reward #transition function for each time step
}
reward_time_list[[length(time_states)]] <- matrix(0, ncol = ncol(reward),
                                                  nrow=nrow(reward))


############################
# solve POMDP ####

## write POMDPx file ####
TR_FUNCTION_ECO <- transition_ecosystem_list
TR_FUNCTION_TEMP <- transition_temp_list
TR_FUNCTION_TIME <- transition_time_list
TR_FUNCTION_TECH <- transition_tech

B_FULL_ECO <- c(rep(0, length(ecosystem_states)-1), 1)
B_FULL_TEMP <- c(1, rep(0, length(temperature_states)-1))
B_FULL_TIME <- c(1, rep(0, length(time_states)-1))
B_FULL_TECH <- c(1, rep(0, length(tech_states)-1))

B_PAR_ECO <- rep(1, length(TR_FUNCTION_ECO))/length(TR_FUNCTION_ECO)
B_PAR_TEMP <- rep(1, length(TR_FUNCTION_TEMP))/length(TR_FUNCTION_TEMP)
B_PAR_TECH <- rep(1, length(transition_tech))/length(transition_tech)

REW <- reward_time_list
GAMMA <- 0.99
file_name <- paste0("variable_parameters/pomdpx/POMDPfull_eco_temp_time_tech")
FILE <- paste0(file_name, ".pomdpx")

write_full_POMDP(TR_FUNCTION_ECO,
                 TR_FUNCTION_TEMP,
                 TR_FUNCTION_TIME,
                 TR_FUNCTION_TECH,
                 B_FULL_ECO,
                 B_FULL_TEMP,
                 B_FULL_TIME,
                 B_FULL_TECH,
                 B_PAR_ECO,
                 B_PAR_TEMP,
                 B_PAR_TECH,
                 REW,
                 GAMMA,
                 FILE)

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

OUTPUT_FILE <- paste0(file_name, ".policyx")

cmd <- paste(path_to_sarsop,
             "--precision", 0.0000001,
             "--timeout", 200,
             "--output", OUTPUT_FILE,
             FILE,
             sep=" ")
system(cmd)

library(ggplot2)
library(tidyverse)
library(dplyr)
library(latex2exp)
library(MDPtoolbox)

#load necessary functions ####
source("variable_parameters/functions variable parameters - MDP.R")
source("variable_parameters/write_POMDPx.R")
#######################################
# define common parameters of model####
#######################################

## discrete ecosystem states ####
N_ecosystem <- 5
ecosystem_states <- seq(0,1,1/N_ecosystem)
sigma_eco <- 0.2

## transition temperatures ####
temperature_data <- read.csv("data IPCC/summarized_data.csv")
temperature_data <- filter(temperature_data,
                           scenario != "Historical")
temperature_data$Year <- temperature_data$Year-min(temperature_data$Year)+1
## discrete temperature variations####
N_temperatures <- 10
Temp_max <- round(max(temperature_data$X95.))
Temp_min <- round(min(temperature_data$X5.))
temperature_states <- seq(Temp_min,Temp_max,
                          Temp_max/N_temperatures)
##time steps####
time_step <- 1
time_states <- unique(temperature_data$Year)
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
                col=factor(tested_delta_t_crit)))+
  theme_bw()+
  labs(x=TeX("$\\Delta T$"),
       y=TeX("Maximum capacity ($K$)"),
       col=TeX("$\\Delta T_{crit}$"))

###################################################################
#build transition function for each tested value of delta_t_crit_K#
###################################################################
transition_ecosystem_list <- list()
for (index_MDP in seq(length(tested_delta_t_crit_K))){
  delta_t_crit_K_now <- tested_delta_t_crit_K[index_MDP]
  sik_bar_now <- sik_bar_function(ecosystem_states,
                                  temperature_states,
                                  DEP_EFFECT,
                                  r_min,
                                  r_max,
                                  delta_t_crit_r,
                                  sigmoid_bool_r,
                                  K_min,
                                  K_max,
                                  delta_t_crit_K_now,
                                  sigmoid_bool_K,
                                  time_step)
  transitions_ecosystem_now <- transition_function_ecosystem(ecosystem_states,
                                                             temperature_states,
                                                             DEP_EFFECT,
                                                             sik_bar_now,
                                                             sigma_eco)
  transition_ecosystem_list[[index_MDP]] <- transitions_ecosystem_now
}


#######################################################
#build transition function for each climate trajectory#
#######################################################
transition_temp_list <- list()
tested_scenarios <- unique(temperature_data$scenario)
for (index_MDP in seq(length(tested_scenarios))){
  scen <- tested_scenarios[index_MDP]

  temperature_data_filter <- temperature_data %>%
    filter(scenario == scen)

  transition_temperatures_now <- transition_function_temperatures(temperature_states,
                                                                  time_states,
                                                                  temperature_data_filter)
  transition_temp_list[[index_MDP]] <- transition_temperatures_now
}

#################################
#build transition function time #
#################################
transition_time_list <- transition_function_times(time_states)

############################################
## build transitions function technology####
############################################
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2),
                           matrix(c(1-p_dev, 0, p_dev, 1), ncol=2))
transition_failure <- list(diag(2),
                           diag(2))
transition_tech <- list(transition_success, transition_failure)

#############
# reward ####
#############
cost_deploy <- 0.1
reward_BAU <- ecosystem_states
reward <- matrix(ncol = 0, nrow = length(reward_BAU))
for (dep_effect in DEP_EFFECT){
  reward_action <- matrix(reward_BAU - dep_effect**2*cost_deploy, ncol=1)
  reward <- cbind(reward, reward_action)
}

reward_time_list <- list()
for (t in seq(length(time_states)-1)){
  reward_time_list[[t]] <- reward #transition function for each time step
}
reward_time_list[[length(time_states)]] <- matrix(0, ncol = ncol(reward),
                                                  nrow=nrow(reward))


############################
# solve POMDP ####

## write POMDPx file ####
TR_FUNCTION_ECO <- transition_ecosystem_list
TR_FUNCTION_TEMP <- transition_temp_list
TR_FUNCTION_TIME <- transition_time_list
TR_FUNCTION_TECH <- transition_tech

B_FULL_ECO <- c(rep(0, length(ecosystem_states)-1), 1)
B_FULL_TEMP <- c(1, rep(0, length(temperature_states)-1))
B_FULL_TIME <- c(1, rep(0, length(time_states)-1))
B_FULL_TECH <- c(1, rep(0, length(tech_states)-1))

B_PAR_ECO <- rep(1, length(TR_FUNCTION_ECO))/length(TR_FUNCTION_ECO)
B_PAR_TEMP <- rep(1, length(TR_FUNCTION_TEMP))/length(TR_FUNCTION_TEMP)
B_PAR_TECH <- rep(1, length(transition_tech))/length(transition_tech)

REW <- reward_time_list
GAMMA <- 0.99
file_name <- paste0("variable_parameters/pomdpx/POMDPfull_eco_temp_time_tech")
FILE <- paste0(file_name, ".pomdpx")

write_full_POMDP(TR_FUNCTION_ECO,
                 TR_FUNCTION_TEMP,
                 TR_FUNCTION_TIME,
                 TR_FUNCTION_TECH,
                 B_FULL_ECO,
                 B_FULL_TEMP,
                 B_FULL_TIME,
                 B_FULL_TECH,
                 B_PAR_ECO,
                 B_PAR_TEMP,
                 B_PAR_TECH,
                 REW,
                 GAMMA,
                 FILE)

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

OUTPUT_FILE <- paste0(file_name, ".policyx")

cmd <- paste(path_to_sarsop,
             "--precision", 0.0000001,
             "--timeout", 200,
             "--output", OUTPUT_FILE,
             FILE,
             sep=" ")
system(cmd)
