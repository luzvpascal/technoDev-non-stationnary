library(dplyr)
library(tidyverse)
## discrete POMDP formulation ####
source("~/technoDev non-stationnary/deployment POMDP/write_deployment_POMDPx.R")
source("~/technoDev non-stationnary/deployment POMDP/read_solutions.R")
source("~/technoDev non-stationnary/deployment POMDP/simulations.R")
## discrete ecosystem states ####
N_ecosystem <- 20
ecosystem_states <- seq(0,1,1/N_ecosystem)

## discrete temperature variations####
N_temperatures <-100
Temp_max <- 4
temperature_states <- seq(0,Temp_max,
                          Temp_max/N_temperatures)

##time steps####
horizon <- 200
N_times <- 100
time_step <- horizon/N_times
time_states <- seq(0, horizon, time_step)

### definition of transition function ####
#calculate sik_bar_BAU = si + si*r*(1-si/K) - deltaTk/eta*si**q/(si**q+b**q)
#calculate sik_bar_DEPLOY = si + si*r*(1-si/K) - (deltaTk - effect_deploy)/eta*si**q/(si**q+b**q)
## ecosystem dynamics ####
r_min <- -0.2
r_max <- 0.2
increment <- 1
sigma_eco <- 0.01
delta_t_crit_min <- 0+increment
delta_t_crit_max <- 1+increment
#ecosystem_dynamics####
r_function <- function(r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                       delta_t){
  if (delta_t<=delta_t_crit_min){
    r_max
  } else if (delta_t<=delta_t_crit_max){
    r_max - (delta_t-delta_t_crit_min)*(r_max-r_min)/(delta_t_crit_max-delta_t_crit_min)
  } else {
    r_min
  }
}

DEP_EFFECT <- c(0, 1)#DEPLOYMENT EFFECT FOR EACH ACTION
# DEP_EFFECT <- c(0, 0)#DEPLOYMENT EFFECT FOR EACH ACTION

ecosystem_dynamics <- function(x_t, r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                               delta_t, K, dep_effect, time_step){
  r_eff <- r_function(r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                      delta_t-dep_effect)
  return(x_t + time_step*(x_t*r_eff*(1-x_t/K)))
}

sik_bar <- array(dim=c(length(ecosystem_states), length(temperature_states), length(DEP_EFFECT)))

for (index_action in seq(length(DEP_EFFECT))){
  for (index_state in seq(length(ecosystem_states))){
    for(index_temp in seq(length(temperature_states))){
      sik_bar[index_state,index_temp, index_action] <-
          max(0,
              ecosystem_dynamics(ecosystem_states[index_state],
                                 r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                                 temperature_states[index_temp],
                                 K,
                                 DEP_EFFECT[index_action],
                                 time_step))
    }

  }
}

## calculate transition probabilities ecosystem ####
transition_ecosystem <- list()
for (index_action in seq(length(DEP_EFFECT))){
  transition_ecosystem_index_action <-  list()
  for(index_temp in seq(length(temperature_states))){
    transition_ecosystem_index_action[[index_temp]] <- matrix(nrow=length(ecosystem_states),
                                                              ncol=length(ecosystem_states))
    for (i in seq(length(ecosystem_states))){
      for (j in seq(length(ecosystem_states))){
        #transition
        transition_ecosystem_index_action[[index_temp]][i,j] <-
                          pnorm(ecosystem_states[j]+1/(2*N_ecosystem),
                          mean=sik_bar[i, index_temp,index_action],
                          sd=sigma_eco)-
                          pnorm(ecosystem_states[j]-1/(2*N_ecosystem),
                                mean=sik_bar[i, index_temp,index_action],
                                sd=sigma_eco)

        transition_ecosystem_index_action[[index_temp]][i,j] <- round(transition_ecosystem_index_action[[index_temp]][i,j], digits=3)
      }
      #normalise
      transition_ecosystem_index_action[[index_temp]][i,] <-
        transition_ecosystem_index_action[[index_temp]][i,]/(sum(transition_ecosystem_index_action[[index_temp]][i,]))
    }
  }
  transition_ecosystem[[index_action]] <- transition_ecosystem_index_action
}


## temperature dynamics ####
temperature_data <- read.csv("data IPCC/summarized_data.csv")
temperature_data$Year <- temperature_data$Year-min(temperature_data$Year)
temperature_data_filter <- filter(temperature_data,
                           # scenario == "Historical"|scenario=="SSP1_1_9")
                           scenario == "Historical"|scenario=="SSP5_8_5")
                            # scenario == "Historical"|scenario=="SSP3_7_0")

transition_temperatures <- list()
for (delta_tlim_index in seq(length(DELTA_TLIM_VALUES))){
  transition_temperatures[[delta_tlim_index]] <- matrix(nrow=length(time_states),
                                                        ncol=length(temperature_states))
  for (t in seq(length(time_states))){
    if (t< length(time_states)){
      delta_t_avg <- delta_t(time_states[t+1], delta_t0,DELTA_TLIM_VALUES[delta_tlim_index],mu, beta)
    } else {
      delta_t_avg <- delta_t(time_states[t], delta_t0,DELTA_TLIM_VALUES[delta_tlim_index],mu, beta)
    }
    for (j in seq(length(temperature_states))){
      transition_temperatures[[delta_tlim_index]][t, j] <- pnorm(temperature_states[j]+1/(2*N_temperatures),
                                                                 mean=delta_t_avg,
                                                                 sd=sigma_temp)-
                                                          pnorm(temperature_states[j]-1/(2*N_temperatures),
                                                                mean=delta_t_avg,
                                                                sd=sigma_temp)

      transition_temperatures[[delta_tlim_index]][t, j] <- round(transition_temperatures[[delta_tlim_index]][t, j],
                                                                 digits=3)

    }
    transition_temperatures[[delta_tlim_index]][t,] <- transition_temperatures[[delta_tlim_index]][t,]/sum(transition_temperatures[[delta_tlim_index]][t,])
  }
}
## time transition function####
mat <- diag(length(time_states)-1)
mat <- cbind(rep(0,length(time_states)-1), mat)
mat <- rbind(mat, c(rep(0,length(time_states)-1),1))
#reward####
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

## write POMDPx file ####
TR_FUNCTION_ECO <- transition_ecosystem
TR_FUNCTION_TEMP <- transition_temperatures
TR_FUNCTION_TIME <- mat
B_FULL_ECO <- c(rep(0, length(ecosystem_states)-1), 1)
B_FULL_TEMP <- c(1, rep(0, length(temperature_states)-1))
B_FULL_TIME <- c(1, rep(0, length(time_states)-1))
B_PAR <- rep(1, length(DELTA_TLIM_VALUES))/length(DELTA_TLIM_VALUES)
REW <- reward_time_list
GAMMA <- 0.99
file_name <- paste0("variable growth rate/",
                    "actions_",paste0(DEP_EFFECT, collapse = "_"),
                    "_models_", paste0(DELTA_TLIM_VALUES, collapse = "_"),
                    "_N_ecosystem_", N_ecosystem,
                    "_N_temperatures_", N_temperatures,
                    "_N_times_", N_times,
                    "_horizon_", horizon
                    )
FILE <- paste0(file_name, ".pomdpx")

write_deployment_POMDP(TR_FUNCTION_ECO,
                      TR_FUNCTION_TEMP,
                      TR_FUNCTION_TIME,
                      B_FULL_ECO,
                      B_FULL_TEMP,
                      B_FULL_TIME,
                      B_PAR,
                      REW,
                      GAMMA,
                      FILE)

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

OUTPUT_FILE <- paste0(file_name, ".policyx")

cmd <- paste(path_to_sarsop,
             "--precision", 0.0000001,
             "--timeout", 60 ,
             "--output", OUTPUT_FILE,
             FILE,
             sep=" ")
# system(cmd)
