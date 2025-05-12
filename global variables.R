#######################################
# define common parameters of model####
#######################################
# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
# library(ggpubr)
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
r_min <- 0
r_max <- 0.2
# tested_delta_t_crit_r <- 1
tested_delta_t_crit_r <- seq(1, 2.5,by=0.15)
## capacity parameters ####
K_min <- 0
K_max <- 1

## deployment effects ####
DEP_EFFECT <- c(0,1)
cost_deploy <- 0.05

## discount factor ####
# gamma <- 0.9
# gamma <- 0.95
gamma <- 0.99
# gamma <- 0.9999
GAMMA <- gamma
# tested delta_t_crit ####
tested_delta_t_crit_K <- seq(1, 2.5,by=0.15) #tested values of delta_t_crit_K

# tested climate scenarios ####
climate_scenarios <- unique(temperature_data$scenario)
# all possible parameters value uncertain response ####
all_scenarios <- expand.grid(
                            delta_t_crit_r = tested_delta_t_crit_r,
                            delta_t_crit_K = tested_delta_t_crit_K,
                            sigmoid_bool_r = c(FALSE),
                            sigmoid_bool_K = c(TRUE),
                            # sigmoid_bool_r = c(TRUE, FALSE),
                            # sigmoid_bool_K = c(TRUE, FALSE),
                            dep_effect = c(1),
                            scenario = climate_scenarios)
#for 100 models
filtered_scenarios <- all_scenarios %>%
  group_by(sigmoid_bool_r, sigmoid_bool_K) %>%
  filter(
    (sigmoid_bool_r | delta_t_crit_r == first(delta_t_crit_r)) &
      (sigmoid_bool_K | delta_t_crit_K == first(delta_t_crit_K))
  ) %>%
  ungroup()

#for 2 models
filtered_scenarios_2_models <- expand.grid(
  delta_t_crit_r = c(2.5),
  delta_t_crit_K =  c(1,2.5),
  sigmoid_bool_r = TRUE,
  sigmoid_bool_K = TRUE,
  dep_effect = c(1),
  scenario = climate_scenarios)

#for 4 models
filtered_scenarios_4_models <- expand.grid(
  delta_t_crit_r = c(1,2.5),
  delta_t_crit_K =  c(1,2.5),
  # delta_t_crit_r = c(2.35,1.75,1.90,2.5),
  # delta_t_crit_K =  c(1,2.2,2.2,2.35),
  sigmoid_bool_r = TRUE,
  sigmoid_bool_K = TRUE,
  dep_effect = c(1),
  scenario = climate_scenarios)

#for 16 models
filtered_scenarios_16_models <- expand.grid(
  delta_t_crit_r = seq(1, 2.5,length.out=4),
  delta_t_crit_K = seq(1, 2.5,length.out=4),
  sigmoid_bool_r = TRUE,
  sigmoid_bool_K = TRUE,
  dep_effect = c(1),
  scenario = climate_scenarios)

# all possible parameters value uncertain climate scenarios ####
scenarios_uncertain_climate <- expand.grid(
  delta_t_crit_r = c(1.5),
  delta_t_crit_K =  c(1.5),
  sigmoid_bool_r = TRUE,
  sigmoid_bool_K = TRUE,
  dep_effect = c(1),
  scenario = climate_scenarios)
# ncores####
ncores <- parallelly::availableCores()
