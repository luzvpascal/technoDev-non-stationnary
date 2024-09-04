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

## discount factor ####
gamma <- 0.9999

# tested delta_t_crit ####
tested_delta_t_crit_K <- seq(1.5,4,0.5) #tested values of delta_t_crit_K

# all possible parameters ####
all_scenarios <- expand.grid(delta_t_crit_K = tested_delta_t_crit_K,
                             scenario = unique(temperature_data$scenario))

