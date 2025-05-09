# Load the parallel package
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
## load global variables ####
source("global variables.R")
source("helper functions/read_solutions.R")
source("helper functions/simulations.R")
source("helper functions/write_POMDPx.R")
source("helper functions/functions variable parameters - MDP.R")
source("variable_parameters/build transition and reward function.R")
source("helper functions/simulations success failure.R")

##technology####
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2),
                           matrix(c(1-p_dev, 0, p_dev, 1), ncol=2))
transition_failure <- list(diag(2),
                           diag(2))
transition_tech <- list(transition_success, transition_failure)

## POMDP definition #####
TR_FUNCTION_TECH <- transition_tech
REW <- Reward
GAMMA <- gamma

B_FULL_ECO <- c(rep(0, nrow(Reward)))
B_FULL_ECO[N_ecosystem+1] <- 1
B_FULL_TECH <- c(1, rep(0, length(tech_states)-1))

B_PAR <- 1
B_PAR_TECH <- rep(1, length(transition_tech))/length(transition_tech)

file_name <- paste0("pomdpx/POMDPscenario")

test_scenario <- 30
TR_FUNCTION_ECO <- transition_matrix_list[[test_scenario]]
OUTPUT_FILE <- paste0(file_name, test_scenario,".policyx")
alphas <- read_policyx2(OUTPUT_FILE)

state_prior_eco = tuple_to_index(1, N_ecosystem+ 1, N_ecosystem + 1)
state_prior_tech = 1
Tmax = 85
initial_belief_state = B_PAR
initial_belief_state_tech = B_PAR_TECH
transition_ecosystem = list(TR_FUNCTION_ECO)
transition_tech = TR_FUNCTION_TECH
reward = REW
true_model = 1
true_model_tech = 1
alpha_momdp = alphas
disc = GAMMA
optimal_policy = TRUE
naive_policy = NA
alpha_indexes = FALSE

# initialise Num_mod and Num_state
Num_mod <- length(initial_belief_state)
Num_mod_tech <- length(initial_belief_state_tech)

#number of states
Num_s_eco <- dim(transition_ecosystem[[1]])[1]
Num_s_tech <- nrow(transition_tech[[1]][[1]])
Num_s <- Num_s_eco*Num_s_tech

Num_a <- dim(transition_ecosystem[[1]])[3]
#initialise sequence of actions and rewards
actions <- c()
V <- c(0) #initial reward is 0

if (alpha_indexes){indexes <- c()}

state_eco <- state_prior_eco
state_tech <- state_prior_tech

state_current <- factored_state(state_eco, state_tech,
                                Num_s_eco, Num_s_tech)

mod_probs <- data.frame(belief_success=seq(0,1,length.out=50))
mod_probs <- mod_probs%>%
  mutate(belief_failure=1-belief_success)
mod_probs <- as.matrix(mod_probs)
mod_probs <- unname(mod_probs)
action_data <- data.frame()
for (state_eco in seq(Num_s_eco)){
  for (i in seq(nrow(mod_probs))){
  #compute next best action0
    state_current <- factored_state(state_eco, state_tech,
                                    Num_s_eco, Num_s_tech)

    output <- interp_policy2(mod_probs[i,],
                             obs = state_current,
                             alpha = alpha_momdp$vectors,
                             alpha_action = alpha_momdp$action,
                             alpha_obs = alpha_momdp$obs,
                             alpha_index = alpha_momdp$index)

    opt_action <- output[[2]][1]

    action_data <- rbind(action_data,
                         data.frame(state_eco=state_eco,
                                    belief=mod_probs[i,1],
                                    action=opt_action))
  }
}


action_data %>%
  rowwise() %>%
  mutate(state_year=index_to_year(state_eco, N_ecosystesm+1),
         state_ecosystem=index_to_eco(state_eco,N_ecosystem+1))%>%
  mutate(state_ecosystem=ecosystem_states[state_ecosystem])%>%
  filter(state_year %in% seq(1,Tmax,10))%>%
  ggplot()+
  geom_tile(aes(x=belief, y=state_ecosystem,
                fill=factor(action)))+
  facet_wrap(~state_year)
