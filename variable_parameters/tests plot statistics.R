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
test_scenario <- 38
true_scenario <- 38
res_file_sim <- "res/apply"
file_data <- paste(res_file_sim,"_",test_scenario, "_on_",
      true_scenario, ".csv",sep="")

data <- read.csv(file_data, header=FALSE)
names(data) <- c("sim",
                 "state_eco",
                 "state_tech",
                 "state_current",
                 "value",
                 "action",
                 "time",
                 "tech_model")
data <- data%>%
  rowwise() %>%
  mutate(state_year=index_to_year(state_eco, N_ecosystem+1),
         state_ecosystem=index_to_eco(state_eco,N_ecosystem+1))%>%
  mutate(state_ecosystem=ecosystem_states[state_ecosystem])

state_ecosystem_freq <- data %>%
  group_by(tech_model, time, state_ecosystem) %>%
  summarise(freq = n()) %>%
  ungroup()

action_frequencies <- data %>%
  # filter(tech_model == 2) %>%
  mutate(action=ifelse(action == 1,
                       1,
                       ifelse(state_tech==1, 2, 3)))%>%
  group_by(tech_model, time, state_ecosystem, action) %>%
  summarize(frequency = n(), .groups = 'drop') %>%
  # Calculate total occurrences for each combination of tech_model, time, state_ecosystem
  group_by(tech_model, time, state_ecosystem) %>%
  mutate(total_occurrences = sum(frequency),
         frequency_relative = frequency / total_occurrences) %>%
  ungroup()

ggplot(action_frequencies,
       aes(x = time, y = factor(state_ecosystem),
           fill = as.factor(action))) +
  geom_tile(aes(alpha=frequency_relative)) +
  scale_fill_manual(values = c("1" = "red3",
                               "2" = "green4",
                               "3" = "orange")) +  # Assign different colors for each action
  # scale_alpha_continuous(range = c(0, 1)) +  # Adjust alpha range
  labs(x = "Time", y = "State Ecosystem", fill = "Action", alpha = "Relative Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  facet_wrap(~tech_model)
