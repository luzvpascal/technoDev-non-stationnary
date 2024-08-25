##
setwd("~/technoDev non-stationnary/value of information/")
library(MDPtoolbox)
library(tidyverse)
source("functions_VOI.R")
#2states
#2actions
#10 time steps
time_horizon <- 50
gamma <- 0.99
Num_s <- 2
Num_a <- 2
N_EXP <- 1000
# type_transitions <- "Random"
# type_transitions <- "Random walk"
TYPE_TRANSITIONS <- c("Random","Random walk","Gradient")
TYPE_SD <- c(0.1, 0.01)

#reward#### #stationary reward
reward <- matrix(seq(Num_s),nrow=Num_s, ncol = Num_a)

reward_time_list <- list()
for (t in seq(time_horizon-1)){
  reward_time_list[[t]] <- reward #transition function for each time step
}
reward_time_list[[time_horizon]] <- matrix(0, nrow=Num_s, ncol = Num_a)
# reward factored states ####
reward_matrix<-array(0, dim=c(Num_s*time_horizon,
                              Num_a))
for (a in seq (Num_a)){
  for (t in seq(time_horizon)){
    if (t<time_horizon){
      reward_matrix[seq((t-1)*Num_s+1,(t)*Num_s),a] <- reward_time_list[[t]][,a]
    } else {
      reward_matrix[seq((t-1)*Num_s+1,(t)*Num_s),a] <- reward_time_list[[t]][,a]
    }
  }
}

## RUN EXPERIMENTS ####

results <- data.frame(type_transitions=character(),
                      rEVPI=numeric(),
                      sd=numeric())

for (type_transitions in TYPE_TRANSITIONS){
  for (sd in TYPE_SD){
    for (n_exp in seq(N_EXP)){
      transition_matrix <- transition_function(time_horizon,
                                               gamma,
                                               Num_s,
                                               Num_a,
                                               type_transitions,
                                               sd)

      #compute rEVPI
      rEVPI <- compute_rEVPI(transition_matrix,
                             reward_matrix,
                             time_horizon,
                             gamma,
                             Num_s)
      row <- data.frame(type_transitions=type_transitions,
                        rEVPI=rEVPI,
                        sd=sd)

      results <- rbind(results,
                       row)
    }
  }
}

## VISUALISE RESULTS ####
results <- results%>%
  mutate(sd = ifelse(type_transitions=="Random", 0.1, sd),
         sd_text = ifelse(type_transitions=="Random", "", sd),
         combined_factor = ifelse(type_transitions=="Random", "Random",
                                  paste(results$type_transitions, " (sd=",
                                        results$sd_text, ")", sep = "")))
# Convert combined_factor to a factor with levels ordered alphabetically
results$combined_factor <- factor(results$combined_factor,
                                  levels = sort(unique(results$combined_factor)))

# Create the boxplot
ggplot(results, aes(y = combined_factor, x = rEVPI, fill=type_transitions)) +
  geom_boxplot() +
  labs(x = "Value of knowing non-stationarity (%)",
       y = "") +
  theme_minimal()
