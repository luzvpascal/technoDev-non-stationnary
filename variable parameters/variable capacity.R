# s(t+1) ~ lognormal(s(t) + r(t)s(t)(1-s(t)/K))
# r(t) = r(0)(delta_t_crit - delta_t(t))/eta
library(sigmoid)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(tidyverse)
## parameters of the system ####
r <- 0.7
K <- 1
sigma_eco <- 0.1
eta <- 10
delta_t_crit <- 1.5 #degrees

K_min <- 0
K_max <- 1
increment <- 1
delta_t_crit_min <- 1
delta_t_crit_max <- delta_t_crit_min+increment
#ecosystem_dynamics####
K_function <- function(K_min, K_max, delta_t_crit_min,
                       delta_t_crit_max,
                       delta_t){
  # if (delta_t<=delta_t_crit_min){
  #   K_max
  # } else if (delta_t<=delta_t_crit_max){
  #   K_max - (delta_t-delta_t_crit_min)*(K_max-K_min)/(delta_t_crit_max-delta_t_crit_min)
  # } else {
  #   K_min
  # }
  K_min + (K_max-K_min)*(1-1/(1+exp(-5*(delta_t - (delta_t_crit_min+delta_t_crit_max)/2))))
}

## growth rate plot ####
K_eff <- c()
K_deploy <- c()
tested_delta <- seq(0, 3, 0.01)
for (delta_t in tested_delta){
  K_eff <- c(K_eff, K_function(K_min, K_max, delta_t_crit_min,delta_t_crit_max,
                               delta_t))
  K_deploy<- c(K_deploy, K_function(K_min, K_max, delta_t_crit_min,delta_t_crit_max,
                                    delta_t-0.5))
}

K_eff_data <- data.frame(temp=tested_delta,
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
  geom_hline(yintercept = 0,
             col="grey")+
  labs(x=TeX("$\\Delta T$"),
       y=TeX("Maximum capacity ($K$)"))
# K_eff_plot
## ecosystem dynamics ####
ecosystem_dynamics <- function(x_t, r, K, time_step){
  return(x_t + time_step*(x_t*r*(1-x_t/K)))
}

## temperature data ####
data <- read.csv("data IPCC/summarized_data.csv")
scenarios_future <- unique(data$scenario)[-1]
data_ecosystem <- data.frame()
i <- 1
for (scenario_current in scenarios_future){
    temperature_data <- filter(data,
                             scenario == "Historical"|scenario==scenario_current)
    temperature <- temperature_data$Mean

    horizon <- length(temperature)
    time_step <- 1
    time_states <- seq(1, horizon, time_step)
    ## trajectory simulation ####
    x_t <- 0.99
    K_eff_list <- c()
    dep_effect=0
    # dep_effect=0.5
    for (t in time_states){
        K_eff <- K_function(K_min, K_max, delta_t_crit_min,delta_t_crit_max,
                            temperature[t]-dep_effect)
        K_eff_list <- c(K_eff_list, K_eff)
        x_new <- ecosystem_dynamics(x_t[t], r, K_eff, time_step)
        # x_new <- rnorm(1,log((x_new)),sigma_eco)
        # x_new <- exp(x_new)
        if (x_new == 0){
          x_t <- c(x_t,x_new)
        } else {
          x_new <- rlnorm(1, log((x_new)), sigma_eco)
          x_t <- c(x_t,x_new)
        }
    }

    data_scenario_current <- data.frame(Year=time_states+1949,
                                      x_t=x_t[-length(x_t)],
                                      K=K_eff_list,
                                      # scenario = paste(i, temperature_data$scenario))
                                      scenario = scenario_current)
    data_ecosystem <- rbind(data_ecosystem,
                            data_scenario_current)
    # i <- i+1
}

shift_r_time <-which(K_eff_list < 0)[1]+1948

temp_plot <- data %>%
  ggplot()+
  theme_bw()+
  geom_line(aes(x=Year, y=Mean, group=scenario, col=scenario))+
  labs(y=TeX("$\\Delta T$"))+
  guides(fill="none", col="none")

state <- data_ecosystem %>%
  group_by(scenario, Year) %>%
  mutate(x_t=mean(x_t)) %>%
  ggplot()+
  geom_line(aes(x=Year, y = x_t, group=scenario, col=scenario))+
  theme_bw()+
  labs(y="State")

K_plot <-  ggplot(data_ecosystem)+
  geom_line(aes(x=Year, y = K, group=scenario, col=scenario))+
  theme_bw()+
  labs(y="Max capacity (K)")+
  lims(y=c(K_min, K_max))

library(ggpubr)
ggarrange(temp_plot,
          K_plot,
          state,
          ncol=1,
          align = "hv",
          common.legend = TRUE,
          legend="right")
