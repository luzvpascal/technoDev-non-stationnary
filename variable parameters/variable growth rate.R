# s(t+1) ~ lognormal(s(t) + r(t)s(t)(1-s(t)/K))
# r(t) = r(0)(delta_t_crit - delta_t(t))/eta
library(sigmoid)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(tidyverse)
## parameters of the system ####
r <- 0.3
K <- 1
sigma_eco <- 0.01
eta <- 10
delta_t_crit <- 1.5 #degrees

r_min <- -0.2
r_max <- 0.2
increment <- 1
delta_t_crit_min <- 1
delta_t_crit_max <- delta_t_crit_min+increment
#ecosystem_dynamics####
r_function <- function(r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                       delta_t){
  # if (delta_t<=delta_t_crit_min){
  #   r_max
  # } else if (delta_t<=delta_t_crit_max){
  #   r_max - (delta_t-delta_t_crit_min)*(r_max-r_min)/(delta_t_crit_max-delta_t_crit_min)
  # } else {
  #   r_min
  # }
  r_min + (r_max-r_min)*(1-1/(1+exp(-5*(delta_t - (delta_t_crit_min+delta_t_crit_max)/2))))
}

## growth rate plot ####
r_eff <- c()
r_deploy <- c()
tested_delta <- seq(0, 3, 0.01)
for (delta_t in tested_delta){
  r_eff <- c(r_eff, r_function(r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                               delta_t))
  r_deploy<- c(r_deploy, r_function(r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                                    delta_t-0.5))
}

r_eff_data <- data.frame(temp=tested_delta,
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
## ecosystem dynamics ####
ecosystem_dynamics <- function(x_t, r, K, time_step){
  return(x_t + time_step*(x_t*r*(1-x_t/K)))
}

## temperature data ####
data <- read.csv("data IPCC/summarized_data.csv")
scenarios_future <- unique(data$scenario)[-1]
data_ecosystem <- data.frame()
for (scenario_current in scenarios_future){
  temperature_data <- filter(data,
                             scenario == "Historical"|scenario==scenario_current)
  temperature <- temperature_data$Mean

  horizon <- length(temperature)
  time_step <- 1
  time_states <- seq(1, horizon, time_step)
  ## trajectory simulation ####
  x_t <- 0.5
  r_eff_list <- c()
  dep_effect=0
  # dep_effect=0.5
  for (t in time_states){
    r_eff <- r_function(r_min, r_max, delta_t_crit_min,delta_t_crit_max,
                        temperature[t]-dep_effect)
    r_eff_list <- c(r_eff_list, r_eff)
    x_new <- ecosystem_dynamics(x_t[t], r_eff, K, time_step)
    # x_new <- rnorm(1,log((x_new)),sigma_eco)
    # x_new <- exp(x_new)
    # x_new <- rlnorm(1, log((x_new)), sigma_eco)
    x_t <- c(x_t,x_new)
  }

  data_scenario_current <- data.frame(Year=time_states+1949,
                                      x_t=x_t[-length(x_t)],
                                      r=r_eff_list,
                                      scenario = temperature_data$scenario)
  data_ecosystem <- rbind(data_ecosystem,
                          data_scenario_current)

}


temp_plot <- data %>%
  ggplot()+
  theme_bw()+
  geom_line(aes(x=Year, y=Mean, group=scenario, col=scenario),
            linewidth = 1.1)+
  labs(y=TeX("$\\Delta T$"))

state <- ggplot(data_ecosystem)+
  geom_line(aes(x=Year, y = x_t, group=scenario, col=scenario))+
  theme_bw()+
  labs(y="State")

r_plot <- ggplot(data_ecosystem)+
  geom_line(aes(x=Year, y = r, group=scenario, col=scenario))+
  theme_bw()+
  labs(y="Growth rate (r)")+
  lims(y=c(r_min,r_max))

library(ggpubr)
ggarrange(temp_plot,
          r_plot,
          state,
          ncol=1,
          align = "hv",
          common.legend = TRUE)
