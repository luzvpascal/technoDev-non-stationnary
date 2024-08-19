library(latex2exp)
library(ggplot2)
library(ggpubr)
library(tidyverse)

## Temperature trajectory ####
# delta_t0 = 0.89
true_model <- 1
delta_t0 = c(0, 0)
DELTA_TLIM_VALUES = c(1.5,4.5)
mu = c(0.0074, 0.002)
beta= c(0.0128, 0.02)
sigma_temp <- 0.2

horizon <- 200
# time <- c(rep(0,100), seq(0,horizon))
time <- c(seq(0,horizon))

delta_t <- function(t, delta_t0,delta_tlim,mu, beta){
  gamma= beta - mu*(delta_tlim - delta_t0)
  return(delta_t0 + gamma*t - (1-exp(-mu*t))*(gamma*t - (delta_tlim - delta_t0)))
}

## ecosystem dynamics ####
r <- 0.7
K <- 1
b <- 0.15
q <- 2
sigma_eco <- 0.02
eta <- 5.8
dep_effect <- 0.5
ecosystem_dynamics <- function(x_t, r, K, phi_t, q, b, dep_effect){
  if (phi_t >=1){
    phi_new <- max(phi_t-dep_effect, 0)
  } else {
    phi_new <- phi_t
  }
  return(x_t + x_t*r*(1-x_t/K)-(phi_new/eta*x_t**q)/(x_t**q+b**q))
}

##
x_t <- 1
trajectory <- c(x_t)
trajectory_delta_t <- c(delta_t0)

delta_t_seq <- mapply(delta_t, time,
                      MoreArgs = list(delta_t0 = delta_t0[true_model],
                                      delta_tlim = DELTA_TLIM_VALUES[true_model],
                                      mu = mu[true_model],
                                      beta = beta[true_model]))
delta_t_seq <- pmax(0, delta_t_seq + rnorm(length(delta_t_seq),
                                   0,
                                   sigma_temp))
for (i in seq(length(delta_t_seq))){
  x_t <- ecosystem_dynamics(x_t, r, K, delta_t_seq[i], q, b, dep_effect)
  x_t <- min(max(0, x_t + rnorm(1,0,sigma_eco)), 1)
  trajectory <- c(trajectory, x_t)
}
data <- data.frame(delta_t = delta_t_seq,
                   time =  seq(length(delta_t_seq)),
                   state=trajectory[-length(trajectory)])
## belief trajectory ####
likelihood_normal <- function(model, observation, time_step){
  avg = delta_t(time_step, delta_t0[true_model], DELTA_TLIM_VALUES[model],
                mu[true_model], beta[true_model])
  return(dnorm(observation, mean=avg, sd = sigma_temp))
}

belief_model_1 <- 0.5
belief_model_2 <- 1-belief_model_1

for (i in seq(nrow(data))){
  belief_model_1_update <- likelihood_normal(1,
                                             data$delta_t[i],
                                             i)*belief_model_1[i]
  belief_model_2_update <- likelihood_normal(2,
                                             data$delta_t[i],
                                             i)*belief_model_2[i]

  belief_model_1 <- c(belief_model_1,
                      belief_model_1_update/(belief_model_1_update+belief_model_2_update))
  belief_model_2 <- c(belief_model_2,
                      belief_model_2_update/(belief_model_1_update+belief_model_2_update))
}
data$belief_model_1 <- belief_model_1[-length(belief_model_1)]
data$belief_model_2 <- belief_model_2[-length(belief_model_2)]

### plots ####
temp_plot <- ggplot(data)+
  geom_line(aes(x=time, y=delta_t))+
  theme_bw() +
  labs(x = "time (yrs)",
       y = expression(Delta ~ T ~ (degree * C)))

state_plot <- ggplot(data)+
  geom_line(aes(x=time, y=state))+
  lims(y=c(0,1))+
  theme_bw() +
  labs(x = "time (yrs)",
       y = "state")+
  geom_hline(yintercept = 1,
             linetype="dashed")

belief_plot <- data %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = belief_model_1), col="blue")

belief_plot <- belief_plot +
  geom_line(aes(y = belief_model_2), col="red") +
  theme_bw() +
  labs(x = "time (yrs)",
       y = expression(omega)) +
  lims(y = c(0, 1))

ggarrange(temp_plot,
          state_plot,
          belief_plot,
          ncol=1,
          common.legend = TRUE)
