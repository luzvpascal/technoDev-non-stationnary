library(latex2exp)
library(ggplot2)
library(tidyverse)

delta_t0 = 0.89
DELTA_TLIM_VALUES = c(1.5,4)
mu = 0.0074
beta=0.0128
sigma <- 0.5

horizon <- 500
time <- seq(0,horizon)

delta_t <- function(t, delta_t0,delta_tlim,mu, beta){
  gamma= beta - mu*(delta_tlim - delta_t0)
  return(delta_t0 + gamma*t - (1-exp(-mu*t))*(gamma*t - (delta_tlim - delta_t0)))
}
results <- data.frame()

for (delta_tlim in DELTA_TLIM_VALUES){
  delta_t_seq <- mapply(delta_t, time,
                        MoreArgs = list(delta_t0 = delta_t0,
                                        delta_tlim = delta_tlim,
                                        mu = mu,
                                        beta = beta))
  res <- data.frame(delta_t = delta_t_seq
                    # + rnorm(horizon+1, 0,sigma)
                    ,
                    time=time,
                    delta_tlim=delta_tlim)
  results <- rbind(results, res)
}

results$action <- FALSE
# results_action <- results %>%
#   mutate(delta_t = delta_t-0.5,
#          action=TRUE)

results %>%
  mutate(model = ifelse(delta_tlim == DELTA_TLIM_VALUES[1], "m2", "m1"),
         lower_bound = delta_t - 1.96 * sigma,
         upper_bound = delta_t + 1.96 * sigma) %>%
  ggplot(aes(x = time, y = delta_t, col = model)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = model), alpha = 0.2, color = NA) +
  theme_bw() +
  labs(x = "time (yrs)",
       y = expression(Delta ~ T ~ (degree * C)),
       col = "Model") +
  lims(y = c(0, 5))

## belief trajectory####
likelihood_normal <- function(model, observation, time_step){
  avg = delta_t(time_step, delta_t0, DELTA_TLIM_VALUES[model], mu, beta)
  return(dnorm(observation, mean=avg, sd = sigma))
}

belief_model_1 <- 0.5
belief_model_2 <- 1-belief_model_1

observation_sequence <- results%>%
  filter(delta_tlim == DELTA_TLIM_VALUES[1])%>%
  select(delta_t, time)
observation_sequence$delta_t <- observation_sequence$delta_t++ rnorm(horizon+1, 0,sigma)
for (i in seq(nrow(observation_sequence))){
  belief_model_1_update <- likelihood_normal(1,
                                             observation_sequence$delta_t[i],
                                             i)*belief_model_1[i]
  belief_model_2_update <- likelihood_normal(2,
                                             observation_sequence$delta_t[i],
                                             i)*belief_model_2[i]

  belief_model_1 <- c(belief_model_1,
                      belief_model_1_update/(belief_model_1_update+belief_model_2_update))
  belief_model_2 <- c(belief_model_2,
                      belief_model_2_update/(belief_model_1_update+belief_model_2_update))
}

observation_sequence$belief_model_1 <- belief_model_1[-length(belief_model_1)]
observation_sequence$belief_model_2 <- belief_model_2[-length(belief_model_2)]

p1 <- observation_sequence %>%
  ggplot(aes(x = time, y = delta_t)) +
  geom_line() +
  theme_bw() +
  labs(x = "time (yrs)",
       y = expression(Delta ~ T ~ (degree * C))) +
  lims(y = c(0, 5))
p2 <- observation_sequence %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = belief_model_1), col="blue") +
  geom_line(aes(y = belief_model_2), col="red") +
  theme_bw() +
  labs(x = "time (yrs)",
       y = expression(omega)) +
  lims(y = c(0, 1))

ggpubr::ggarrange(p1,p2,
                  nrow = 2)
