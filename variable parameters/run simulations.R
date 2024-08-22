library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
source("deployment POMDP/read_solutions.R")
source("variable parameters/simulations.R")
alphas <- read_policyx2(OUTPUT_FILE)

data <- trajectory(state_prior_eco = tuple_to_index(1,
                                                    N_ecosystem+1,
                                                    N_ecosystem+1),
                   state_prior_tech = 1,
                   Tmax=85,
                   # initial_belief_state = B_PAR,
                   # initial_belief_state = c(0.99,0.01),
                   # initial_belief_state = c(0.01,0.99),
                   initial_belief_state = c(0,1),
                   # initial_belief_state = c(1,0),
                   initial_belief_state_tech = B_PAR_TECH,
                   transition_ecosystem=TR_FUNCTION_ECO,
                   transition_tech = TR_FUNCTION_TECH,
                   reward=REW,
                   true_model=2,
                   true_model_tech = 2,
                   alpha_momdp = alphas,
                   disc = GAMMA,
                   optimal_policy = TRUE,
                   naive_policy = NA,
                   alpha_indexes=FALSE)

time_tech_ready <- ifelse(data$data_output$state_tech[nrow(data$data_output)] == 1,
                          Inf,
                          min(which(data$data_output$state_tech==2)))-1
plot_eco_states <-
  data$data_output[-nrow(data$data_output),] %>%
  rowwise() %>%
  mutate(state_year=index_to_year(state_eco, N_ecosystem+1),
         state_ecosystem=index_to_eco(state_eco,N_ecosystem+1))%>%
  mutate(state_ecosystem=ecosystem_states[state_ecosystem])%>%
  ggplot()+
  geom_line(aes(x=(time)*time_step,
                y=(state_ecosystem)))+
  labs(
    x="time (yrs)",
    y="ecosystem state"
  )+
  # lims(y=c(0,1))+
  geom_vline(xintercept = time_tech_ready, col="purple") +
  theme_minimal()

plot_action_states <-
  data$data_output[-nrow(data$data_output),] %>%
  mutate(action_name=ifelse(action==1, "BAU",
                            ifelse(state_tech == 1, "Develop", "Deploy")))%>%
  ggplot()+
  geom_point(aes(x=(time)*time_step,
                 y=1,
                 col=action_name),
             shape=15)+
  labs(
    x="time (yrs)",
    col="action",
    y=""
  )+
  theme_minimal()+
  geom_vline(xintercept = time_tech_ready, col="purple")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

data_beliefs_mod <- data$mod_probs_mod[-nrow(data$mod_probs_mod),] %>%
  as.data.frame()
names(data_beliefs_mod) <- tested_delta_t_crit_K

plot_beliefs_mod <- data_beliefs_mod %>%
  mutate(time=(seq(nrow(data_beliefs_mod))-1)*time_step)%>%
  pivot_longer(cols = !time, names_to = "model",
               values_to = "proba")%>%
  ggplot()+
  geom_line(aes(x=time,
                y=proba,
                col=model))+
  # scale_color_manual(values=c("blue","red"))+
  labs(
    x="time (yrs)",
    y = expression(omega),
    col=TeX("$\\Delta T_{crit}$")
  )+
  geom_vline(xintercept = time_tech_ready, col="purple") +
  theme_minimal()

data_beliefs_tech <- data$mod_probs_tech[-nrow(data$mod_probs_tech),] %>%
  as.data.frame()
names(data_beliefs_tech) <- c("Success","Failure")
plot_beliefs_tech <- data_beliefs_tech %>%
  mutate(time=(seq(nrow(data$mod_probs[-nrow(data$mod_probs),]))-1)*time_step)%>%
  pivot_longer(cols = !time, names_to = "model",
               values_to = "proba")%>%
  ggplot()+
  geom_line(aes(x=time,
                y=proba,
                col=model))+
  scale_color_manual(values=c("black","green"))+
  labs(
    x="time (yrs)",
    y = expression(b),
    col ="Development\nmodel"
  )+
  geom_vline(xintercept = time_tech_ready, col="purple") +
  theme_minimal()

K_eff_data <- data.frame()
for (tested_delta_t_crit in tested_delta_t_crit_K){
  K_eff <- c()
  for (delta_t in temperature_data_filter$Mean){
    K_eff <- c(K_eff, K_function(K_min, K_max, tested_delta_t_crit,
                                 delta_t,sigmoid_bool_K))
  }
  K_eff_data <- rbind(K_eff_data,
                      data.frame(time=temperature_data_filter$Year,
                                 K_eff=K_eff,
                                 tested_delta_t_crit=tested_delta_t_crit)
  )
}

K_eff_plot <- ggplot(K_eff_data)+
  geom_line(aes(x=time, y = K_eff,
                group=tested_delta_t_crit,
                col=as.factor(tested_delta_t_crit)))+
  # theme_bw()+
  labs(x="time (yrs)",
       y=TeX("Maximum capacity ($K$)"),
       col=TeX("$\\Delta T_{crit}$")) +
  theme_minimal()

ggarrange(plot_eco_states,
          # K_eff_plot,
          plot_action_states,
          plot_beliefs_mod,
          plot_beliefs_tech,
          align = "v",
          ncol=1,
          heights = c(2,1,2,2))

data$data_output[-nrow(data$data_output),] %>%
  mutate(action_name=ifelse(action==1, "BAU",
                    ifelse(state_tech == 1, "Develop", "Deploy"))) %>%
  group_by(action_name)%>%
  summarise(n())
