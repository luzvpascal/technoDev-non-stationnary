library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
alphas <- read_policyx2(OUTPUT_FILE)

data <- trajectory(state_prior_eco = length(ecosystem_states)-1,
                   state_prior_temp = 1,
                   state_prior_time = 1,
                   state_prior_tech = 1,
                   Tmax=length(time_states),
                   # initial_belief_state = B_PAR,
                   # initial_belief_state = c(0,1),
                   initial_belief_state = c(1,0),
                   initial_belief_state_tech = B_PAR_TECH,
                   transition_ecosystem=TR_FUNCTION_ECO,
                   transition_temperatures=TR_FUNCTION_TEMP,
                   transition_time=TR_FUNCTION_TIME,
                   transition_tech = TR_FUNCTION_TECH,
                   reward_time_list=REW,
                   # true_model=2,
                   true_model=1,
                   true_model_tech = 2,
                   alpha_momdp = alphas,
                   disc = GAMMA,
                   optimal_policy = TRUE,
                   naive_policy = NA,
                   alpha_indexes=FALSE)

time_tech_ready <- ifelse(data$data_output$state_tech[nrow(data$data_output)] == 1,
                          length(time_states) +1,
                          min(which(data$data_output$state_tech==2)))-1
plot_eco_states <-
  data$data_output[-nrow(data$data_output),] %>%
  ggplot()+
  geom_line(aes(x=(state_time-1)*time_step,
                y=(state_eco-1)/N_ecosystem))+
  labs(
    x="time (yrs)",
    y="ecosystem state"
  )+
  lims(y=c(0,1))+
geom_vline(xintercept = time_tech_ready, col="purple")

plot_action_states <-
  data$data_output[-nrow(data$data_output),] %>%
  mutate(action_name=ifelse(action==1, "BAU",
                            ifelse(state_tech == 1, "Develop", "Deploy")))%>%
  ggplot()+
  geom_point(aes(x=(state_time-1)*time_step,
                y=1,
                col=action_name),
             shape=15)+
  labs(
    x="time (yrs)",
    col="action",
    y=""
  )+
  geom_vline(xintercept = time_tech_ready, col="purple")

plot_temp_states <-
  data$data_output[-nrow(data$data_output),] %>%
  mutate(temp = (state_temp-1)*Temp_max/N_temperatures,
         temp_mitigated = ifelse(state_tech==2,pmax(0,temp-(DEP_EFFECT[action])),
                                 temp))%>%
  ggplot()+
  geom_line(aes(x=(state_time-1)*time_step,
                y=temp_mitigated,
              col="Mitigated"))+
  geom_line(aes(x=(state_time-1)*time_step,
                y=temp,
                col="Observed")
            )+
  scale_colour_manual(values=c("red",
                               "black"))+
  labs(
    x="time (yrs)",
    y = expression(Delta ~ T ~ (degree * C)),
    col = "Temperature"
  )+
  geom_vline(xintercept = time_tech_ready, col="purple")+
  lims(y=c(0,Temp_max))


data_beliefs_mod <- data$mod_probs_mod[-nrow(data$mod_probs_mod),] %>%
  as.data.frame()
names(data_beliefs_mod) <- DELTA_TLIM_VALUES

plot_beliefs_mod <- data_beliefs_mod %>%
  mutate(time=(seq(nrow(data_beliefs_mod))-1)*time_step)%>%
  pivot_longer(cols = !time, names_to = "model",
               values_to = "proba")%>%
  ggplot()+
  geom_line(aes(x=time,
                y=proba,
                col=model))+
  scale_color_manual(values=c("blue","red"))+
  labs(
    x="time (yrs)",
    y = expression(omega),
    col="climate model"
  )+
  geom_vline(xintercept = time_tech_ready, col="purple")

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
  geom_vline(xintercept = time_tech_ready, col="purple")
ggarrange(plot_temp_states,
          plot_eco_states,
          plot_action_states,
          plot_beliefs_mod,
          plot_beliefs_tech,
          align = "v",
          ncol=1)

table(data$data_output$action)
