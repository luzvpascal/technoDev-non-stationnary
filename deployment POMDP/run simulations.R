library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
alphas <- read_policyx2(OUTPUT_FILE)

data <- trajectory(state_prior_eco = length(ecosystem_states),
                   state_prior_temp = 1,
                   state_prior_time = 1,
                   Tmax=length(time_states),
                   initial_belief_state = B_PAR,
                   transition_ecosystem=TR_FUNCTION_ECO,
                   transition_temperatures=TR_FUNCTION_TEMP,
                   transition_time=TR_FUNCTION_TIME,
                   reward_time_list=REW,
                   true_model=2,
                   alpha_momdp = alphas,
                   disc = GAMMA,
                   optimal_policy = TRUE,
                   naive_policy = NA,
                   alpha_indexes=FALSE)

plot_eco_states <-
  data$data_output[-nrow(data$data_output),] %>%
  ggplot()+
  geom_line(aes(x=(state_time-1)*time_step,
                y=(state_eco-1)/N_ecosystem))+
  labs(
    x="time (yrs)",
    y="ecosystem state"
  )+
  lims(y=c(0,1))

plot_temp_states <-
  data$data_output[-nrow(data$data_output),] %>%
  mutate(temp = (state_temp-1)*Temp_max/N_temperatures,
         temp_mitigated = pmax(0,temp-(DEP_EFFECT[action])))%>%
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
  ) +
  lims(y=c(0, Temp_max))


data_beliefs <- data$mod_probs[-nrow(data$mod_probs),] %>%
  as.data.frame()
names(data_beliefs) <- DELTA_TLIM_VALUES
plot_beliefs <- data_beliefs %>%
  mutate(time=(seq(nrow(data$mod_probs[-nrow(data$mod_probs),]))-1)*time_step)%>%
  pivot_longer(cols = !time, names_to = "model",
               values_to = "proba")%>%
  ggplot()+
  geom_line(aes(x=time,
                y=proba,
                col=model))+
  # scale_color_brewer(palette="RdYlBu")+
  scale_color_manual(values=c("blue","red"))+
  labs(
    x="time (yrs)",
    y = expression(omega)
  )

ggarrange(plot_temp_states,
          plot_eco_states,
          plot_beliefs,
          align = "v",
          ncol=1)
