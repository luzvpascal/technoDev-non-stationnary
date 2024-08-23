voi_data_full <- read.csv(res_file, header = FALSE)
names(voi_data_full) <- c("time",
                          "tech_model",
                          "mean_ecosystem",
                          "sd_ecosystem",
                          "mean_tech",
                          "sd_tech",
                          "mean_action_dev",
                          "sd_action_dev",
                          "mean_action_deploy",
                          "sd_action_deploy",
                          "mean_value",
                          "sd_value",
                          "index_MDP_true",
                          "index_MDP_test")

voi_data_analysis_full <- voi_data_full %>%
  mutate(true_delta_crit = all_scenarios$delta_t_crit_K[index_MDP_true],
         test_delta_crit = all_scenarios$delta_t_crit_K[index_MDP_test],
         true_IPCC = all_scenarios$scenario[index_MDP_true],
         test_IPCC = all_scenarios$scenario[index_MDP_test],
  )%>%
  filter(time == 84)%>%
  # filter(tech_model == 2)%>%
  group_by(index_MDP_true)%>%
  mutate(max_value = max(mean_value),
            test_value = mean_value) %>%
  mutate(r_EVPI=((max_value-test_value)/max_value))

voi_plot_full <- voi_data_analysis_full %>%
  ggplot(aes(x=index_MDP_true , y = index_MDP_test))+
  # ggplot(aes(x=true_IPCC , y = test_IPCC))+
  # ggplot(aes(x=true_delta_crit , y = test_delta_crit))+
  geom_tile(aes(fill=r_EVPI*100))+
  scale_fill_gradient(low = "lightyellow", high = "red") +
  facet_wrap(~tech_model)+
  theme_minimal() +
  coord_equal()+
  labs(title = "",
       x = "True scenario",
       y = "Assumed scenario",
       fill = "rEVPI (%)")+
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.ticks = element_blank(),    # Remove axis ticks
    axis.line = element_blank(),      # Remove axis lines
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()
  )

for (k in seq(0,length(unique(temperature_data$scenario)))){
  voi_plot_full <- voi_plot_full+
    geom_vline(xintercept = length(tested_delta_t_crit_K)*(k)+0.5)+
    geom_hline(yintercept = length(tested_delta_t_crit_K)*(k)+0.5)
}
for (k in seq(length(unique(all_scenarios$scenario)))){
  scenario_text <- unique(all_scenarios$scenario)[k]
  if (scenario_text!="all"){
    position_text <- length(tested_delta_t_crit_K)*(k-1/2)+0.5
    voi_plot_full <- voi_plot_full+
      annotate("text", x=position_text, y=-1,label=scenario_text)+
      annotate("text", y=position_text, x=-1,label=scenario_text, angle=90)
  }
}
voi_plot_full


voi_data_best_policy_full <- voi_data_analysis_full %>%
  ungroup()%>%
  group_by(index_MDP_test)%>%
  # group_by(test_IPCC)%>%
  # group_by(test_delta_crit)%>%
  summarize(meanEVPI=mean(r_EVPI))%>%
  arrange(meanEVPI)
tail(voi_data_best_policy_full)

index_MDP <- 1
voi_data_full_analysis <- voi_data_full %>%
  filter(index_MDP_true==index_MDP)%>%
  filter(index_MDP_test %in% c(4,7,index_MDP))

states <- voi_data_full_analysis %>%
  ggplot()+
  labs(
    x="time (yrs)",
    y="ecosystem state"
  )+
  geom_ribbon(aes(x = time * time_step,
                  ymax=mean_ecosystem+sd_ecosystem,
                  ymin=mean_ecosystem-sd_ecosystem,
                  group = index_MDP_test,
                  fill = factor(index_MDP_test)),
              # fill = "blue",
              alpha = 0.2)+
  geom_line(aes(x=(time)*time_step,
                y=(mean_ecosystem),
                group = index_MDP_test,
                colour = factor(index_MDP_test)))+
  facet_wrap(~tech_model)+
  theme_minimal()
states

states_tech <- voi_data_full_analysis %>%
  ggplot()+
  labs(
    x="time (yrs)",
    y="tech state"
  )+
  geom_ribbon(aes(x = time * time_step,
                  ymax=mean_tech+sd_tech,
                  ymin=mean_tech-sd_tech,
                  group = index_MDP_test,
                  fill = factor(index_MDP_test)),
              # fill = "blue",
              alpha = 0.2)+
  geom_line(aes(x=(time)*time_step,
                y=(mean_tech),
                group = index_MDP_test,
                colour = factor(index_MDP_test)))+
  facet_wrap(~tech_model)+
  theme_minimal()
states_tech

values <- voi_data_full_analysis %>%
  ggplot()+
  labs(
    x="time (yrs)",
    y="value"
  )+
  geom_ribbon(aes(x = time * time_step,
                  ymax=mean_value+sd_value,
                  ymin=mean_value-sd_value,
                  group = index_MDP_test,
                  fill = factor(index_MDP_test)),
              alpha = 0.2)+
  geom_line(aes(x=(time)*time_step,
                y=(mean_value),
                group = index_MDP_test,
                colour = factor(index_MDP_test)))+
  facet_wrap(~tech_model)+
  theme_minimal()
values

actions_dev <- voi_data_full_analysis %>%
  ggplot()+
  geom_line(aes(x=(time)*time_step,
                y=mean_action_dev,
                group = index_MDP_test,
                colour = factor(index_MDP_test)))+
  labs(
    x="time (yrs)",
    col="action",
    y=""
  )+
  facet_wrap(~tech_model)+
  theme_minimal()
actions_dev

actions_deploy <- voi_data_full_analysis %>%
  ggplot()+
  geom_line(aes(x=(time)*time_step,
                y=mean_action_deploy,
                group = index_MDP_test,
                colour = factor(index_MDP_test)))+
  labs(
    x="time (yrs)",
    col="Deploy",
    y=""
  )+
  facet_wrap(~tech_model)+
  theme_minimal()
actions_deploy

ggarrange(states,
          actions,
          values,
          ncol=1)

