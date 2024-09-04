library(ggnewscale)

## temperature data ####
temperature_data <- read.csv("data IPCC/summarized_data.csv")
temperature_data <- filter(temperature_data,
                           scenario != "Historical")
##
selected_scenarios <- c(19,21,24)+6

## performance data ####
voi_data_full <- read.csv("variable_parameters/res/voi_POMDP_pars_climate.csv", header = FALSE)
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

voi_data_full <- voi_data_full %>%
  filter(index_MDP_test==index_MDP_true)%>%
  filter(index_MDP_test %in% selected_scenarios)%>%
  mutate(true_delta_crit = all_scenarios$delta_t_crit_K[index_MDP_true],
         test_delta_crit = all_scenarios$delta_t_crit_K[index_MDP_test],
         true_IPCC = all_scenarios$scenario[index_MDP_true],
         test_IPCC = all_scenarios$scenario[index_MDP_test],
         tech_model = ifelse(tech_model==1, "Successful", "Failed"),
         time=time+min(temperature_data$Year),
         scenario_label = factor(paste0(true_delta_crit, " ", true_IPCC))
  )

## K_eff for tested scenarios ####
K_eff_data <- data.frame()

for (index in selected_scenarios){
  climate <- all_scenarios$scenario[index]
  delta_t_crit <- all_scenarios$delta_t_crit_K[index]
  tested_delta <- temperature_data[which(temperature_data$scenario==climate),]$Mean
  time <- temperature_data[which(temperature_data$scenario==climate),]$Year
  K_eff <- c()
  for (delta_t in tested_delta){
    K_eff <- c(K_eff, K_function(K_min, K_max, delta_t_crit,
                                 delta_t,sigmoid_bool_K))
  }

  K_eff_data <- rbind(K_eff_data,
                      data.frame(time=time,
                                 K_eff=K_eff,
                                 delta_t_crit=delta_t_crit,
                                 climate=climate,
                                 index=index)
  )
}
# Create the scenario labels as a named vector
K_eff_data <- K_eff_data %>%
  mutate(scenario_label = factor(paste0(delta_t_crit, " ", climate)))

# Plot using facet_wrap with labeller
K_eff_plot <- ggplot(K_eff_data) +
  geom_line(aes(x = time, y = K_eff)) +
  facet_wrap(~scenario_label, ncol = 1) +
  theme_bw() +
  labs(x = TeX("Year"),
       y = TeX("Maximum carrying capacity ($K$)"),
       col = TeX("Inflection\ntemperature\n$(\\Delta T_{crit})$"))

## action plots ####
actions_dev <- voi_data_full  %>%
  filter(tech_model=="Successful")%>%
  ggplot() +
  geom_tile(aes(x = time,
                y = 1,  # Make y-axis a factor
                fill = mean_action_dev)
            # ,col="black"
            ) +   # Use fill for color
  scale_fill_gradient(low = "white", high = "navy")+
  labs(
    x = "Year",
    fill = "Frequency\ndevelopment",  # Update the fill legend label
    y = "Selected action"
  ) +
  new_scale_fill() +
  geom_tile(aes(x = time,
                y = 0,  # Make y-axis a factor
                fill = mean_action_deploy)
            # , col="black"
            ) +   # Use fill for color
  scale_fill_gradient(low = "white", high = "darkred") +  # Use gradient for continuous data
  labs(fill = "Frequency\ndeployment") +
  facet_wrap(~scenario_label, ncol=1) +
  theme_bw()+
  theme(
    axis.text.y = element_blank(),     # Remove y-axis text labels
    axis.ticks.y = element_blank(),    # Remove y-axis ticks
    axis.title.y = element_blank()     # Remove y-axis title
  )

## ecosystem state plots ###
states <- voi_data_full %>%
  ggplot()+
  labs(
    x="Year",
    y="Average coral cover"
  )+
  geom_line(aes(x=time,
                y=mean_ecosystem,
                group = factor(tech_model),
                colour = factor(tech_model)))+
  facet_wrap(~scenario_label, ncol=1)+
  theme_bw()+
  lims(y=c(0,1))

combined_plot <-ggpubr::ggarrange(K_eff_plot + ggtitle("A"),
                                  states+ theme(legend.position = "none")+ ggtitle("B"),
                                  actions_dev+ theme(legend.position = "none")+ ggtitle("C"),
                                  widths =c(1,1,1),
                                  ncol=3)
ggsave(plot = combined_plot,
       filename = "figures/known_dynamics.pdf",
       width = 20,
       height = 15,
       units = "cm")

