library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
source("helper functions/read_solutions.R")
source("helper functions/simulations.R")
source("helper functions/write_POMDPx.R")
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")

## performance data ####
voi_data_uncertain_response <- read.csv("res/value_uncertain_response_deployment.csv",
                          header = TRUE)

voi_data_uncertain_response <- voi_data_uncertain_response %>%
  mutate(r_EVPI=(max_value-test_value)/max_value)

names_test <-  names(voi_data_uncertain_response)[grep("_test$", names(voi_data_uncertain_response))]

best_perf_scenario <- voi_data_uncertain_response %>%
  group_by(across(all_of(names_test)))%>%
  summarise(mean_r_EVPI=mean(r_EVPI),
            sd_EVPI=sd(r_EVPI))%>%
  ungroup()%>%
  group_by(scen_test)%>%
  slice_min(order_by = mean_r_EVPI, with_ties = FALSE)
  # slice_max(order_by = mean_r_EVPI, with_ties = FALSE)

cols_to_match <- c("delta_t_crit_r_test", "delta_t_crit_K_test",
                   "sigmoid_bool_r_test", "sigmoid_bool_K_test",
                   "DEP_EFFECT_test", "scen_test")

# Filter voi_data_uncertain_response to keep rows that match any row in best_perf_scenario
filtered_voi_data_uncertain_response <- voi_data_uncertain_response %>%
  semi_join(best_perf_scenario, by = cols_to_match)


## merge with stationary strategies ####
voi_data_uncertain_response_stationary <- read.csv("res/value_uncertain_response_deployment_stationary_strategy.csv",
                                                   header = TRUE)

voi_data_uncertain_response_stationary <- voi_data_uncertain_response_stationary %>%
  # filter((sigmoid_bool_r==TRUE & sigmoid_bool_K==TRUE))%>%
  mutate(delta_t_crit_r=ifelse(sigmoid_bool_r, delta_t_crit_r,2.75),
         delta_t_crit_K=ifelse(sigmoid_bool_K, delta_t_crit_K,2.75))%>%
  mutate(r_EVPI=(max_value-test_value)/max_value)

names_test_stationary <-  names(voi_data_uncertain_response_stationary)[grep("_test$",
                                                                             names(voi_data_uncertain_response_stationary))]

best_perf_scenario_stationary <- voi_data_uncertain_response_stationary %>%
  group_by(across(all_of(names_test)))%>%
  summarise(mean_r_EVPI=mean(r_EVPI),
            sd_EVPI=sd(r_EVPI))%>%
  ungroup()%>%
  group_by(scen_test)%>%
  slice_min(order_by = mean_r_EVPI, with_ties = FALSE)
# slice_max(order_by = r_EVPI, with_ties = FALSE)

cols_to_match_stationary  <- c("delta_t_crit_r_test", "delta_t_crit_K_test",
                               "sigmoid_bool_r_test", "sigmoid_bool_K_test",
                               "DEP_EFFECT_test", "scen_test","stationary_test")

# Filter voi_data_uncertain_response to keep rows that match any row in best_perf_scenario
filtered_voi_data_uncertain_response_stationary <- voi_data_uncertain_response_stationary %>%
  semi_join(best_perf_scenario_stationary, by = cols_to_match_stationary)

## get the average scenario ####
voi_data_uncertain_response_average <- read.csv("res/value_uncertain_response_deployment_average_model.csv",
                                                   header = TRUE)

voi_data_uncertain_response_average <- voi_data_uncertain_response_average %>%
  # filter((sigmoid_bool_r==TRUE & sigmoid_bool_K==TRUE))%>%
  mutate(delta_t_crit_r=ifelse(sigmoid_bool_r, delta_t_crit_r,2.75),
         delta_t_crit_K=ifelse(sigmoid_bool_K, delta_t_crit_K,2.75))%>%
  mutate(r_EVPI=(max_value-test_value)/max_value)

## get the AM strategy (nicol 2025) ####
voi_data_uncertain_response_AM <- read.csv("res/value_uncertain_response_deployment_AM.csv",
                                           header = FALSE)

names(voi_data_uncertain_response_AM) <- c("time","mean_ecosystem","sd_ecosystem",
                                           "mean_action","sd_action_deploy","mean_value",
                                           "sd_value","max_value","delta_t_crit_r",
                                           "delta_t_crit_K","sigmoid_bool_r","sigmoid_bool_K",
                                           "DEP_EFFECT","scen","scen_test")
voi_data_uncertain_response_AM <- voi_data_uncertain_response_AM %>%
  filter(time==max(time))%>%
  mutate(delta_t_crit_r=ifelse(sigmoid_bool_r, delta_t_crit_r,2.75),
         delta_t_crit_K=ifelse(sigmoid_bool_K, delta_t_crit_K,2.75))%>%
  mutate(r_EVPI=(max_value-mean_value)/max_value)


## merge non-stationary policies, stationary strategies, optimal average model ####
filtered_voi_data_uncertain_response$stationary_test <- "Best non-adaptive"
filtered_voi_data_uncertain_response_stationary$stationary_test <- "Best non-adaptive stationary"
voi_data_uncertain_response_average$stationary_test <- "Passive AM model averaging"
voi_data_uncertain_response_AM$stationary_test <- "Simplified active AM"

common_columns <- intersect(names(filtered_voi_data_uncertain_response),
                            names(voi_data_uncertain_response_AM)
                            )
filtered_voi_data_uncertain_response <- filtered_voi_data_uncertain_response %>%
  select(all_of(common_columns))
filtered_voi_data_uncertain_response_stationary <- filtered_voi_data_uncertain_response_stationary %>%
  select(all_of(common_columns))
voi_data_uncertain_response_average <- voi_data_uncertain_response_average %>%
  select(all_of(common_columns))
voi_data_uncertain_response_AM <- voi_data_uncertain_response_AM %>%
  select(all_of(common_columns))

filtered_voi_data_uncertain_response <- rbind(filtered_voi_data_uncertain_response,
                                              filtered_voi_data_uncertain_response_stationary,
                                              voi_data_uncertain_response_average,
                                              voi_data_uncertain_response_AM
                                              )

## Value of information best performance###
voi_uncertain_response <- voi_data_uncertain_response_AM %>%
# voi_uncertain_response <- filtered_voi_data_uncertain_response %>%
  ggplot(aes(x = delta_t_crit_K, y = delta_t_crit_r)) +
  geom_tile(aes(fill = r_EVPI * 100)) +
  geom_text(aes(label = round(r_EVPI * 100, digits=2)), color = "black") +  # Add values inside the tiles
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  # coord_equal()+
  labs(
    title = "",
    x = TeX("Tipping temperature for K"),
    y = TeX("Tipping temperature for r"),
    fill = "Value of discovering\nthe true non-stationary\nmodel (%)"
  ) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )+
  facet_wrap(~stationary_test+ scen)
voi_uncertain_response
# ggsave(plot = voi_temperatures_best_perf,
#        filename = "figures/voi_cost_of_stationarity_dep_best_strategies.pdf",
#        width = 20,
#        height = 10,
#        units = "cm")
#
mu <- filtered_voi_data_uncertain_response %>%
  group_by(scen, stationary_test) %>%
  summarise(grp.mean=median(r_EVPI))

density_plot <- filtered_voi_data_uncertain_response %>%
  ggplot(aes(x=r_EVPI, fill=stationary_test))+
  facet_wrap(~scen, ncol=5)+
  # geom_vline(data=mu, aes(xintercept=grp.mean, color=stationary_test),
  #            linetype="dashed")+
  geom_density(alpha=0.5)+
  theme_minimal()

box_plots <- filtered_voi_data_uncertain_response %>%
  ggplot(aes(y=r_EVPI, x=stationary_test, fill=stationary_test))+
  geom_boxplot(alpha=0.8)+
  labs(fill="Strategy under uncertainty",
       x="",
       y="Value of discovering\nthe non-stationary model")+
  facet_wrap(~scen, ncol=5)+
  guides(fill = guide_legend(ncol = 2)) +
  # stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "black", fill = "black") +
  # stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "red", size = 1) +
  # stat_summary(fun.y = mean, geom = "text", aes(label = round(after_stat(y), 2)),
  #              size = 3, color = "black") +
  theme_classic()+
  theme(
    axis.line.x = element_blank() ,
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),    # Remove x-axis labels
    legend.position = "top"           # Move the legend to the top
  )+
  geom_hline(yintercept = 0, color="black")
box_plots
ggsave(plot = box_plots,
       filename = "figures/box_plots_value_non_stationary_model.pdf",
       width = 30,
       height = 10,
       units = "cm")

