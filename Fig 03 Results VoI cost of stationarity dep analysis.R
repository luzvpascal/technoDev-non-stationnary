library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")

## performance data ####
tested_scenarios_names <- factor(c("Non-stationary (optimal)",
                                   "stationary strategy\nat 2015",
                                   "stationary strategy\nat 2100",
                                   "stationary strategy\n(average temperature)"),
                                 levels=c("Non-stationary (optimal)",
                                          "stationary strategy\nat 2015",
                                          "stationary strategy\n(average temperature)",
                                          "stationary strategy\nat 2100"))

voi_data_full <- read.csv(paste0("res_onlyK/cost_of_stationarity_deployment_gamma_",gamma,".csv"),
                          header = TRUE)
# voi_data_full <- read.csv(paste0("res/cost_of_stationarity_deployment_gamma_",gamma,".csv"),
#                           header = TRUE)

## Valut of information best performance####
voi_data_full <- voi_data_full %>%
  mutate(stationary_test = tested_scenarios_names[stationary_test])
mean_values <- voi_data_full %>%
  group_by(scen) %>%
  summarise(r_EVPI = mean(r_EVPI))

boxplots_r_EVPI <- voi_data_full %>%
  ggplot() +  # Add x aesthetic
  geom_violin(aes(x = "", y = r_EVPI * 100), fill = "grey") +
  theme_classic() +
  labs(y = "Value of non-stationarity (%)",
       x = "") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  facet_wrap(~scen, ncol = 5)+
  geom_point(data=mean_values,
             aes(x = "", y = r_EVPI * 100),
             shape=23, size = 3, fill = "black")



voi_temperatures_r_EVPI <- voi_data_full %>%
  ggplot(aes(x = delta_t_crit_K, y = delta_t_crit_r)) +
  geom_tile(aes(fill = r_EVPI * 100)) +
  scale_fill_gradient(low = "white", high = "purple") +
  theme_classic() +
  coord_equal()+
  labs(
    title = "",
    x = TeX("Inflection temperature for K ($\\Delta T_{K})"),
    y = TeX("Inflection temperature for r ($\\Delta T_{r})"),
    fill = "Value of non-stationarity (%)"
  ) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )+
  facet_wrap(~scen, ncol=5)

voi_temperatures_best_strategies <- voi_data_full %>%
  ggplot(aes(x = delta_t_crit_K, y = delta_t_crit_r)) +
  geom_tile(aes(fill = stationary_test)) +
  scale_fill_manual(values = c("yellow", "orange","red"))+
  theme_classic() +
  coord_equal()+
  labs(
    title = "",
    x = TeX("Inflection temperature for K ($\\Delta T_{K})"),
    y = TeX("Inflection temperature for r ($\\Delta T_{r})"),
    fill = "Best stationary strategy"
  ) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )+
  facet_wrap(~scen, ncol=5)

voi_temperatures_best_perf <- ggpubr::ggarrange(boxplots_r_EVPI + ggtitle("(A)"),
                                                voi_temperatures_r_EVPI + ggtitle("(B)"),
                                                voi_temperatures_best_strategies + ggtitle("(C)"),
                                                ncol=1,
                                                align = "hv")
ggsave(plot = voi_temperatures_best_perf,
       filename = "figures/voi_cost_of_stationarity_dep_best_strategies.pdf",
       width = 20,
       height = 20,
       units = "cm")
