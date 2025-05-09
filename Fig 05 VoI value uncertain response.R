library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
cols_to_select <- c("delta_t_crit_r","delta_t_crit_K",
                    "sigmoid_bool_r" ,"sigmoid_bool_K" ,
                    "DEP_EFFECT" , "scen","scen_test","max_value","test_value",
                    "sd_test_value",
                    "experiment")

#######################################
## SIMULATION BASED ###################
#######################################
## active AM ##################
# voi_data_uncertain_response_AM <- read.csv( paste0("res/value_uncertain_response_deployment_AM_gamma_",gamma,".csv"),
#                                            header = FALSE)
# names(voi_data_uncertain_response_AM) <- c("time","mean_ecosystem","sd_ecosystem",
#                                            "mean_action","sd_action_deploy","mean_value",
#                                            "sd_value","max_value","delta_t_crit_r",
#                                            "delta_t_crit_K","sigmoid_bool_r","sigmoid_bool_K",
#                                            "DEP_EFFECT","scen","scen_test","experiment")
voi_data_uncertain_response_AM <- read.csv( paste0("res/value_uncertain_response_deployment_AM_perron_LB_gamma_",gamma,".csv"),
                                           header = FALSE)
names(voi_data_uncertain_response_AM) <- c("mean_value", "sd_value","max_value","delta_t_crit_r",
                                           "delta_t_crit_K","sigmoid_bool_r","sigmoid_bool_K",
                                           "DEP_EFFECT","scen","scen_test","experiment")


voi_data_uncertain_response_AM <- voi_data_uncertain_response_AM %>%
  mutate(test_value=mean_value) %>%
  mutate(sd_test_value=sd_value/sqrt(1000)) %>%
  # filter(time==max(time))%>%
  select(all_of(cols_to_select))
## passive AM ##################
voi_data_uncertain_response_passive_AM <- read.csv("res/value_uncertain_response_deployment_passive_AM.csv",
                                                   header = FALSE)

names(voi_data_uncertain_response_passive_AM) <- c("time","mean_ecosystem","sd_ecosystem",
                                           "mean_action","sd_action_deploy","mean_value",
                                           "sd_value","max_value","delta_t_crit_r",
                                           "delta_t_crit_K","sigmoid_bool_r","sigmoid_bool_K",
                                           "DEP_EFFECT","scen","scen_test","experiment")

voi_data_uncertain_response_passive_AM <- voi_data_uncertain_response_passive_AM %>%
  mutate(test_value=mean_value) %>%
  mutate(sd_test_value=sd_value/sqrt(100)) %>%
  filter(time==max(time))%>%
  select(all_of(cols_to_select))

## best non-adaptive ##################
voi_data_uncertain_response_best_nonadaptive  <-
  read.csv(
    paste0("res/value_uncertain_response_deployment_best_non_adaptive_solution_gamma_",gamma,".csv"),
        header = TRUE)
voi_data_uncertain_response_best_nonadaptive$sd_test_value <- 0

voi_data_uncertain_response_best_nonadaptive <-
  voi_data_uncertain_response_best_nonadaptive %>%
  select(all_of(cols_to_select))
## best stationary #####
voi_data_uncertain_response_best_stationary  <-
  read.csv(
    paste0("res/value_uncertain_response_deployment_stationary_strategy_solution_gamma_",gamma,".csv"),
           header = TRUE)

voi_data_uncertain_response_best_stationary$sd_test_value <- 0
voi_data_uncertain_response_best_stationary <-
  voi_data_uncertain_response_best_stationary %>%
  select(all_of(cols_to_select))

## rbind all results #####
full_voi_data <- rbind(voi_data_uncertain_response_AM,
                       voi_data_uncertain_response_passive_AM,
                       voi_data_uncertain_response_best_nonadaptive,
                       voi_data_uncertain_response_best_stationary)

## rename ####
full_voi_data <- full_voi_data %>%
  # filter(experiment != "experiment3")%>%
  mutate(scen_test = ifelse(scen_test == "AM_85",
                           "Active AM",
                           scen_test))%>%
  mutate(scen_test =ifelse(scen_test == "AM_10",
                           "Simplified active AM (granularity 10)",
                           scen_test))%>%
  mutate(scen_test =ifelse(scen_test == "passive_AM",
                           "Passive AM",
                           scen_test))%>%
  mutate(scen_test =ifelse(scen_test == "best non-adaptive",
                           "Best non-adaptive",
                           scen_test))%>%
  mutate(scen_test =ifelse(scen_test == "best stationary",
                           "Best stationary",
                           scen_test))%>%
  mutate(scen_test = factor(scen_test,
                            levels = c("Active AM",
                                      "Simplified active AM (granularity 10)",
                                      "Passive AM",
                                      "Best non-adaptive",
                                      "Best stationary"
                            ))) %>%
  mutate(experiment =ifelse(experiment == "experiment1",
                           "2 competing models",
                           experiment))%>%
  mutate(experiment =ifelse(experiment == "experiment2",
                            "4 competing models",
                            experiment)) %>%
  mutate(experiment = factor(experiment,
                            levels = c("2 competing models",
                                       "4 competing models"
                                       )))

## calculate uncertainty ####

full_voi_data <- full_voi_data %>%
  mutate(test_var = sd_test_value**2)%>%
  group_by(experiment, scen, scen_test)%>%
  summarize(avg_max = mean(max_value),
         avg_test = mean(test_value),
         EV = ((avg_max-avg_test)/avg_max),
         n = n(),
         var = sum(test_var)/((n*avg_max)**2),
         sd = sqrt(var),
         lower_bound = EV - 1.96*sd,
         upper_bound = EV + 1.96*sd
         )

## point estimates for each scenario ####
boxplots <- full_voi_data %>%
  ggplot() +
  geom_point(aes(x = scen_test,
                  y = EV,
                  col=scen_test),
               size = 2)+
  geom_errorbar(aes(x = scen_test,
                    ymin = lower_bound,
                    ymax = upper_bound,
                    col = scen_test),
                width = 0.3)+ # Adjust width for aesthetics
  guides(col=guide_legend(nrow=2,byrow=TRUE))+
  theme_classic()+
  labs(y=TeX("$\\frac{EV_{certainaty} - EV_{uncertainaty}}{EV_{certainaty}}$"),
       x="",
       fill="Strategy",
       col="Strategy")+
  # facet_wrap(~experiment)+
  facet_wrap(~experiment+scen,ncol=5)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "top")+
  scale_color_manual(values = c("darkblue",
                                "purple",
                                "darkgreen",
                                "darkred",
                                "orange"))+
  lims(y=c(0, max(full_voi_data$upper_bound)))
boxplots

# ggsave("figures/box_plots_value_non_stationary_model.pdf",
#        boxplots,
#        height = unit(6,"cm"),
#        width = unit(8,"cm"),
#        )
