library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
## performance data ####
voi_data_uncertain_response <- read.csv(
  res_file <- paste0("res/value_uncertain_response_deployment_best_non_adaptive_gamma_",gamma,".csv"),
  header = TRUE)


## run experiments ####
voi_solution <- data.frame()
experiments_list_r <- list()
experiments_list_K <- list()

experiments_r <- combn(seq(1, 2.5,by=0.3),1)
experiments_r <- round(experiments_r, digits=3)
experiments_K <- combn(seq(1, 2.5,by=0.3),2)
experiments_K <- round(experiments_K, digits=3)
names_test <-  names(voi_data_uncertain_response)[grep("_test$", names(voi_data_uncertain_response))]

experiments <- expand.grid(delta_t_crit_r=seq(1, 2.5,by=0.3),
                           delta_t_crit_K=seq(1, 2.5,by=0.3))
experiments_test <- expand.grid(delta_t_crit_r_test=seq(1, 2.5,by=0.3),
                           delta_t_crit_K_test=seq(1, 2.5,by=0.3))
exp_indexes <- combn(nrow(experiments),2)

for (index_exp in seq(ncol(exp_indexes))){
  print(index_exp)
  experiments_now <- experiments[exp_indexes[,index_exp],]
  experiments_test_now <- experiments_test[exp_indexes[,index_exp],]
  scenanarios_now_r <- experiments_now$delta_t_crit_r
  scenanarios_now_K <- experiments_now$delta_t_crit_K

  # experiments_list[[length(experiments_list)+1]] <- scenanarios_now
  #filter responses
  voi_data_uncertain_response_exp <- voi_data_uncertain_response %>%
    semi_join(experiments_now)%>%
    semi_join(experiments_test_now)

  #best perf scenario
  best_perf_scenario <- voi_data_uncertain_response_exp %>%
    group_by(across(all_of(names_test)))%>%
    summarise(mean_test_value=mean(test_value),
              sd_test_value=sd(test_value))%>%
    ungroup()%>%
    group_by(scen_test)%>%
    slice_max(order_by = mean_test_value, with_ties = FALSE)

  # Filter voi_data_uncertain_response to keep rows that match any row in best_perf_scenario
  filtered_voi <- voi_data_uncertain_response_exp %>%
    semi_join(best_perf_scenario, by = names_test)

  filtered_voi <- filtered_voi%>%
    group_by(scen)%>%
    summarise(rEVPI=(mean(max_value)-mean(test_value ))/mean(max_value))

  filtered_voi$delta_t_crit_K_1 <- scenanarios_now_K[1]
  filtered_voi$delta_t_crit_K_2 <- scenanarios_now_K[2]

  filtered_voi$delta_t_crit_r_1 <- scenanarios_now_r[1]
  filtered_voi$delta_t_crit_r_2 <- scenanarios_now_r[2]

  voi_solution <- rbind(voi_solution,
                        filtered_voi)
}


mean_values <- voi_solution %>%
  # filter(delta_t_crit_r_1==delta_t_crit_r_2)%>%
  # filter(delta_t_crit_r_1==2.5)%>%
  # filter(delta_t_crit_K_1==1)%>%
  # filter(delta_t_crit_K_2==2.5)%>%
  group_by(scen) %>%
  summarise(rEVPI = mean(rEVPI))

case_values <- voi_solution %>%
  filter(delta_t_crit_r_1==delta_t_crit_r_2)%>%
  filter(delta_t_crit_r_1==2.5)%>%
  filter(delta_t_crit_K_1==1)%>%
  filter(delta_t_crit_K_2==2.5)%>%
  group_by(scen) %>%
  summarise(rEVPI = mean(rEVPI))

voi_solution %>%
  # filter(delta_t_crit_r_1==delta_t_crit_r_2)%>%
  # filter(delta_t_crit_r_1==2.5)%>%
  # filter(delta_t_crit_K_1==1)%>%
  # filter(delta_t_crit_K_2==2.5)%>%
  ggplot()+
  labs(x="",
       y="Value of discovering the ecosystem\nresponse to climate change (%)")+
  geom_violin(aes(x="",y=rEVPI*100),fill="grey")+
  facet_wrap(~scen,nrow=1)+
  geom_point(data=mean_values,
             aes(x = "", y = rEVPI * 100),
             shape=23, size = 3, fill = "black")+
  geom_point(data=case_values,
             aes(x = "", y = rEVPI * 100),
             shape=23, size = 3, fill = "red")+
  theme_classic()



voi_solution %>%
  filter(delta_t_crit_r_1==delta_t_crit_r_2)%>%
  mutate(abs_diff_K=abs(delta_t_crit_K_1-delta_t_crit_K_2))%>%
  mutate(abs_diff_r=abs(delta_t_crit_r_1-delta_t_crit_r_2))%>%
  group_by(abs_diff_K,delta_t_crit_r_1,scen)%>%
  summarise(rEVPI=median(rEVPI))%>%
  ggplot()+
  geom_tile(aes(x=abs_diff_K, y=delta_t_crit_r_1, fill=(rEVPI*100)))+
  scale_fill_gradient(low="white",high="black")+
  facet_wrap(~scen,nrow=1)+
  theme_classic()

voi_solution%>%
  slice_max(order_by = rEVPI, with_ties = FALSE)
