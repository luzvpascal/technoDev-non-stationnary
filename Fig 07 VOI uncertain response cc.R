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
   # paste0("res_onlyK/value_uncertain_response_deployment_best_non_adaptive_gamma_",gamma,".csv"),
   paste0("res/value_uncertain_response_deployment_best_non_adaptive_gamma_",gamma,".csv"),
  header = TRUE)


## run experiments ####
voi_solution <- data.frame()

names_test <-  names(voi_data_uncertain_response)[grep("_test$", names(voi_data_uncertain_response))]

experiments <- expand.grid(delta_t_crit_r=round(tested_delta_t_crit_r,digits=3),
                           delta_t_crit_K=round(tested_delta_t_crit_K,digits=3))
experiments_test <- experiments
N_experiments <- 5000

tested_k <- c(2,4,10,16,20,40,100)
list_highest_EVPI <- rep(0,length(tested_k))
list_indexes <- list()
start <- Sys.time()
for (index_k in seq_along(tested_k)){
  k <- tested_k[index_k]
  print(k)
  for (i in seq(min(N_experiments, choose(nrow(experiments),k)))){
    index_exp <- sample(nrow(experiments), k,replace = FALSE)
  # for (index_exp in seq(ncol(exp_indexes))){
    experiments_now <- experiments[index_exp,]
    experiments_test_now <- experiments_test[index_exp,]
    # experiments_now <- experiments[exp_indexes[,index_exp],]
    # experiments_test_now <- experiments_test[exp_indexes[,index_exp],]
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

    filtered_voi$delta_t_crit_K_min <- min(scenanarios_now_K)
    filtered_voi$delta_t_crit_K_max <- max(scenanarios_now_K)
    filtered_voi$delta_t_crit_r_min <- min(scenanarios_now_r)
    filtered_voi$delta_t_crit_r_max <- max(scenanarios_now_r)

    filtered_voi$k <- k
    voi_solution <- rbind(voi_solution,
                          filtered_voi)

    mean_EVPI <- mean(filtered_voi$rEVPI)
    if (list_highest_EVPI[index_k] < mean_EVPI){
      list_highest_EVPI[index_k] <- mean_EVPI
      list_indexes[[index_k]] <- index_exp
    }
  }
}

end <- Sys.time()
print(end-start)
# write.csv(voi_solution,  paste0("res_onlyK/VOI_uncertain_response_gamma_",gamma,".csv"), row.names = FALSE)
write.csv(voi_solution,  paste0("res/VOI_uncertain_response_gamma_",gamma,".csv"), row.names = FALSE)

# voi_solution <- read.csv(paste0("res_onlyK/VOI_uncertain_response_gamma_",gamma,".csv"))
voi_solution <- read.csv(paste0("res/VOI_uncertain_response_gamma_",gamma,".csv"))
mean_values <- voi_solution %>%
  mutate(rEVPI=rEVPI*100)%>%
  group_by(scen,k) %>%
  # group_by(k) %>%
  summarise(mean_EVPI = mean(rEVPI),
            lower=quantile(rEVPI,0.025),
            upper=quantile(rEVPI,0.975),
            mini=min(rEVPI),
            maxi=max(rEVPI))
max_k <- max(mean_values$k)
value_vs_nmodels <- mean_values %>%
  ggplot()+
  labs(x="Number of competing models",
       y="Value of discovering\nthe ecosystem\nresponse to climate change (%)",
       fill="",
       col="")+
  # geom_boxplot(aes(x=k,y=rEVPI*100,group=k),fill="grey")+
  geom_ribbon(aes(x=k, ymin=mini,ymax=maxi, fill="Minimum and maximum"))+
  geom_ribbon(aes(x=k, ymin=lower,ymax=upper, fill="95%-quantiles"))+
  scale_fill_manual(values=c("lightblue4","lightblue2"))+
  # scale_alpha_discrete(range =c(0.1,0.05))+
  geom_line(aes(x=k,y=mean_EVPI,group="Average",col="Average"))+
  geom_point(aes(x=k,y=mean_EVPI,group="Average",col="Average"))+
  scale_colour_manual(values="black")+
  facet_wrap(~scen,nrow=1)+
  scale_x_continuous(trans='log10')+
  # scale_x_continuous(breaks=seq(2,max_k,4))+
  theme_classic()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

value_vs_difference <- voi_solution %>%
  # filter(k==2)%>%
  mutate(abs_diff_K=abs(delta_t_crit_K_max-delta_t_crit_K_min))%>%
  mutate(abs_diff_r=abs(delta_t_crit_r_max-delta_t_crit_r_min))%>%
  group_by(abs_diff_K,abs_diff_r,scen)%>%
  summarise(rEVPI=mean(rEVPI))%>%
  ggplot()+
  geom_tile(aes(x=abs_diff_r, y=abs_diff_K, fill=(rEVPI*100)))+
  # geom_tile(aes(x=k, y=abs_diff_K, fill=(rEVPI*100)))+
  scale_fill_gradient(low="white",high="darkblue")+
  facet_wrap(~scen,nrow=1)+
  labs(x=TeX("Maximum difference between models carrying capaticities max($\\Delta T_{K_i}-\\Delta T_{K_j}$)"),
       y=TeX("Maximum difference\nbetween models growth rate\n max($\\Delta T_{r_i}-\\Delta T_{r_j}$)"),
       fill="Average value of discovering the\necosystem response to climate change (%)")+
  # scale_x_continuous(breaks=seq(2,11,3))+
  scale_x_continuous(breaks=seq(0.1,1.5,0.7))+
  scale_y_continuous(breaks=seq(0.1,1.5,0.7))+
  theme_classic()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot <- ggarrange(value_vs_nmodels+ggtitle("(A)"),
                  value_vs_difference+ggtitle("(B)"),
                  align = "hv",
                  nrow=2)

ggsave(plot = plot,
       # filename = "figures_onlyK/voi_value_discovering_response.svg",
       filename = "figures/voi_value_discovering_response.svg",
       width = 20,
       height = 15,
       units = "cm")

##############################
voi_solution%>%
  # filter(k==100)%>%
  filter(k==4)%>%
  # filter(k==2)%>%
  group_by(scen)%>%
  slice_max(order_by = rEVPI)
  # slice_max(order_by = mean_EVPI)

experiments[list_indexes[[3]],]
