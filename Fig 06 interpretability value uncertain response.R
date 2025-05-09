library(ggnewscale)
library(latex2exp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MDPtoolbox)
## load global variables ####
source("global variables.R")
source("helper functions/read_solutions.R")
source("helper functions/functions variable parameters - MDP.R")
source("helper functions/simulations transition between models.R")
source("helper functions/generate_transition_reward_list_cost_stationarity.R")
source("helper functions/simulations AM.R")
source("helper functions/write_hmMDP_any_AM.R")
source("helper functions/build transition functions MOMDP.R")
source("helper functions/perron_lower_bound.R")

## K and r plot ####

tested_delta <- seq(-0.25, 4, 0.005)
tested_delta_t_crit <- unique(filtered_scenarios_2_models$delta_t_crit_K)
# tested_delta_t_crit <- c(1.3,2.5)
K_eff_data <- data.frame()
for (delta_t_crit in tested_delta_t_crit ){
  K_eff <- c()
  for (delta_t in tested_delta){
    K_eff <- c(K_eff, K_function(K_min, K_max, delta_t_crit,
                                 delta_t,TRUE))
  }

  K_eff_data_now <- data.frame(temp=tested_delta,
                           K_eff=c(K_eff),
                           delta_t_crit=delta_t_crit)

  K_eff_data <- rbind(K_eff_data,
                      K_eff_data_now)

}

K_eff_plot <- K_eff_data %>%
  ggplot() +
  geom_line(aes(x = temp, y = K_eff, group=delta_t_crit, linetype=factor(delta_t_crit)),
            linewidth=1) +
  labs(
    x = TeX("Variation of average temperature $(\\Delta T)$"),
    y = TeX("Carrying capacity $(K)$")
    ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c(
      TeX("$K_{min} = 0$"),
      TeX("0.5"),
      TeX("$K_{max} = 1$")
    )
  ) +
  annotate("text",x=tested_delta_t_crit[1]-0.65, y=0.25, label="Low thermal\nresistance\nmodel (belief 1-w)",angle=0)+
  annotate("text",x=tested_delta_t_crit[2]+0.65, y=0.75, label="High thermal\nresistance\nmodel (belief w)",angle=0)+
  theme_classic()+
  theme(
    legend.position = "none"
  )
ggsave("figures/AM_interpretable_solutions_K_eff_plot.svg",
       K_eff_plot,
       width = 12,
       height = 10,
       units="cm")

## K plot through time ####
data <- read.csv( "data IPCC/summarized_data.csv")
res_data <- data.frame()
for (delta_t_crit in tested_delta_t_crit ){
  data_t <- data
  data_t <- data_t %>%
  mutate(K_eff = K_function(K_min, K_max, delta_t_crit,
                            Mean,TRUE),
         K_eff_low = K_function(K_min, K_max, delta_t_crit,
                                X5.,TRUE),
         K_eff_up = K_function(K_min, K_max, delta_t_crit,
                               X95.,TRUE)
  )
  data_t$delta_t_crit <- delta_t_crit
  res_data <- rbind(res_data, data_t)
}
combined_plot <- res_data %>%
  filter(scenario %in% c("SSP1_1_9","SSP2_4_5","SSP5_8_5"))%>%
  ggplot()+
  geom_ribbon(aes(x =Year,
                  ymin=K_eff_low, ymax=K_eff_up, group=factor(delta_t_crit),
                  fill=factor(delta_t_crit)),
              alpha=0.1)+
  theme_classic()+
  geom_line(aes(x=Year, y=K_eff, group=factor(delta_t_crit), col=factor(delta_t_crit)),
            linewidth = 1.1)+
  # scale_colour_manual(values=c("lightblue","darkred"))+
  labs(y=TeX("Carrying capacity $(K)$"),
       col="")+
  guides(fill = "none")+
  theme(legend.position = "none")+
  facet_wrap(~scenario,ncol=1)+
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c(
      TeX("$K_{min} = 0$"),
      # TeX("$\\frac{K_{max} + K_{min}}{2} = 0.5$"),
      TeX("0.5"),
      TeX("$K_{max} = 1$")
    )
  )
combined_plot
ggsave("figures/AM_interpretable_solutions_K_eff_plot.svg",
       combined_plot,
       width = 8,
       height = 12,
       units="cm")
## interpretability of AM solutions####

results <- data.frame()
selected_times <- c(1, 26, 51, 76)
# for (scen in c("SSP1_1_9", "SSP5_8_5")){
for (scen in c("SSP1_1_9","SSP2_4_5", "SSP5_8_5")){
    filtered_scenarios_now <- filtered_scenarios_2_models %>% filter(scenario==scen)
    temperature_data_filter <- temperature_data %>% filter(scenario == scen)
    ## build transition matrix for current problem####
    transition_matrix_list <- list()
    solution_list <- list()
    for (index_config in seq(nrow(filtered_scenarios_now))) {
      print(index_config)
      #set scenarios variables ####
      config <- filtered_scenarios_now[index_config,]
      delta_t_crit_r <- config$delta_t_crit_r
      delta_t_crit_K <- config$delta_t_crit_K
      sigmoid_bool_r <- config$sigmoid_bool_r
      sigmoid_bool_K <- config$sigmoid_bool_K
      DEP_EFFECT <- c(0, config$dep_effect,config$dep_effect*2)

      ## Call the function to generate transition matrices and rewards
      transition_matrix <- transition_function(
        ecosystem_states, temperature_states, temperature_data_filter, DEP_EFFECT,
        r_min, r_max, delta_t_crit_r, sigmoid_bool_r, K_min, K_max, delta_t_crit_K,
        sigmoid_bool_K, time_step, sigma_eco
      )

      transition_matrix_list[[index_config]] <- transition_matrix
      Reward <- reward_function(
        ecosystem_states, seq(max(temperature_data_filter$Year)),
        DEP_EFFECT, cost_deploy
      )

      solution <- mdp_value_iteration(transition_matrix,
                                      Reward,
                                      gamma)

      solution_list[[index_config]] <- solution
    }

    ### end####
    N_models <- 85


    #read POMDP solutions####
    file_name <- paste0("pomdpx_gamma",gamma,
                        "/value_uncertain_response_deployment_AM_",scen,
                        "_",N_models,"_experiment",1)
    FILE <- paste0(file_name, ".pomdpx")
    OUTPUT_FILE <- paste0(file_name, ".policyx")

    alphas <- read_policyx2(OUTPUT_FILE)
    alphas <- perron_lower_bound(transition_matrix_list,
                                 solution_list,
                                 Reward,
                                 gamma,
                                 alphas)

    ## obtain results ####
    # for (time_step in seq(max(temperature_data$Year))){
    # for (time_step in seq(1,85,length.out=5)){
    for (time_index in seq_along(selected_times)){
      print(time_index)
      for (state_index in seq(1,N_ecosystem+1)){
        current_state <- tuple_to_index(selected_times[time_index], state_index, N_ecosystem+1)
        for (belief in seq(0,1,0.01)){
          B_PAR <- c(belief, 1-belief)
          output <- interp_policy2(B_PAR,
                                    obs = current_state,
                                    alpha = alphas$vectors,
                                    alpha_action = alphas$action,
                                    alpha_obs = alphas$obs,
                                    alpha_index = alphas$index)

          results_now <- data.frame(state=state_index,coral_cover=ecosystem_states[state_index],
                                    time=selected_times[time_index]+2014,belief=1-belief,
                                    time_index=time_index,
                                    action=as.character(output[[2]]-1),value=output[[1]], scen=scen)

          results <- rbind(results, results_now)
        }
      }
    }
}

## create polygones for each solution####
slant <- 0.01/2
width <- 0.01
height <- 0.1

slanted_tiles <- results %>%
  rowwise() %>%
  mutate(
    id = cur_group_id()
  ) %>%
  do({
    x <- .$belief
    y <- .$coral_cover
    action <- .$action
    time_index <- .$time_index
    time <- .$time
    id <- .$id
    scen <- .$scen

    # Define slanted tile corners
    data.frame(
      x = c(x, x + width, x + width , x),
      y = c(y, y, y + height, y + height),
      action = action,
      time = time,
      time_index = time_index,
      id = id,
      scen=scen
    )
  }) %>%
  ungroup()

##plots preparation####
library(ggplot2)
diff_time <- 2.6
slanted_tiles$y_plot <- slanted_tiles$y+slanted_tiles$x*slant/width
slanted_tiles$x_plot <- slanted_tiles$x+(slanted_tiles$time_index-1)*diff_time
#axis data sets
x_axis_data <- data.frame(x = -0.05, xend = 1.05, y = -0.05-slant/width*0.05, yend = 1.05*slant/width-0.05,
                          time=seq(4))
x_axis_data <- x_axis_data%>%
  mutate(x=x+(time-1)*diff_time,
         xend=xend+(time-1)*diff_time)

y_axis_data <- data.frame(x = -0.05, xend = -0.05, y = -0.05-slant/width*0.05, yend = 1+0.05,time=seq(4))
y_axis_data <- y_axis_data%>%
  mutate(x=x+(time-1)*diff_time,
         xend=xend+(time-1)*diff_time)
#axis  data sets
annotation_x_axis <- expand.grid(x = seq(0, 1, by = 0.5), time=seq(4))
annotation_x_axis <- annotation_x_axis%>%
  mutate(x_plot=x+(time-1)*diff_time,
         y = x*slant/width-0.125,
         label=as.character(x),
         angle=45)
annotation_y_axis <- expand.grid(y = seq(0, 1, by = 0.5), time=seq(4))
annotation_y_axis <- annotation_y_axis%>%
  mutate(x_plot=-0.125+(time-1)*diff_time,
         label=as.character(y),
         angle=0)
# axis labels data sets
label_x_axis <- data.frame(x = 0.65, time=seq(4))
label_x_axis <- label_x_axis%>%
  mutate(x_plot=x+(time-1)*diff_time,
         y = x*slant/width-0.25,
         label="Belief (w)",
         angle=45)
label_y_axis <- data.frame(x = -0.5, y=0.5, time=seq(4))
label_y_axis <- label_y_axis%>%
  mutate(x_plot=x+(time-1)*diff_time,
         label="Coral cover (%)",
         angle=90)

##triple dot annotation
label_triple_dots <- data.frame(y = 0.75, time=seq(3),label="...",angle=0)
label_triple_dots <- label_triple_dots%>%
  mutate(x = (time-1)*diff_time+diff_time*0.6)
## time segment
time_segment <- data.frame(x = -0.5, xend = 1.5, y = -0.3, yend = -0.3,
                           time=4)
time_segment <- time_segment%>%
  mutate(xend=xend+(time-1)*diff_time)

time_annotations <- data.frame(x = 0.5, y = -0.4,
                               time=seq(4))
time_annotations <- time_annotations%>%
  mutate(x_plot=x+(time-1)*diff_time,
         label=selected_times[time]+2014,
         angle=0)

time_ticks <- data.frame(x = 0.5, xend = 0.5, y = -0.32, yend = -0.28,
                           time=seq(4))
time_ticks <- time_ticks%>%
  mutate(x=x+(time-1)*diff_time,
         xend=xend+(time-1)*diff_time)


time_label <- data.frame(x = 0.5, y = -0.5,
                         time=2,
                         label="Time (yrs)",
                         angle=0)

time_label <- time_label%>%
  mutate(x_plot=x+(time-1)*diff_time+diff_time/2)
## resulting plot####
res_plot <- slanted_tiles%>%
  # filter(scen!="SSP2_4_5")%>%
  ggplot(aes(x = x_plot, y = y_plot, group = id, fill=as.character(action)))+
  scale_fill_manual(values=c("grey","lightgreen","darkgreen"))+
  geom_polygon() +
  facet_wrap(~scen,nrow=3)+
  coord_fixed(ratio = 2)+
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom"
  )+
  labs(fill = TeX("Local temperature mitigation ($\\Delta T^{techno}$ °C)   ")) +
  geom_segment(data=x_axis_data,
               aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE) +  # x-axis lines
  geom_segment(data=y_axis_data,aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE) + #y axis lines
  geom_text(data=annotation_x_axis, aes( x=x_plot, y=y, label=label, angle=angle), inherit.aes = FALSE)+#x axis breaks
  geom_text(data=annotation_y_axis, aes( x=x_plot, y=y, label=label, angle=angle), hjust = 1,inherit.aes = FALSE)+#yaxis breaks
  geom_text(data=label_x_axis, aes(x=x_plot, y=y, label=label, angle=angle), inherit.aes = FALSE) + #x-axis labels
  geom_text(data=label_y_axis, aes(x=x_plot, y=y, label=label, angle=angle), inherit.aes = FALSE) + #y-axis labels
  geom_text(data=label_triple_dots, aes(x=x, y=y, label=label, angle=angle), size=5,inherit.aes = FALSE)+#triple dots
  geom_segment(data=time_segment,aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.1,"cm")),
               inherit.aes = FALSE)+
  geom_segment(data=time_ticks,aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE)+
  geom_text(data=time_annotations, aes(x=x_plot, y=y, label=label, angle=angle), size=4,inherit.aes = FALSE)+#triple dots
  geom_text(data=time_label, aes(x=x_plot, y=y, label=label, angle=angle), size=5,inherit.aes = FALSE)#triple dots


# both_plots <- ggarrange(K_eff_plot,
#                         res_plot,
#                         nrow=1,
#                         widths = c(2,5),
#                         heights = c(1,2.5))

ggsave("figures/AM_interpretable_solutions.svg",
       res_plot,
       width = 30,
       height = 30,
       units="cm")
