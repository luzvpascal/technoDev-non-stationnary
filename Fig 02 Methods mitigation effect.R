library(ggplot2)
library(tidyverse)
library(latex2exp)
source("global variables.R")
source("helper functions/functions variable parameters - MDP.R")

## plot temperature ####
library(ggplot2)
library(tidyverse)
library(latex2exp)
source("global variables.R")
source("helper functions/functions variable parameters - MDP.R")

## load temperature data ####
data <- read.csv( "data IPCC/summarized_data.csv")

data <- data%>%
  filter(scenario=="Historical"|scenario=="SSP2_4_5")
data$Mitigation <- "None"
data_mitigation <- data%>%
  mutate(Mean = ifelse(scenario=="Historical", Mean,
                           Mean-1),
         X5. = ifelse(scenario=="Historical", X5.,
                      X5.-1),
         X95. = ifelse(scenario=="Historical", X95.,
                       X95.-1))
data_mitigation$Mitigation <- "Yes"

data <- rbind(data,
              data_mitigation)

data <- data%>%
  mutate(Mitigation = ifelse(scenario=="Historical",
                             "Historical",
                             Mitigation))

## temperature plot ####
temp_plot <- ggplot(data)+
  theme_classic()+
  geom_ribbon(aes(x =Year,
                  ymin=X5., ymax=X95., group=Mitigation,
                  fill=Mitigation),
              alpha=0.1)+
  geom_line(aes(x=Year, y=Mean, group=Mitigation, col=Mitigation),
            linewidth = 1.1)+
  labs(y=TeX("Temperature variation  $\\Delta T$ (°C)"),
       col="")+
  guides(col = "none")+
  theme(legend.position = c(0.15, 0.70))
temp_plot

## Combine ####
tested_delta_t_crit <-1.5
data <- data %>%
  mutate(K_eff = K_function(K_min, K_max, tested_delta_t_crit,
                            Mean,TRUE),
         K_eff_low = K_function(K_min, K_max, tested_delta_t_crit,
                                X5.,TRUE),
         K_eff_up = K_function(K_min, K_max, tested_delta_t_crit,
                               X95.,TRUE)
  )

combined_plot <- ggplot(data)+
  geom_ribbon(aes(x =Year,
                  ymin=K_eff_low, ymax=K_eff_up, group=Mitigation,
                  fill=Mitigation),
              alpha=0.1)+
  theme_classic()+
  geom_line(aes(x=Year, y=K_eff, group=Mitigation, col=Mitigation),
            linewidth = 1.1)+
  labs(y=TeX("Carrying capacity $(K)$"),
       col="")+
  guides(fill = "none")+
  theme(legend.position = "none")+
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c(
      TeX("$K_{min} = 0$"),
      # TeX("$\\frac{K_{max} + K_{min}}{2} = 0.5$"),
      TeX("0.5"),
      TeX("$K_{max} = 1$")
    )
  )

## total plot ####
total_plot <- ggpubr::ggarrange(temp_plot,
                                combined_plot,
                                ncol=2,
                                align = "hv",
                                common.legend = TRUE)

# total_total
ggsave(plot=total_plot,
       filename= "figures/mitigation.svg",
       width = 20,
       height=10,
       units ="cm")
