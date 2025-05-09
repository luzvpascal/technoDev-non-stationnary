library(ggplot2)
library(tidyverse)
library(latex2exp)
source("global variables.R")
source("helper functions/functions variable parameters - MDP.R")

## plot temperature ####
data <- read.csv( "data IPCC/summarized_data.csv")
temp_plot <- ggplot(data)+
  geom_ribbon(aes(x =Year,
                  ymin=X5., ymax=X95., group=scenario,
                  fill=scenario),
              alpha=0.1)+
  theme_classic()+
  geom_line(aes(x=Year, y=Mean, group=scenario, col=scenario),
            linewidth = 1.1)+
  labs(y=TeX("Variation of average temperature $(\\Delta T)$"),
       col="")+
  scale_color_brewer(palette = "YlOrRd")+
  scale_fill_brewer(palette = "YlOrRd")+
  guides(fill = "none")+
  theme(legend.position = c(0.15, 0.70))
temp_plot
## plot temperature ####
tested_delta <- seq(0, 3, 0.005)
tested_delta_t_crit <-( max(tested_delta)+min(tested_delta))/2
K_eff <- c()
for (delta_t in tested_delta){
  K_eff <- c(K_eff, K_function(K_min, K_max, tested_delta_t_crit,
                               delta_t,TRUE))
}

K_eff_data <- data.frame(temp=tested_delta,
                         K_eff=c(K_eff))

K_eff_plot <- K_eff_data %>%
  ggplot() +
  geom_line(aes(x = temp, y = K_eff)) +
  labs(
    x = TeX("Variation of average temperature $(\\Delta T)$"),
    y = TeX("Carrying capacity $(K)$")
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c(
      TeX("$K_{min} = 0$"),
      # TeX("$\\frac{K_{max} + K_{min}}{2} = 0.5$"),
      TeX("0.5"),
      TeX("$K_{max} = 1$")
    )
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 1.5, 2, 3),
    labels = c("0", "1", TeX("$\\Delta T_{K} = 1.5$"), "2", "3")
  ) +
  geom_hline(yintercept = c(0, 0.5, 1), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = tested_delta_t_crit, linetype = "dashed", color = "gray") +
  theme_classic()
K_eff_plot

## Combine ####
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
                  ymin=K_eff_low, ymax=K_eff_up, group=scenario,
                  fill=scenario),
              alpha=0.1)+
  theme_classic()+
  geom_line(aes(x=Year, y=K_eff, group=scenario, col=scenario),
            linewidth = 1.1)+
  labs(y=TeX("Carrying capacity $(K)$"),
       col="")+
  scale_color_brewer(palette = "YlOrRd")+
  scale_fill_brewer(palette = "YlOrRd")+
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
                                K_eff_plot,
                                combined_plot,
                                ncol=1,
                                align = "hv",
                                common.legend = TRUE)

ggsave(plot=total_plot,
       filename= "figures/K_eff_conceptual.svg",
       width = 10,
       height=25,
       units ="cm")
