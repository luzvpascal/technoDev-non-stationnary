library(ggplot2)
library(tidyverse)
library(latex2exp)
source("global variables.R")
## Read and merge datasets ####
folder <- "data IPCC/panel_a/"
files <- list.files(path = folder, full.names = TRUE)

names <-  gsub(paste0(folder, "tas_global_"), "", files)
names <-  gsub(".csv", "", names)

data <- data.frame()
for (index in seq(length(files))){
  new_file <- files[index]
  new_name <- names[index]
  data_new <- read.csv(new_file)
  data_new$scenario <- new_name
  data <- rbind(data, data_new)
}

write.csv(data, "data IPCC/summarized_data.csv", row.names = FALSE)

## plot temperature ####
data <- read.csv( "data IPCC/summarized_data.csv")
temp_plot <- ggplot(data)+
  theme_bw()+
  geom_ribbon(aes(x =Year,
                  ymin=X5., ymax=X95., group=scenario,
                  fill=scenario),
              alpha=0.1)+
  geom_line(aes(x=Year, y=Mean, group=scenario, col=scenario),
            linewidth = 1.1)+
  labs(y=TeX("Temperature variation $(\\Delta T)$"),
       col="")+
  guides(fill = "none")+
  theme(legend.position = c(0.15, 0.70))

## plot K_eff ####
tested_delta <- seq(-1, 6, 0.01)
K_eff_data <- data.frame()
for (tested_delta_t_crit in tested_delta_t_crit_K){
  K_eff <- c()
  for (delta_t in tested_delta){
    K_eff <- c(K_eff, K_function(K_min, K_max, tested_delta_t_crit,
                                 delta_t,sigmoid_bool_K))
  }

  K_eff_data <- rbind(K_eff_data,
                      data.frame(temp=tested_delta,
                                 K_eff=K_eff,
                                 tested_delta_t_crit=tested_delta_t_crit)
  )
}

K_eff_plot <- ggplot(K_eff_data)+
  geom_line(aes(x=temp, y = K_eff,
                group=factor(tested_delta_t_crit),
                col=factor(tested_delta_t_crit)))+
  theme_bw()+
  labs(x=TeX("Temperature variation $(\\Delta T)$"),
       y=TeX("Maximum carrying capacity ($K$)"),
       col=TeX("Inflection\ntemperature\n$(\\Delta T_{crit})$"))+
  scale_color_brewer(palette = "Spectral")+
  theme(legend.position = c(0.15, 0.3))

total <- ggpubr::ggarrange(temp_plot,
                  K_eff_plot,
                  labels=c("A","B"))
ggsave(plot=total,
       filename= "figures/temperatures and capacity.svg",
       width = 30,
       height=10,
       units ="cm")
