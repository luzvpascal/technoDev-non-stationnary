library(ggplot2)
library(tidyverse)
library(latex2exp)
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

temp_plot <- ggplot(data)+
  theme_bw()+
  geom_ribbon(aes(x =Year,
                  ymin=X5., ymax=X95., group=scenario,
                  fill=scenario),
              alpha=0.1)+
  geom_line(aes(x=Year, y=Mean, group=scenario, col=scenario),
            linewidth = 1.1)+
  labs(y=TeX("$\\Delta T$"))

