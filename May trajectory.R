library(ggplot2)
library(ggpubr)

r <- 0.7
K <- 1
b <- 0.2
q <- 2
sigma <- 0.02

alpha_slope <- c(0.001)
alpha <- 0.001
trajectory <- c(x_t)
trajectory_mu <- c()
trajectory_alpha <- c(alpha_t)
horizon <- 500
res <- data.frame()
CONSERVATION_ACTION <- TRUE

for (CONSERVATION_ACTION in c(TRUE, FALSE)){
  x_t <- 0.8
  alpha_t <- 0.19
  # alpha_t <- 0
  trajectory <- c(x_t)
  trajectory_alpha <- c(alpha_t)
  for (i in seq(horizon)){
    mu_t <- x_t + x_t*r*(1-x_t/K)-(alpha_t*x_t**q)/(x_t**q+b**q)
    # x_t <- rlnorm(1, meanlog = mu_t, sdlog=sigma)
    x_t <- mu_t
    # x_t <- max(rnorm(1, mu_t,sigma),0)
    trajectory <- c(trajectory, x_t)
    # alpha_t <- 0.19 + sin(2*pi*i/20)*0.05
    # alpha_t <-alpha_t+alpha
    if (i < 100){
      alpha_t <-alpha_t
    } else if (i>=100 & i<200){
      alpha_t <-alpha_t+alpha
      if (CONSERVATION_ACTION){

      }

    } else {
      alpha_t <-alpha_t-alpha/2
      if (CONSERVATION_ACTION){
        if (i>=300){
          alpha_t <- alpha_t-alpha/2
        }
      }
      # alpha_t <- max(alpha_t, 0.15)
      # alpha_t <- 0.19
    }
    # alpha_t <- max(rnorm(1, alpha_t,sigma/50),0)

    trajectory_alpha <- c(trajectory_alpha, alpha_t)
  }
  data <- data.frame(time = seq(0,horizon),
                     state=trajectory,
                     alpha=trajectory_alpha,
                     alpha_slope=alpha,
                     conservation_action=CONSERVATION_ACTION)

  res <- rbind(res, data)
}


res$conservation_action <- as.factor(res$conservation_action)
# res <- filter(res,
#               conservation_action=="FALSE")
state_plot <- ggplot(res)+
  geom_line(aes(x=time, y=state, group=conservation_action,
                col=conservation_action),
            linewidth=1.2)+
  geom_vline(xintercept = 400,
             linetype="dashed")+
  geom_hline(yintercept = 0.81,
             linetype="dashed")+
  geom_vline(xintercept = 100,
             linetype="dashed")+
  geom_vline(xintercept = 200,
             linetype="dashed")+
  geom_vline(xintercept = 300,
             linetype="dashed")+
  lims(y=c(0,1))


alpha_plot <- ggplot(res)+
  geom_line(aes(x=time, y=alpha, group=conservation_action,
                col=conservation_action),
            linewidth=1.2)+
  geom_hline(yintercept = trajectory_alpha[1],
             linetype="dashed",
             col="red")+
  geom_vline(xintercept = 400,
             linetype="dashed")+
  geom_vline(xintercept = 100,
             linetype="dashed")+
  geom_vline(xintercept = 300,
             linetype="dashed")+
  geom_vline(xintercept = 200,
             linetype="dashed")+
  labs(y="pressure")

ggarrange(alpha_plot,
          state_plot,
          ncol=1,
          common.legend = TRUE)
