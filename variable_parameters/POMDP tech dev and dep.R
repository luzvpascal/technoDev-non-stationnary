source("variable_parameters/write_POMDPx.R")
##technology####
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2),
                           matrix(c(1-p_dev, 0, p_dev, 1), ncol=2))
transition_failure <- list(diag(2),
                           diag(2))
transition_tech <- list(transition_success, transition_failure)

##
TR_FUNCTION_ECO <- transition_matrix_list
TR_FUNCTION_TECH <- transition_tech
REW <- Reward
GAMMA <- gamma

B_FULL_ECO <- c(rep(0, nrow(Reward)))
B_FULL_ECO[N_ecosystem+1] <- 1
B_FULL_TECH <- c(1, rep(0, length(tech_states)-1))

B_PAR <- rep(1, length(transition_matrix_list))/length(transition_matrix_list)
B_PAR_TECH <- rep(1, length(transition_tech))/length(transition_tech)


file_name <- paste0("POMDPtest")

FILE <- paste0(file_name, ".pomdpx")

write_full_POMDP(TR_FUNCTION_ECO,
                 TR_FUNCTION_TECH,
                 B_FULL_ECO,
                 B_FULL_TECH,
                 B_PAR,
                 B_PAR_TECH,
                 REW,
                 GAMMA,
                 FILE)

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

OUTPUT_FILE <- paste0(file_name, ".policyx")

cmd <- paste(path_to_sarsop,
             "--precision", 0.0000001,
             "--timeout",3600,
             "--output", OUTPUT_FILE,
             FILE,
             sep=" ")
system(cmd)


