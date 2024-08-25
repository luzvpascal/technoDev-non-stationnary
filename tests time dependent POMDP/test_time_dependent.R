#2states
#2actions
#10 time steps
time_horizon <- 10

#transition####
transition_base_action1 <- matrix(c(0.5,0.5,0.5,0.5), ncol = 2)
transition_base_action2 <- matrix(c(0.1,0.3,0.9,0.7), ncol = 2)

transition_base <- array(c(transition_base_action1,
                           transition_base_action2), dim=c(2,2,2))

transition_time_list <- list()
for (t in seq(time_horizon)){
  transition_time_list[[t]] <- transition_base #transition function for each time step
}
#time transition
mat <- diag(time_horizon-1)
mat <- cbind(rep(0,time_horizon-1), mat)
mat <- rbind(mat, c(rep(0,time_horizon-1),1))
#reward####
reward <- matrix(c(1,2,0.8,1.8), ncol = 2)

reward_time_list <- list()
for (t in seq(time_horizon-1)){
  reward_time_list[[t]] <- reward #transition function for each time step
}
reward_time_list[[time_horizon]] <- matrix(0, ncol = 2, nrow=2)

write_hmMDP(TR_FUNCTION=transition_time_list,
            MODEL_TR=mat,
            B_FULL=c(1,0),
            B_PAR=c(1, rep(0, time_horizon-1)),
            REW=reward_time_list,
            GAMMA=0.99,
            FILE="test.pomdpx"
            )

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

cmd <- paste(path_to_sarsop,
             "--precision", 0.00001,
             "--timeout", 60 ,
             "--output", "test.policyx",
             "test.pomdpx",
             sep=" ")
system(cmd)
