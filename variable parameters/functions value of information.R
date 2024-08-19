compute_rEVPI_stationarity <- function(transition_matrix,
                          reward_matrix,
                          time_horizon,
                          gamma,
                          Num_s){
  ## SOLVE MDP ####
  solution_MDP <- mdp_value_iteration(transition_matrix,reward_matrix,
                                      gamma)
  policy_MDP <- matrix(solution_MDP$policy,
                       ncol=Num_s, byrow = TRUE)

  ## value of information ###
  policy_time_0 <- policy_MDP[1,]
  policy_time_0 <- rep(policy_time_0, time_horizon)

  solution_MDP_time_0 <- mdp_eval_policy_matrix(transition_matrix,reward_matrix,
                                                gamma,
                                                policy_time_0)
  return((solution_MDP$V[1]-solution_MDP_time_0[1])/solution_MDP$V[1]*100)
}

