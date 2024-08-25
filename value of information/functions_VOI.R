transition_function <- function(time_horizon,
                                gamma,
                                Num_s,
                                Num_a,
                                type_transitions,
                                sd){
  
  #inputs
  # time_horizon: time horizon
  # gamma: discount factor
  # Num_s: number of states
  # Num_a: number of actions
  # type_transitions: type of transitions:
      # options include: Random (resamples new transition functions from uniform distribution)
      # options include: Random walk (resamples new transition functions using a random walk for each action)
      # options include: Gradient (new transition function obtained with gradient direction)
  # sd: standard deviation for the random walk or for the max magnitude of gradient
  
  ############
  #returns
  #transition_matrix:
  #transition function of factored set of states 
  #dim=c(Num_s*time_horizon, Num_s*time_horizon, Num_a))
  
  
  if (type_transitions == "Random"){
    transition_time_list <- list()
    for (t in seq(time_horizon)){
      transition_base <- array(0, dim=c(Num_s,Num_s,Num_a))
      for (a in seq(Num_a)){
        transition_base_a <- matrix(runif(Num_s*Num_s),
                                    Num_s,Num_s)
        transition_base_a <- sweep(transition_base_a, 1, rowSums(transition_base_a), FUN="/")
        transition_base[,,a] <- transition_base_a
      }
      transition_time_list[[t]] <-
        transition_base #transition function for each time step
    }
  } else if (type_transitions == "Random walk"){
    # Initialize the list to hold transition matrices for each time step
    transition_time_list <- list()
    
    # Generate the initial transition matrix for t = 1
    transition_base <- array(0, dim=c(Num_s, Num_s, Num_a))
    for (a in seq(Num_a)){
      transition_base_a <- matrix(runif(Num_s * Num_s), Num_s, Num_s)
      transition_base_a <- sweep(transition_base_a, 1, rowSums(transition_base_a), FUN="/")
      transition_base[,,a] <- transition_base_a
    }
    transition_time_list[[1]] <- transition_base
    
    # Generate transition matrices for t = 2 to time_horizon
    for (t in seq(2, time_horizon)){
      previous_transition_base <- transition_time_list[[t-1]]
      transition_base <- array(0, dim=c(Num_s, Num_s, Num_a))
      for (a in seq(Num_a)){
        # Apply small perturbation to the previous transition matrix
        perturbation <- matrix(runif(Num_s * Num_s, min = -sd, max = sd), Num_s, Num_s)
        transition_base_a <- previous_transition_base[,,a] + perturbation
        
        # Ensure non-negative values and re-normalize each row to sum to 1
        transition_base_a[transition_base_a < 0] <- 0
        transition_base_a <- sweep(transition_base_a, 1, rowSums(transition_base_a), FUN="/")
        transition_base[,,a] <- transition_base_a
      }
      transition_time_list[[t]] <- transition_base
    }
  } else if (type_transitions == "Gradient"){
    # Define user-specified gradient matrices for each action
    # These gradients should be of the same dimension as the transition matrices
    gradient_list <- list()
    for (a in seq(Num_a)){
      gradient_list[[a]] <- matrix(runif(Num_s * Num_s, min = -sd,
                                         max = sd), Num_s, Num_s)
    }
    
    # Initialize the list to hold transition matrices for each time step
    transition_time_list <- list()
    
    # Generate the initial transition matrix for t = 1
    transition_base <- array(0, dim=c(Num_s, Num_s, Num_a))
    for (a in seq(Num_a)){
      transition_base_a <- matrix(runif(Num_s * Num_s), Num_s, Num_s)
      transition_base_a <- sweep(transition_base_a, 1, rowSums(transition_base_a), FUN="/")
      transition_base[,,a] <- transition_base_a
    }
    transition_time_list[[1]] <- transition_base
    
    # Generate transition matrices for t = 2 to time_horizon
    for (t in seq(2, time_horizon)){
      previous_transition_base <- transition_time_list[[t-1]]
      transition_base <- array(0, dim=c(Num_s, Num_s, Num_a))
      for (a in seq(Num_a)){
        # Apply the gradient to the previous transition matrix
        transition_base_a <- previous_transition_base[,,a] + gradient_list[[a]]
        
        # Ensure non-negative values and re-normalize each row to sum to 1
        transition_base_a[transition_base_a < 0] <- 0
        transition_base_a <- sweep(transition_base_a, 1, rowSums(transition_base_a), FUN="/")
        transition_base[,,a] <- transition_base_a
      }
      transition_time_list[[t]] <- transition_base
    }
  }
  
  #transition factored states ####
  transition_matrix <-array(0, dim=c(Num_s*time_horizon,
                                     Num_s*time_horizon,
                                     Num_a))
  
  for (a in seq (Num_a)){
    for (t in seq(time_horizon)){
      if (t<time_horizon){
        transition_matrix[seq((t-1)*Num_s+1,(t)*Num_s),seq(t*Num_s+1,(t+1)*Num_s),a] <- transition_time_list[[t]][,,a]
      } else {
        transition_matrix[seq((t-1)*Num_s+1,(t)*Num_s),seq((t-1)*Num_s+1,(t)*Num_s),a] <- transition_time_list[[t]][,,a]
      }
    }
  }
  return(transition_matrix)
}

compute_rEVPI <- function(transition_matrix,
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