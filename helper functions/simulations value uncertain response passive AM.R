update_belief <- function(transition_ecosystem,
                          current_state,
                          next_state,
                          current_action,
                          current_belief_mod){
  if (length(current_belief_mod)==1){
    return(1)
  }
  next_belief <- rep(0, length(current_belief_mod))
  for (mod in seq(length(transition_ecosystem))){
      next_belief[mod] <-transition_ecosystem[[mod]][current_state,next_state,current_action]*current_belief_mod[mod]
  }
  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

weighted_average_model <- function(transition_ecosystem,
                                   current_belief_mod){
  ## calculate average transition function
  avg_transition_matrix <- transition_ecosystem[[1]] * 0

  # Loop through each transition matrix and add it to the avg_transition_matrix
  for (index_model in seq_along(transition_ecosystem)) {
    avg_transition_matrix <- avg_transition_matrix + transition_ecosystem[[index_model]]*current_belief_mod[index_model]
  }
  return(avg_transition_matrix)
}

trajectory <- function(state_prior_eco,
                       Tmax,
                       initial_belief_state,
                       transition_ecosystem,
                       true_transition_ecosystem,
                       reward,
                       disc = 0.95) {

  #inputs
  # state_prior: index of observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # transition_ecosystem: list of candidate transitions
  # true_transition_ecosystem: true transition among candidates
  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  # initialise Num_mod and Num_state
  Num_mod <- length(initial_belief_state)

  #number of states
  Num_s_eco <- dim(transition_ecosystem[[1]])[1]
  Num_a <- dim(transition_ecosystem[[1]])[3]
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c() #initial reward is 0

  state_eco <- state_prior_eco
  current_state <- state_prior_eco
  mod_probs <- matrix(initial_belief_state, nrow = 1)

  for (i in seq(Tmax)) {
    if (state_eco[i]==1){
      state_eco <- c(state_eco, rep(1, Tmax+1-i))
      current_state <- c(current_state, rep(current_state[i], Tmax+1-i))
      actions <- c(actions, rep(1, Tmax+1-i))
      V <- c(V, rep(V[i-1], Tmax+1-i))
      break
    }

    #get the weighted average model
    avg_transition_matrix <- weighted_average_model(transition_ecosystem,
                                                    mod_probs[i,])

    ## crop avg_transition_matrix and reward to relevant times
    left_indexes <- seq(tuple_to_index(i, 1, N_ecosystem+1),
                        Num_s_eco)
    avg_transition_matrix_cropped <-avg_transition_matrix[left_indexes,left_indexes,]
    reward_cropped <- reward[left_indexes,]
    #solve MDP of new average model
    solution <- mdp_value_iteration(avg_transition_matrix_cropped,
                                    reward_cropped,
                                    disc)

    #get the action
    actions <- c(actions, solution$policy[state_eco[i]])

    #update reward
    #update reward
    if (i == 1){
      V <- reward[state_eco[i], actions[i]]
    } else {
      V <- c(V, V[i-1] + disc**(i)*reward[state_eco[i], actions[i]])
    }

    if (is.na(V[i])){
      break
    }
    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))


    prob_dist <-  c(true_transition_ecosystem[current_state[i],,actions[i]])

    current_state <- c(current_state,
                   sample(seq(Num_s_eco), size=1, replace = TRUE,
                          prob =prob_dist))

    state_eco <- c(state_eco,
                   index_to_eco(current_state[i+1], N_ecosystem+1))

    #update beliefs

    belief_state <- update_belief(transition_ecosystem,
                                  current_state[i],
                                  current_state[i+1],
                                  actions[i],
                                  mod_probs[i,])

    mod_probs <- rbind(mod_probs, belief_state)

  }

  data_output <- data.frame(state_eco=state_eco)
  data_output$current_state <- current_state
  data_output$value <- c(V, V[Tmax])
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)

  return(list(data_output=data_output,
              mod_probs = mod_probs))
}

run_simulation_passive_AM <- function(i) {
  trajectory(state_prior_eco = N_ecosystem+1,
             Tmax = 84,
             initial_belief_state = B_PAR,
             transition_ecosystem = transition_matrix_list,
             true_transition_ecosystem = true_transition_ecosystem_now,
             reward = REW,
             disc = GAMMA)$data_output[-85,]
}
