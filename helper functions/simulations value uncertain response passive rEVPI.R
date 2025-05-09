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

best_perf_scenario_index <- function(current_state,
                                     current_belief_mod,
                                     voi_data_uncertain_response){
  a <- mutate(voi_data_uncertain_response,
           weight = current_belief_mod[index_MDP_true])

  a <- group_by(a, index_MDP_test)
  a <- summarise(a, mean_weight_r_EVPI=mean(r_EVPI*weight))

  a <- slice_min(a,order_by = mean_weight_r_EVPI, with_ties = FALSE)

  return(a$index_MDP_test)
}

trajectory <- function(state_prior_eco,
                       Tmax,
                       initial_belief_state,
                       transition_ecosystem,
                       true_transition_ecosystem,
                       reward,
                       voi_data_uncertain_response,
                       solution_list,
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
  V <- c(0) #initial reward is 0

  state_eco <- state_prior_eco
  current_state <- state_prior_eco
  mod_probs <- matrix(initial_belief_state, nrow = 1)

  for (i in seq(Tmax)) {
    if (state_eco[i]==1){
      state_eco <- c(state_eco, rep(1, Tmax+1-i))
      current_state <- c(current_state, rep(current_state[i], Tmax+1-i))
      actions <- c(actions, rep(1, Tmax+1-i))
      V <- c(V, rep(V[i], Tmax+1-i))
      break
    }

    #get the weighted average model
    best_index_MDP_test <- best_perf_scenario_index(current_state[i],
                                                    mod_probs[i,],
                                                    voi_data_uncertain_response)
    #get the action
    actions <- c(actions, solution_list[[best_index_MDP_test]]$policy[current_state[i]])

    #update reward
    V <- c(V, V[i] + disc**(i-1)*reward[current_state[i], actions[i]])

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
  data_output$value <- V
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)

  return(list(data_output=data_output,
              mod_probs = mod_probs))
}

run_simulation_passive_rEVPI <- function(i) {
  trajectory(state_prior_eco = N_ecosystem+1,
             Tmax = 84,
             initial_belief_state = B_PAR,
             transition_ecosystem = transition_matrix_list,
             true_transition_ecosystem = true_transition_ecosystem_now,
             reward = REW,
             voi_data_uncertain_response = voi_data,
             solution_list = solution_list,
             disc = GAMMA)$data_output[-85,]
}
