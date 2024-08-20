update_belief <- function(transition_temperatures,
                          current_time,
                          next_temp,
                          current_belief){

  next_belief <- rep(0, length(current_belief))

  for (mod in seq(length(transition_temperatures))){

    next_belief[mod] <- transition_temperatures[[mod]][current_time,next_temp]*current_belief[mod]
  }

  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

factored_state <- function(state_eco, state_temp, state_time,
                           Num_s_eco, Num_s_temp, Num_s_time){
  return(state_time + Num_s_time*(state_temp-1)
         + Num_s_time*Num_s_temp*(state_eco-1))
}

trajectory <- function(state_prior_eco,
                       state_prior_temp,
                       state_prior_time,
                       Tmax,
                       initial_belief_state,
                       transition_ecosystem,
                       transition_temperatures,
                       transition_time,
                       reward_time_list,
                       true_model,
                       alpha_momdp,
                       disc = 0.95,
                       optimal_policy = TRUE,
                       naive_policy = NA,
                       alpha_indexes=FALSE) {

  #inputs
  # state_prior: index of observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # tr_mdp: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # rew_mdp: reward function of the reefs. matrix of dim (X, A)
  # tr_momdp: transition function of the hmMDP as returned by transition_hmMDP (X.Y,X.Y,A)
  # obs_momdp:observation function as returned by obs_hmMDP(X.Y,X,A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  # initialise Num_mod and Num_state
  Num_mod <- length(initial_belief_state)

  #number of states
  Num_s_eco <- nrow(transition_ecosystem[[1]][[1]])
  Num_s_temp <- ncol(transition_temperatures[[1]])
  Num_s_time <- nrow(transition_time)
  Num_s <- Num_s_eco*Num_s_temp*Num_s_time

  Num_a <- length(transition_ecosystem)
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c(0) #initial reward is 0

  if (alpha_indexes){indexes <- c()}

  state_eco <- state_prior_eco
  state_temp <- state_prior_temp
  state_time <- state_prior_time

  state_current <- factored_state(state_eco, state_temp, state_time,
                                  Num_s_eco, Num_s_temp, Num_s_time)

  mod_probs <- matrix(initial_belief_state, ncol = Num_mod)

  for (i in seq(Tmax)) {
    #compute next best action0
    if (optimal_policy){

      output <- interp_policy2(mod_probs[i,],
                               obs = state_current[i],
                               alpha = alpha_momdp$vectors,
                               alpha_action = alpha_momdp$action,
                               alpha_obs = alpha_momdp$obs,
                               alpha_index = alpha_momdp$index)

      actions <- c(actions, output[[2]][1])
      if (alpha_indexes){indexes <- c(indexes, output[[3]][1])}

    } else {
      act <- naive_policy(state[i], i)
      actions <- c(actions, act)
    }

    #update reward
    V <- c(V, V[i] + disc**(i-1)*reward_time_list[[state_time[i]]][state_eco[i],
                                                                   actions[i]])

    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    state_eco <- c(state_eco,
                   sample(seq(Num_s_eco), size=1, replace = TRUE,
                   prob = c(transition_ecosystem[[actions[i]]][[state_temp[i]]][state_eco[i],])))

    #transition depends on time here
    state_temp <- c(state_temp,
                   sample(seq(Num_s_temp), size=1, replace = TRUE,
                          prob = c(transition_temperatures[[true_model]][state_time[i],])))

    state_time <- c(state_time,
                    sample(seq(Num_s_time), size=1, replace = TRUE,
                           prob = c(transition_time[state_time[i],])))

    state_current <- c(state_current,
                       factored_state(state_eco[i+1], state_temp[i+1], state_time[i+1],
                                      Num_s_eco, Num_s_temp, Num_s_time))

    #update beliefs reefs
    belief_state<- update_belief(transition_temperatures,
                                 state_time[i],
                                 state_temp[i+1],
                                 mod_probs[i,])

    mod_probs <- rbind(mod_probs, belief_state)
  }

  data_output <- data.frame(state_eco=state_eco)
  data_output$state_temp <- state_temp
  data_output$state_time <- state_time
  data_output$state_current <- state_current
  data_output$value <- V
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs = mod_probs))
}
