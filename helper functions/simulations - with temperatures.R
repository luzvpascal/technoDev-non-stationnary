belief_eco <- function(current_belief, Num_mod_eco){
  m <- matrix(current_belief, nrow = Num_mod_eco)
  mod_belief <- c(apply(m, 1, sum))
  return(mod_belief)
}

belief_temp <- function(current_belief, Num_mod_temp){
  m <- matrix(current_belief, nrow = Num_mod_temp)
  mod_belief <- c(apply(m, 1, sum))
  return(mod_belief)
}

belief_tech <- function(current_belief, Num_mod_tech){
  m <- matrix(current_belief, nrow = Num_mod_tech)
  tech_belief <- c(apply(m, 1, sum))
  return(tech_belief)
}


belief <- function(belief_eco,belief_temp,belief_tech){
  a <- matrix(belief_eco, ncol=1)
  b <- matrix(belief_temp, nrow=1)
  c <- matrix(belief_tech, nrow=1)

  belief <- c(matrix(c(a%*%b), ncol=1)%*%c) #double check
  return(belief)
}

update_belief_eco <- function(transition_ecosystem,
                               current_eco,
                               current_temp,
                               next_eco,
                               current_action,
                               current_belief_eco){

  next_belief <- rep(0, length(current_belief_eco))

  for (mod in seq(length(transition_ecosystem))){
    next_belief[mod] <- transition_ecosystem[[mod]][[current_action]][current_eco,next_eco, current_temp]*current_belief_eco[mod]
  }

  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

update_belief_temp <- function(transition_temperatures,
                              current_time,
                              next_temp,
                              current_belief_temp){
  next_belief <- rep(0, length(current_belief_temp))
  for (mod in seq(length(transition_temperatures))){
    next_belief[mod] <- transition_temperatures[[mod]][current_time,next_temp]*current_belief_temp[mod]
  }
  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

update_belief_tech <- function(transition_tech,
                               current_state_tech,
                               next_state_tech,
                               current_action,
                               current_belief_tech){
  next_belief <- rep(0, length(current_belief_tech))
  for (mod in seq(length(transition_tech))){
    next_belief[mod] <- transition_tech[[mod]][[current_action]][current_state_tech,next_state_tech]*current_belief_tech[mod]
  }
  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

update_belief <- function(transition_ecosystem,
                          current_eco,
                          current_temp,
                          next_eco,
                          current_action,
                          current_belief_eco,
                          transition_temperatures,
                          current_time,
                          next_temp,
                          current_belief_temp,
                          transition_tech,
                          current_state_tech,
                          next_state_tech,
                          current_belief_tech
                          ){

  next_belief_eco <- update_belief_eco(transition_ecosystem,
                                       current_eco,
                                       current_temp,
                                       next_eco,
                                       current_action,
                                       current_belief_eco)
  next_belief_temp <- update_belief_temp(transition_temperatures,
                                       current_time,
                                       next_temp,
                                       current_belief_temp)

  next_belief_tech <- update_belief_tech(transition_tech,
                                         current_state_tech,
                                         next_state_tech,
                                         current_action,
                                         current_belief_tech)

  next_belief <- belief(next_belief_eco, next_belief_temp, next_belief_tech)
  return(next_belief)
}

factored_state <- function(state_eco, state_temp, state_time, state_tech,
                           Num_s_eco, Num_s_temp, Num_s_time, Num_s_tech){
  return(state_tech + Num_s_tech*(state_time-1) +
           Num_s_tech*Num_s_time*(state_temp-1) +
           Num_s_tech*Num_s_time*Num_s_temp*(state_eco-1))
}

trajectory <- function(state_prior_eco,
                       state_prior_temp,
                       state_prior_time,
                       state_prior_tech,
                       Tmax,
                       initial_belief_state_eco,
                       initial_belief_state_temp,
                       initial_belief_state_tech,
                       transition_ecosystem,
                       transition_temperatures,
                       transition_time,
                       transition_tech,
                       reward_time_list,
                       true_model_eco,
                       true_model_temp,
                       true_model_tech,
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
  Num_mod_eco <- length(initial_belief_state_eco)
  Num_mod_temp <- length(initial_belief_state_temp)
  Num_mod_tech <- length(initial_belief_state_tech)

  #number of states
  Num_s_eco <- nrow(transition_ecosystem[[1]][[1]])
  Num_s_temp <- ncol(transition_temperatures[[1]])
  Num_s_time <- nrow(transition_time)
  Num_s_tech <- nrow(transition_tech[[1]][[1]])
  Num_s <- Num_s_eco*Num_s_temp*Num_s_time*Num_s_tech

  Num_a <- length(transition_ecosystem[[1]][[1]])
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c(0) #initial reward is 0

  if (alpha_indexes){indexes <- c()}

  state_eco <- state_prior_eco
  state_temp <- state_prior_temp
  state_time <- state_prior_time
  state_tech <- state_prior_tech

  state_current <- factored_state(state_eco, state_temp, state_time,state_tech,
                                  Num_s_eco, Num_s_temp, Num_s_time,Num_s_tech)

  mod_probs_eco <- matrix(initial_belief_state_eco, nrow = 1)
  mod_probs_temp <- matrix(initial_belief_state_temp, nrow = 1)
  mod_probs_tech <- matrix(initial_belief_state_tech, nrow = 1)
  mod_probs <- matrix(belief(initial_belief_state_eco,
                             initial_belief_state_temp,
                             initial_belief_state_tech), nrow = 1)

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
                          prob = c(transition_ecosystem[[true_model_eco]][[actions[i]]][state_eco[i],,state_temp[i]])))

    #transition depends on time here
    state_temp <- c(state_temp,
                    sample(seq(Num_s_temp), size=1, replace = TRUE,
                           prob = c(transition_temperatures[[true_model_tech]][state_time[i],])))

    state_time <- c(state_time,
                    sample(seq(Num_s_time), size=1, replace = TRUE,
                           prob = c(transition_time[state_time[i],])))

    state_tech <- c(state_tech,
                    sample(seq(Num_s_tech), size=1, replace = TRUE,
                           prob = c(transition_tech[[true_model_tech]][[actions[i]]][state_tech[i],])))

    state_current <- c(state_current,
                       factored_state(state_eco[i+1],
                                      state_temp[i+1],
                                      state_time[i+1],
                                      state_tech[i+1],
                                      Num_s_eco, Num_s_temp, Num_s_time, Num_s_tech))

    #update beliefs

    #belief mod
    current_belief_eco <- belief_eco(mod_probs_eco[i,], Num_mod_eco)
    #belief temp
    current_belief_temp <- belief_temp(mod_probs_temp[i,], Num_mod_temp)
    #belief tech
    current_belief_tech <- belief_tech(mod_probs[i,], Num_mod_tech)

    #global belief
    belief_state <- update_belief(transition_ecosystem = transition_ecosystem,
                                  current_eco = state_eco[i],
                                  current_temp = state_temp[i],
                                  next_eco = state_eco[i+1],
                                  current_action = actions[i],
                                  current_belief_eco = current_belief_eco,
                                  transition_temperatures = transition_temperatures,
                                  current_time = state_time[i],
                                  next_temp = state_temp[i+1],
                                  current_belief_temp=current_belief_temp,
                                  transition_tech=transition_tech,
                                  current_state_tech = state_tech[i],
                                  next_state_tech = state_tech[i+1],
                                  current_belief_tech = current_belief_tech
    )

    mod_probs_eco <- rbind(mod_probs_eco, current_belief_eco)
    mod_probs_temp <- rbind(mod_probs_temp, current_belief_temp)
    mod_probs_tech <- rbind(mod_probs_tech, current_belief_tech)
    mod_probs <- rbind(mod_probs, belief_state)
  }

  data_output <- data.frame(state_eco=state_eco)
  data_output$state_temp <- state_temp
  data_output$state_time <- state_time
  data_output$state_tech <- state_tech
  data_output$state_current <- state_current
  data_output$value <- V
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs_eco=mod_probs_eco,
              mod_probs_temp=mod_probs_temp,
              mod_probs_tech=mod_probs_tech,
              mod_probs = mod_probs))
}
