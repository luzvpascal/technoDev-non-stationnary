belief_tech <- function(current_belief, Num_mod_tech){
  m <- matrix(current_belief, nrow = Num_mod_tech)
  tech_belief <- c(apply(m, 1, sum))
  return(tech_belief)
}

belief_mod <- function(current_belief, Num_mod_tech){
  m <- matrix(current_belief, nrow = Num_mod_tech)
  mod_belief <- c(apply(m, 2, sum))
  return(mod_belief)
}

belief <- function(belief_mod,belief_tech){
  a <- matrix(belief_tech, ncol=1)
  b <- matrix(belief_mod, nrow=1)
  belief <- c(a%*%b) #double check
  return(belief)
}

update_belief_mod <- function(transition_ecosystem,
                              current_state,
                              next_state,
                              current_action,
                              current_belief_mod){
  next_belief <- rep(0, length(current_belief_mod))
  for (mod in seq(length(transition_ecosystem))){
    next_belief[mod] <- transition_ecosystem[[mod]][current_state,next_state,current_action]*current_belief_mod[mod]
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
                          current_state,
                          next_state,
                          current_belief_mod,
                          transition_tech,
                          current_state_tech,
                          next_state_tech,
                          current_action,
                          current_belief_tech){

  next_belief_mod <- update_belief_mod(transition_ecosystem,
                                       current_state,
                                       next_state,
                                       current_action,
                                       current_belief_mod)

  next_belief_tech <- update_belief_tech(transition_tech,
                                         current_state_tech,
                                         next_state_tech,
                                         current_action,
                                         current_belief_tech)

  next_belief <- belief(next_belief_mod, next_belief_tech)
  return(next_belief)
}


factored_state <- function(state_eco, state_tech,
                           Num_s_eco, Num_s_tech){
  return(state_tech + Num_s_tech*(state_eco-1))
}

trajectory <- function(state_prior_eco,
                       state_prior_tech,
                       Tmax,
                       initial_belief_state,
                       initial_belief_state_tech,
                       transition_ecosystem,
                       transition_tech,
                       reward,
                       true_model,
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
  Num_mod <- length(initial_belief_state)
  Num_mod_tech <- length(initial_belief_state_tech)

  #number of states
  Num_s_eco <- dim(transition_ecosystem[[1]])[1]
  Num_s_tech <- nrow(transition_tech[[1]][[1]])
  Num_s <- Num_s_eco*Num_s_tech

  Num_a <- dim(transition_ecosystem[[1]])[3]
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c(0) #initial reward is 0

  if (alpha_indexes){indexes <- c()}

  state_eco <- state_prior_eco
  state_tech <- state_prior_tech

  state_current <- factored_state(state_eco, state_tech,
                                  Num_s_eco, Num_s_tech)

  mod_probs_mod <- matrix(initial_belief_state, nrow = 1)
  mod_probs_tech <- matrix(initial_belief_state_tech, nrow = 1)
  mod_probs <- matrix(belief(initial_belief_state, initial_belief_state_tech), nrow = 1)

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
    V <- c(V, V[i] + disc**(i-1)*reward[state_eco[i], actions[i]])

    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    state_eco <- c(state_eco,
                   sample(seq(Num_s_eco), size=1, replace = TRUE,
                          prob = c(transition_ecosystem[[true_model]][state_eco[i], ,actions[i]])))

    state_tech <- c(state_tech,
                    sample(seq(Num_s_tech), size=1, replace = TRUE,
                           prob = c(transition_tech[[true_model_tech]][[actions[i]]][state_tech[i],])))

    state_current <- c(state_current,
                       factored_state(state_eco[i+1],
                                      state_tech[i+1],
                                      Num_s_eco, Num_s_tech))

    #update beliefs

    #belief mod
    current_belief_mod <- belief_mod(mod_probs[i,], Num_mod_tech)
    #belief tech
    current_belief_tech <- belief_tech(mod_probs[i,], Num_mod_tech)

    #
    belief_state <- update_belief(transition_ecosystem,
                                  state_eco[i],
                                  state_eco[i+1],
                                  current_belief_mod,
                                  transition_tech,
                                  state_tech[i],
                                  state_tech[i+1],
                                  actions[i],
                                  current_belief_tech)

    mod_probs_mod <- rbind(mod_probs_mod, current_belief_mod)
    mod_probs_tech <- rbind(mod_probs_tech, current_belief_tech)
    mod_probs <- rbind(mod_probs, belief_state)
  }

  data_output <- data.frame(state_eco=state_eco)
  data_output$state_tech <- state_tech
  data_output$state_current <- state_current
  data_output$value <- V
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs_mod=mod_probs_mod,
              mod_probs_tech=mod_probs_tech,
              mod_probs = mod_probs))
}
