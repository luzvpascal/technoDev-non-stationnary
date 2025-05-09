belief_state_function <- function(state, belief, N_state){
  #input:
  #state: integer between 1 and number of possible states
  #belief: belief state over possible models
  #N_state: number of possible obs states

  #returns: vector of belief over models and observable states

  mat_sol <- matrix(0,nrow = N_state,ncol=length(belief))
  mat_sol[state,] <- belief

  return(matrix(mat_sol,nrow=1))
}
update_belief <- function(belief_state_momdp, transition, observation, z0, a0){
  #inputs:
  # belief_state_momdp: vector, prior on partially observable variables
  # transition: transition function, array of dim (X x Y, X x Y, A)
  # observation: observation function, array of dim (X x Y, X x Y, A)
  # z0: last observation
  # a0: last action

  # output: updated belief state
  L <- length(belief_state_momdp)
  belief <-
    vapply(seq_len(L), function(i){
      belief_state_momdp %*% transition[, i, a0] * observation[i, z0, a0]
    }, numeric(1))
  belief / sum(belief)
}

sum_Nstate_by_Nstate <- function(vector, N_state){
  #vector: vector of beliefs state over possible models and obs states
  #N_state: number of possible obs states

  #return belief over possible models only
  mat <- matrix(vector, nrow=N_state)
  return(colSums(mat))
}

trajectory <- function(state_prior,
                       Tmax,
                       initial_belief_state,
                       tr_mdp,
                       rew_mdp,
                       tr_momdp,
                       obs_momdp,
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
  Num_s <- nrow(rew_mdp)
  Num_a <- ncol(rew_mdp)
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c() #initial reward is 0

  if (alpha_indexes){indexes <- c()}
  state <- state_prior
  mod_probs <- matrix(initial_belief_state, ncol = Num_mod)

  for (i in seq(Tmax)) {
    if (state[i]==1){
      state_eco <- c(state, rep(1, Tmax+1-i))
      current_state <- c(current_state, rep(current_state[i], Tmax+1-i))
      actions <- c(actions, rep(1, Tmax+1-i))
      V <- c(V, rep(V[i], Tmax+1-i))
      break
    }
    #compute next best action0
    if (optimal_policy){
      output <- interp_policy2(mod_probs[i,],
                               obs = state[i],
                               alpha = alpha_momdp$vectors,
                               alpha_action = alpha_momdp$action,
                               alpha_obs = alpha_momdp$obs,
                               alpha_index = alpha_momdp$index)
      actions <- c(actions, output[[2]][1])
      if (alpha_indexes){indexes <- c(indexes, output[[3]][1])}

    } else {
      act <- naive_policy(state[i],
                          i)
      actions <- c(actions, act)
    }

    #update reward
    if (i==1){
      V <- rew_mdp[state[i], actions[i]]
      r <- rew_mdp[state[i], actions[i]]
    } else {
      V <- c(V, V[i-1] + disc**(i)*rew_mdp[state[i], actions[i]])
      r <- c(r, rew_mdp[state[i], actions[i]])
    }

    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    state <- c(state, sample(seq(Num_s), size=1, replace = TRUE,
                             prob = c(tr_mdp[state[i], ,actions[i]])))

    #update beliefs reefs
    belief_state<- update_belief(belief_state_function(state[i],mod_probs[i,],Num_s ),
                                 tr_momdp,
                                 obs_momdp,
                                 state[i+1],
                                 actions[i])

    mod_probs <- rbind(mod_probs, sum_Nstate_by_Nstate(belief_state, Num_s))
  }

  data_output <- data.frame(state=state)
  data_output$value <- c(V,V[length(V)])
  data_output$reward <- c(r,r[length(r)])
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs = mod_probs))
}

run_simulation_AM <- function(i) {
  trajectory(state_prior = length(ecosystem_states),
             Tmax = 85,
             initial_belief_state = B_PAR,
             tr_mdp = transition_matrix_list[[index_config]],
             rew_mdp = REW,
             tr_momdp = tr_momdp,
             obs_momdp = obs_momdp,
             alpha_momdp = alphas,
             disc = GAMMA,
             optimal_policy = TRUE,
             naive_policy = NA,
             alpha_indexes=FALSE)$data_output[86,]
}
