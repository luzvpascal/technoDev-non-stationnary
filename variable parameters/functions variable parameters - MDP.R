#in this file, we build MDP for managing ecosystem with variable capacity

## ecosystem dynamics ####
ecosystem_dynamics <- function(x_t, r, K, time_step){
  return(x_t + time_step*(x_t*r*(1-x_t/K)))
}

#variable r function ####
r_function <- function(r_min, r_max, delta_t_crit,
                       delta_t, sigmoid_bool){
  if (sigmoid_bool){
    r_min + (r_max-r_min)*(1-1/(1+exp(-5*(delta_t - delta_t_crit/2))))
  } else {
    r_max
  }
}

#variable K function ####
K_function <- function(K_min, K_max, delta_t_crit,
                       delta_t, sigmoid_bool){
  if (sigmoid_bool){
    K_min + (K_max-K_min)*(1-1/(1+exp(-5*(delta_t - delta_t_crit))))
  } else {
    K_max
  }
}

#average transition ecosystem states ####
sik_bar_function <- function(ecosystem_states,
                             temperature_states,
                             DEP_EFFECT,
                             r_min,
                             r_max,
                             delta_t_crit_r,
                             sigmoid_bool_r,
                             K_min,
                             K_max,
                             delta_t_crit_K,
                             sigmoid_bool_K,
                             time_step){
  #ecosystem_states is a vector describing discretized possible values of ecosytem states between 0 and 1
  #temperature_states is a vector describing discretized possible values of temperatures states between 0 and 5
  #DEP_EFFECT is a vector describing possible values of temperatures mitigation (actions)

  sik_bar <- array(dim=c(length(ecosystem_states),
                         length(temperature_states),
                         length(DEP_EFFECT)))

  for (index_action in seq(length(DEP_EFFECT))){
    for (index_state in seq(length(ecosystem_states))){
      for(index_temp in seq(length(temperature_states))){

        r_eff <-r_function(r_min, r_max, delta_t_crit_r,
                           temperature_states[index_temp]-DEP_EFFECT[index_action],
                           sigmoid_bool_r)
        K_eff <-K_function(K_min, K_max, delta_t_crit_K,
                           temperature_states[index_temp]-DEP_EFFECT[index_action],
                           sigmoid_bool_K)
        sik_bar[index_state,
                index_temp,
                index_action] <- max(0, ecosystem_dynamics(ecosystem_states[index_state],
                                                    r_eff,
                                                    K_eff,
                                                    time_step))
      }
    }
  }
  return(sik_bar)
}


transition_function_ecosystem <- function(ecosystem_states,
                                          temperature_states,
                                          DEP_EFFECT,
                                          sik_bar,
                                          sigma_eco){
  N_ecosystem <- length(ecosystem_states)
  transition_ecosystem <- list()
  for (index_action in seq(length(DEP_EFFECT))){
    #for each possible action
    transition_ecosystem_index_action <- array(0, dim=c(length(ecosystem_states),
                                                     length(ecosystem_states),
                                                     length(temperature_states)))

    for(index_temp in seq(length(temperature_states))){

      transition_ecosystem_index_action[1,1, index_temp] <- 1

      for (i in seq(2, length(ecosystem_states))){

        transition_ecosystem_index_action[i,1, index_temp] <-
          pnorm(log(ecosystem_states[1]+1/(2*(N_ecosystem-1))),
                mean = log(sik_bar[i, index_temp,index_action]),
                sd =sigma_eco)
        for (j in seq(2,length(ecosystem_states))){
          #transition
          if ((ecosystem_states[j]-1/(2*(N_ecosystem-1)))<0){
            print(i)
            print(j)
            break
          }
          transition_ecosystem_index_action[i,j, index_temp] <-
            pnorm(log(ecosystem_states[j]+1/(2*(N_ecosystem-1))),
                   mean = log(sik_bar[i, index_temp,index_action]),
                   sd =sigma_eco)-
            pnorm(log(ecosystem_states[j]-1/(2*(N_ecosystem-1))),
                  mean = log(sik_bar[i, index_temp,index_action]),
                  sd=sigma_eco)

        }
        #normalise
        transition_ecosystem_index_action[i, , index_temp] <-
          transition_ecosystem_index_action[i, , index_temp]/
          (sum(transition_ecosystem_index_action[i, , index_temp]))
      }
    }
    transition_ecosystem[[index_action]] <- transition_ecosystem_index_action
  }
  return(transition_ecosystem)
}


transition_function_temperatures <- function(temperature_states,
                                             time_states,
                                             temperature_data){

    transition_temperatures <- matrix(nrow=length(time_states),
                                      ncol=length(temperature_states))

    for (t in seq(length(time_states))){
      delta_t_avg <- temperature_data$Mean[t]
      sigma_temp<- (temperature_data$Mean[t]-temperature_data$X5.[t])/qnorm(0.95)
      for (j in seq(length(temperature_states))){
        transition_temperatures[t, j] <- pnorm(temperature_states[j]+1/(2*N_temperatures),
                                                                   mean=delta_t_avg,
                                                                   sd=sigma_temp)-
                                          pnorm(temperature_states[j]-1/(2*N_temperatures),
                                                mean=delta_t_avg,
                                                sd=sigma_temp)
#
#         transition_temperatures[t, j] <- round(transition_temperatures[t, j],
#                                                                    digits=3)

      }
      transition_temperatures[t,] <- transition_temperatures[t,]/sum(transition_temperatures[t,])
    }
  return(transition_temperatures)
}

##transition ecosystem over time ####
build_transition_ecosystem_time <- function(transition_ecosystem, transition_temperatures) {
  # Get the dimensions
  N_ecosystem <- dim(transition_ecosystem[[1]])[1]
  N_temperatures <- dim(transition_ecosystem[[1]])[3]
  N_times <- dim(transition_temperatures)[1]
  N_actions <- length(transition_ecosystem)

  # Initialize the result array
  transition_ecosystem_time_total <- list()
  for (action in seq(N_actions)){

    transition_ecosystem_time <- array(0, dim = c(N_ecosystem, N_ecosystem, N_times))

    # Loop over each time point
    for (t in 1:N_times) {
      # Get the temperature probabilities at time t
      temp_probs <- transition_temperatures[t, ]

      # Calculate the weighted sum for each ecosystem transition
      for (i in 1:N_ecosystem) {
        for (j in 1:N_ecosystem) {
          transition_ecosystem_time[i, j, t] <- sum(temp_probs * transition_ecosystem[[action]][i, j, ])
        }
      }
    }
    transition_ecosystem_time_total[[action]] <- transition_ecosystem_time
  }

  return(transition_ecosystem_time_total)
}

## transition function times states ####
transition_function_times <- function(time_states){
  mat <- diag(length(time_states)-1)
  mat <- cbind(rep(0,length(time_states)-1), mat)
  mat <- rbind(mat, c(rep(0,length(time_states)-1),1))
  return(mat)
}

# Function to convert a tuple (t, i) to an index####
tuple_to_index <- function(t, i, N_ecosystem) {
  return((t - 1) * N_ecosystem + i)
}

# Function to convert an index back to a tuple (t, i)####
index_to_tuple <- function(index, N_ecosystem) {
  t <- ((index - 1) %/% N_ecosystem) + 1
  i <- ((index - 1) %% N_ecosystem) + 1
  return(c(t, i))
}

index_to_year <- function(index, N_ecosystem){
  t <- index_to_tuple(index, N_ecosystem)
  return(t[1])
}

index_to_eco <- function(index, N_ecosystem){
  t <- index_to_tuple(index, N_ecosystem)
  return(t[2])
}

transition_function_ecosystem_time <- function(transition_ecosystem_time, transition_times) {
  N_actions <- length(transition_ecosystem_time)
  N_ecosystem <- dim(transition_ecosystem_time[[1]])[1]
  N_times <- dim(transition_ecosystem_time[[1]])[3]

  # Initialize the result array
  combined_transition_matrix <- array(0, dim = c(N_ecosystem * N_times, N_ecosystem * N_times, N_actions))

  # Loop over each action
  for (a in 1:N_actions) {
    # Get the transition_ecosystem_time for the current action
    current_transition_ecosystem_time <- transition_ecosystem_time[[a]]

    # Loop over each starting time
    for (t in 1:N_times) {
      # Loop over each ending time
      time_prob <- transition_times[t, ]

      for (p in which(time_prob>0)) {
        # Get the time transition probability

        # Loop over each starting ecosystem state
        for (i in 1:N_ecosystem) {
          # Loop over each ending ecosystem state
          start_index <- tuple_to_index(t, i, N_ecosystem)
          for (j in 1:N_ecosystem) {
            # Compute the combined index for the larger transition matrix
            end_index <- tuple_to_index(p, j, N_ecosystem)

            # Get the ecosystem transition probability
            ecosystem_prob <- current_transition_ecosystem_time[i, j, t]

            # Set the combined transition probability
            combined_transition_matrix[start_index, end_index, a] <- round(ecosystem_prob * time_prob[p],
                                                                           digits = 3)
          }
          combined_transition_matrix[start_index, , a] <- combined_transition_matrix[start_index, , a]/sum(combined_transition_matrix[start_index, , a])
        }
      }
    }
  }
  return(combined_transition_matrix)
}

## general transition function ####
transition_function <- function(ecosystem_states,
                                temperature_states,
                                temperature_data_filter,
                                DEP_EFFECT,
                                r_min,
                                r_max,
                                delta_t_crit_r,
                                sigmoid_bool_r,
                                K_min,
                                K_max,
                                delta_t_crit_K,
                                sigmoid_bool_K,
                                time_step,
                                sigma_eco){


  #compute sik_bar
  # print("test sik_bar")
  sik_bar <- sik_bar_function(ecosystem_states,
                              temperature_states,
                              DEP_EFFECT,
                              r_min,
                              r_max,
                              delta_t_crit_r,
                              sigmoid_bool_r,
                              K_min,
                              K_max,
                              delta_t_crit_K,
                              sigmoid_bool_K,
                              time_step)

  ## transitions function of ecosystem states with temperature ####
  # print("test transition_function_ecosystem")
  transition_ecosystem <- transition_function_ecosystem(ecosystem_states,
                                                        temperature_states,
                                                        DEP_EFFECT,
                                                        sik_bar,
                                                        sigma_eco)

  ## transition function of temperature with time ####
  # print("test transition_function_temperatures")
  transition_temperatures <- transition_function_temperatures(temperature_states,
                                                              temperature_data_filter$Year,
                                                              temperature_data_filter)

  #combine transition ecosystem with time state ####
  # print("test build_transition_ecosystem_time")
  transition_ecosystem_time <- build_transition_ecosystem_time(transition_ecosystem,
                                                               transition_temperatures)

  ## transition function of time ####
  # print("test transition_function_times")
  transition_times <- transition_function_times(temperature_data_filter$Year)


  ## transition function of tupple ecosystem state x time state ####
  # print("test transition_function_ecosystem_time")
  combined_transition_matrix <- transition_function_ecosystem_time(transition_ecosystem_time,
                                                                   transition_times)

  return(combined_transition_matrix)
}


# reward function matrix ####
reward_function <- function(ecosystem_states,
                            time_states,
                            DEP_EFFECT,
                            cost_deploy){
  N_ecosystem <- length(ecosystem_states)
  N_times <- length(time_states)
  N_actions <- length(DEP_EFFECT)

  # Initialize the reward matrix
  reward_matrix <- matrix(0, nrow = N_ecosystem * N_times, ncol = N_actions)

  # Compute the base reward for each ecosystem state
  reward_BAU <- ecosystem_states

  # Loop over actions to compute rewards
  for (action_index in seq(N_actions)) {
    reward_action <- reward_BAU - DEP_EFFECT[action_index]^2 * cost_deploy

    # Expand reward_action to include all time states
    for (t in (1:(N_times-1))) {
      for (i in 1:N_ecosystem) {
        index <- tuple_to_index(t, i, N_ecosystem)
        reward_matrix[index, action_index] <- reward_action[i]
      }
    }
  }
  reward_matrix
}

##simulation function ####
simulate_mdp_trajectory <- function(transition_function,
                                    reward_function,
                                    solution, start_state, num_steps) {
  # Extract the optimal policy
  policy <- solution$policy

  # Initialize the trajectory
  trajectory <- data.frame(state = integer(num_steps),
                           action = integer(num_steps),
                           reward = numeric(num_steps))

  # Set the initial state
  current_state <- start_state

  for (t in 1:num_steps) {
    # Determine the action to take according to the policy
    action <- policy[current_state]

    # Record the state, action, and reward
    trajectory$state[t] <- current_state
    trajectory$action[t] <- action
    trajectory$reward[t] <- reward_function[current_state, action]

    # Transition to the next state based on the transition probabilities
    next_state_probabilities <- transition_function[current_state, , action]
    next_state <- sample(1:length(next_state_probabilities), 1, prob = next_state_probabilities)

    # Update the current state
    current_state <- next_state
  }

  return(trajectory)
}
