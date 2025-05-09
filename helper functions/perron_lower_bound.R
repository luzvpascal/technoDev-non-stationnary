perron_lower_bound <- function(list_models,
                               solution_list,
                               reward,
                               disc,
                               alphas){
  ##list_models: list like object of possible models
  ##reward: reward function
  ##disc: discount factor
  ##alphas: list like object of alpha vetors, action, observation, index.

  ## returns alphas incremented with the alpha vectors, corresponding action, observation and index

  ## Calculate lower bound####
  vectors <- c()
  actions <- c()
  observations <- c()
  for (index_MDP_test in seq_along(list_models)){
    actions <- c(actions, solution_list[[index_MDP_test]]$policy)
    observations <- c(observations, seq(nrow(Reward)))
    vectors_test <- matrix(0, ncol=nrow(Reward), nrow=length(list_models))
    #apply policy of index_MDP_test to index_MDP_true
    for (index_MDP_true in seq_along(list_models)){

      solution_test <- mdp_eval_policy_matrix(transition_matrix_list[[index_MDP_true]],
                                              Reward,
                                              gamma,
                                              solution_list[[index_MDP_test]]$policy)
      vectors_test[index_MDP_true,] <- solution_test
    }
    vectors <- cbind(vectors,vectors_test)
  }

  alphas$vectors <- cbind(alphas$vectors, vectors)
  alphas$action <- c(alphas$action, actions)
  alphas$obs <- c(alphas$obs, observations)
  alphas$index <- seq(ncol(alphas$vectors))

  return(alphas)
}
