# Define the function to run a single simulation
run_simulation_failure <- function(i) {
  trajectory(
    state_prior_eco = tuple_to_index(1,8, N_ecosystem + 1),
    state_prior_tech = 1,
    Tmax = 85,
    initial_belief_state = B_PAR,
    initial_belief_state_tech = B_PAR_TECH,
    transition_ecosystem = TR_FUNCTION_ECO,
    transition_tech = TR_FUNCTION_TECH,
    reward = REW,
    true_model = TRUE_MODEL,
    true_model_tech = 2,
    alpha_momdp = alphas,
    disc = GAMMA,
    optimal_policy = TRUE,
    naive_policy = NA,
    alpha_indexes = FALSE
  )$data_output[-86,]
}
run_simulation_success <- function(i) {
  trajectory(
    state_prior_eco = tuple_to_index(1,8, N_ecosystem + 1),
    state_prior_tech = 1,
    Tmax = 85,
    initial_belief_state = B_PAR,
    initial_belief_state_tech = B_PAR_TECH,
    transition_ecosystem = TR_FUNCTION_ECO,
    transition_tech = TR_FUNCTION_TECH,
    reward = REW,
    true_model = TRUE_MODEL,
    true_model_tech = 1,
    alpha_momdp = alphas,
    disc = GAMMA,
    optimal_policy = TRUE,
    naive_policy = NA,
    alpha_indexes = FALSE
  )$data_output[-86,]
}
