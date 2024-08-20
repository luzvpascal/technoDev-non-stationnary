## some additional funcitons ####
read_policyx2 <- function(file){
  #inputs:
  #file as tring with location of policyx file

  #outpout:
  #list of
  #vectors: alpha vectors
  #action: optimal action for each alpha vector
  #obs: observation corresponding to each alpha vector
  xml <- xml2::read_xml(file)
  xml_vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)),
                                                " ")[[1]])
  get_action <- function(v) as.numeric(xml2::xml_attr(v, "action"))
  get_obs <- function(v) as.numeric(xml2::xml_attr(v, 'obsValue'))
  n_states <- length(get_vector(xml_vectors[[1]]))
  alpha <- vapply(xml_vectors, get_vector, numeric(n_states))
  alpha_action <- vapply(xml_vectors, get_action, double(1)) + 1
  alpha_obs <- vapply(xml_vectors, get_obs, double(1)) + 1
  list(vectors=matrix(alpha, nrow=n_states), action=alpha_action, obs=alpha_obs, index = seq(ncol(alpha)))
}


interp_policy2 <- function (belief_state_momdp, obs, alpha, alpha_action, alpha_obs, alpha_index){
  #inputs:
  # belief_state_momdp: vector, prior on partially observable variables length Num_mod
  # obs: last observed state integer 0 for low and 1 for high
  # alpha: alpha vectors as returned by read_policyx2
  # alpha_action: actions as returned by read_policyx2
  # alpha_obs: obsevations as returned by read_policyx2
  # alpha_obs: indexes as returned by read_policyx2

  # output: list of: value function, optimal action as integer, index of corresponding alpha vector

  id <- which(alpha_obs == obs)
  alpha2 <- alpha[,id]
  alpha_action2 <- alpha_action[id]
  alpha_index2 <- alpha_index[id]
  a <- belief_state_momdp %*% alpha2
  if (sum(a == 0) == length(a)) {
    output <- list(0, 1)
  }
  else {
    output <- list(max(a), alpha_action2[which.max(a)], alpha_index2[which.max(a)])
  }
  output
}
