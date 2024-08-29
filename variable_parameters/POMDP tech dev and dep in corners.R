source("variable_parameters/write_POMDPx.R")
##technology####
p_dev <- 0.1**time_step
tech_states <- seq(2)
transition_success <- list(diag(2),
                           matrix(c(1-p_dev, 0, p_dev, 1), ncol=2))
transition_failure <- list(diag(2),
                           diag(2))
transition_tech <- list(transition_success, transition_failure)

##
TR_FUNCTION_ECO <- transition_matrix_list
TR_FUNCTION_TECH <- transition_tech
REW <- Reward
GAMMA <- gamma

B_FULL_ECO <- c(rep(0, nrow(Reward)))
B_FULL_ECO[N_ecosystem+1] <- 1
B_FULL_TECH <- c(1, rep(0, length(tech_states)-1))

# Load necessary libraries
library(stringr)

# Read the template file
template_file <- "variable_parameters/pomdpx/fullPOMDP.pomdpx"
template_content <- readLines(template_file)
B_PAR_to_replace <- paste(rep(1, length(transition_matrix_list)) / (length(transition_matrix_list)), collapse = " ")
# Loop through each index_MPD and modify the specific <ProbTable> section
for (index_MPD in seq(16, length(TR_FUNCTION_ECO))) {
  print(index_MPD)

  file_name <- paste0("variable_parameters/pomdpx/fullPOMDPweighton", index_MPD)

  B_PAR <- c(0.2 * rep(1, length(transition_matrix_list)) / (length(transition_matrix_list) - 1))
  B_PAR[index_MPD] <- 0.8

  # Convert B_PAR to a string with spaces separating the values
  B_PAR_string <- paste(B_PAR, collapse = " ")

  # Use regex to replace the specific <ProbTable> content
  modified_content <- str_replace(template_content,
                                  B_PAR_to_replace,
                                  B_PAR_string)

  # Write the modified content to a new file
  output_file <- paste0(file_name, ".pomdpx")
  writeLines(modified_content, output_file)

  # Run the sarsop command
  path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")
  OUTPUT_FILE <- paste0(file_name, ".policyx")

  cmd <- paste(path_to_sarsop,
               "--precision", 0.0000001,
               "--timeout", 60,
               "--output", OUTPUT_FILE,
               output_file,
               sep = " ")
  system(cmd)
}
