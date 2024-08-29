setwd("variable_parameters/pomdpx/")
# Function to extract <Vector> lines from a file treated as text
extract_vector_lines <- function(file) {
  lines <- readLines(file)
  vector_lines <- grep("<Vector", lines, value = TRUE)
  return(vector_lines)
}

# List all files that start with "fullPOMDP" and end with ".policyx"
files <- list.files(pattern = "^fullPOMDP.*\\.policyx$")

# Extract <Vector> lines from each file and combine them
all_vector_lines <- unlist(lapply(files, extract_vector_lines))

# Create the combined content for the new XML file
header <- '<?xml version="1.0" encoding="ISO-8859-1"?>\n<Policy version="0.1" type="value" model="variable_parameters/pomdpx/Combined.pomdpx" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="policyx.xsd">\n<AlphaVector vectorLength="2" numObsValue="1870" numVectors="XXX">\n'
footer <- '</AlphaVector> </Policy>'

# Replace "XXX" with the actual number of vectors
header <- gsub("XXX", length(all_vector_lines), header)

# Combine the header, all vector lines, and the footer
combined_content <- c(header, all_vector_lines, footer)

# Write the combined content to a new XML file
writeLines(combined_content, "CombinedPolicy.policyx")

cat("Combined file created: CombinedPolicy.policyx\n")
