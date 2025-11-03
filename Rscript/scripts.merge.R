# merge model scripts into a single file

# Get a list of the R script files
script_files <- list.files(path = "../Rscript", pattern = "\\.R$", 
                           full.names = TRUE)
# Create a header for vector to store the combined content
combined_script_content <- 
  c("# Loxahatchee 39 Box Model: Concatenated Source code for all scripts",
  "#    (each script filename is enclosed by ####) \n")

# Loop through the files, read their content, and append to the combined content
for (file_path in script_files) {
  # Add a comment indicating the source file
  combined_script_content <- c(combined_script_content, 
                               paste0("#### Source: ", 
                                      basename(file_path), 
                                      " ####\n"))
  
  # Read the lines of the current script
  current_script_lines <- readLines(file_path)
  
  # Append the lines to the combined content
  combined_script_content <- c(combined_script_content, current_script_lines,
                               "\n\n") # Add extra newlines for separation
} # end for

# Write the combined content to the new file
writeLines(combined_script_content, con = "combined_scripts.R.txt")

# cleanup
rm(script_files, combined_script_content, current_script_lines)
