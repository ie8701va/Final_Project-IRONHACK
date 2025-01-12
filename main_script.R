# Set working directory to the project folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Adjust if not using RStudio

# Define the path to the scripts folder
scripts_folder <- "scripts"

# List all R scripts in the folder
script_files <- list.files(path = scripts_folder, pattern = "\\.R$", full.names = TRUE)

# Print all found scripts to help debugging
cat("Found the following scripts in the folder:\n")
print(script_files)

# Define the desired order of the scripts (use the full filenames from the 'script_files' list)
script_order <- c(
  "Functions.R",           # Functions script
  "DataCleaning.R",        # Data cleaning script
  "SentimentAnalysis.R",   # Sentiment analysis script
  "PredictiveModeling.R"   # Predictive modelling script
)

# Check if all scripts exist in the directory
script_files_in_order <- script_files[basename(script_files) %in% script_order]

# Print the matching scripts for debugging
cat("Matching scripts from the folder:\n")
print(script_files_in_order)

# Make sure the files exist in the directory and are in the correct order
if(length(script_files_in_order) == length(script_order)) {
  # Reorder the scripts based on the predefined order
  script_files_in_order <- script_files_in_order[match(script_order, basename(script_files_in_order))]
  
  # Source each script in the defined order
  for (script in script_files_in_order) {
    cat("Sourcing:", script, "\n")  # Log progress
    source(script)                   # Runs the code in the script
  }
  
  cat("All scripts sourced and executed successfully!\n")
} else {
  cat("Some scripts are missing or not in the correct order!\n")
}
