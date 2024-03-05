
library(dplyr)

################################
#### Merge all files by rows in a folder
################################

setwd("/work/larylab/Mao_Ding/bifurcatoR/vignettes/Simulation_Study/Module_2")

directory_path = "results/raw_results"

# List all files in the directory
file_list <- list.files(directory_path)

# Initialize an empty list to store the data from each file
data_list <- list()

# Loop through each file and read its contents
for (file in file_list) {
  # Construct the full file path
  file_path <- paste0(directory_path, file)
  
  # Read the file (adjust the read function based on the file type, e.g., read.csv, read.table, readRDS, etc.)
  file_data <- readRDS(file_path)
  
  # Append the data to the list
  data_list[[file]] <- file_data
}

df = bind_rows(data_list)

saveRDS(df, "results/module2_all_tests_sim(1000).rds")