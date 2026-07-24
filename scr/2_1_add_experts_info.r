# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.1

# Description:
##############
# This script merges the output CSV file of the script 
# 1_1_identify_plantnet_top5_v4.R with a CSV file containing expert 
# identifications for a subset of the observations from the supplementary 
# material of Pernat et al. (2024) (https://doi.org/10.1002/ece3.11537).

# Inputs
##############
# The output CSV file of the script 1_1_identify_plantnet_top5_v4.R

# Outputs
##############
# A merged data frame in a CSV file in the "outputs/predicted_files" 
# directory with the name "<input_file_name>_experts.csv" containing 
# the same columns as the input CSV with the addition of the following:
# - type -> the organ identified by the expert (e.g., "plant_flower")
# - family_exp -> the family identified by the expert
# - genus_exp -> the genus identified by the expert
# - species_exp -> the species identified by the expert
# The number of unique SampleIDs in the merged data frame can be less 
# than the number of unique SampleIDs in the input CSV file because not
# all observations have been identified by experts.


#############################################################################
# Initialization of configuration                                           #
#############################################################################

# It is for loading the configuration from the config.yaml file. (e.g., 
# API key for Pl@ntNet). The config.yaml file should be in the root directory
# of the project.
get <- config::get
cfg_init <- get(file = "config.yaml", config = "initialisation")
cfg <- get(file = "config.yaml", config = cfg_init$yaml_config)


#############################################################################
# Settings                                                                  #
#############################################################################

# the name and directory of the CSV file containing expert identifications
input_file_name_experts <- "isomex_experts_cor.csv" 
input_dir_experts <- "inputs"

# the name and directory of the CSV file containing Pl@ntNet predictions
# from the script 1_1_identify_plantnet_top5_v4.R
input_file_name_pred <- sprintf("%s_pn.csv", cfg[["global"]]$base_file_name)
input_dir_pred <- "outputs/predicted_files"

# the output directory where the merged CSV file will be saved
output_dir <- "outputs/predicted_files"


#############################################################################
# Path management                                                           #
#############################################################################

input_path_experts <- file.path(input_dir_experts,
                        input_file_name_experts)

input_path_pred <- file.path(input_dir_pred,
                        input_file_name_pred)

# Remove .csv extension
base_name <- tools::file_path_sans_ext(input_file_name_pred)
# Replace "_intermediate" with "_final"
output_file_name <- paste0(base_name, "_experts.csv")
# Create the full output path
output_path <- file.path(output_dir, output_file_name)

if (!file.exists(input_path_experts)) {
  stop(sprintf("Input file not found: %s", input_path_experts))
}

if (!file.exists(input_path_pred)) {
  stop(sprintf("Input file not found: %s", input_path_pred))
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


############################################################################
# Main                                                                      #
#############################################################################

# Read the input CSV file containing image URLs and SampleIDs and other metadata
df_pred <- read.csv(
  input_path_pred,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA")
)

# Read the Pl@ntNet predictions CSV file containing SampleIDs and URLs
df_experts <- read.csv(
  input_path_experts,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA")
)

# Ensure that the SampleID columns are character type for proper merging
df_pred$SampleID <- as.character(df_pred$SampleID)
df_experts$SampleID <- as.character(df_experts$SampleID)

# Merge the two data frames by SampleID, keeping all rows from the base data frame
combined_df <- merge(
  df_pred,
  df_experts,
  by = "url",
  all.x = TRUE,
  sort = FALSE
)

# Merge the two data frames by SampleID, keeping all rows from the base data frame
combined_df <- merge(df_pred,
                     df_experts[, c("url", "type", "family_exp", "genus_exp", "species_exp")],
                     by = "url")

# Sort the rows by SampleID  and rank
rank_order <- suppressWarnings(as.numeric(combined_df$rank))
combined_df <- combined_df[order(combined_df$SampleID, rank_order), ]

# Number of unique SampleIDs in the combined data frame
num_unique_sample_ids <- length(unique(combined_df$SampleID))

# Write the combined data frame to the output CSV file
write.table(
  combined_df,
  output_path,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  fileEncoding = "UTF-8"
)

# Number of unique SampleIDs in the combined data frame
num_unique_sample_ids <- length(unique(combined_df$SampleID))

# Print the number of unique SampleIDs in the combined data frame
message(sprintf("Number of unique SampleIDs in the combined data frame: %d", num_unique_sample_ids))

# Print a message indicating that the combined file has been saved
message(sprintf("Done. Combined file saved to: %s", output_path))