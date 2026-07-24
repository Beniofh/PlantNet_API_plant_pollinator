# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.1

# Description:
##############
# This script filters the output CSV file of the script 
# 1_1_identify_plantnet_top5_v4.R to keep only the rows where :
# - predicted_organ is "plant_flower" with a predicted_organ_score >= 0.5
# - rank is equal to 1 with a confidence_score >= 0.5

# Inputs
##############
# The output CSV file of the script 1_1_identify_plantnet_top5_v4.R

# Outputs
##############
# file in the "outputs" directory with the name :
# - "<input_file_name>_filtred.csv" with the same columns as the input CSV
# - "<input_file_name>_filtred_summary.csv" -> taxon by year summary 


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

# The name of a output CSV file of the script 1_1_identify_plantnet_top5_v4.R
input_file_name <- sprintf("%s_pn.csv", cfg[["global"]]$base_file_name)

# the input directory where the CSV file is located
input_dir <- "outputs/predicted_files"

# the output directory where the results will be saved
output_dir <- "outputs/analysis"

# The threshold for filtering Plant_flower predictions based on the organ probability score.
# Plant_flower predictions with a probability score below this threshold will be 
# filtered into no_plant_flower.
probability_threshold_flower <- cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_flower

# The threshold for filtering taxon predictions based on the taxon probability score.
# Predictions with a probability score below this threshold will be filtered out.
probability_threshold_taxon <- cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_taxon

#############################################################################
# Packages                                                                  #
#############################################################################

library(dplyr)
library(ggplot2)
library(stringr)

#############################################################################
# Path management                                                           #
#############################################################################

input_file_path <- file.path(input_dir, input_file_name)
input_file_name_base <- tools::file_path_sans_ext(input_file_name)
output_file_path_1 <- file.path(output_dir, sprintf("%s_filtred.csv", input_file_name_base))
output_file_path_2 <- file.path(output_dir, sprintf("%s_filtred_summary.csv", input_file_name_base))

if (!file.exists(input_dir)) {
  stop(sprintf("Input directory not found: %s", input_dir))
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#############################################################################
# Main                                                                      #
#############################################################################

# read the input CSV file
df <- read.csv(input_file_path,
               sep = ";",
               stringsAsFactors = FALSE,
               check.names = TRUE,
               na.strings = c("", "NA"))

# keep only the rows where where latitude and longitude are not NA and where error is NA 
# or not equal to "fail_launch_download_image"
df_filtre <- df %>% filter(!is.na(latitude) &
                           !is.na(longitude) &
                           (is.na(error) | error != "fail_launch_download_image"))


# In the "predicted_organ" column, replace the values:
# - "plant_flower" if predicted_organ is "flower"
# - "no_plant_flower" for the other values
df_filtre <- df_filtre %>% mutate(predicted_organ = case_when(
      predicted_organ == "flower" ~ "plant_flower",
      TRUE ~ "no_plant_flower"))

# In the "predicted_organ" column, replace the values with "no_plant_flower" 
# if predicted_organ_score < 0.5
df_filtre <- df_filtre %>%
  mutate(
    predicted_organ = if_else(
      predicted_organ_score < probability_threshold_flower,
      "no_plant_flower",
      predicted_organ
    )
  )

# Filter the data frame to keep only the rows where predicted_organ is 
#"plant_flower" and rank is equal to 1
df_filtre <- df_filtre %>%
  filter(
    predicted_organ == "plant_flower",
    rank == 1
  )

# Count the number of rows in the filtered data frame where predicted_organ
# is "plant_flower"
n_pred_flowered <- nrow(df_filtre)

# Filter the data frame to keep only the rows where confidence_score >= 0.5
df_filtre <- df_filtre %>%
  filter(
    confidence_score >= probability_threshold_taxon,
  )

# Count the number of rows in the filtered data frame where predicted_organ
n_pred_flowered_use <- nrow(df_filtre)

# Write the filtered data frame to a CSV file
write.table(df_filtre,
            file = output_file_path_1,
            sep = ";",
            row.names = FALSE,
            quote = TRUE,
            na = "")

# Create a summary data frame with counts and percentages of predicted_genus by year
df_filtre_summary <- df_filtre %>%
  filter(!is.na(year), !is.na(predicted_genus)) %>%
  count(year, predicted_genus, name = "n") %>%
  group_by(year) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(n, pct),
    names_glue = "{year}_{.value}",
    values_fill = list(n = 0, pct = 0)
  ) %>%
  arrange(predicted_genus)

# Write the summary data frame to a CSV file
write.table(df_filtre_summary,
            file = output_file_path_2,
            sep = ";",
            row.names = FALSE,
            quote = TRUE,
            na = "")

# Print messages indicating the output files and counts
message("========================================")
cat(sprintf("Output file written: %s\n", output_file_path_1))
cat(sprintf("Output file written: %s\n", output_file_path_2))
message("========================================")
cat(sprintf("Number of rows in the filtered data with predicted_organ == 'plant_flower': %d\n", n_pred_flowered))
cat(sprintf("Number of rows in the filtered data with predicted_organ == 'plant_flower' and confidence_score >= %g: %d\n", cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_taxon, n_pred_flowered_use))
message("========================================")
