# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.3

# Description:
##############
# This script calculates the accuracy of Pl@ntNet predictions for genus and
# family against expert identifications in the output CSV file of the
# script 2_1_add_experts_info.R. The script also calculates the macro
# accuracy with Bayesian smoothing for genus and family predictions.

# Inputs
##############
# The output CSV file of the script 2_1_add_experts_info.R

# Outputs
##############
# -1) A CSV file in the "outputs/analysis" directory with the name
# "<input_file_name>_acc_by_obs.csv" containing the sample-level family/genus
# predictions and exact-match accuracies.
# -2) A CSV file in the "outputs/analysis" directory with the name
# "<input_file_name>_acc_by_group.csv" containing the accuracy by group for
# genus and family in a single file.
# -3) A CSV file in the "outputs/analysis" directory with the name
# "<input_file_name>_acc_stat.csv" containing the number of unique SampleIDs
# in the initial dataset, the number of rows in the filtered data with
# predicted_organ == 'plant_flower', the number of rows in the filtered data
# with predicted_organ == 'plant_flower' and confidence_score >= 0.5, the
# micro accuracy, macro accuracy, and macro accuracy with Bayesian smoothing
# for genus and family predictions.


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

# the name and directory of the CSV file containing Pl@ntNet predictions
# from the script 2_1_add_experts_info.r
input_file_name <- sprintf("%s_pn_experts.csv", cfg[["global"]]$base_file_name)
input_dir <- "outputs/predicted_files"
# the output directory where the results will be saved
output_dir <- "outputs/analysis"


#############################################################################
# Packages and Functions                                                    #
#############################################################################

library(dplyr)
library(ggplot2)
source("utils/metrics_functions.r")


#############################################################################
# Path management                                                           #
#############################################################################

input_file_path <- file.path(input_dir, input_file_name)
base_name <- tools::file_path_sans_ext(input_file_name)
output_file_path_1 <- file.path(output_dir, paste0(base_name, "_acc_by_group.csv"))
output_file_path_2 <- file.path(output_dir, paste0(base_name, "_acc_by_obs.csv"))
output_file_path_stat <- file.path(output_dir, paste0(base_name, "_acc_stat.csv"))


#############################################################################
# Main                                                                      #
#############################################################################

# Load the data from the CSV file
df <- read.csv(input_file_path,
               sep = ";",
               stringsAsFactors = FALSE,
               check.names = TRUE,
               na.strings = c("", "NA"))

# Count the number of unique SampleIDs in the initial dataset
n_id_initial <- length(unique(df$SampleID))

# keep only the rows where where latitude and longitude are not NA and where error is NA 
# or not equal to "fail_launch_download_image"
df_filtre <- df %>% filter(!is.na(latitude) &
                           !is.na(longitude) &
                           (is.na(error) | error != "fail_launch_download_image"))


# change the values in the "type" column "plant_no_flower" and "no_plant"
# by "no_plant_flower"
df_filtre <- df_filtre %>% mutate(type = case_when(
      type %in% c("plant_no_flower", "no_plant") ~ "no_plant_flower",
      TRUE ~ type))

# In the "predicted_organ" column, replace the values:
# - "plant_flower" if predicted_organ is "flower"
# - "no_plant_flower" for the other values
df_filtre <- df_filtre %>% mutate(predicted_organ = case_when(
      predicted_organ == "flower" ~ "plant_flower",
      TRUE ~ "no_plant_flower"))

# if predicted_organ_score < 0.5, replace the value in the "predicted_organ" 
# column with "no_plant_flower"
df_filtre <- df_filtre %>%
  mutate(
    predicted_organ = if_else(
      predicted_organ_score < cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_flower,
      "no_plant_flower",
      predicted_organ
    )
  )

# Filter the data frame to keep only the rows where:
# - genus_exp != "unidentifiable"
# - family_exp != "unidentifiable"
# - predicted_organ == "plant_flower"
# - rank == 1 or NA
df_filtre_taxon <- df_filtre %>%
  filter(
    genus_exp != "unidentifiable",
    family_exp != "unidentifiable",
    predicted_organ == "plant_flower",
    rank == 1 | is.na(rank)
  )

# Count the number of unique SampleIDs in the filtered dataset
n_id_filtred <- length(unique(df_filtre_taxon$SampleID))

# Keep only the rows where confidence_score >= 0.5
df_filtre_taxon <- df_filtre_taxon %>%
  filter(
    confidence_score >= cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_taxon,
  )

# evaluate genus and family predictions against expert identifications
eval_genus <- evaluation(df_filtre_taxon, "genus_exp", "predicted_genus", 3)
eval_family <- evaluation(df_filtre_taxon, "family_exp", "predicted_family", 3)

# Print the evaluation results to the console
cat(sprintf("---\n"))
cat(sprintf("Number of unique SampleIDs in the initial dataset: %d\n", n_id_initial))
cat(sprintf("Number of rows in the filtered data with predicted_organ == 'plant_flower': %d\n", n_id_filtred))
cat(sprintf("Number of rows in the filtered data with predicted_organ == 'plant_flower' and confidence_score >= %g: %d\n", cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_taxon, eval_genus$num_samples))
cat(sprintf("---\n"))
cat(sprintf("Micro accuracy (genus_exp vs predicted_genus): %.4f\n", eval_genus$micro_accuracy))
cat(sprintf("Macro accuracy (genus_exp vs predicted_genus): %.4f\n", eval_genus$macro_accuracy))
cat(sprintf("Macro accuracy (genus_exp vs predicted_genus) with Bayesian smoothing: %.4f\n", eval_genus$macro_accuracy_bayes))
cat(sprintf("---\n"))
cat(sprintf("Micro accuracy (family_exp vs predicted_family): %.4f\n", eval_family$micro_accuracy))
cat(sprintf("Macro accuracy (family_exp vs predicted_family): %.4f\n", eval_family$macro_accuracy))
cat(sprintf("Macro accuracy (family_exp vs predicted_family) with Bayesian smoothing: %.4f\n", eval_family$macro_accuracy_bayes))
cat(sprintf("---\n"))

# Create a data frame to store the metrics and their values
df_stat <- data.frame(
  metric = c(
    "Number of unique SampleIDs in the initial dataset",
    "Number of rows in the filtered data with predicted_organ == 'plant_flower'",
    sprintf("Number of rows in the filtered data with predicted_organ == 'plant_flower' and confidence_score >= %g", cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_taxon),
    "Micro accuracy (genus_exp vs predicted_genus)",
    "Macro accuracy (genus_exp vs predicted_genus)",
    "Macro accuracy (genus_exp vs predicted_genus) with Bayesian smoothing",
    "Micro accuracy (family_exp vs predicted_family)",
    "Macro accuracy (family_exp vs predicted_family)",
    "Macro accuracy (family_exp vs predicted_family) with Bayesian smoothing"
  ),
  value = c(
    as.numeric(n_id_initial),
    as.numeric(n_id_filtred),
    as.numeric(eval_genus$num_samples),
    round(eval_genus$micro_accuracy, 4),
    round(eval_genus$macro_accuracy, 4),
    round(eval_genus$macro_accuracy_bayes, 4),
    round(eval_family$micro_accuracy, 4),
    round(eval_family$macro_accuracy, 4),
    round(eval_family$macro_accuracy_bayes, 4)
  ),
  stringsAsFactors = FALSE
)

# Save the metrics data frame to a CSV file
write.table(df_stat,
            file = output_file_path_stat,
            sep = ";",
            row.names = FALSE,
            quote = TRUE,
            na = "")

# Print a message indicating the output file path for the metrics
cat(sprintf("Output file written: %s\n", output_file_path_stat))

# Export accuracy by group for genus and family in a single file.
df_acc_group <- bind_rows(
  eval_genus$micro_accuracy_by_group %>%
    transmute(
      taxon_level = "genus",
      expected_taxon = expected_value,
      n_samples = n_samples,
      accuracy = acc_group
    ),
  eval_family$micro_accuracy_by_group %>%
    transmute(
      taxon_level = "family",
      expected_taxon = expected_value,
      n_samples = n_samples,
      accuracy = acc_group
    )
)

# Save the accuracy by group data frame to a CSV file
write.table(df_acc_group,
            file = output_file_path_1,
            sep = ";",
            row.names = FALSE,
            quote = TRUE,
            na = "")

# Print a message indicating the output file path for the accuracy by group
cat(sprintf("Output file written: %s\n", output_file_path_1))

# Export sample-level family/genus predictions and exact-match accuracies.
df_sortie <- df_filtre_taxon %>%
  transmute(
    SampleID = SampleID,
    image_url = image_url,
    year = year,
    latitude = latitude,
    longitude = longitude,
    expected_family = family_exp,
    predicted_family = predicted_family,
    acc_familly = !is.na(family_exp) & !is.na(predicted_family) & family_exp == predicted_family,
    expected_genus = genus_exp,
    predicted_genus = predicted_genus,
    acc_genus = !is.na(genus_exp) & !is.na(predicted_genus) & genus_exp == predicted_genus
  )

# Save the sample-level accuracy data frame to a CSV file
write.table(df_sortie,
            file = output_file_path_2,
            sep = ";",
            row.names = FALSE,
            quote = TRUE,
            na = "")

# Print a message indicating the output file path for the sample-level accuracy
cat(sprintf("Output file written: %s\n", output_file_path_2))