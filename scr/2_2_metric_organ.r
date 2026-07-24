# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.1

# Description:
##############
# This script creates a confusion matrix for the predicted organ (predicted_organ)
# against the organ identified by experts (type) in the output CSV file of the script
# 2_1_add_experts_info.R. The script also calculates the macro accuracy and
# weighted precision for predictions of the flower.

# Inputs
##############
# The output CSV file of the script 2_1_add_experts_info.R

# Outputs
##############
# -1) A PNG file in the "outputs/analysis" directory with the name 
# "<input_file_name>_cf_organ.png" containing the confusion matrix plot.
# -2) The number of unique SampleIDs in the initial dataset and after filtering
# in the console output and in a CSV file in the "outputs/analysis" directory 
# with the name "<input_file_name>_cf_organ_stat.csv".
# -3) The macro accuracy and weighted precision for predictions of the flower
# in the console output and in a CSV file in the "outputs/analysis" directory
# with the name "<input_file_name>_cf_organ_stat.csv"


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
# Packages                                                                  #
#############################################################################

library(dplyr)
library(ggplot2)


#############################################################################
# Path management                                                           #
#############################################################################

input_file_path <- file.path(input_dir, input_file_name)
base_name <- tools::file_path_sans_ext(input_file_name)
output_file_path <- file.path(output_dir, paste0(base_name, "_cf_organ.png"))
output_stats_path <- file.path(output_dir, paste0(base_name, "_cf_organ_stat.csv"))


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

# In the "error" column, replace the values containing "species" by "species_not_found"
df_filtre$error[grepl("species", df_filtre$error, ignore.case = TRUE)] <- "species_not_found"

# Filter the data frame to keep only the rows where:
# - error = NA or "species_not_found"
# - rank = 1 or NA
df_filtre_organ <- df_filtre %>% filter(
  (is.na(error) | error == "species_not_found") &
  (is.na(rank) | rank == 1))

# Count the number of unique SampleIDs in the filtered dataset
n_id_filtred <- length(df_filtre_organ$SampleID)

# In the "predicted_organ" column is modified to "no_plant_flower" 
# if the predicted_organ_score is less than 0.5
df_filtre_organ <- df_filtre_organ %>%
  mutate(
    predicted_organ = if_else(
      predicted_organ_score < cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_flower,
      "no_plant_flower",
      predicted_organ
    )
  )

# Create a confusion matrix for predicted_organ vs type
cm <- table(
  Prediction = df_filtre_organ$predicted_organ,
  Reference  = df_filtre_organ$type
)

# Calculate macro accuracy (mean of recall across classes)
recall <- diag(cm) / colSums(cm)
macro_acc <- mean(recall)
# Calculate weighted precision for "plant_flower" class
cof_multi <- 1/(colSums(cm)["no_plant_flower"]/sum(cm))
FP <- cm["plant_flower", "no_plant_flower"]
TP <- cm["plant_flower", "plant_flower"]
weighted_precision <- TP / (TP + FP*cof_multi)

# Create a data frame from the confusion matrix for plotting
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Prediction", "Reference", "count")
cm_df <- cm_df %>%
  group_by(Reference) %>%
  mutate(pct_ref = if (sum(count) > 0) count / sum(count) else 0) %>%
  ungroup() %>%
  mutate(label = sprintf("%d\n(%.1f%%)", count, 100 * pct_ref))
# Custom labels and colors for the confusion matrix.
organ_class_labels <- c(
  no_plant_flower = "Plant without flower\nor no plant",
  plant_flower = "Plant with flower"
)

# Create the confusion matrix plot using ggplot2
cm_plot <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = pct_ref)) +
  geom_tile(color = "#000000", linewidth = 0.5) +
  geom_text(aes(label = label), size = 5, fontface = "bold", color = "#000000") +
  scale_fill_gradient(low = "#f8fafc", high = "#77933C") +
  scale_x_discrete(labels = organ_class_labels, limits = rev(names(organ_class_labels))) +
  scale_y_discrete(labels = organ_class_labels) +
  labs(
    x = "Reference",
    y = "Prediction",
    fill = "%",
    caption = sprintf("n sample = %d", nrow(df_filtre_organ))
  ) +
  coord_equal() +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, color = "#000000"),
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(color = "#000000"),
    axis.text.y = element_text(color = "#000000", angle = 90, vjust = 0.5, hjust = 0.5),
  )

# Save the confusion matrix plot as a PNG file
ggsave(output_file_path, cm_plot, width = 7, height = 6, dpi = 300)

# Create a data frame to store the metrics and their values
stats_df <- data.frame(
  metric = c(
    "Number of unique SampleIDs in the initial dataset",
    "Number of unique SampleIDs after filtering",
    "Macro Accuracy",
    "Weighted Precision for 'plant_flower'"
  ),
  value = c(
    as.numeric(n_id_initial),
    as.numeric(n_id_filtred),
    round(macro_acc, 4),
    round(weighted_precision, 4)
  ),
  stringsAsFactors = FALSE
)

# Save the metrics data frame to a CSV file
write.table(
  stats_df,
  file = output_stats_path,
  sep = ";",
  row.names = FALSE,
  quote = TRUE,
  na = ""
)

# Print the confusion matrix and metrics to the console
print(cm)
print(paste("Number of unique SampleIDs in the initial dataset:", n_id_initial))
print(paste("Number of unique SampleIDs after filtering:", n_id_filtred))
print(paste("Macro Accuracy:", round(macro_acc, 4)))
print(paste("Weighted Precision for 'plant_flower':", round(weighted_precision, 4)))
cat(sprintf("Confusion matrix plot written: %s\n", output_file_path))
cat(sprintf("Stats file written: %s\n", output_stats_path))
