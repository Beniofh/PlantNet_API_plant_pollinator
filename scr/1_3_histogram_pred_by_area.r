# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.1

# Description:
##############
# This script creates a double histogram plot of the predicted genus or family 
# between two geographic areas (e.g., continents) based on latitude and longitude 
# values in the output CSV file of the script.

# Inputs
##############
# The output CSV file of the script 1_2_filter_indentify_plantnet_top5.R

# Outputs
##############
# A PNG file in the "outputs/analysis" directory with the name 
# "<input_file_name>_filtred_by_<group_name>.png" containing the double histogram plot.


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

# The name of a output CSV file of the script 1_2_filter_indentify_plantnet_top5.R
input_file_name <- sprintf("%s_pn.csv", cfg[["global"]]$base_file_name)

# the input directory where the CSV file is located
input_dir <- "outputs/predicted_files"

# the output directory where the results will be saved
output_dir <- "outputs/analysis"

# The column to be used for the histogram
# Options: "predicted_genus" or "predicted_family"
study_col <- cfg[["1_3_histogram_pred_by_area"]]$study_col

# Define the names and geographic bounding boxes of the two groups 
# to compare
group_1 <-cfg[["1_3_histogram_pred_by_area"]]$group_1
long_min_1 <- cfg[["1_3_histogram_pred_by_area"]]$long_min_1
long_max_1 <- cfg[["1_3_histogram_pred_by_area"]]$long_max_1
lat_min_1 <- cfg[["1_3_histogram_pred_by_area"]]$lat_min_1
lat_max_1 <- cfg[["1_3_histogram_pred_by_area"]]$lat_max_1

group_2 <- cfg[["1_3_histogram_pred_by_area"]]$group_2
long_min_2 <- cfg[["1_3_histogram_pred_by_area"]]$long_min_2
long_max_2 <- cfg[["1_3_histogram_pred_by_area"]]$long_max_2
lat_min_2 <- cfg[["1_3_histogram_pred_by_area"]]$lat_min_2
lat_max_2 <- cfg[["1_3_histogram_pred_by_area"]]$lat_max_2

# define the name of the grouping variable for the plot
group_name <- cfg[["1_3_histogram_pred_by_area"]]$group_name

# minimum number of occurrences for a taxon to be included in the plot
sum_occ_min <- cfg[["1_3_histogram_pred_by_area"]]$sum_occ_min


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
output_plot_file <- sprintf("%s_filtred_by_%s.png", base_name, group_name)
output_plot_path <- file.path(output_dir, output_plot_file)

#############################################################################
# Main                                                                      #
#############################################################################

# Load the data from the CSV file
df <- read.csv(
  input_file_path,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA")
)

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
      predicted_organ_score < cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_flower,
      "no_plant_flower",
      predicted_organ
    )
  )

# Filter the data frame to keep only the rows where predicted_organ is
#"plant_flower", confidence_score for taxon is >= 0.5, and rank is 
# equal to 1
df_filtre <- df_filtre %>%
  filter(
    predicted_organ == "plant_flower",
    confidence_score >= cfg[["1_2_filter_indentify_plantnet_top5"]]$probability_threshold_taxon,
    rank == 1
  )

# Create a new column 'study_col' based on 'predicted_genus'
df_filtre['study_col'] <- df_filtre[study_col]

# Categorize each row by group
df_filtre <- df_filtre %>%
  mutate(
    group = case_when(
      longitude >= long_min_1 & longitude <= long_max_1 &
      latitude >= lat_min_1 & latitude <= lat_max_1 ~ group_1,
      longitude >= long_min_2 & longitude <= long_max_2 &
      latitude >= lat_min_2 & latitude <= lat_max_2 ~ group_2,
      TRUE ~ NA_character_
    )
  )

# Keep only observations assigned to one of the two target groups.
df_filtre <- df_filtre %>%
  filter(!is.na(group))

# Create a summary data frame with counts and percentages of study_col by group
summary_df_filtre <- df_filtre %>%
  filter(!is.na(study_col)) %>%
  count(study_col, group, name = "n") %>%
  tidyr::complete(
    study_col,
    group = c(group_1, group_2),
    fill = list(n = 0)
  ) %>%
  group_by(group) %>%
  mutate(group_total = sum(n), pct = if_else(group_total > 0, 100 * n / group_total, 0)) %>%
  ungroup() %>%
  group_by(study_col) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(
    signed_pct = if_else(group == group_1, -pct, pct),
    signed_n = if_else(group == group_1, -n, n)
  ) %>%
  arrange(desc(total_n), study_col, group)

# Create an ordered factor for study_col based on total_n for plotting
summary_order <- summary_df_filtre %>%
  distinct(study_col, total_n) %>%
  arrange(desc(total_n), study_col) %>%
  pull(study_col)

# Order the summary data frame by the ordered study_col
summary_df_filtre <- summary_df_filtre %>%
  mutate(study_col = factor(study_col, levels = rev(summary_order)))

# Create a data frame for plotting, filtering out study_col with total_n < 5
plot_df_filtre <- summary_df_filtre %>%
  filter(total_n >= sum_occ_min) %>%
  mutate(
    group = factor(group, levels = c(group_1, group_2))
  )

# Count the number of unique SampleIDs in the filtered dataset
n_sample <- nrow(df_filtre)

# Count the number of unique SampleIDs in the filtered plot dataset
n_sample_plot <- sum(plot_df_filtre$n)

# Check if there are any samples to plot after filtering by sum_occ_min
if (n_sample_plot == 0) {
  stop("No data available for plotting after filtering by sum_occ_min.")
}

# Create an ordered factor for study_col based on total_n for plotting
plot_order <- plot_df_filtre %>%
  distinct(study_col, total_n) %>%
  arrange(desc(total_n), study_col) %>%
  pull(study_col)

# Order the plot data frame by the ordered study_col
plot_df_filtre <- plot_df_filtre %>%
  mutate(study_col = factor(study_col, levels = rev(plot_order)))

# Define colors for the groups
group_colors <- stats::setNames(
  c("#77933C", "#C3D69B"),
  c(group_1, group_2)
)

# Calculate the maximum absolute value of signed_pct for setting x-axis limits
max_abs_n <- max(abs(plot_df_filtre$signed_pct), na.rm = TRUE)

if (grepl("genus", study_col)) {
    x_lab <- sprintf("Percentage of predicted genus by %s", group_name)
    y_lab <- "Predicted genus"
} else {
    x_lab <- sprintf("Percentage of predicted family by %s", group_name)
    y_lab <- "Predicted family"
}

# Create the double histogram plot using ggplot2
gg <- ggplot(plot_df_filtre, aes(x = signed_pct, y = study_col, fill = group)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "#111827", linewidth = 0.6) +
  annotate("text", x = -max_abs_n / 2, y = Inf, label = group_1, fontface = "bold", size = 4.5, vjust = -0.6) +
  annotate("text", x = max_abs_n / 2, y = Inf, label = group_2, fontface = "bold", size = 4.5, vjust = -0.6) +
  geom_text(
    aes(label = if_else(n > 0, as.character(n), ""), hjust = if_else(group == group_1, 1.1, -0.1)),
    color = "#111827",
    size = 3.5
  ) +
  scale_fill_manual(values = group_colors) +
  scale_x_continuous(
    labels = abs,
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    x = x_lab,
    y = y_lab,
    caption = sprintf("number of samples displayed : %d/%d", n_sample_plot, n_sample)
  ) +
  guides(fill = "none") +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(color = "#111827"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, color = "#111827"),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  )

# Add coord_cartesian with clip = "off" to allow annotations outside the plot area
gg <- gg + coord_cartesian(clip = "off")

# Save the plot to a PNG file
ggsave(output_plot_path, gg, width = 11, height = 10, dpi = 300)

# Print a message indicating that the plot has been saved
cat(sprintf("Plot written: %s\n", output_plot_path))
