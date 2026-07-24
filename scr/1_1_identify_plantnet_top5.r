# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.5

# Description:
##############
# This script identifies plants from image URLs using the Pl@ntNet API and exports
# the top-5 taxonomical predictions, organs predictions and confidence scores to a
# CSV file.

# inputs
##############
# The output CSV file of 0_1_inat_data_creation.r
# if you are in "auto" mode (see project_type below).

# outputs
##############
# file in the "outputs" directory with the name 
#"<input_file_name>_pn.csv"
# containing the following columns.


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

# The name of a output CSV file of the script 0_1_inat_data_creation.r
input_file_name <- sprintf("%s.csv", cfg[["global"]]$base_file_name)
input_dir <- "outputs/fetched_data"
# the output directory where the results will be saved
output_dir <- "outputs/predicted_files"
# the temporary directory where the images will be downloaded
temp_dir <- "temp"

# PlantNet API endpoint for identification. It may stop working or change 
# depending on updates to PlantNet If it stops working, check the website
# https://my-api.plantnet.org to update it.
API_URL <- cfg_init$plantnet_api_identify_url

# You can choose a more specific flora. 
# See: https://my.plantnet.org/doc/references/newfloras.
# /!\ /!\ /!\ /!\ /!\ 
# If you choose "auto", the script will use the "flora" column in the input CSV
# file to determine the flora for each image. If you choose "auto" user must run
# the script "0_2_add_plantnet_flora.R" first to add the flora information in 
# the output CSV file of the script "0_1_inat_data_creation.R".
# /!\ /!\ /!\ /!\ /!\ 
project_type <- cfg[["1_1_identify_plantnet_top5"]]$project_type

# The API key for the Pl@ntNet API. You can obtain a free API key by registering at
# https://my.plantnet.org. Here the API key is loaded from the config.yaml file 
# using the config package. Please copy your API key in the config.yaml file in 
# the root directory of the project and save it before running the script.
key <- cfg_init$plantnet_api_key

# The number of top predictions to retrieve from the Pl@ntNet API.
nb_results <- cfg[["1_1_identify_plantnet_top5"]]$nb_results

# The "no_reject" parameter determines whether to include rejected predictions in the results.
# Set to "true" to include rejected predictions, or "false" to exclude them.
# see https://my.plantnet.org/doc/getting-started/faq (chapter "rejection") for details
no_reject = cfg[["1_1_identify_plantnet_top5"]]$no_reject 


#############################################################################
# Packages, Functions                                                       #
#############################################################################

library(httr)
library(jsonlite)
source("utils/plantnet_identify_functions.r")

#############################################################################
# Path management                                                           #
#############################################################################

input_path <- file.path(input_dir,
                        input_file_name)

output_path <- file.path(output_dir,
                         paste0(tools::file_path_sans_ext(input_file_name),
                          "_pn.csv"))

if (!file.exists(input_path)) {
  stop(sprintf("Input file not found: %s", input_path))
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, recursive = TRUE)
}


#############################################################################
# Main                                                                      #
#############################################################################

# Read the input CSV file containing image URLs and SampleIDs
df <- read.csv(
  input_path,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA")
)

# Check the planned requests and daily identification quota
planned_requests <- nrow(df)
quota_info <- tryCatch(
  get_daily_identify_quota(key),
  error = function(e) {
    stop(sprintf("Could not retrieve PlantNet quota information: %s", conditionMessage(e)))
    NULL
  }
)

# If the quota information is available, compare the planned requests with the remaining quota
if (!is.null(quota_info)) {
  if (planned_requests > quota_info$remaining) {
    stop(sprintf(
      "\n========================================
      \nPlanned identification requests (%d)
      \nexceed remaining quota (%d).
      \n========================================",
      planned_requests,
      quota_info$remaining
    ))
  } else {
    message(sprintf(
      "\n========================================
      \nPlanned identification requests: %d. 
      \nRemaining identification requests available
      \nwith the provided API key: %d out of %d.
      \n========================================",
      planned_requests,
      quota_info$remaining,
      quota_info$total
    ))
  }
}

# If the 'flora' column is missing and the project type is set to 'auto', stop the script with an error message.
if (is.null(df$flora) && project_type == "auto") {
  stop("You are in 'auto' mode, but the 'flora' column is missing in the input CSV file. Please run the script '0_2_add_plantnet_flora.R' first to add the flora information.")
}

# set the list to store the results for each row in the input data frame
records <- list()
# initialize the index for the records list
idx <- 1L

# loop through each row of the input data frame
for (i in seq_len(nrow(df))) {
  #--------------------------------------------------------------------------
  image_url <- df$image_url[i]
  sample_id <- df$SampleID[i]
  message(sprintf("[%d/%d] Processing SampleID=%s", i, nrow(df), sample_id))
  if (project_type == "auto") {project <- df$flora[i] } else {project <- project_type}
  #--------------------------------------------------------------------------  
  # create a temporary file path for the downloaded image
  # the temporary file will be deleted after processing each row
  # see "if (file.exists(temp_img)) file.remove(temp_img)"
  temp_img <- paste0(temp_dir, "/temp_img.jpg")
  # download the image from the provided image URL
  download_response <- download_image(image_url, temp_img)
  #--------------------------------------------------------------------------
  # request the Pl@ntNet API to identify the plant in the image
  # with the specified API key, project, and number of results
  if (is.null(download_response$error)) {
    api_response <- tryCatch(
      identify_with_plantnet(
        API_URL = API_URL,
        key = key,
        project = project,
        nb_results = nb_results,
        image_url = image_url,
        temp_img = temp_img
      ),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )
    error <- api_response$error %||% NULL
  } else {
    error <- download_response$error
  }
  #--------------------------------------------------------------------------
  # If there was an error in the API response or image download, create a 
  # record with the error message
  if (!is.null(error)) {
    records[[idx]] <- data.frame(
      SampleID = as.character(sample_id),
      source_image_url = image_url,
      rank = NA_integer_,
      predicted_organ = NA_character_,
      predicted_organ_score = NA_real_,
      predicted_family = NA_character_,
      predicted_genus = NA_character_,
      predicted_scientific_name = NA_character_,
      predicted_scientific_name_without_author = NA_character_,
      confidence_score = NA_real_,
      error = as.character(error),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  #--------------------------------------------------------------------------
  # If the API response was successful, extract the predictions and create
  # records for each rank of prediction
  } else {
    predictions <- api_response$predicted_taxa %||% list()
    predicted_organ <- as.character(api_response$predicted_organs[[1]]$organ %||% NA_character_)
    predicted_organ_score <- as.numeric(api_response$predicted_organs[[1]]$score %||% NA_real_)
    for (rank_i in seq_len(nb_results)) {
      pred <- if (rank_i <= length(predictions)) predictions[[rank_i]] else NULL
      species <- pred$species %||% NULL
      records[[idx]] <- data.frame(
        SampleID = as.character(sample_id),
        source_image_url = image_url,
        rank = rank_i,
        predicted_organ = predicted_organ,
        predicted_organ_score = predicted_organ_score,
        predicted_family = as.character((species$family %||% NULL)$scientificNameWithoutAuthor %||% NA_character_),
        predicted_genus = as.character((species$genus %||% NULL)$scientificNameWithoutAuthor %||% NA_character_),
        predicted_scientific_name = as.character(species$scientificName %||% NA_character_),
        predicted_scientific_name_without_author = as.character(species$scientificNameWithoutAuthor %||% NA_character_),
        confidence_score = as.numeric(pred$score %||% NA_real_),        
        error = if (is.null(pred)) "missing rank_from_API" else NA_character_,
        stringsAsFactors = FALSE
      )
  idx <- idx + 1L
    }
  }
  #--------------------------------------------------------------------------
  # Light pacing to avoid hitting API limits too quickly.
  Sys.sleep(0.2)
}

# Combine the list of records into a single data frame
df_records <- do.call(rbind, records)

# Ensure that the SampleID columns are character type for proper merging
df$SampleID <- as.character(df$SampleID)
df_records$SampleID <- as.character(df_records$SampleID)

# Merge the two data frames by SampleID, keeping all rows from the base data frame
df_final <- merge(
  df,
  df_records,
  by = "SampleID",
  all.x = TRUE,
  sort = FALSE
)

# Sort the rows by SampleID  and rank
rank_order <- suppressWarnings(as.numeric(df_final$rank))
df_final <- df_final[order(match(df_final$SampleID, df$SampleID), rank_order), ]

# Select and reorder the columns for the final output
cols <- c("SampleID",
          "url",
          "image_url",
          "latitude",
          "longitude",
          "positional_accuracy",
          "observed_on",
          "year",
          "flora",
          "predicted_family",
          "predicted_genus",
          "predicted_scientific_name",
          "predicted_scientific_name_without_author",
          "rank",
          "confidence_score",
          "predicted_organ",
          "predicted_organ_score",
          "error")
df_final <- df_final[, intersect(cols, names(df_final))]

#save the results to a CSV file in the output directory
write.table(
  df_final,
  output_path,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  fileEncoding = "UTF-8"
)

# remove the temporary image file if it exists
if (file.exists(temp_img)) file.remove(temp_img)
# remove the temp directory and its contents
if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
# final message indicating that the results have been saved to the output path
message("========================================")
message(sprintf("Done. Results saved to: %s", output_path))
message("========================================")