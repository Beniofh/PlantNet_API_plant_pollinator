# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.2


# Description:
##############

# This script fetches the flora ID for each observation in a CSV file based on
# the latitude and longitude of the observation using the Pl@ntNet API. The flora
# ID is added as a new column in the output CSV file. The flora ID can be used
# in subsequent scripts to identify the plant species in the observations using
# the Pl@ntNet API (script 1_1_identify_plantnet_top5_v4.R) if the project_type 
# setting is set in "auto" in the script 1_1_identify_plantnet_top5_v4.R. If is 
# not set in "auto", you do not need to run this script.

# This can improve Pl@ntNet’s identification performance by using models specifically
# trained for these flora. However, this is only relevant if you know that there 
# are few or no "exotic" plants in the area under study. This is because these 
# exotic plants are not listed in the local flora and cannot be identified correctly.

# The flora ID is determined based on the geographic location of the observation
# and the available flora projects in the Pl@ntNet API. For each GPS coordinate 
# the script sends a GET request to the Pl@ntNet API endpoint v2_projects to 
# retrieve the list of flora projects available for that location (listed in order
# of geographical proximity). In this list, the first project devoted to flora whose
# name begins with "k-" is used. The floras beginning with "k-" are those based on
# Plants Of The World Online (POWO). If no specific flora project is found for the 
# location, a default "all" flora ID is assigned.

# inputs
##############

# The output CSV file of 0_1_inat_data_creation.r 

# outputs
##############

# file in the "outputs" directory with the name 
# "<input_file_name>.csv" with the same columns as the input CSV file plus
# a new column "flora" containing the flora ID for each observation.


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

# the input directory where the CSV file is located
input_dir <- "outputs/fetched_data"

# the output directory where the results will be saved
output_dir <- "outputs/fetched_data"

# PlantNet API endpoint for flora. It may stop working or change 
# depending on updates to PlantNet If it stops working, check the website
# https://my-api.plantnet.org to update it.
API_URL <- cfg_init$plantnet_api_project_url

# The API key for the Pl@ntNet API. You can obtain a free API key by registering at
# https://my.plantnet.org. Here the API key is loaded from the config.yaml file 
# using the config package. Please copy your API key in the config.yaml file in 
# the root directory of the project and save it before running the script.
key <- cfg_init$plantnet_api_key

#############################################################################
# Packages and Functions                                                    #
#############################################################################

library(httr)
library(jsonlite)
source("utils/plantnet_project_functions.r")


#############################################################################
# Path management                                                           #
#############################################################################

input_path <- file.path(input_dir,
                        input_file_name)
output_path <- file.path(output_dir,
                         input_file_name)

if (!file.exists(input_path)) {
  stop(sprintf("Input file not found: %s", input_path))
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


#############################################################################
# Main                                                                      #
#############################################################################

# Read the input CSV file containing image URLs, SampleIDs and other metadata
df <- read.csv(
  input_path,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA")
)

# Add a new column 'flora' to the dataframe to store the flora ID for each sample
df$flora <- NA_character_

# Loop through each row of the dataframe to get the flora ID based on 
# latitude and longitude
for (i in seq_len(nrow(df))) {
  lat <- df$latitude[i]
  lon <- df$longitude[i]
  sample_id <- df$SampleID[i]
  api_response <- GET_v2_projects(endpoint, key, lat, lon)
  status <- httr::status_code(api_response)
  if (status < 300) {  
    # extract the content of the HTTP response (JSON text) 
    content_txt <- httr::content(api_response, as = "text", encoding = "UTF-8")
    # convert the JSON content to a list and return it
    parsed <- jsonlite::fromJSON(content_txt)
    # extract the project IDs that start with "k-" in o
    k_flora_ids <- parsed$id[startsWith(parsed$id, "k-")]
    # if there are any k-flora IDs, use the first one; otherwise, default to "all"
    if (length(k_flora_ids) != 0) {
        flora_id <- k_flora_ids[1]
        } else {
            flora_id <- "all"
        }
  } else {
    flora_id <- sprintf("API_error_(HTTP_%s)",status)
  }
  df$flora[i] <- flora_id
  message(sprintf("[%d/%d] Processing SampleID=%s", i, nrow(df), sample_id))  
}

# Write the updated dataframe with flora IDs to a new CSV file
write.table(
  df,
  output_path,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  fileEncoding = "UTF-8"
)

# Print a message indicating that the process is complete and the results have been saved
message(sprintf("Done. Results saved to: %s", output_path))