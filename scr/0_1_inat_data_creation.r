# Author:       Benjamin Bourel
# Created:      2026-07-17
# Modified:     2026-07-24
# Version:      1.2

# Description:
##############
# This script fetches observations of a species with research quality 
# grade from the iNaturalist API and saves them to a CSV file. It is the first
# image to be fetched for each observation if several images are available.
# It is possible to filter by year, to filter geographic bounding box and 
# to change the quality grade (see Settings below).

# inputs
##############
# only the following parameters are required to be set in the "Settings" section below.

# outputs
##############
# file in the "inputs" directory with the name "<base_file_name>.csv" 
# containing the following columns.
# -> scientific_name
# -> SampleID (iNaturalist observation ID)
# -> url (link to the observation on iNaturalist)
# -> image_url (link to the first image of the observation)
# -> quality_grade
# -> observed_on
# -> year
# -> latitude
# -> longitude
# -> positional_accuracy
# -> place_guess
# -> user_login


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

# The output directory where the CSV file will be saved.
output_dir <- "outputs/fetched_data"

# iNaturalist API endpoint for observations. It may stop working or change 
# depending on updates to iNaturalist. If it stops working, check the website
# https://api.inaturalist.org/ to update it.
inat_url <- cfg_init$inat_obs_url

# Species name to fetch observations for. It must be the scientific name of the species.
input_taxon_name <- cfg[["0_1_inat_data_creation"]]$input_taxon_name

# The base name for the output CSV file.
base_file_name <- cfg[["global"]]$base_file_name

# Quality grade filter. Options: "research", "needs_id", "casual".
quality_grade <- cfg[["0_1_inat_data_creation"]]$quality_grade

# Optional year filter.
# Set to FALSE to fetch all years, or TRUE and define year_min/year_max.
use_year_filter <- cfg[["0_1_inat_data_creation"]]$use_year_filter
year_min <- cfg[["0_1_inat_data_creation"]]$year_min
year_max <- cfg[["0_1_inat_data_creation"]]$year_max

# Optional geographic bounding box filter.
# Set to TRUE and provide min/max lat/lon to restrict observations.
use_geo_bbox <- cfg[["0_1_inat_data_creation"]]$use_geo_bbox
swlat <- cfg[["0_1_inat_data_creation"]]$swlat
swlng <- cfg[["0_1_inat_data_creation"]]$swlng
nelat <- cfg[["0_1_inat_data_creation"]]$nelat
nelng <- cfg[["0_1_inat_data_creation"]]$nelng

# Settings for pagination and retries
# number of observations per page (max 200)
per_page <- cfg[["0_1_inat_data_creation"]]$per_page
# maximum number of pages to fetch (max 100)
max_pages <- cfg[["0_1_inat_data_creation"]]$max_pages
# number of retries for failed requests
max_retries <- cfg[["0_1_inat_data_creation"]]$max_retries
# base delay in seconds between requests 
# (exponential backoff is applied on retries)
base_delay_sec <- cfg[["0_1_inat_data_creation"]]$base_delay_sec

#############################################################################
# Packages and Functions                                                    #
#############################################################################

library(jsonlite)
bind_rows <- dplyr::bind_rows
source("utils/inaturalist_functions.r")


#############################################################################
# Path management                                                           #
#############################################################################

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file_name <- sprintf("%s.csv", base_file_name)
output_path <- file.path(output_dir, output_file_name)

#############################################################################
# Main                                                                      #
#############################################################################

# Initialize variables for storing results and tracking progress
all_rows <- list()
total_announced <- FALSE
retrieved_count <- 0L

# Loop through pages of observations from the iNaturalist API
if (use_geo_bbox) {
  bbox_vals <- c(swlat, swlng, nelat, nelng)
  if (any(is.na(bbox_vals))) {
    stop("Geo bbox is enabled but one or more bbox coordinates are NA.")
  }
  if (swlat >= nelat || swlng >= nelng) {
    stop("Invalid bbox: require swlat < nelat and swlng < nelng.")
  }
}

# Check year filter validity if enabled
if (use_year_filter) {
  if (is.na(year_min) || is.na(year_max)) {
    stop("Year filter is enabled but year_min or year_max is NA.")
  }
  if (year_min > year_max) {
    stop("Invalid year filter: require year_min <= year_max.")
  }
}

# Loop through pages of observations from the iNaturalist API
for (page in seq_len(max_pages)) {
  query <- list(
    taxon_name = input_taxon_name,
    quality_grade = quality_grade,
    photos = "true",
    order = "desc",
    order_by = "observed_on",
    per_page = per_page,
    page = page
  )
  # Add optional geo bbox filters to the query if enabled
  if (use_geo_bbox) {
    query$swlat <- swlat
    query$swlng <- swlng
    query$nelat <- nelat
    query$nelng <- nelng
  }
  # Add optional year filter to the query if enabled
  if (use_year_filter) {
    query$d1 <- sprintf("%d-01-01", year_min)
    query$d2 <- sprintf("%d-12-31", year_max)
  }
  # Perform the GET request with retries
  resp <- safe_get_with_retry(
    url = inat_url,
    query = query,
    max_retries = max_retries
  )
  # Parse the JSON response
  payload <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
  # Announce total observations to retrieve if not already done
  if (!total_announced) {
    total_obs <- payload$total_results
    if (is.null(total_obs) || is.na(total_obs)) {
      message("========================================")
      message("Total observations to retrieve: unknown")
      message("========================================")
    } else {
      message("========================================")
      message(sprintf("Total observations to retrieve: %d", as.integer(total_obs)))
      message("========================================")
    }
    total_announced <- TRUE
  }
  # Extract the results from the payload
  results <- payload$results
  # If no results are returned, break the loop
  if (is.null(results) || length(results) == 0) {
    break
  }
  # Convert each observation to a data frame row and accumulate the results
  page_rows <- lapply(results, observation_to_row, input_taxon_name = input_taxon_name)
  all_rows <- c(all_rows, page_rows)
  retrieved_count <- retrieved_count + length(results)
  # Print progress message for the current page
  message(sprintf("Fetched page %d: %d observations\nobservations retrieved: %d/%d", page, length(results), retrieved_count, total_obs))
  # If the number of results is less than per_page, it indicates the last page, so break the loop
  if (length(results) < per_page) {
    break
  }
  # Sleep for a short duration to avoid hitting API rate limits
  Sys.sleep(base_delay_sec)
}

# If no observations were found, stop with an error message
if (length(all_rows) == 0) {
  stop("No observations found for the given filters.")
}

# Combine all rows into a single data frame and write to CSV
out_df <- bind_rows(all_rows)

# Write the output data frame to a CSV file
write.table(
  out_df,
  file = output_path,
  sep = ";",
  row.names = FALSE,
  quote = TRUE,
  na = ""
)

# Print completion message with output file path and number of rows exported
message("========================================")
message(sprintf("Done. File written: %s", output_path))
message(sprintf("Rows exported: %d", nrow(out_df)))
message("========================================")
