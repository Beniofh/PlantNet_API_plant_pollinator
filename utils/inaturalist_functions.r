# Pakages for functions
library(httr)

# Function to safely perform GET requests with retries and exponential backoff.
safe_get_with_retry <- function(url, query, max_retries = 6, timeout_sec = 60) {
  for (attempt in seq_len(max_retries)) {
    resp <- GET(url = url, query = query, timeout(timeout_sec))
    code <- status_code(resp)
    if (code >= 200 && code < 300) {
      return(resp)
    }
    if (code == 429 || code >= 500) {
      retry_after <- headers(resp)[["retry-after"]]
      wait_sec <- if (!is.null(retry_after)) as.numeric(retry_after) else (2 ^ (attempt - 1))
      if (is.na(wait_sec)) wait_sec <- 2 ^ (attempt - 1)
      Sys.sleep(wait_sec)
      next
    }
    stop(sprintf("Request failed with status %s: %s", code, content(resp, as = "text", encoding = "UTF-8")))
  }
  stop("Request failed after max retries.")
}

# Function to extract the first image URL from an observation.
extract_first_image_url <- function(obs) {
  op <- obs$observation_photos
  if (is.null(op) || length(op) == 0) return(NA_character_)
  first_photo <- op[[1]]$photo
  if (is.null(first_photo)) return(NA_character_)
  if (!is.null(first_photo$original_url) && nzchar(first_photo$original_url)) {
    return(first_photo$original_url)
  }
  if (!is.null(first_photo$url) && nzchar(first_photo$url)) {
    # Convert default thumbnail-like URL to original when possible.
    return(gsub("/square\\.", "/original.", first_photo$url))
  }
  NA_character_
}

# Function to extract coordinates from an observation.
extract_coordinates <- function(obs) {
  lon <- NA_real_
  lat <- NA_real_
  if (!is.null(obs$geojson) && !is.null(obs$geojson$coordinates) && length(obs$geojson$coordinates) >= 2) {
    lon <- suppressWarnings(as.numeric(obs$geojson$coordinates[[1]]))
    lat <- suppressWarnings(as.numeric(obs$geojson$coordinates[[2]]))
  } else if (!is.null(obs$location) && nzchar(obs$location)) {
    parts <- strsplit(obs$location, ",")[[1]]
    if (length(parts) == 2) {
      lat <- suppressWarnings(as.numeric(trimws(parts[[1]])))
      lon <- suppressWarnings(as.numeric(trimws(parts[[2]])))
    }
  }
  list(latitude = lat, longitude = lon)
}

# Function to convert an observation to a data frame row.
observation_to_row <- function(obs, input_taxon_name) {
  coords <- extract_coordinates(obs)
  data.frame(
    scientific_name = input_taxon_name,
    SampleID = obs$id,
    url = paste0("https://www.inaturalist.org/observations/", obs$id),
    image_url = ifelse(is.na(extract_first_image_url(obs)), NA_character_, extract_first_image_url(obs)),
    quality_grade = ifelse(is.null(obs$quality_grade), NA_character_, obs$quality_grade),
    observed_on = ifelse(is.null(obs$observed_on), NA_character_, obs$observed_on),
    year = ifelse(is.null(obs$observed_on), NA_integer_, as.integer(substr(obs$observed_on, 1, 4))),
    latitude = ifelse(is.na(coords$latitude), NA_real_, coords$latitude),
    longitude = ifelse(is.na(coords$longitude), NA_real_, coords$longitude),
    positional_accuracy = ifelse(is.null(obs$positional_accuracy), NA_integer_, obs$positional_accuracy),
    place_guess = ifelse(is.null(obs$place_guess), NA_character_, obs$place_guess),
    user_login = ifelse(is.null(obs$user$login), NA_character_, obs$user$login),
    stringsAsFactors = FALSE
  )
}