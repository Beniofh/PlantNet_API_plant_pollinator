# This function returns the first non-null value from a list of arguments.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# This function downloads an image from a given URL and saves it to a 
# temporary file. If the download fails, it stops the function with an
# error message for the next steps to handle the error.
download_image <- function(image_url, temp_img) {
  # Downloads the image from the provided URL and stops this
  # function with an error if the download fails  
  download_response <- tryCatch(
    {
      response <- httr::GET(
        image_url,
        httr::write_disk(temp_img, overwrite = TRUE),
        httr::timeout(60),
        httr::user_agent("R plantnet script")
      )
      status <- httr::status_code(response)
      if (status >= 300) {
        list(results = response,
             error = sprintf("fail_download_image_(HTTP_%s)", status))
      } else {
        list(results = response,
             error = NULL)
      }
    },
    error = function(e) {
      (list(results = NULL, error = "fail_launch_download_image"))
    }
  )
  return(download_response)
}

# This function sends a POST request to the
# Pl@ntNet API with the image file and returns the 
# parsed JSON response.
identify_with_plantnet <- function(API_URL,
                                   key,
                                   project,
                                   nb_results,
                                   image_url,
                                   temp_img){
  # create the basic endpoint URL with query parameters                                  
  endpoint <- sprintf("%s/%s", API_URL, project)
  # add query parameters to the endpoint URL
  # see https://my.plantnet.org/doc/api/identify
  endpoint <- httr::modify_url(endpoint, query = list(
    `api-key` = key,
    `nb-results` = as.character(nb_results),
    `no-reject` = no_reject,
    `include-related-images` = "false"
    )
  )
  # request to the Pl@ntNet API with the image file and query parameters
  api_response <- httr::POST(
    endpoint,
    body = list(images = httr::upload_file(temp_img)),
    encode = "multipart",
    httr::timeout(60),
    httr::user_agent("R plantnet script")
  )
  # check the HTTP status code of the api response and stop the function with 
  # an error if the request failed for the next steps to handle the error.
  status <- httr::status_code(api_response)
  # extract the content of the HTTP api_response (JSON text) 
  content_txt <- httr::content(api_response, as = "text", encoding = "UTF-8")
  if (status >= 300) {
    stop(sprintf("API_error_(HTTP_%s): %s",
                 status,
                 as.character(sub('.*"message":"([^"]+)".*', '\\1', content_txt))))
  }
  content_txt <- httr::content(api_response, as = "text", encoding = "UTF-8")
  # convert the JSON content to a list and return it
  parsed <- jsonlite::fromJSON(content_txt, simplifyVector = FALSE)
  # return a list containing the results and predicted organs
  list(
    # the list of predicted probable plant species, by order of confidence score decreasing
    predicted_taxa = parsed$results %||% list(),
    # gives the top1 of predicted organs for each input image
    predicted_organs = parsed$predictedOrgans %||% list()
  )
}

# This function checks the daily identify quota for the Pl@ntNet API and returns
get_daily_identify_quota <- function(key) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  quota_endpoint <- httr::modify_url(
    "https://my-api.plantnet.org/v2/quota/daily",
    query = list(
      day = today,
      `api-key` = key
    )
  )
  # send a GET request to the quota endpoint and handle any errors
  quota_response <- httr::GET(
    quota_endpoint,
    httr::timeout(30),
    httr::user_agent("R plantnet quota check")
  )
  # check the HTTP status code of the quota response and stop the function with 
  # an error if the request failed for the next steps to handle the error.
  status <- httr::status_code(quota_response)
  content_txt <- httr::content(quota_response, as = "text", encoding = "UTF-8")
  if (status >= 300) {
    stop(sprintf("Quota API error (HTTP_%s): %s", status, content_txt))
  }
  # convert the JSON content to a list and extract the identify quota information
  parsed <- jsonlite::fromJSON(content_txt, simplifyVector = TRUE)
  # extract the identify quota information from the parsed response
  identify_quota <- parsed$quota$identify
  if (is.null(identify_quota)) {
    stop("Quota API response does not contain identify quota information.")
  }
  # return a list containing the count, total, and remaining identify quota
  list(
    count = identify_quota$count %||% NA_integer_,
    total = identify_quota$total %||% NA_integer_,
    remaining = identify_quota$remaining %||% NA_integer_
  )
}
