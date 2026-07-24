# This function sends a GET request to the Pl@ntNet API endpoint v2_projects
# with the provided parameters
GET_v2_projects <- function(endpoint, key, lat, lon) {
  # send a GET request to the Pl@ntNet API endpoint with the provided parameters
  # and return the parsed JSON response as a list
  # create the basic endpoint URL with query parameters                                  
    endpoint <- sprintf(API_URL)
    # add query parameters to the endpoint URL
    # see https://my.plantnet.org/doc/api/identify
    endpoint <- httr::modify_url(endpoint, query = list(
        `api-key` = key,
        `lat` = as.character(lat),
        `lon` = as.character(lon),
        `type` = "kt"
        )
    )
  # send the GET request to the Pl@ntNet API endpoint and handle any errors
  api_response <- httr::GET(endpoint,
                       httr::timeout(60),
                       httr::user_agent("R plantnet script")
                      )
  return(api_response)
}