#' Initialize API settings
#'
#' This function initializes the API configuration and settings.
#' @param token Your API token as a string.
#' @param version API version, default is "v0".
#' @return A list containing API settings.
#' @export
initialize_api <- function(token, version = "v0") {
  base_url <- sprintf("https://makedistribution.com/s/api/%s", version)
  list(base_url = base_url, token = token)
}

#' Make an HTTP GET request
#'
#' @param api_settings List containing base URL and token
#' @param endpoint API endpoint for the GET request
#' @return Parsed JSON response
#'
make_get_request <- function(api_settings, endpoint) {
  url <- paste0(api_settings$base_url, endpoint)
  response <- httr::GET(url, httr::add_headers(Authorization = paste0("Token ", api_settings$token)))
  if (httr::http_error(response)) {
    message("Error in GET request: ", httr::content(response, "text"))
    httr::stop_for_status(response)
  }
  httr::content(response, "parsed")
}

#' Make an HTTP POST request
#'
#' @param api_settings List containing base URL and token
#' @param endpoint API endpoint for the POST request
#' @param body A list containing the body of the POST request
#' @return Parsed JSON response
#'
make_post_request <- function(api_settings, endpoint, body) {
  url <- paste0(api_settings$base_url, endpoint)
  response <- httr::POST(url, httr::add_headers(Authorization = paste0("Token ", api_settings$token)), body = body, encode = "json")
  if (httr::http_error(response)) {
    message("Error in POST request: ", httr::content(response, "text"))
    httr::stop_for_status(response)
  }
  httr::content(response, "parsed")
}


#' Create and query a distribution
#'
#' @param api_settings List containing API settings
#' @param family A string representing the requested distribution family
#' @param arguments A list containing the arguments specific to the distribution
#' @param x Vector of points at which to evaluate the function
#' @param endpoint_suffix A string indicating whether to use the 'pdf' or 'cdf' endpoint
#' @return Vector of function values at points x
#' @export
query_distribution <- function(api_settings, family, arguments, x, endpoint_suffix) {
  # Format the body for the POST request
  body <- list(
    family = list(requested = family),
    arguments = arguments
  )
  
  # Create the distribution
  create_response <- make_post_request(api_settings, "/1d/dists/", body)
  dist_id <- create_response$id
  
  # Format the query parameter for x values
  x_query <- paste(x, collapse = ",")
  endpoint <- sprintf("/1d/dists/%s/%s/?x=%s", dist_id, endpoint_suffix, x_query)
  
  # Make the GET request to query the function
  response <- make_get_request(api_settings, endpoint)
  
  # Extract the values in the same order as x
  if (endpoint_suffix == "pdf") {
    values <- vapply(response, function(item) item$density, numeric(1))
  } else if (endpoint_suffix == "cdf") {
    values <- vapply(response, function(item) item$p, numeric(1))
  }
  
  values
}

#' Query the density of a distribution
#'
#' @param api_settings List containing API settings
#' @param family A string representing the requested distribution family
#' @param arguments A list containing the arguments specific to the distribution
#' @param x Vector of points at which to evaluate the density
#' @return Vector of density values
#' @export
dmakedist <- function(api_settings, family, arguments, x) {
  query_distribution(api_settings, family, arguments, x, "pdf")
}

#' Query the cumulative distribution function of a distribution
#'
#' @param api_settings List containing API settings
#' @param family A string representing the requested distribution family
#' @param arguments A list containing the arguments specific to the distribution
#' @param x Vector of points at which to evaluate the CDF
#' @return Vector of CDF values at points x
#' @export
pmakedist <- function(api_settings, family, arguments, x) {
  query_distribution(api_settings, family, arguments, x, "cdf")
}


