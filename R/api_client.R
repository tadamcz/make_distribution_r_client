#' Initialize API settings
#'
#' This function initializes the API configuration and settings. If a token is provided, 
#' it will be included in all API requests for authorization.
#' @param token Your API token as a string (optional).
#' @param version API version, default is "v0".
#' @return A list containing API settings.
#' @export
#' 
initialize_api <- function(token = NULL, version = "v0") {
  # If token is null, try to read it from the default environment variable
  if (is.null(token)) {
    token <- Sys.getenv("MAKEDISTRIBUTION_API_TOKEN")
    if (token == "") {
      logger::log_debug("Token not provided, and MAKEDISTRIBUTION_API_TOKEN environment variable is not set. API requests will be made without authorization.")
    }
  }
  # Coerce empty strings returned by Sys.getenv to NULL
  if (token == "") {
    token <- NULL
  }
  base_url <- sprintf("https://makedistribution.com/s/api/%s", version)
  list(base_url = base_url, token = token)
}

#' Make an HTTP GET request
#'
#' This function performs an HTTP GET request to a specified endpoint. If a token is provided in the API settings,
#' it will include an Authorization header.
#' @param api_settings List containing base URL and token (optional)
#' @param endpoint API endpoint for the GET request
#' @return Parsed JSON response
make_get_request <- function(api_settings = NULL, endpoint) {
  if (is.null(api_settings)) {
    api_settings <- initialize_api()
  }
  url <- paste0(api_settings$base_url, endpoint)
  headers <- if (!is.null(api_settings$token)) httr::add_headers(Authorization = paste0("Token ", api_settings$token)) else list()
  logger::log_debug("GET request to ", url, " with headers: ", headers)
  response <- httr::GET(url, headers)
  if (httr::http_error(response)) {
    message("Error in GET request: ", httr::content(response, "text"))
    httr::stop_for_status(response)
  }
  httr::content(response, "parsed")
}

#' Make an HTTP POST request
#'
#' This function performs an HTTP POST request to a specified endpoint with the given body. 
#' If a token is provided in the API settings, it will include an Authorization header.
#' @param api_settings List containing base URL and token
#' @param endpoint API endpoint for the POST request
#' @param body A list containing the body of the POST request
#' @return Parsed JSON response
make_post_request <- function(api_settings, endpoint, body) {
  if (is.null(api_settings)) {
    api_settings <- initialize_api()
  }
  url <- paste0(api_settings$base_url, endpoint)
  headers <- if (!is.null(api_settings$token)) httr::add_headers(Authorization = paste0("Token ", api_settings$token)) else list()
  logger::log_debug("POST request to ", url, " with headers: ", headers, " and body: ", body)
  response <- httr::POST(url, headers, body = body, encode = "json")
  if (httr::http_error(response)) {
    message("Error in POST request: ", httr::content(response, "text"))
    httr::stop_for_status(response)
  }
  httr::content(response, "parsed")
}


#' Validate the input arguments
#'
#' This function ensures that the correct set of arguments is provided.
#' @param path The URL path for an existing distribution.
#' @param family The distribution family.
#' @param arguments The distribution arguments.
#' @return TRUE if validation passes, otherwise stops with an error message.
validate_arguments <- function(path, family, arguments) {
  if (!is.null(path) && (!is.null(family) || !is.null(arguments))) {
    stop("Provide either a path or both family and arguments, but not both.")
  }
  if (is.null(path) && (is.null(family) || is.null(arguments))) {
    stop("Both family and arguments must be provided if path is not.")
  }
  TRUE
}

#' Get Distribution Details
#'
#' This function retrieves the details of a distribution specified by its path.
#' @param path The URL path for the distribution to retrieve.
#' @param api_settings List containing API settings (optional).
#' @return Parsed JSON response containing the distribution details.
#' @export
get_distribution <- function(path, api_settings = NULL) {

  # Ensure exactly one leading and trailing slash
  path <- gsub("^/|/$", "", path)
  path <- paste0("/", path, "/")
  make_get_request(api_settings, path)
}

#' Construct the endpoint for the distribution function query
#'
#' @param path The URL path for an existing distribution.
#' @param endpoint_suffix The type of distribution function (pdf, cdf, qf, samples).
#' @param value The values to pass in the query (size for samples, x for pdf/cdf, p for qf).
#' @return A string representing the constructed endpoint.
construct_endpoint <- function(path, endpoint_suffix, value) {
  # Determine the query parameter based on the endpoint suffix
  query_param <- switch(endpoint_suffix,
                        "pdf" = "x",
                        "cdf" = "x",
                        "qf" = "p",
                        "samples" = "size",
                        stop("Invalid endpoint_suffix provided"))
  
  # Strip leading and trailing slashes from the path
  path <- gsub("^/|/$", "", path)
  
  # Construct the endpoint URL with the appropriate query string
  if (endpoint_suffix == "samples") {
    # For samples, value represents the size directly, no need to collapse
    sprintf("/%s/%s/?%s=%d", path, endpoint_suffix, query_param, value)
  } else {
    # For other types, value is a vector that needs to be collapsed into a comma-separated string
    sprintf("/%s/%s/?%s=%s", path, endpoint_suffix, query_param, paste(value, collapse = ","))
  }
}


#' Create a new distribution and query it
#'
#' @param api_settings List containing API settings
#' @param family The distribution family
#' @param arguments The distribution arguments
#' @param value The query values
#' @param endpoint_suffix The distribution function type
#' @return Vector of function values
create_and_query_distribution <- function(api_settings, family, arguments, value, endpoint_suffix) {
  body <- list(family = list(requested = family), arguments = arguments)
  create_response <- make_post_request(api_settings, "/1d/dists/", body)
  dist_id <- create_response$id
  query_distribution(api_settings, sprintf("/1d/dists/%s", dist_id), value, endpoint_suffix)
}

#' Query an existing distribution
#'
#' @param api_settings List containing API settings
#' @param path The URL path for an existing distribution
#' @param value The query values
#' @param endpoint_suffix The distribution function type
#' @return Vector of function values
query_distribution <- function(api_settings, path, value, endpoint_suffix) {
  endpoint <- construct_endpoint(path, endpoint_suffix, value)
  response <- make_get_request(api_settings, endpoint)
  # Extract the values based on the endpoint type
  if (endpoint_suffix == "pdf") {
    vapply(response, function(item) item$density, numeric(1))
  } else if (endpoint_suffix == "cdf") {
    vapply(response, function(item) item$p, numeric(1))
  } else if (endpoint_suffix == "qf") {
    vapply(response, function(item) item$x, numeric(1))
  } else if (endpoint_suffix == "samples") {
    unlist(response$samples)
  }
}

#' Density of a Distribution
#'
#' This function queries the density of a specified distribution at given points.
#' @param x Vector of points at which to evaluate the density.
#' @param family A string representing the requested distribution family (optional if path is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if path is provided).
#' @param path A string representing the URL path of an existing distribution (optional if family and arguments are provided).
#' @param api_settings List containing API settings (optional).
#' @return Vector of density values.
#' @export
dmakedist <- function(x, family = NULL, arguments = NULL, path = NULL, api_settings = NULL) {
  validate_arguments(path, family, arguments)
  if (!is.null(path)) {
    query_distribution(api_settings, path, x, "pdf")
  } else {
    create_and_query_distribution(api_settings, family, arguments, x, "pdf")
  }
}

#' Cumulative Distribution Function
#'
#' This function queries the cumulative distribution function of a specified distribution at given points.
#' @param x Vector of points at which to evaluate the CDF.
#' @param family A string representing the requested distribution family (optional if path is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if path is provided).
#' @param path A string representing the URL path of an existing distribution (optional if family and arguments are provided).
#' @param api_settings List containing API settings (optional).
#' @return Vector of CDF values.
#' @export
pmakedist <- function(x, family = NULL, arguments = NULL, path = NULL, api_settings = NULL) {
  validate_arguments(path, family, arguments)
  if (!is.null(path)) {
    query_distribution(api_settings, path, x, "cdf")
  } else {
    create_and_query_distribution(api_settings, family, arguments, x, "cdf")
  }
}

#' Quantile Function
#'
#' This function queries the quantile function of a specified distribution for given probabilities.
#' @param p Vector of probabilities at which to evaluate the quantile function.
#' @param family A string representing the requested distribution family (optional if path is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if path is provided).
#' @param path A string representing the URL path of an existing distribution (optional if family and arguments are provided).
#' @param api_settings List containing API settings.
#' @return Vector of quantile values.
#' @export
qmakedist <- function(p, family = NULL, arguments = NULL, path = NULL, api_settings = NULL) {
  validate_arguments(path, family, arguments)
  if (!is.null(path)) {
    query_distribution(api_settings, path, p, "qf")
  } else {
    create_and_query_distribution(api_settings, family, arguments, p, "qf")
  }
}

#' Random Sampling
#'
#' This function queries random samples from a specified distribution.
#' @param size Integer specifying the number of samples to retrieve.
#' @param family A string representing the requested distribution family (optional if path is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if path is provided).
#' @param path A string representing the URL path of an existing distribution (optional if family and arguments are provided).
#' @param api_settings List containing API settings.
#' @return Vector of random samples.
#' @export
rmakedist <- function(size, family = NULL, arguments = NULL, path = NULL, api_settings = NULL) {
  validate_arguments(path, family, arguments)
  if (!is.null(path)) {
    query_distribution(api_settings, path, size, "samples")
  } else {
    create_and_query_distribution(api_settings, family, arguments, size, "samples")
  }
}
