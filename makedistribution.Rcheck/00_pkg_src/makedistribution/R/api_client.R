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

#' Validate the input arguments
#'
#' This function ensures that the correct set of arguments is provided.
#' @param slug The URL slug for an existing distribution.
#' @param family The distribution family.
#' @param arguments The distribution arguments.
#' @return TRUE if validation passes, otherwise stops with an error message.
validate_arguments <- function(slug, family, arguments) {
  if (!is.null(slug) && (!is.null(family) || !is.null(arguments))) {
    stop("Provide either a slug or both family and arguments, but not both.")
  }
  if (is.null(slug) && (is.null(family) || is.null(arguments))) {
    stop("Both family and arguments must be provided if slug is not.")
  }
  TRUE
}

#' Construct the endpoint for the distribution function query
#'
#' @param slug The URL slug for an existing distribution (optional).
#' @param family The distribution family (used if slug is not provided).
#' @param endpoint_suffix The type of distribution function (pdf, cdf, qf, samples).
#' @param value The values to pass in the query (size, x, or p).
#' @return A string representing the constructed endpoint.
construct_endpoint <- function(slug, family, endpoint_suffix, value) {
  if (!is.null(slug)) {
    sprintf("%s/%s/?%s=%s", slug, endpoint_suffix, if (endpoint_suffix == "qf") "p" else "x", paste(value, collapse = ","))
  } else {
    # Assumes a dist_id is needed for new distributions which should be managed by another function
    stop("Slug must be provided for endpoint construction, or use a function to create a distribution first.")
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
  query_existing_distribution(api_settings, sprintf("/1d/dists/%s", dist_id), value, endpoint_suffix)
}

#' Query an existing distribution
#'
#' @param api_settings List containing API settings
#' @param slug The URL slug for an existing distribution
#' @param value The query values
#' @param endpoint_suffix The distribution function type
#' @return Vector of function values
query_existing_distribution <- function(api_settings, slug, value, endpoint_suffix) {
  endpoint <- construct_endpoint(slug, NULL, endpoint_suffix, value)
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
#' @param api_settings List containing API settings.
#' @param family A string representing the requested distribution family (optional if slug is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if slug is provided).
#' @param slug A string representing the URL slug of an existing distribution (optional if family and arguments are provided).
#' @return Vector of density values.
#' @export
dmakedist <- function(x, api_settings, family = NULL, arguments = NULL, slug = NULL) {
  validate_arguments(slug, family, arguments)
  if (!is.null(slug)) {
    query_existing_distribution(api_settings, slug, x, "pdf")
  } else {
    create_and_query_distribution(api_settings, family, arguments, x, "pdf")
  }
}

#' Cumulative Distribution Function
#'
#' This function queries the cumulative distribution function of a specified distribution at given points.
#' @param x Vector of points at which to evaluate the CDF.
#' @param api_settings List containing API settings.
#' @param family A string representing the requested distribution family (optional if slug is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if slug is provided).
#' @param slug A string representing the URL slug of an existing distribution (optional if family and arguments are provided).
#' @return Vector of CDF values.
#' @export
pmakedist <- function(x, api_settings, family = NULL, arguments = NULL, slug = NULL) {
  validate_arguments(slug, family, arguments)
  if (!is.null(slug)) {
    query_existing_distribution(api_settings, slug, x, "cdf")
  } else {
    create_and_query_distribution(api_settings, family, arguments, x, "cdf")
  }
}

#' Quantile Function
#'
#' This function queries the quantile function of a specified distribution for given probabilities.
#' @param p Vector of probabilities at which to evaluate the quantile function.
#' @param api_settings List containing API settings.
#' @param family A string representing the requested distribution family (optional if slug is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if slug is provided).
#' @param slug A string representing the URL slug of an existing distribution (optional if family and arguments are provided).
#' @return Vector of quantile values.
#' @export
qmakedist <- function(p, api_settings, family = NULL, arguments = NULL, slug = NULL) {
  validate_arguments(slug, family, arguments)
  if (!is.null(slug)) {
    query_existing_distribution(api_settings, slug, p, "qf")
  } else {
    create_and_query_distribution(api_settings, family, arguments, p, "qf")
  }
}

#' Random Sampling
#'
#' This function queries random samples from a specified distribution.
#' @param size Integer specifying the number of samples to retrieve.
#' @param api_settings List containing API settings.
#' @param family A string representing the requested distribution family (optional if slug is provided).
#' @param arguments A list containing the arguments specific to the distribution (optional if slug is provided).
#' @param slug A string representing the URL slug of an existing distribution (optional if family and arguments are provided).
#' @return Vector of random samples.
#' @export
rmakedist <- function(size, api_settings, family = NULL, arguments = NULL, slug = NULL) {
  validate_arguments(slug, family, arguments)
  if (!is.null(slug)) {
    query_existing_distribution(api_settings, slug, size, "samples")
  } else {
    create_and_query_distribution(api_settings, family, arguments, size, "samples")
  }
}
