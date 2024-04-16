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
