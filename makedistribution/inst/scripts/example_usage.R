# example_usage.R

# Load the package
library(makedistribution)

# Retrieve the API token from an environment variable
api_token <- Sys.getenv("API_TOKEN")

# Check if the token is available
if (api_token == "") {
  stop("API token is not set in environment variables.")
}

# Initialize API settings using the API token
api_settings <- initialize_api(api_token)

# Specify the distribution family
distribution_family <- "cinterp5_01"

# General arguments for the distribution
arguments <- list(
  quantiles = list(
    list(p = 0.1, x = 0),
    list(p = 0.5, x = 1),
    list(p = 0.9, x = 4)
  )
)

# Points at which to evaluate the density
x_values <- seq(-3, 3, by = 0.5)

# Get densities
densities <- dmakedist(api_settings, distribution_family, arguments, x_values)
print("densities:")
print(densities)

probabilities <- pmakedist(api_settings, distribution_family, arguments, x_values)
print("probabilities:")
print(probabilities)