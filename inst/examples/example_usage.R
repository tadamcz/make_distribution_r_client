# Load the package
library(makedistribution)

# Retrieve the API token from an environment variable
api_token <- Sys.getenv("MAKEDISTRIBUTION_API_TOKEN")

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

# Points at which to evaluate the PDF or CDF
x_values <- seq(-3, 3, by = 0.5)

# Get densities
densities <- dmakedist(x_values, api_settings, distribution_family, arguments)
print("densities:")
print(densities)

# Get probabilities
probabilities <- pmakedist(x_values, api_settings, distribution_family, arguments)
print("probabilities:")
print(probabilities)

# Get quantiles
p_values <- seq(0.1, 0.9, by = 0.2)  # Points at which to evaluate the quantile function
quantiles <- qmakedist(p_values, api_settings, distribution_family, arguments)
print("x-values:")
print(quantiles)

# Generate random variates
sample_size <- 16
samples <- rmakedist(sample_size, api_settings, distribution_family, arguments)
print("samples:")
print(samples)

# Or, query an existing distribution object
# by passing `slug` instead of `family` and `arguments
distribution_slug <- "/1d/dists/odist_2UjledFsyZHE608XAeKYkw"
densities <- dmakedist(x_values, api_settings, slug = distribution_slug)
print("densities:")
print(densities)

# Get basic information about the distribution
# e.g. fit status
distribution_info <- get_distribution(api_settings, distribution_slug)
print("distribution_info:")
print(distribution_info)