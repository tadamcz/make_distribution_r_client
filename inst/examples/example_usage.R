# Load the package
library(makedistribution)

# Either set the environment variable `MAKEDISTRIBUTION_API_TOKEN`
# or customize your api settings with `initialize_api` function, e.g.
# `api_settings <- initialize_api(token = "your_api_token", version = "v42")`
# Then include the `api_settings` object in subsequent function calls.

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
densities <- dmakedist(x_values, distribution_family, arguments)
cat("densities:\n")
print(densities)

# Get probabilities
probabilities <- pmakedist(x_values, distribution_family, arguments)
cat("probabilities:\n")
print(probabilities)

# Get quantiles
p_values <- seq(0.1, 0.9, by = 0.2)  # Points at which to evaluate the quantile function
quantiles <- qmakedist(p_values, distribution_family, arguments)
cat("quantiles:\n")
print(quantiles)

# Generate random variates
sample_size <- 16
samples <- rmakedist(sample_size, distribution_family, arguments)
cat("samples:\n")
print(samples)

# Or, query an existing distribution object
# by passing `path` instead of `family` and `arguments
distribution_path <- "/1d/dists/odist_2UjledFsyZHE608XAeKYkw"
densities <- dmakedist(x_values, path = distribution_path)
cat("densities:\n")
print(densities)

# Get basic information about the distribution
# e.g. fit status
distribution_info <- get_distribution(distribution_path)
cat("distribution_info:\n")
print(distribution_info)