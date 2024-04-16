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

# Print the initialized settings to show what they look like
print(api_settings)

