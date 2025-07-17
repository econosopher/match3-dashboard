# deploy.R - A safe deployment script

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rsconnect, here)

# Load environment variables from .Renviron (if not already loaded)
# This is mainly for interactive use; shinyapps.io doesn't use this file.
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Retrieve credentials from environment variables
account_name <- Sys.getenv("SHINY_ACC_NAME")
account_token <- Sys.getenv("SHINY_TOKEN")
account_secret <- Sys.getenv("SHINY_SECRET")

# Check if the environment variables are set
if (account_name == "" || account_token == "" || account_secret == "") {
  stop("ERROR: Shiny credentials are not set in the .Renviron file. ",
       "Please ensure SHINY_ACC_NAME, SHINY_TOKEN, and SHINY_SECRET are set.")
}

# Set account info programmatically
rsconnect::setAccountInfo(
  name = account_name,
  token = account_token,
  secret = account_secret
)

# Deploy the application, using the here() function for a robust path
rsconnect::deployApp(
  appDir = here::here(),
  appName = 'match3-dashboard',
  account = account_name,
  server = 'shinyapps.io'
) 