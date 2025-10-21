#install.packages(c("bigrquery","gargle"))
library(bigrquery); library(gargle)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "ida.hrisikesa@sid-indonesia.org"  # your Google account
)

# Point gargle at the Desktop client JSON you downloaded
client <- gargle::gargle_oauth_client_from_json("client_desktop.json")

# (Optional) extra logging if something goes wrong:
# gargle::with_gargle_verbosity("debug", {
#   token <- gargle::credentials_user_oauth2(client = client,
#              scopes = "https://www.googleapis.com/auth/bigquery.readonly",
#              cache = TRUE, email = "ida.hrisikesa@sid-indonesia.org")
# })

token <- gargle::credentials_user_oauth2(
  client = client,
  scopes = "https://www.googleapis.com/auth/bigquery.readonly",
  cache  = TRUE,
  email  = "ida.hrisikesa@sid-indonesia.org"
)

# Register it with bigrquery and write into the cache
bigrquery::bq_auth(token = token, cache = TRUE)

# Confirm the token file exists
list.files(".secrets")
