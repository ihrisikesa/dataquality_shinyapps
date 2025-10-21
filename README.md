<<<<<<< HEAD
# Laporan Monitoring Kualitas Data — Shiny (modular)

## Run locally
```r
install.packages(c("shiny","bslib","DT","bigrquery","DBI","dbplyr","dplyr","stringr","lubridate","tidyr","purrr","readr","ggplot2","tibble"))
shiny::runApp("laporan-shiny-split")
```

## Deploy
- Prefer service account: set `GOOGLE_APPLICATION_CREDENTIALS` on server.
- Optional: set `GCP_PROJECT_ID` and `GCP_DATASET_ID` env vars.
- App uses a single BigQuery connection in `global.R`.
=======
# dataquality_shinyapps
dataquality_shinyapps
>>>>>>> d465ac3ce572e37c112a1ff196d13559ea39ce6a
