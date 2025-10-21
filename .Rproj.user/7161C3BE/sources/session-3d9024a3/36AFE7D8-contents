# global.R

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT)
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(purrr); library(readr); library(tibble); library(ggplot2)
  library(dbplyr);                    # lazy SQL via dplyr
  # BigQuery/DBI are loaded only in live mode below to keep deps light in local mode
})

# ---- Helpers to sanitize Shiny choices ----
# Convert named vectors -> named lists; list-of-lists -> flat labels/values
as_choices <- function(x) {
  if (is.null(x)) return(NULL)
  
  # If it's a data.frame (common mistake), pick the first column as values
  if (is.data.frame(x)) {
    vals <- x[[1]]
    nms  <- if (ncol(x) >= 2) x[[2]] else NULL
    if (!is.null(nms)) names(vals) <- as.character(nms)
    return(as_choices(vals))
  }
  
  # Atomic vector with names -> named LIST (what jsonlite/toJSON + Shiny like)
  if (is.atomic(x) && length(names(x))) {
    out <- as.list(as.character(x))
    names(out) <- names(x)
    return(out)
  }
  
  # Plain atomic vector (no names) -> leave as character vector
  if (is.atomic(x)) return(as.character(x))
  
  # List: if any element is itself a list or not length-1, coerce to flat labels
  if (is.list(x)) {
    # If it's a named list of atomic scalars, Shiny is fine with that.
    ok <- all(vapply(x, function(el) is.atomic(el) && length(el) == 1, logical(1)))
    if (ok) return(x)
    
    # Otherwise make a simple character vector of labels
    # e.g., c("Option 1","Option 2",...) with same names if present
    lbls <- vapply(x, function(el) {
      if (is.atomic(el) && length(el) == 1) as.character(el)
      else if (!is.null(attr(el, "label"))) as.character(attr(el, "label"))
      else if (!is.null(names(el)) && length(el) == 1) as.character(el[[1]])
      else "[item]"
    }, character(1))
    names(lbls) <- names(x)
    return(as.character(lbls))
  }
  
  # Fallback: coerce to character vector
  as.character(x)
}

# Convenience wrapper for all inputs that take choices=
safe_choices <- function(choices) as_choices(choices)


# ---------- Source R/*.R (modules, utils) ----------
# In global.R
source_dir <- function(path = "R") {
  fs <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in fs) sys.source(f, envir = globalenv())
}
source_dir("R")


# ---------- Config ----------
PROJECT_ID <- Sys.getenv("GCP_PROJECT_ID", "spheres-lombok-barat")
DATASET_ID <- Sys.getenv("GCP_DATASET_ID", "raw_data")
BQ_TABLE   <- "epus_laporan_pelayanan_pasien_new"

# Local-data mode is the default for shinyapps.io
USE_LOCAL_DATA  <- tolower(Sys.getenv("USE_LOCAL_DATA", "true")) %in% c("true","1","yes")
LOCAL_DATA_PATH <- Sys.getenv("LOCAL_DATA_PATH", "data/epus_subset.rds")

# ---------- Local data loader (RDS / CSV(.gz) / optional encrypted .enc) ----------
load_local_data <- function(path) {
  if (!file.exists(path)) stop("Local data file not found: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    readRDS(path)
  } else if (ext %in% c("csv","gz")) {
    if (!requireNamespace("vroom", quietly = TRUE)) stop("Package 'vroom' required for CSV; install it.")
    vroom::vroom(path, show_col_types = FALSE)
  } else if (ext == "enc") {
    # Optional encrypted bundle (set DATA_KEY in env if used)
    if (!requireNamespace("openssl", quietly = TRUE)) stop("Package 'openssl' required for .enc; install it.")
    key <- Sys.getenv("DATA_KEY", "")
    if (!nzchar(key)) stop("DATA_KEY env var is required to decrypt: ", path)
    sz  <- file.info(path)$size
    enc <- readBin(path, what = "raw", n = sz)
    raw <- openssl::aes_cbc_decrypt(enc, key = openssl::sha256(charToRaw(key)))
    unserialize(raw)
  } else {
    stop("Unsupported local data file extension: .", ext)
  }
}

# ---------- Select backend ----------
if (USE_LOCAL_DATA) {
  # ---- Local cached data mode (preferred for deployment) ----
  data_static <- load_local_data(LOCAL_DATA_PATH)
  # (PII already masked/hashed in the exporter; no raw identifiers here)
  con <- NULL  # signal "local mode" to utils_bq.R
  message("[init] Running in LOCAL mode with ", nrow(data_static), " rows from: ", LOCAL_DATA_PATH)
} else {
  # ---- Live BigQuery mode (for local dev) ----
  suppressPackageStartupMessages({ library(bigrquery); library(DBI) })
  
  # Auth strategy:
  # 1) If a service-account JSON is provided via env var, use it (non-interactive)
  # 2) Otherwise fall back to interactive OAuth (local dev)
  ga_json <- Sys.getenv("GCP_SERVICE_ACCOUNT_JSON", "")
  if (nzchar(ga_json)) {
    tf <- tempfile(fileext = ".json")
    writeLines(ga_json, tf)
    try(bigrquery::bq_auth(path = tf), silent = TRUE)
  } else {
    try(bigrquery::bq_auth(), silent = TRUE)
  }
  
  con <- DBI::dbConnect(
    bigrquery::bigquery(),
    project = PROJECT_ID,
    dataset = DATASET_ID,
    billing = PROJECT_ID
  )
  message("[init] Running in LIVE BigQuery mode: ", PROJECT_ID, ".", DATASET_ID, " / table: ", BQ_TABLE)
}

# ---------- Small helpers available globally ----------
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) {
    if (!is.null(a) && length(a) > 0 && !(is.character(a) && identical(a, ""))) a else b
  }
}

# Note:
# - core_tbl_for_pkm(...) in R/utils_bq.R should handle both modes:
#   * local mode: uses 'data_static'
#   * live mode : uses 'con' + BQ_TABLE (column-minimized select)
# - adm_map in R/utils_cleaning.R should prefer masked/hash cols for NIK/phone.
