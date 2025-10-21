# scripts/refresh_cache.R
# Usage (RStudio): source("scripts/refresh_cache.R")
# Usage (Terminal): Rscript scripts/refresh_cache.R 2025-09-01
#
# - Connects to BigQuery (interactive OAuth locally)
# - Pulls ONLY needed columns between START_DATE and today
# - Derives helper fields (day, PKM, PKM_norm, file_ts, tanggal_ts)
# - Computes NIK/phone validity flags, masked display, salted hashes
# - Adds presence + pseudonyms for doctor/staff (no raw names stored)
# - Adds name_present (presence-only for Nama Pasien)
# - Drops raw PII before saving
# - Appends incrementally and de-dups
# - Saves to data/epus_subset.rds

suppressPackageStartupMessages({
  library(bigrquery); library(DBI); library(dplyr)
  library(stringr);   library(lubridate)
})

# -------- params ------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
START_DATE <- as.Date(if (length(args) >= 1) args[1] else "2025-09-01")
TODAY      <- Sys.Date()

PROJECT_ID    <- "spheres-lombok-barat"
DATASET_ID    <- "raw_data"
TABLE         <- "epus_laporan_pelayanan_pasien_new"
existing_path <- "data/epus_subset.rds"

# Columns your app actually needs
WANT_COLS <- c(
  # base / id fields
  "puskesmas_name","puskesmas_id","tanggal","file_name",
  # clinical (clin_map)
  "berat_badan","tinggi","lingkar_perut","sistole","diastole","detak_nadi","nafas","suhu",
  # admin (will be processed & raw dropped)
  "nik","nama_pasien","no_telp","tgllahir","alamat",
  "dokter_tenaga_medis","diagnosa_1","perawat_bidan_nutrisionist_sanitarian","umur_tahun"
)

# -------- helpers -----------------------------------------------------------

# Safe SHA-256 (prefers 'digest', falls back to 'openssl')
hash_sha256 <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(s, algo = "sha256", serialize = FALSE))
  } else if (requireNamespace("openssl", quietly = TRUE)) {
    raw <- openssl::sha256(charToRaw(s))
    return(paste0(sprintf("%02x", as.integer(raw)), collapse = ""))
  } else {
    stop("Please install package 'digest' or 'openssl' for hashing.")
  }
}
vhash_sha256 <- function(vec, salt = "") {
  vapply(vec, function(z) {
    if (is.na(z) || !nzchar(z)) return(NA_character_)
    hash_sha256(paste0(salt, z))
  }, character(1))
}

digits_only <- function(x) gsub("[^0-9]", "", as.character(x))
mask_tail <- function(x, keep = 4, mask_char = "•") {
  ifelse(
    is.na(x) | x == "",
    NA_character_,
    paste0(
      strrep(mask_char, pmax(nchar(x) - keep, 0)),
      substr(x, pmax(nchar(x) - keep + 1, 1), nchar(x))
    )
  )
}

# Canonicalize person names (case/space/accents) before hashing
normalize_person <- function(x) {
  y <- tolower(as.character(x))
  y <- stringr::str_squish(y)
  if (requireNamespace("stringi", quietly = TRUE)) {
    y <- stringi::stri_trans_general(y, "Latin-ASCII")
  }
  y
}

parse_file_ts <- function(x) {
  # Extract "YYYY-mm-dd HH_MM_SS.mmm" from file_name and parse to POSIXct
  pat <- "\\d{4}-\\d{2}-\\d{2} \\d{2}_\\d{2}_\\d{2}\\.\\d{3}"
  hit <- stringr::str_match(as.character(x), pat)[, 1]
  hit <- stringr::str_replace(
    hit,
    "(\\d{4}-\\d{2}-\\d{2}) (\\d{2})_(\\d{2})_(\\d{2})\\.(\\d{3})",
    "\\1 \\2:\\3:\\4.\\5"
  )
  suppressWarnings(lubridate::ymd_hms(hit, tz = "Asia/Makassar"))
}
parse_tanggal_ts <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  if (inherits(x, "Date"))   return(as.POSIXct(x, tz = "Asia/Makassar"))
  x_chr <- as.character(x)
  a <- suppressWarnings(ymd_hms(x_chr, tz = "Asia/Makassar"))
  a[is.na(a)] <- suppressWarnings(ymd_hm(x_chr, tz = "Asia/Makassar"))[is.na(a)]
  a[is.na(a)] <- suppressWarnings(as.POSIXct(x_chr, tz = "Asia/Makassar"))
  a
}

# -------- incremental bump BEFORE querying ---------------------------------
old <- NULL
if (file.exists(existing_path)) {
  old <- readRDS(existing_path)
  if ("day" %in% names(old)) {
    max_day <- suppressWarnings(max(as.Date(old$day), na.rm = TRUE))
    if (is.finite(max_day) && max_day >= START_DATE) START_DATE <- max_day + 1
  }
}

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# If nothing new to fetch, ensure cache exists and exit
if (!is.null(old) && START_DATE > TODAY) {
  message("No new days to fetch (", format(START_DATE), " > ", format(TODAY), "). Keeping existing cache: ", existing_path)
  if (!file.exists(existing_path)) saveRDS(old, existing_path)
  quit(save = "no")
}

# -------- connect to BigQuery ----------------------------------------------
bigrquery::bq_auth()  # opens browser the first time locally
con <- dbConnect(
  bigrquery::bigquery(),
  project = PROJECT_ID,
  dataset = DATASET_ID,
  billing = PROJECT_ID
)

# -------- minimal, server-side filtered query -------------------------------
src     <- dplyr::tbl(con, TABLE)
db_cols <- colnames(src)
keep    <- intersect(WANT_COLS, db_cols)

day_sql <- "
  COALESCE(
    SAFE_CAST(tanggal AS DATE),
    SAFE_CAST(REGEXP_EXTRACT(CAST(tanggal AS STRING), r'^(\\d{4}-\\d{2}-\\d{2})') AS DATE),
    DATE(SAFE_CAST(tanggal AS DATETIME)),
    DATE(SAFE_CAST(tanggal AS TIMESTAMP), 'Asia/Makassar')
  )"

df <- src %>%
  mutate(
    day      = dbplyr::sql(day_sql),
    PKM      = dbplyr::sql("TRIM(puskesmas_name)"),
    PKM_norm = dbplyr::sql("REGEXP_REPLACE(LOWER(TRIM(puskesmas_name)), r'[^a-z0-9]', '')")
  ) %>%
  filter(day >= !!START_DATE, day <= !!TODAY) %>%
  select(all_of(unique(c(keep, "day", "PKM", "PKM_norm")))) %>%
  collect()

message("Fetched ", format(nrow(df), big.mark = ","), " rows from BQ for ", format(START_DATE), " → ", format(TODAY))

# -------- derive timestamps locally ----------------------------------------
df <- df %>%
  mutate(
    file_ts    = parse_file_ts(file_name),
    tanggal_ts = parse_tanggal_ts(tanggal)
  )

# Guard: ensure PII columns exist even if absent in source
for (nm in c("nik", "no_telp", "nama_pasien", "alamat", "tgllahir",
             "dokter_tenaga_medis","perawat_bidan_nutrisionist_sanitarian")) {
  if (!nm %in% names(df)) df[[nm]] <- NA_character_
}

# -------- PII-safe validation & anonymization -------------------------------
SALT_NIK    <- Sys.getenv("NIK_SALT",     "change-me-local-salt")
SALT_TEL    <- Sys.getenv("PHONE_SALT",   "change-me-local-salt")
SALT_DOC    <- Sys.getenv("DOCTOR_SALT",  "change-me-local-salt")
SALT_STAFF  <- Sys.getenv("STAFF_SALT",   "change-me-local-salt")

df <- df %>%
  # normalize raw values for id/phone
  mutate(
    nik_clean   = dplyr::na_if(digits_only(nik), ""),
    telp_clean  = dplyr::na_if(digits_only(no_telp), "")
  ) %>%
  # normalize person names for hashing (so spacing/case/accents don't create dupes)
  mutate(
    doc_clean   = dplyr::na_if(normalize_person(dokter_tenaga_medis), ""),
    staff_clean = dplyr::na_if(normalize_person(perawat_bidan_nutrisionist_sanitarian), "")
  ) %>%
  # compute lengths & validity flags (NIK/phone)
  mutate(
    nik_len       = nchar(nik_clean),
    nik_len_ok    = !is.na(nik_len)   & (nik_len == 16L),
    phone_len     = nchar(telp_clean),
    phone_len_ok  = !is.na(phone_len) & dplyr::between(phone_len, 10L, 12L)
  ) %>%
  # stable, non-reversible keys for joins/grouping (salted)
  mutate(
    nik_hash      = vhash_sha256(nik_clean,   SALT_NIK),
    phone_hash    = vhash_sha256(telp_clean,  SALT_TEL),
    doctor_hash   = vhash_sha256(doc_clean,   SALT_DOC),
    staff_hash    = vhash_sha256(staff_clean, SALT_STAFF)
  ) %>%
  # masked/pseudonym display (never raw text)
  mutate(
    nik_masked     = if_else(!is.na(nik_clean),   mask_tail(nik_clean,  keep = 4), NA_character_),
    phone_masked   = if_else(!is.na(telp_clean),  mask_tail(telp_clean, keep = 3), NA_character_),
    doctor_present = !is.na(doc_clean),
    staff_present  = !is.na(staff_clean),
    dokter_pseudo  = if_else(doctor_present, paste0("D-", substr(doctor_hash, 1, 8)), NA_character_),
    staff_pseudo   = if_else(staff_present,  paste0("S-", substr(staff_hash,  1, 8)), NA_character_)
  ) %>%
  # presence-only flag for Nama Pasien (so we can measure completeness)
  mutate(
    name_present = !is.na(nama_pasien) & nzchar(trimws(as.character(nama_pasien)))
  ) %>%
  # DROP raw PII before saving (keep only safe fields)
  select(
    puskesmas_name, puskesmas_id, tanggal, file_name,
    berat_badan, tinggi, lingkar_perut, sistole, diastole, detak_nadi, nafas, suhu,
    diagnosa_1, umur_tahun,
    day, PKM, PKM_norm, file_ts, tanggal_ts,
    # NIK/phone derived
    nik_len, nik_len_ok, phone_len, phone_len_ok,
    nik_masked, phone_masked, nik_hash, phone_hash,
    # Safe patient-name presence
    name_present,
    # SAFE doctor/staff fields (pseudonyms + presence)
    dokter_pseudo, doctor_present,
    staff_pseudo,  staff_present
  )

# -------- append & de-dup AFTER PII step -----------------------------------
if (!is.null(old)) {
  df <- dplyr::bind_rows(old, df) %>% dplyr::distinct()
}

# -------- save --------------------------------------------------------------
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
saveRDS(df, existing_path)
message(
  "Saved ", format(nrow(df), big.mark = ","), " rows to ", existing_path,
  " (", format(START_DATE), " → ", format(TODAY), ")"
)

