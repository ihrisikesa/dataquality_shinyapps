# R/utils_bq.R
# Dual-mode helpers for local cached data and live BigQuery.

# --- tiny helpers -----------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

# Canonicalize a string for matching: lowercase, strip accents, keep [a-z0-9]
if (!exists("normalize", mode = "function")) {
  normalize <- function(x) {
    x <- tolower(as.character(x))
    if (requireNamespace("stringi", quietly = TRUE)) {
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    }
    gsub("[^a-z0-9]", "", x)
  }
}

# --- schema helpers ----------------------------------------------------------
# Flatten mapping values to a character vector of candidate source names
.map_candidates <- function(map_obj) {
  if (is.null(map_obj)) return(character(0))
  unname(unlist(map_obj, use.names = FALSE))
}

# Minimal column selector for live BigQuery mode
.minimal_source_tbl <- function(con, table) {
  # base + common admin/clinical columns your app may use
  base_wants <- c(
    "puskesmas_name","puskesmas_id","tanggal","file_name","posyandu",
    "berat_badan","tinggi","lingkar_perut","sistole","diastole","detak_nadi","nafas","suhu",
    "nik","nama_pasien","no_telp","tgllahir","alamat",
    "dokter_tenaga_medis","diagnosa_1","perawat_bidan_nutrisionist_sanitarian","umur_tahun"
  )
  
  # Also pull anything referenced by adm_map / clin_map if present
  cm <- get0("clin_map", ifnotfound = NULL, inherits = TRUE)
  am <- get0("adm_map",  ifnotfound = NULL, inherits = TRUE)
  map_wants <- unique(c(.map_candidates(cm), .map_candidates(am)))
  
  db_tbl  <- dplyr::tbl(con, table)
  db_cols <- colnames(db_tbl)
  
  keep <- intersect(unique(c(base_wants, map_wants)), db_cols)
  db_tbl |> dplyr::select(dplyr::all_of(keep))
}

# Find the best column name in a data frame that holds the Puskesmas name
.find_pkm_col <- function(df) {
  candidates <- c("PKM","puskesmas_name","puskesmas","nama_puskesmas","faskes","faskes_name")
  nms  <- names(df); nms_l <- tolower(nms)
  cand <- tolower(candidates)
  pos  <- which(nms_l %in% cand)
  if (length(pos)) nms[pos[1]] else NULL
}

# --- API: choices & core table ----------------------------------------------

# Get the list of Puskesmas (works in local & live modes)
get_puskesmas_choices <- function(con) {
  tryCatch({
    if (is.null(con)) {
      # LOCAL MODE: read from the cached frame loaded in global.R
      df <- get("data_static", envir = .GlobalEnv)
      col <- .find_pkm_col(df)
      if (is.null(col)) return(character(0))
      x <- df[[col]]
      x <- if (is.factor(x)) as.character(x) else as.character(x)
      x <- trimws(x); x <- x[!is.na(x) & nzchar(x)]
      sort(unique(x))
    } else {
      # LIVE BQ MODE
      table <- get0("BQ_TABLE", ifnotfound = "epus_laporan_pelayanan_pasien_new", inherits = TRUE)
      dplyr::tbl(con, table) |>
        dplyr::distinct(puskesmas_name) |>
        dplyr::filter(!is.na(puskesmas_name), puskesmas_name != "") |>
        dplyr::arrange(puskesmas_name) |>
        dplyr::collect() |>
        dplyr::pull(puskesmas_name) |>
        unique()
    }
  }, error = function(e) {
    message("[get_puskesmas_choices] ", conditionMessage(e))
    character(0)
  })
}

# Core table for a selected PKM (works in local & live modes)
core_tbl_for_pkm <- function(con, pkm_input) {
  PKM_target <- normalize(pkm_input %||% "")
  
  # ---------- LOCAL MODE ----------
  if (is.null(con)) {
    df <- tryCatch(get("data_static", envir = .GlobalEnv), error = function(e) tibble::tibble())
    if (!nrow(df)) return(tibble::tibble())
    
    pkm_col <- .find_pkm_col(df)
    if (is.null(pkm_col)) return(tibble::tibble())
    
    # Local parsers (used only if not already present)
    parse_file_ts_local <- function(x) {
      pat <- "\\d{4}-\\d{2}-\\d{2} \\d{2}_\\d{2}_\\d{2}\\.\\d{3}"
      hit <- stringr::str_match(as.character(x), pat)[,1]
      hit <- stringr::str_replace(hit,
                                  "(\\d{4}-\\d{2}-\\d{2}) (\\d{2})_(\\d{2})_(\\d{2})\\.(\\d{3})",
                                  "\\1 \\2:\\3:\\4.\\5"
      )
      suppressWarnings(lubridate::ymd_hms(hit, tz = "Asia/Makassar"))
    }
    parse_tanggal_ts_local <- function(x) {
      if (inherits(x, "POSIXt")) return(x)
      if (inherits(x, "Date"))   return(as.POSIXct(x, tz="Asia/Makassar"))
      x_chr <- as.character(x)
      a <- suppressWarnings(lubridate::ymd_hms(x_chr, tz="Asia/Makassar"))
      a[is.na(a)] <- suppressWarnings(lubridate::ymd_hm(x_chr, tz="Asia/Makassar"))[is.na(a)]
      a[is.na(a)] <- suppressWarnings(as.POSIXct(x_chr, tz="Asia/Makassar"))
      a
    }
    
    out <- df |>
      dplyr::mutate(
        PKM      = trimws(.data[[pkm_col]]),
        PKM_norm = gsub("[^a-z0-9]", "", tolower(.data$PKM))
      )
    
    if (!"tanggal_ts" %in% names(out)) out <- dplyr::mutate(out, tanggal_ts = parse_tanggal_ts_local(.data$tanggal))
    if (!"day" %in% names(out))        out <- dplyr::mutate(out, day = as.Date(.data$tanggal_ts)) else out <- dplyr::mutate(out, day = as.Date(.data$day))
    if (!"file_ts" %in% names(out))    out <- dplyr::mutate(out, file_ts = parse_file_ts_local(.data$file_name))
    
    return(
      out |>
        dplyr::mutate(file_ts_fallback = dplyr::coalesce(.data$file_ts, .data$tanggal_ts)) |>
        dplyr::filter(.data$PKM_norm == PKM_target)
    )
  }
  
  # ---------- LIVE BIGQUERY MODE ----------
  table <- get0("BQ_TABLE", ifnotfound = "epus_laporan_pelayanan_pasien_new", inherits = TRUE)
  
  .minimal_source_tbl(con, table) |>
    dplyr::mutate(
      PKM      = dbplyr::sql("TRIM(puskesmas_name)"),
      PKM_norm = dbplyr::sql("REGEXP_REPLACE(LOWER(TRIM(puskesmas_name)), r'[^a-z0-9]', '')"),
      file_ts = dbplyr::sql("
        PARSE_TIMESTAMP(
          '%Y-%m-%d %H:%M:%E*S',
          REGEXP_REPLACE(
            REGEXP_EXTRACT(file_name, r'\\d{4}-\\d{2}-\\d{2} \\d{2}_\\d{2}_\\d{2}\\.\\d{3}'),
            r'(\\d{4}-\\d{2}-\\d{2}) (\\d{2})_(\\d{2})_(\\d{2})\\.(\\d{3})',
            r'\\1 \\2:\\3:\\4.\\5'
          ),
          'Asia/Makassar'
        )
      "),
      tanggal_ts = dbplyr::sql("
        COALESCE(
          PARSE_TIMESTAMP('%Y-%m-%d %H:%M:%S', CAST(tanggal AS STRING), 'Asia/Makassar'),
          PARSE_TIMESTAMP('%Y-%m-%d %H:%M',    CAST(tanggal AS STRING), 'Asia/Makassar'),
          SAFE_CAST(tanggal AS TIMESTAMP)
        )
      "),
      day = dbplyr::sql("
        COALESCE(
          SAFE_CAST(tanggal AS DATE),
          SAFE_CAST(REGEXP_EXTRACT(CAST(tanggal AS STRING), r'^(\\d{4}-\\d{2}-\\d{2})') AS DATE),
          DATE(SAFE_CAST(tanggal AS DATETIME)),
          DATE(SAFE_CAST(tanggal AS TIMESTAMP), 'Asia/Makassar')
        )
      "),
      file_ts_fallback = dbplyr::sql("COALESCE(file_ts, tanggal_ts)")
    ) |>
    dplyr::filter(dbplyr::sql(paste0("PKM_norm = '", PKM_target, "'")))
}

# --- date-window reducer ----------------------------------------------------

# Pick the latest file per day within a date window (works for local frames & dbplyr)
latest_file_per_day_between <- function(tbl, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  tbl <- tbl %>%
    dplyr::filter(.data$day >= !!start_date, .data$day <= !!end_date) %>%
    # ensure we have something to sort by
    dplyr::mutate(file_ts_fallback = dplyr::coalesce(.data$file_ts_fallback, .data$file_ts, .data$tanggal_ts))
  
  # If file_name exists, keep the most recent record per (day, file_name) first
  if ("file_name" %in% colnames(tbl)) {
    tbl <- tbl %>%
      dplyr::group_by(.data$day, .data$file_name) %>%
      dplyr::summarise(file_ts_fallback = max(.data$file_ts_fallback, na.rm = TRUE), .groups = "drop")
  } else {
    tbl <- tbl %>%
      dplyr::group_by(.data$day) %>%
      dplyr::summarise(file_ts_fallback = max(.data$file_ts_fallback, na.rm = TRUE), .groups = "drop")
  }
  
  tbl %>%
    dplyr::group_by(.data$day) %>%
    dplyr::slice_max(order_by = .data$file_ts_fallback, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
}
