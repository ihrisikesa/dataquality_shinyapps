digits_only <- function(x) stringr::str_replace_all(x, "[^0-9]", "")
normalize_phone <- function(x) {
  y <- as.character(x)
  y <- stringr::str_replace_all(y, "\\s|[()\\-]", "")
  y <- stringr::str_replace(y, "^\\+62", "0")
  digits_only(y)
}
parse_date_safely <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  y <- suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c("Ymd","ymd","dmy","dmY","mdy","Y-m-d","d-m-Y","m/d/Y",
               "Ymd HMS","ymd HMS","dmy HMS","mdy HMS")
  ))
  as.Date(y)
}

default_valid_rules <- tibble::tribble(
  ~Variable,        ~min_ok, ~max_ok,
  "Berat Badan",        1,     300,
  "Tinggi",            30,     250,
  "Sistole",           50,     260,
  "Diastole",          30,     150,
  "Detak Nadi",        20,     220,
  "Nafas",              5,      80,
  "Suhu",              30,      43,
  "Lingkar Perut",     10,     200
)

valid_rules_from_upload <- function(file_input) {
  if (is.null(file_input)) return(default_valid_rules)
  tryCatch(
    readr::read_csv(file_input$datapath, show_col_types = FALSE) |>
      dplyr::mutate(Variable = as.character(Variable),
                    min_ok = as.numeric(min_ok),
                    max_ok = as.numeric(max_ok)),
    error = function(e) default_valid_rules
  )
}

vars_to_check <- c("Berat Badan","Tinggi","Sistole","Diastole","Detak Nadi","Nafas","Suhu","Lingkar Perut")
vars_to_check_adm <- c("NIK","Nama Pasien","No Telp","Tgl.Lahir","Alamat",
                       "Dokter / Tenaga Medis","Diagnosa 1",
                       "Perawat / Bidan / Nutrisionist / Sanitarian")

infer_type <- function(var) {
  v <- tolower(var)
  dplyr::case_when(
    v == "nik" ~ "nik",
    stringr::str_detect(v, "telp|telepon|hp|phone") ~ "phone",
    stringr::str_detect(v, "tgl\\.?lahir|tanggal\\s*lahir|dob|birth") ~ "date",
    var %in% vars_to_check ~ "numeric",
    TRUE ~ "text"
  )
}

fmt_pct  <- function(x) sprintf("%.1f%%", 100*x)

rename_for_print <- function(df, lab){
  nm <- names(df); names(df) <- ifelse(nm %in% names(lab), unname(lab[nm]), nm); df
}
col_label <- c(
  `No.`="No.", Variable="Variabel", Rows="Baris", Missing="Kosong", `Missing %`="Kosong %",
  Non_missing="Terisi", `Non_missing %`="Terisi %", `Distinct (non-missing)`="Unik (terisi)",
  `Invalid (non-missing)`="Tidak valid (terisi)", Rule="Aturan validasi",
  Min="Min", Q1="Q1", Median="Median", Q3="Q3", Max="Max"
)

summarize_var <- function(df, var, role, rules, ref_date) {
  if (!var %in% names(df)) return(NULL)
  
  tp <- infer_type(var)
  raw <- df[[var]]
  raw_chr  <- if (is.factor(raw)) as.character(raw) else as.character(raw)
  raw_trim <- trimws(raw_chr)
  
  # default missing based on displayed column;
  # for NIK/phone we override below to use precomputed length flags
  is_missing <- is.na(raw_trim) | raw_trim == ""
  
  rows <- length(raw)
  non_missing <- rows - sum(is_missing)
  distinct_nonmissing <- dplyr::n_distinct(raw_trim[!is_missing], na.rm = TRUE)
  
  issue_n <- 0L
  issue_label <- "Invalid (non-missing)"
  
  q_min <- q_q1 <- q_med <- q_q3 <- q_max <- NA_real_
  
  if (tp == "nik") {
    # Prefer privacy-safe flags
    if (all(c("nik_len", "nik_len_ok") %in% names(df))) {
      is_missing <- is.na(df$nik_len) | df$nik_len == 0L
      issue_n    <- sum(!is_missing & !df$nik_len_ok, na.rm = TRUE)
      issue_label <- "Invalid length (≠16)"
      # distinct by hash to avoid masked collisions
      if ("nik_hash" %in% names(df)) {
        distinct_nonmissing <- dplyr::n_distinct(df$nik_hash[!is_missing & !is.na(df$nik_hash)])
      }
    } else {
      # fallback (dev/local only when raw present)
      nik <- ifelse(is_missing, NA_character_, digits_only(raw_trim))
      ok  <- !is_missing & stringr::str_detect(nik, "^\\d{16}$")
      issue_n <- sum(!is_missing & !ok)
      issue_label <- "Invalid length (≠16)"
    }
    
  } else if (tp == "phone") {
    if (all(c("phone_len", "phone_len_ok") %in% names(df))) {
      is_missing <- is.na(df$phone_len) | df$phone_len == 0L
      issue_n    <- sum(!is_missing & !df$phone_len_ok, na.rm = TRUE)
      issue_label <- "Invalid length (10–12)"
      # distinct by hash
      if ("phone_hash" %in% names(df)) {
        distinct_nonmissing <- dplyr::n_distinct(df$phone_hash[!is_missing & !is.na(df$phone_hash)])
      }
    } else {
      ph  <- ifelse(is_missing, NA_character_, normalize_phone(raw_trim))
      len <- nchar(ph)
      ok  <- !is.na(ph) & dplyr::between(len, 10, 12)
      issue_n <- sum(!is_missing & !ok)
      issue_label <- "Invalid length (10–12)"
    }
    
  } else if (tp == "date") {
    d  <- parse_date_safely(raw)
    ok <- !is.na(d) & d >= as.Date("1900-01-01") & d <= ref_date
    issue_n <- sum(!is_missing & !ok)
    issue_label <- "Invalid date"
    
  } else if (tp == "numeric") {
    val <- ifelse(is_missing, NA_real_, readr::parse_number(raw_trim))
    vr  <- rules %>% dplyr::filter(Variable == var)
    bad <- if (nrow(vr)) (is.na(val) | val < vr$min_ok | val > vr$max_ok) else is.na(val)
    issue_n <- sum(!is_missing & bad)
    issue_label <- "Non-numeric / out-of-range"
    
    x_ok <- val[is.finite(val)]
    if (length(x_ok)) {
      qs <- stats::quantile(x_ok, probs = c(0, .25, .5, .75, 1), na.rm = TRUE, names = FALSE)
      q_min <- qs[1]; q_q1 <- qs[2]; q_med <- qs[3]; q_q3 <- qs[4]; q_max <- qs[5]
    }
  }
  
  tibble::tibble(
    Role = role, Variable = var,
    Rows = rows,
    Missing = sum(is_missing),
    `Missing %` = fmt_pct(Missing / pmax(Rows, 1)),
    Non_missing = rows - Missing,
    `Non_missing %` = fmt_pct(Non_missing / pmax(Rows, 1)),
    `Distinct (non-missing)` = distinct_nonmissing,
    `Invalid (non-missing)` = issue_n,
    Rule = issue_label,
    Min = round(q_min, 2), Q1 = round(q_q1, 2),
    Median = round(q_med, 2), Q3 = round(q_q3, 2), Max = round(q_max, 2)
  )
}

