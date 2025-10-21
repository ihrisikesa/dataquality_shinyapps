# R/utils_cleaning.R
# Cleaning + schema helpers used across modules

fmt_date <- function(x) if (is.na(x)) "—" else format(as.Date(x), "%Y-%m-%d")
fmt_pct  <- function(x) sprintf("%.1f%%", 100*x)

# Robust numeric parser used by keep_in_range()
safe_num <- function(x) {
  s <- as.character(x)
  s <- ifelse(stringr::str_detect(s, ",") & !stringr::str_detect(s, "\\."),
              stringr::str_replace_all(s, ",", "."),
              s)
  readr::parse_number(s)
}

# Clip out-of-range OR non-numeric values to NA and create out_* flags
keep_in_range <- function(df_in, rules) {
  if (is.null(df_in) || !nrow(df_in) || is.null(rules)) return(df_in)
  vars <- intersect(rules$Variable, names(df_in))
  patched <- df_in
  for (v in vars) {
    rule <- rules[rules$Variable == v, , drop = FALSE]
    x <- safe_num(patched[[v]])
    out <- !is.na(x) & (x < rule$min_ok[1] | x > rule$max_ok[1])
    patched[[v]] <- ifelse(out, NA_real_, x)
    patched[[paste0("out_", v)]] <- out
  }
  patched
}

# ---------- schema maps ----------
clin_map <- list(
  "Berat Badan"="berat_badan","Tinggi"="tinggi","Lingkar Perut"="lingkar_perut",
  "Sistole"="sistole","Diastole"="diastole","Detak Nadi"="detak_nadi","Nafas"="nafas","Suhu"="suhu"
)

# Prefer masked / presence-only admin fields (no raw PII)
adm_map <- list(
  "NIK"         = c("nik_masked", "nik_hash", "nik_len"),
  "Nama Pasien" = c("name_present"),
  "No Telp"     = c("phone_masked", "phone_hash", "phone_len"),
  "Dokter / Tenaga Medis" = c("dokter_pseudo", "doctor_present"),
  "Perawat / Bidan / Nutrisionist / Sanitarian" = c("staff_pseudo", "staff_present"),
  "Diagnosa 1"  = "diagnosa_1",
  "Umur Tahun"  = "umur_tahun",
  "Tgl.Lahir"   = character(0),
  "Alamat"      = character(0)
)


# ---------- aliasing helpers ----------
norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

resolve_columns <- function(df_names, friendly_map) {
  df_norm <- setNames(norm_name(df_names), df_names)
  out <- list()
  for (friendly in names(friendly_map)) {
    cands <- unlist(friendly_map[[friendly]])
    # direct hit
    hit <- cands[cands %in% df_names]
    if (length(hit)) { out[[friendly]] <- hit[[1]]; next }
    # normalized hit (handles small spelling differences / case)
    cand_norm <- norm_name(cands)
    match_idx <- match(cand_norm, df_norm); match_idx <- match_idx[!is.na(match_idx)]
    if (length(match_idx)) out[[friendly]] <- names(df_norm)[match_idx[[1]]]
  }
  unlist(out)
}

add_alias_cols <- function(df, res_map) {
  for (friendly in names(res_map)) {
    real <- res_map[[friendly]]
    if (length(real) == 0) next
    real <- real[real %in% names(df)]
    if (!length(real)) next
    if (!(friendly %in% names(df))) df[[friendly]] <- df[[real[1]]]
  }
  df
}

# ---------- main schema applier ----------
apply_schema <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  
  # Add a printable 'Tanggal' if we already have day
  if ("day" %in% names(df) && !"Tanggal" %in% names(df)) df$Tanggal <- as.Date(df$day)
  
  resolved_clin <- resolve_columns(names(df), clin_map)
  resolved_adm  <- resolve_columns(names(df),  adm_map)
  
  df <- add_alias_cols(df, resolved_clin)
  df <- add_alias_cols(df, resolved_adm)
  
  # Coerce typical numeric variables
  numeric_vars_all <- c("Tinggi","Umur Tahun","Berat Badan","Lingkar Perut","Sistole","Diastole","Nafas","Detak Nadi","Suhu")
  have_num <- intersect(numeric_vars_all, names(df))
  if (length(have_num)) {
    df <- dplyr::mutate(
      df,
      dplyr::across(
        dplyr::all_of(have_num),
        ~ suppressWarnings(as.numeric(chartr(",", ".", gsub("[^0-9,.-]", "", as.character(.)))))
      )
    )
  }
  
  # Remember which source columns were used
  attr(df, "resolved_clin") <- resolved_clin
  attr(df, "resolved_adm")  <- resolved_adm
  df
}
