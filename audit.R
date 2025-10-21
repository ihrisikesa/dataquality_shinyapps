d <- readRDS("data/epus_subset.rds")

# 1) Columns that must NOT exist anymore (raw PII)
stopifnot(!any(c("nik","no_telp","nama_pasien","alamat","tgllahir",
                 "dokter_tenaga_medis","perawat_bidan_nutrisionist_sanitarian") %in% names(d)))

# 2) Columns that SHOULD exist (safe fields)
must_have <- c("nik_masked","phone_masked","nik_hash","phone_hash",
               "nik_len","nik_len_ok","phone_len","phone_len_ok",
               "name_present","dokter_pseudo","doctor_present",
               "staff_pseudo","staff_present")
stopifnot(all(must_have %in% names(d)))

# 3) Mask format checks (••••1234 for NIK; •••123 for phone) — allow NA
nik_ok <- is.na(d$nik_masked) | grepl("^•+\\d{4}$", d$nik_masked)
phone_ok <- is.na(d$phone_masked) | grepl("^•+\\d{3}$", d$phone_masked)
table(nik_ok)
table(phone_ok)

# 4) Hash format checks (64 hex chars or NA)
hex64 <- "^[0-9a-f]{64}$"
nik_hash_ok   <- is.na(d$nik_hash)   | grepl(hex64, d$nik_hash)
phone_hash_ok <- is.na(d$phone_hash) | grepl(hex64, d$phone_hash)
table(nik_hash_ok)
table(phone_hash_ok)

# 5) Pseudonym format checks (D-xxxxxxxx, S-xxxxxxxx) — x is hex
pseudo8 <- "^[DS]-[0-9a-f]{8}$"
doc_pseudo_ok   <- is.na(d$dokter_pseudo) | grepl(pseudo8, d$dokter_pseudo)
staff_pseudo_ok <- is.na(d$staff_pseudo)  | grepl(pseudo8, d$staff_pseudo)
table(doc_pseudo_ok)
table(staff_pseudo_ok)

# 6) Sanity: no character columns contain long raw digit runs (≥10 digits)
char_cols <- names(d)[vapply(d, is.character, logical(1))]
leaks <- lapply(char_cols, function(col) which(grepl("\\d{10,}", d[[col]])))
names(leaks) <- char_cols
Filter(length, leaks)   # should return an empty list

d %>%
  dplyr::select(nik_masked, phone_masked, nik_hash, phone_hash,
                dokter_pseudo, staff_pseudo) %>%
  dplyr::filter(!is.na(nik_masked) | !is.na(phone_masked) |
                  !is.na(dokter_pseudo) | !is.na(staff_pseudo)) %>%
  head(20)

