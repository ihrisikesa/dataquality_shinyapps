# R/mod_quality_summary.R

mod_quality_summary_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Quality Summary",
    
    # --- Heaping Step-6 table (pivot with Average row) ---
    h5("Heaping — Proporsi Total Preferred (Integer + Decimal)"),
    div(
      class = "mb-2",
      fluidRow(
        column(6, checkboxInput(ns("heap_latest_only"), "Show latest date only", FALSE)),
        column(6, downloadButton(ns("heap_dl_csv"), "Download heaping table (CSV)",
                                 class = "btn btn-sm btn-secondary pull-right"))
      )
    ),
    DT::DTOutput(ns("heap_tbl")),
    tags$hr(),
    
    # --- Existing summaries ---
    h5("Admin — Kelengkapan & Validitas"),
    DT::DTOutput(ns("adm")),
    tags$hr(),
    h5("Clinical — Kelengkapan, Validitas, & Ringkasan Numerik"),
    DT::DTOutput(ns("clin"))
  )
}

mod_quality_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- helpers ----------
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    resolve0 <- function(x) {                    # call if reactive/func, else return as-is
      if (is.null(x)) return(NULL)
      if (is.function(x)) tryCatch(x(), error = function(e) NULL) else x
    }
    get2 <- function(x, nm, default = NULL) {    # safe getter from list
      if (is.null(x)) return(default)
      if (is.list(x) && !is.null(x[[nm]])) return(x[[nm]])
      default
    }
    is_integerish <- function(x, tol = 1e-8) is.finite(x) & abs(x - round(x)) < tol
    parse_num <- function(x) {
      s <- as.character(x)
      s <- gsub("[^0-9,.-]", "", s)
      s <- gsub(",", ".", s, fixed = TRUE)
      suppressWarnings(as.numeric(s))
    }
    
    # Try to guess a date column name, return name or NULL
    guess_date_col <- function(df) {
      if (!is.data.frame(df)) return(NULL)
      nms <- tolower(names(df))
      candidates <- c(
        "tanggal","date","tgl","day","visit_date","tanggal_pengukuran","tanggal_kunjungan"
      )
      hit <- candidates[candidates %in% nms]
      if (length(hit)) {
        # return original-cased name
        names(df)[match(hit[1], nms)]
      } else NULL
    }
    
    # ---------- Admin / Clinical summary tables ----------
    build_tables <- reactive({
      x <- data$ACTIVE(); req(x)
      
      frames <- resolve0(data$frames)  # may be a function -> resolve
      resolved_date <- get2(frames, "resolved_date", Sys.Date())
      rules <- resolve0(data$rules)
      ref_date <- as.Date(isolate(resolved_date %||% Sys.Date()))
      
      adm_present  <- intersect(vars_to_check_adm, names(x))
      clin_present <- intersect(vars_to_check,     names(x))
      
      adm_tbl  <- purrr::map_dfr(adm_present,  ~ summarize_var(x, .x, "Admin",    rules, ref_date)) |>
        dplyr::select(-Role, -Min, -Q1, -Median, -Q3, -Max) |>
        dplyr::arrange(Variable) |>
        tibble::rowid_to_column(var = "No.")
      clin_tbl <- purrr::map_dfr(clin_present, ~ summarize_var(x, .x, "Clinical", rules, ref_date)) |>
        dplyr::select(-Role) |>
        dplyr::arrange(Variable) |>
        tibble::rowid_to_column(var = "No.")
      
      list(
        adm  = rename_for_print(adm_tbl,  col_label),
        clin = rename_for_print(clin_tbl, col_label)
      )
    })
    
    output$adm <- DT::renderDT({
      t <- build_tables()
      DT::datatable(t$adm, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })
    output$clin <- DT::renderDT({
      t <- build_tables()
      DT::datatable(t$clin, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })
    
    # ---------- Heaping Step-6 (auto-discover monthly/period frame) ----------
    # Prefer: data$data_month() -> frames$month / $Monthly / $history ... -> first df in frames with a date column -> global data_month
    data_month_r <- reactive({
      # 1) dedicated monthly reactive?
      val <- resolve0(data$data_month)
      if (is.data.frame(val)) return(val)
      
      # 2) look inside frames() for a likely candidate
      frames <- resolve0(data$frames)
      if (is.list(frames)) {
        # try common keys first
        keys <- c("month","Month","monthly","Monthly","history","History","window","range",
                  "df_month","dfMonthly","data_month","dataMonthly")
        for (k in keys) {
          obj <- get2(frames, k)
          if (is.data.frame(obj)) return(obj)
        }
        # fallback: scan any data.frame with a date-ish column
        df_list <- Filter(is.data.frame, frames)
        if (length(df_list)) {
          for (df in df_list) {
            if (!is.null(guess_date_col(df))) return(df)
          }
          return(df_list[[1]])  # absolute fallback: first data.frame
        }
      }
      
      # 3) global fallback
      resolve0(get0("data_month", ifnotfound = NULL))
    })
    
    vars_to_check_r <- reactive({
      val <- resolve0(data$vars_to_check)
      if (is.null(val)) {
        val <- get0("vars_to_check", ifnotfound =
                      c("Berat Badan","Tinggi","Lingkar Perut","Sistole","Diastole","Detak Nadi","Nafas","Suhu"))
      }
      val
    })
    
    heap_pivot_r <- reactive({
      df0 <- data_month_r()
      vtc <- vars_to_check_r()
      
      validate(need(is.data.frame(df0) && nrow(df0) > 0, "No monthly/period data available in frames()."))
      validate(need(is.atomic(vtc) && length(vtc) > 0, "No clinical variables to check."))
      
      # Ensure we have a column named Tanggal (rename if we can detect one)
      date_col <- if ("Tanggal" %in% names(df0)) "Tanggal" else guess_date_col(df0)
      validate(need(!is.null(date_col), "Could not find a date column (e.g., Tanggal/Date) in the selected frame."))
      
      value_prop_threshold <- 0.15
      exclude_vars <- c("Nafas", "Suhu")
      
      # normalize date + numerics
      df_base <- df0 |>
        dplyr::mutate(
          Tanggal = dplyr::case_when(
            inherits(.data[[date_col]], "Date")   ~ .data[[date_col]],
            inherits(.data[[date_col]], "POSIXt") ~ as.Date(.data[[date_col]]),
            TRUE                                  ~ suppressWarnings(as.Date(.data[[date_col]]))
          )
        ) |>
        dplyr::filter(!is.na(Tanggal))
      
      clin_vars <- setdiff(intersect(vtc, names(df_base)), exclude_vars)
      validate(need(length(clin_vars) > 0, "None of the clinical variables are present in the frame."))
      
      df_base <- df_base |> dplyr::mutate(dplyr::across(dplyr::all_of(clin_vars), parse_num))
      
      df_long <- df_base |>
        dplyr::select(Tanggal, dplyr::all_of(clin_vars)) |>
        tidyr::pivot_longer(cols = dplyr::all_of(clin_vars),
                            names_to = "Variabel", values_to = "Value") |>
        dplyr::filter(!is.na(Value)) |>
        dplyr::mutate(
          is_int          = is_integerish(Value),
          int_last_digit  = dplyr::if_else(is_int, floor(abs(Value)) %% 10, NA_real_),
          frac_last_tenth = dplyr::if_else(!is_int, round((Value %% 1), 1), NA_real_)
        )
      
      denoms <- df_long |> dplyr::count(Tanggal, Variabel, name = "Total_n")
      
      int_full <- df_long |>
        dplyr::filter(is_int) |>
        dplyr::count(Tanggal, Variabel, int_last_digit, name = "n") |>
        dplyr::left_join(denoms, by = c("Tanggal","Variabel")) |>
        dplyr::mutate(prop = n / Total_n) |>
        dplyr::group_by(Tanggal, Variabel) |>
        dplyr::summarise(
          Int_Preferred_Digits      = paste0(sort(int_last_digit[prop >= value_prop_threshold]), collapse = ", "),
          Int_Preferred_Digits_Prop = paste0(sprintf("%.1f%%", (prop*100)[prop >= value_prop_threshold]), collapse = ", "),
          Int_pref_n                = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Int_Max_Last_Digit_Prop   = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        ) |>
        dplyr::mutate(
          Int_Preferred_Digits      = ifelse(Int_Preferred_Digits == "", "-", Int_Preferred_Digits),
          Int_Preferred_Digits_Prop = ifelse(Int_Preferred_Digits_Prop == "", "-", Int_Preferred_Digits_Prop)
        )
      
      dec_full <- df_long |>
        dplyr::filter(!is_int, !is.na(frac_last_tenth), frac_last_tenth != 0) |>
        dplyr::count(Tanggal, Variabel, frac_last_tenth, name = "n") |>
        dplyr::left_join(denoms, by = c("Tanggal","Variabel")) |>
        dplyr::mutate(prop = n / Total_n) |>
        dplyr::group_by(Tanggal, Variabel) |>
        dplyr::summarise(
          Dec_Preferred_Tenths      = paste0(sort(frac_last_tenth[prop >= value_prop_threshold]), collapse = ", "),
          Dec_Preferred_Tenths_Prop = paste0(sprintf("%.1f%%", (prop*100)[prop >= value_prop_threshold]), collapse = ", "),
          Dec_pref_n                = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Dec_Max_Tenth_Prop        = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        ) |>
        dplyr::mutate(
          Dec_Preferred_Tenths      = ifelse(Dec_Preferred_Tenths == "", "-", Dec_Preferred_Tenths),
          Dec_Preferred_Tenths_Prop = ifelse(Dec_Preferred_Tenths_Prop == "", "-", Dec_Preferred_Tenths_Prop)
        )
      
      combined <- denoms |>
        dplyr::left_join(int_full, by = c("Tanggal","Variabel")) |>
        dplyr::left_join(dec_full, by = c("Tanggal","Variabel")) |>
        dplyr::mutate(
          Int_pref_n   = dplyr::coalesce(Int_pref_n, 0L),
          Dec_pref_n   = dplyr::coalesce(Dec_pref_n, 0L),
          Total_pref_n = Int_pref_n + Dec_pref_n,
          Total_Preferred_Prop = dplyr::if_else(Total_n > 0, round(100 * Total_pref_n / Total_n, 2), NA_real_)
        ) |>
        dplyr::select(
          Tanggal, Variabel, Total_n, Total_pref_n,
          Int_Preferred_Digits,  Int_Preferred_Digits_Prop,  Int_Max_Last_Digit_Prop,
          Dec_Preferred_Tenths,  Dec_Preferred_Tenths_Prop,  Dec_Max_Tenth_Prop,
          Total_Preferred_Prop
        )
      
      preferred <- combined |>
        dplyr::transmute(
          Tanggal, Variabel,
          label = ifelse(is.na(Total_Preferred_Prop), "-",
                         sprintf("%.2f%% (%d/%d)", Total_Preferred_Prop, Total_pref_n, Total_n)),
          pct = Total_Preferred_Prop, num = Total_pref_n, den = Total_n
        )
      
      if (isTRUE(input$heap_latest_only) && nrow(preferred)) {
        latest_date <- max(preferred$Tanggal, na.rm = TRUE)
        preferred <- dplyr::filter(preferred, Tanggal == latest_date)
      }
      
      preferred <- preferred |> dplyr::mutate(Tanggal_chr = format(Tanggal, "%Y-%m-%d"))
      date_levels <- preferred |>
        dplyr::distinct(Tanggal, Tanggal_chr) |>
        dplyr::arrange(Tanggal) |>
        dplyr::pull(Tanggal_chr)
      
      pivot_labels <- preferred |>
        dplyr::select(Variabel, Tanggal_chr, label) |>
        tidyr::pivot_wider(names_from = Tanggal_chr, values_from = label) |>
        dplyr::select(Variabel, dplyr::all_of(date_levels))
      
      avg_row <- preferred %>%
        dplyr::filter(num > 0, den > 0) %>%                                # <-- omit 0.00% rows
        dplyr::group_by(Tanggal_chr) %>%
        dplyr::summarise(
          num_sum = sum(num, na.rm = TRUE),
          den_sum = sum(den, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          pct = dplyr::if_else(den_sum > 0, round(100 * num_sum / den_sum, 2), NA_real_),
          Variabel = "Average",
          label = ifelse(is.na(pct), "-", sprintf("%.2f%% (%d/%d)", pct, num_sum, den_sum))
        ) %>%
        dplyr::select(Variabel, Tanggal_chr, label) %>%
        tidyr::pivot_wider(names_from = Tanggal_chr, values_from = label) %>%
        dplyr::select(Variabel, dplyr::all_of(date_levels))
      
      
      dplyr::bind_rows(pivot_labels, avg_row)
    })
    
    output$heap_tbl <- DT::renderDT({
      tbl <- heap_pivot_r()
      DT::datatable(
        tbl,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          scrollX = TRUE,
          pageLength = 50
        )
      )
    })
    
    output$heap_dl_csv <- downloadHandler(
      filename = function() {
        paste0(
          "heaping_step6_", format(Sys.time(), "%Y%m%d_%H%M%S"),
          ifelse(isTRUE(input$heap_latest_only), "_latest", ""), ".csv"
        )
      },
      content = function(file) readr::write_csv(heap_pivot_r(), file)
    )
  })
}
