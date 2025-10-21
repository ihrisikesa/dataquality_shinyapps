# R/mod_heaping_trend.R
# Heaping (window aggregate) + Trend (per-day) without changing your algorithm.

mod_heaping_trend_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Heaping (window + trend)",
    fluidRow(
      column(
        width = 6,
        h5("Heaping per Variabel (agregasi 1 window)"),
        plotOutput(ns("plot_heaping"), height = 420)
      ),
      column(
        width = 6,
        h5("Ringkasan Heaping — Tabel"),
        htmlOutput(ns("table_heaping"))
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 3,
        uiOutput(ns("trend_var_ui")),
        checkboxInput(ns("trend_include_avg"), "Tampilkan garis rata-rata harian (dashed)", TRUE),
        checkboxInput(ns("drop_allSunday"), "Drop semua hari Minggu pada plot", FALSE),
        textInput(ns("exclude_dates"), "Exclude tanggal (YYYY-MM-DD, pisahkan koma)", "")
      ),
      column(
        width = 9,
        h5(textOutput(ns("trend_title"))),
        plotOutput(ns("plot_heaping_trend"), height = 420)
      )
    )
  )
}

mod_heaping_trend_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    suppressPackageStartupMessages({
      library(dplyr); library(tidyr); library(stringr)
      library(ggplot2); library(knitr); library(kableExtra); library(lubridate); library(tibble)
    })
    
    # ---- constants & helpers (same logic as your Rmd) ----
    value_prop_threshold <- 0.15   # ≥15% ⇒ preferred
    is_integerish <- function(x, tol = 1e-8) is.finite(x) & abs(x - round(x)) < tol
    safe_num <- function(x) {
      s <- as.character(x)
      s <- ifelse(stringr::str_detect(s, ",") & !stringr::str_detect(s, "\\."),
                  stringr::str_replace_all(s, ",", "."), s)
      readr::parse_number(s)
    }
    pretty_var <- function(x){
      keep_upper <- c("NIK","BPJS","ID","UGD","IGD","KIA","ANC","MTBS","BMI","SPO2")
      x <- stringr::str_replace_all(x, "[._]+", " ")
      x <- stringr::str_replace_all(x, stringr::regex("\\bTgl\\.?\\b", ignore_case = TRUE), "Tanggal")
      x <- stringr::str_replace_all(x, stringr::regex("\\bNo\\b",      ignore_case = TRUE), "No.")
      x <- stringr::str_squish(x)
      ifelse(toupper(x) %in% keep_upper, toupper(x), tools::toTitleCase(tolower(x)))
    }
    
    # ---- source data for the current scope (like your Rmd: range preferred) ----
    get_base_df <- reactive({
      fr <- data$frames(); req(fr)
      sc <- isolate(data$scope())
      base <- switch(
        sc,
        "range"      = fr$data_range_curr,
        "range_pair" = fr$data_range_curr,
        "week"       = fr$data_week,
        "month"      = fr$data_month,
        fr$data_df
      )
      if (exists("apply_schema", mode = "function")) base <- apply_schema(base)
      base
    })
    
    # ---- long format with parsed numerics (shared for aggregate & trend) ----
    df_long <- reactive({
      base_df <- get_base_df(); req(base_df)
      
      # clinical candidates
      clin_vars_all <- if (exists("vars_to_check")) vars_to_check else character(0)
      if (!length(clin_vars_all) && exists("valid_rules")) clin_vars_all <- unique(valid_rules$Variable)
      clin_vars <- intersect(clin_vars_all, names(base_df))
      
      if (!nrow(base_df) || !length(clin_vars))
        return(list(df = tibble(), vars = character(0)))
      
      # Coerce Tanggal + parse numeric
      df_base <- base_df %>%
        mutate(
          Tanggal = dplyr::case_when(
            "Tanggal" %in% names(base_df) & inherits(Tanggal, "Date")   ~ Tanggal,
            "Tanggal" %in% names(base_df) & inherits(Tanggal, "POSIXt") ~ as.Date(Tanggal),
            "Tanggal" %in% names(base_df)                               ~ suppressWarnings(as.Date(Tanggal)),
            TRUE                                                        ~ as.Date(NA)
          )
        ) %>%
        mutate(across(
          all_of(clin_vars),
          ~ {
            s <- as.character(.)
            s <- gsub("[^0-9,.-]", "", s)     # keep digits, sign, dot, comma
            s <- gsub(",", ".", s, fixed = TRUE)
            suppressWarnings(as.numeric(s))
          }
        ))
      
      # Exclude "Nafas" & "Suhu" as per your rule
      clin_vars2 <- setdiff(intersect(clin_vars, names(df_base)), c("Nafas","Suhu"))
      if (!length(clin_vars2)) return(list(df = tibble(), vars = character(0)))
      
      dfl <- df_base %>%
        select(any_of(c("Tanggal", "poliruangan", "Perawat / Bidan / Nutrisionist / Sanitarian")),
               all_of(clin_vars2)) %>%
        tidyr::pivot_longer(
          cols = all_of(clin_vars2),
          names_to = "Variabel", values_to = "Value"
        ) %>%
        filter(!is.na(Value)) %>%
        mutate(
          is_int          = is_integerish(Value),
          int_last_digit  = if_else(is_int, floor(abs(Value)) %% 10, NA_real_),
          frac_last_tenth = if_else(!is_int, round((Value %% 1), 1), NA_real_)
        )
      
      list(df = dfl, vars = clin_vars2)
    })
    
    # ---- Window aggregate (same as your table/plot) ----
    heap_result <- reactive({
      dl <- df_long(); dfl <- dl$df; req(nrow(dfl) > 0)
      
      denoms <- dfl %>% group_by(Variabel) %>% summarise(Total_n = n(), .groups = "drop")
      
      int_full <- dfl %>%
        filter(is_int) %>%
        group_by(Variabel, int_last_digit) %>%
        summarise(n = n(), .groups = "drop") %>%
        left_join(denoms, by = "Variabel") %>%
        mutate(prop = n / Total_n, prop_pct = round(prop * 100, 1)) %>%
        group_by(Variabel) %>%
        summarise(
          Int_Preferred_Digits      = paste0(sort(int_last_digit[prop >= value_prop_threshold]), collapse = ", "),
          Int_Preferred_Digits_Prop = paste0(sprintf("%.1f%%", prop_pct[prop >= value_prop_threshold]), collapse = ", "),
          Int_pref_n                = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Int_Max_Last_Digit_Prop   = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        ) %>%
        mutate(
          Int_Preferred_Digits      = ifelse(Int_Preferred_Digits == "", "-", Int_Preferred_Digits),
          Int_Preferred_Digits_Prop = ifelse(Int_Preferred_Digits_Prop == "", "-", Int_Preferred_Digits_Prop)
        )
      
      dec_full <- dfl %>%
        filter(!is_int, !is.na(frac_last_tenth), frac_last_tenth != 0) %>%
        group_by(Variabel, frac_last_tenth) %>%
        summarise(n = n(), .groups = "drop") %>%
        left_join(denoms, by = "Variabel") %>%
        mutate(prop = n / Total_n, prop_pct = round(prop * 100, 1)) %>%
        group_by(Variabel) %>%
        summarise(
          Dec_Preferred_Tenths      = paste0(sort(frac_last_tenth[prop >= value_prop_threshold]), collapse = ", "),
          Dec_Preferred_Tenths_Prop = paste0(sprintf("%.1f%%", prop_pct[prop >= value_prop_threshold]), collapse = ", "),
          Dec_pref_n                = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Dec_Max_Tenth_Prop        = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        ) %>%
        mutate(
          Dec_Preferred_Tenths      = ifelse(Dec_Preferred_Tenths == "", "-", Dec_Preferred_Tenths),
          Dec_Preferred_Tenths_Prop = ifelse(Dec_Preferred_Tenths_Prop == "", "-", Dec_Preferred_Tenths_Prop)
        )
      
      combined <- denoms %>%
        left_join(int_full, by = "Variabel") %>%
        left_join(dec_full, by = "Variabel") %>%
        mutate(
          Int_pref_n           = coalesce(Int_pref_n, 0L),
          Dec_pref_n           = coalesce(Dec_pref_n, 0L),
          Total_pref_n         = Int_pref_n + Dec_pref_n,
          Total_Preferred_Prop = ifelse(Total_n > 0, round(100 * Total_pref_n / Total_n, 2), NA_real_)
        ) %>%
        select(
          Variabel, Total_n, Total_pref_n,
          Int_Preferred_Digits,  Int_Preferred_Digits_Prop,  Int_Max_Last_Digit_Prop,
          Dec_Preferred_Tenths,  Dec_Preferred_Tenths_Prop,  Dec_Max_Tenth_Prop,
          Total_Preferred_Prop
        )
      
      fr <- data$frames()
      cap <- if (!is.null(fr$range_start) && !is.null(fr$range_end)) {
        paste0("Heaping (Integer + Decimal, tanpa double-count) — agregasi satu window: ",
               format(as.Date(fr$range_start), "%Y-%m-%d"), " s.d. ",
               format(as.Date(fr$range_end), "%Y-%m-%d"))
      } else {
        "Heaping (Integer + Decimal, tanpa double-count) — agregasi satu window terpilih"
      }
      
      p <- if (nrow(combined)) {
        ggplot(combined, aes(x = reorder(Variabel, Total_Preferred_Prop), y = Total_Preferred_Prop)) +
          geom_col(width = 0.65) + coord_flip() +
          labs(title = "Heaping per Variabel (agregasi window)", x = NULL, y = "Total Preferred Digits (%)") +
          theme_minimal(base_size = 13)
      } else NULL
      
      list(plot = p, table = combined, cap = cap)
    })
    
    output$plot_heaping <- renderPlot({
      hr <- heap_result(); req(!is.null(hr$plot)); hr$plot
    })
    
    output$table_heaping <- renderUI({
      hr <- heap_result(); tbl <- hr$table; req(nrow(tbl) > 0)
      kb <- knitr::kable(tbl, caption = hr$cap) |>
        kableExtra::kable_styling(full_width = TRUE,
                                  bootstrap_options = c("striped","hover","condensed")) |>
        kableExtra::row_spec(nrow(tbl), bold = TRUE)
      HTML(kb)
    })
    
    # ---- Per-day (trend) using the SAME algorithm, computed per Tanggal & Variabel ----
    daily_combined <- reactive({
      dl <- df_long(); dfl <- dl$df; req(nrow(dfl) > 0)
      
      denoms_day <- dfl %>%
        filter(!is.na(Tanggal)) %>%
        group_by(Tanggal, Variabel) %>%
        summarise(Total_n = n(), .groups = "drop")
      
      int_day <- dfl %>%
        filter(!is.na(Tanggal), is_int) %>%
        group_by(Tanggal, Variabel, int_last_digit) %>%
        summarise(n = n(), .groups = "drop") %>%
        left_join(denoms_day, by = c("Tanggal","Variabel")) %>%
        mutate(prop = n / Total_n) %>%
        group_by(Tanggal, Variabel) %>%
        summarise(
          Int_pref_n              = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Int_Max_Last_Digit_Prop = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        )
      
      dec_day <- dfl %>%
        filter(!is.na(Tanggal), !is_int, !is.na(frac_last_tenth), frac_last_tenth != 0) %>%
        group_by(Tanggal, Variabel, frac_last_tenth) %>%
        summarise(n = n(), .groups = "drop") %>%
        left_join(denoms_day, by = c("Tanggal","Variabel")) %>%
        mutate(prop = n / Total_n) %>%
        group_by(Tanggal, Variabel) %>%
        summarise(
          Dec_pref_n         = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Dec_Max_Tenth_Prop = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        )
      
      comb_day <- denoms_day %>%
        left_join(int_day, by = c("Tanggal","Variabel")) %>%
        left_join(dec_day, by = c("Tanggal","Variabel")) %>%
        mutate(
          Int_pref_n           = coalesce(Int_pref_n, 0L),
          Dec_pref_n           = coalesce(Dec_pref_n, 0L),
          Total_pref_n         = Int_pref_n + Dec_pref_n,
          Total_Preferred_Prop = ifelse(Total_n > 0, round(100 * Total_pref_n / Total_n, 2), NA_real_)
        ) %>%
        arrange(Tanggal, Variabel)
      
      comb_day
    })
    
    # ---- Trend controls & plot ----
    output$trend_var_ui <- renderUI({
      dc <- daily_combined()
      vars <- sort(unique(dc$Variabel))
      selectInput(session$ns("trend_var"), "Pilih variabel", choices = vars, selected = head(vars, 1))
    })
    
    output$trend_title <- renderText({
      var <- req(input$trend_var)
      lbl <- tryCatch(data$label(), error = function(e) NULL)
      paste0("Heaping — ", pretty_var(var), if (!is.null(lbl)) paste0(" — ", lbl) else "")
    })
    
    parse_exclude_dates <- function(txt) {
      if (!nzchar(txt %||% "")) return(as.Date(character(0)))
      raw <- unlist(strsplit(txt, "[,;\\s]+"))
      raw <- raw[nzchar(raw)]
      suppressWarnings(as.Date(raw))
    }
    
    output$plot_heaping_trend <- renderPlot({
      comb_day <- daily_combined(); req(nrow(comb_day) > 0)
      var <- req(input$trend_var)
      
      plot_data <- comb_day %>%
        filter(Variabel == var) %>%
        select(Tanggal, Variabel, Total_Preferred_Prop) %>%
        filter(!is.na(Total_Preferred_Prop))
      
      # exclude dates (plot only)
      exd <- parse_exclude_dates(input$exclude_dates)
      if (length(exd)) plot_data <- dplyr::filter(plot_data, !(Tanggal %in% exd))
      if (isTRUE(input$drop_allSunday))
        plot_data <- dplyr::filter(plot_data, lubridate::wday(Tanggal, week_start = 1) != 7)
      
      validate(need(nrow(plot_data) > 0, "Tidak ada data untuk mem-plot tren heaping per variabel."))
      
      # overall daily average (matching filtered days)
      avg_data <- plot_data %>%
        group_by(Tanggal) %>%
        summarise(Average_Prop = mean(Total_Preferred_Prop, na.rm = TRUE), .groups = "drop")
      
      dmin <- min(plot_data$Tanggal, na.rm = TRUE)
      dmax <- max(plot_data$Tanggal, na.rm = TRUE)
      
      p <- ggplot(
        plot_data,
        aes(x = Tanggal, y = Total_Preferred_Prop, group = 1)
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(y = "Total Preferred Digits (%)", x = "Date") +
        scale_x_date(
          breaks = seq(dmin, dmax, by = "1 day"),
          date_labels = "%Y-%m-%d",
          expand = c(0, 0)
        ) +
        coord_cartesian(ylim = c(0, 100)) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (isTRUE(input$trend_include_avg)) {
        p <- p + geom_line(
          data = avg_data, aes(Tanggal, Average_Prop),
          linetype = "dashed", linewidth = 1.1, inherit.aes = FALSE
        )
      }
      p
    })
  })
}
