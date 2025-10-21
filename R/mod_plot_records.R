# R/mod_plot_records.R
# Quality table (kable) + plots + detailed logs (value-level outliers, counts, strict drops)

mod_plot_records_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Quality Tables",
    # 1) Guardrail summary
    fluidRow(
      column(
        width = 12,
        h5("Ringkasan Outlier Klinis (RAW data vs. valid_rules)"),
        htmlOutput(ns("guardrail_kable")),
        tags$hr()
      )
    ),
    # 2) Admin plots
    #fluidRow(
    #  column(
     #   width = 6,
    #    h5("Admin — Kelengkapan (Non-missing %)"),
    #    plotOutput(ns("adm_bar"), height = 360)
    #  ),
     # column(
     #   width = 6,
     #   h5("Admin — Komposisi Status (%)"),
     #   plotOutput(ns("adm_stack"), height = 420)
    #  )
  #  ),
    tags$hr(),
    # 3) Clinical plots (selector)
  #  fluidRow(
  #    column(
   #     width = 4,
   #     uiOutput(ns("clin_selector")),
   #     checkboxInput(ns("show_labels"), "Tampilkan label numerik", value = TRUE)
   #   ),
   #   column(
    #    width = 8,
    #    h5(textOutput(ns("clin_title"))),
    #    plotOutput(ns("clin_interval"), height = 300),
   #     br(),
   #     plotOutput(ns("clin_box"), height = 320)
  #    )
  #  ),
    tags$hr(),
    # 4) EXTRA INFORMATION (after plots)
    fluidRow(
      column(
        width = 12,
        h5("Outlier: nilai yang dihapus (value-level)"),
        htmlOutput(ns("outlier_log_table")),
        br(),
        h5("Outlier — ringkasan jumlah per variabel/unit"),
        htmlOutput(ns("outlier_counts_table")),
        br(),
        h5("Baris yang di-drop oleh strict filter (beserta variabel penyebab)"),
        htmlOutput(ns("dropped_rows_table"))
      )
    )
  )
}

mod_plot_records_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    suppressPackageStartupMessages({
      library(dplyr); library(tidyr); library(stringr); library(readr); library(tibble)
      library(ggplot2); library(scales); library(knitr); library(htmltools)
    })
    
    # ---- helpers (local, no .data pronoun) ----
    fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)
    pretty_var <- function(x){
      keep_upper <- c("NIK","BPJS","ID","UGD","IGD","KIA","ANC","MTBS","BMI","SPO2")
      x <- stringr::str_replace_all(x, "[._]+", " ")
      x <- stringr::str_replace_all(x, stringr::regex("\\bTgl\\.?\\b", ignore_case = TRUE), "Tanggal")
      x <- stringr::str_replace_all(x, stringr::regex("\\bNo\\b",      ignore_case = TRUE), "No.")
      x <- stringr::str_squish(x)
      ifelse(toupper(x) %in% keep_upper, toupper(x), tools::toTitleCase(tolower(x)))
    }
    safe_num <- function(x) {
      s <- as.character(x)
      s <- ifelse(stringr::str_detect(s, ",") & !stringr::str_detect(s, "\\."),
                  stringr::str_replace_all(s, ",", "."), s)
      readr::parse_number(s)
    }
    kable_or_dt <- function(df, caption, align = NULL, pageLength = 20) {
      if (is.null(df) || !nrow(df)) {
        return(div(strong("(kosong)")))
      }
      if (requireNamespace("kableExtra", quietly = TRUE)) {
        kb <- knitr::kable(df, format = "html", booktabs = TRUE, caption = caption, align = align %||% "l")
        kb <- kableExtra::kable_styling(kb, full_width = FALSE,
                                        bootstrap_options = c("striped","hover","condensed"))
        return(HTML(kb))
      } else if (requireNamespace("DT", quietly = TRUE)) {
        return(DT::datatable(df, options = list(pageLength = pageLength, scrollX = TRUE), caption = caption))
      } else {
        return(HTML(knitr::kable(df, format = "html", caption = caption)))
      }
    }
    
    # ---- RAW df for current scope (pre guardrail cleaning) + schema mapping ----
    get_raw_df_for_scope <- reactive({
      fr <- data$frames(); req(fr)
      sc <- isolate(data$scope())
      raw <- switch(
        sc,
        "range"      = fr$data_range_curr,
        "range_pair" = fr$data_range_curr, # summarize current window
        "week"       = fr$data_week,
        "month"      = fr$data_month,
        fr$data_df                            # default: daily
      )
      if (exists("apply_schema", mode = "function")) raw <- apply_schema(raw)
      raw
    })
    
    # ===============================
    # A) Guardrail summary (RAW vs rules)
    # ===============================
    build_guardrail_summary <- reactive({
      df_raw <- get_raw_df_for_scope(); req(df_raw)
      rules  <- data$rules();            req(rules)
      
      clin_vars_with_rules <- intersect(rules$Variable, names(df_raw))
      if (!length(clin_vars_with_rules)) return(tibble())
      
      df_long_flag <- df_raw %>%
        mutate(row_id = dplyr::row_number()) %>%
        dplyr::select(row_id, dplyr::all_of(clin_vars_with_rules)) %>%
        tidyr::pivot_longer(-row_id, names_to = "Variable", values_to = "raw") %>%
        dplyr::mutate(val = safe_num(raw)) %>%
        dplyr::left_join(rules, by = "Variable") %>%
        dplyr::mutate(
          outlier = !is.na(val) & (val < min_ok | val > max_ok),
          ok      = is.na(val) | !outlier
        )
      
      df_long_flag %>%
        dplyr::group_by(Variable, min_ok, max_ok) %>%
        dplyr::summarise(
          Rows        = dplyr::n(),
          Non_missing = sum(!is.na(val)),
          Outliers    = sum(outlier, na.rm = TRUE),
          `Outlier %` = ifelse(Non_missing > 0, Outliers / Non_missing, NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(Outliers)) %>%
        dplyr::mutate(`Outlier %` = fmt_pct(`Outlier %`)) %>%
        dplyr::rename(`Valid min` = min_ok, `Valid max` = max_ok)
    })
    
    output$guardrail_kable <- renderUI({
      sm <- build_guardrail_summary()
      kable_or_dt(sm, caption = "Ringkasan Outlier Klinis (RAW data vs. valid_rules)",
                  align = c("l","r","r","r","r","r"))
    })
    
    # ===============================
    # B) Admin/Clinical tables for plotting (from ACTIVE = guardrail-cleaned)
    # ===============================
    quality_tables <- reactive({
      x <- data$ACTIVE(); req(x)
      rules <- data$rules()
      ref_date <- as.Date(isolate(data$frames()$resolved_date %||% Sys.Date()))
      
      stopifnot(exists("summarize_var"), exists("vars_to_check"), exists("vars_to_check_adm"))
      
      adm_present  <- intersect(vars_to_check_adm, names(x))
      clin_present <- intersect(vars_to_check,     names(x))
      
      adm_tbl  <- purrr::map_dfr(adm_present,  ~ summarize_var(x, .x, "Admin",   rules, ref_date)) %>%
        dplyr::select(-Role, -Min, -Q1, -Median, -Q3, -Max) %>%
        dplyr::arrange(Variable)
      
      clin_tbl <- purrr::map_dfr(clin_present, ~ summarize_var(x, .x, "Clinical", rules, ref_date)) %>%
        dplyr::select(-Role) %>%
        dplyr::arrange(Variable)
      
      list(adm = adm_tbl, clin = clin_tbl)
    })
    
    # ===============================
    # C) Plots
    # ===============================
    output$adm_bar <- renderPlot({
      qt <- quality_tables(); adm_tbl <- qt$adm; req(nrow(adm_tbl) > 0)
      adm_plot <- adm_tbl %>%
        mutate(
          Variable_lbl     = pretty_var(Variable),
          non_missing_rate = Non_missing / pmax(Rows, 1)
        ) %>%
        arrange(non_missing_rate) %>%
        mutate(Variable_lbl = factor(Variable_lbl, levels = Variable_lbl))
      
      ggplot(adm_plot, aes(x = Variable_lbl, y = non_missing_rate)) +
        geom_col(width = 0.6, fill = "#2C7BE5") +
        geom_text(aes(label = fmt_pct(non_missing_rate)), hjust = -0.05, size = 3) +
        coord_flip(clip = "off") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                           limits = c(0, 1.05),
                           expand = expansion(mult = c(0, .02))) +
        labs(title = "Admin — Kelengkapan (Non-missing %)", x = "Terisi (%)", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
    })
    
    output$adm_stack <- renderPlot({
      qt <- quality_tables(); adm_tbl <- qt$adm; req(nrow(adm_tbl) > 0)
      LABEL_THRESH <- 0.03
      adm_status <- adm_tbl %>%
        transmute(
          Variable_lbl = pretty_var(Variable),
          Missing = Missing / pmax(Rows, 1),
          Invalid = `Invalid (non-missing)` / pmax(Rows, 1),
          Valid   = pmax(0, 1 - Missing - Invalid)
        ) %>%
        tidyr::pivot_longer(c(Valid, Invalid, Missing), names_to = "Status", values_to = "Rate") %>%
        mutate(Status = factor(Status, levels = c("Valid","Invalid","Missing")))
      
      ggplot(adm_status, aes(y = Variable_lbl, x = Rate, fill = Status)) +
        geom_col() +
        geom_text(
          aes(label = ifelse(Rate >= LABEL_THRESH, fmt_pct(Rate), "")),
          position = position_stack(vjust = 0.5),
          color = "white", size = 3
        ) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        labs(title = "Admin — Komposisi Status (%)", x = "Persentase", y = NULL) +
        theme_minimal(base_size = 11)
    })
    
    output$clin_selector <- renderUI({
      qt <- quality_tables(); clin_tbl <- qt$clin
      vars <- unique(clin_tbl$Variable)
      if (!length(vars)) vars <- character(0)
      selectInput(session$ns("clin_var"), "Clinical variable", choices = vars, selected = head(vars, 1))
    })
    
    output$clin_title <- renderText({
      req(input$clin_var)
      paste0(pretty_var(input$clin_var), " — Ringkasan")
    })
    
    output$clin_interval <- renderPlot({
      qt <- quality_tables(); clin_tbl <- qt$clin; rules <- data$rules()
      req(nrow(clin_tbl) > 0, input$clin_var)
      
      row <- clin_tbl %>% dplyr::filter(Variable == input$clin_var)
      req(nrow(row) > 0)
      lbl <- pretty_var(input$clin_var)
      miss_pct  <- ifelse(row$Rows[1] > 0, row$Missing[1] / row$Rows[1], NA_real_)
      inval_pct <- ifelse(row$Non_missing[1] > 0, row$`Invalid (non-missing)`[1] / row$Non_missing[1], NA_real_)
      vr <- if (!is.null(rules)) rules %>% dplyr::filter(Variable == input$clin_var) else tibble()
      
      ggplot(row, aes(y = 0)) +
        { if (nrow(vr))
          geom_rect(aes(xmin = vr$min_ok[1], xmax = vr$max_ok[1],
                        ymin = -0.14, ymax = 0.14,
                        fill = "Rentang valid (min_ok–max_ok)"),
                    inherit.aes = FALSE)
        } +
        geom_errorbarh(aes(xmin = Min, xmax = Max, colour = "Min–Maks"),
                       height = 0.25, linewidth = 0.6, na.rm = TRUE) +
        geom_errorbarh(aes(xmin = Q1, xmax = Q3, colour = "IQR (Q1–Q3)"),
                       height = 0, linewidth = 4, na.rm = TRUE) +
        geom_point(aes(x = Median, colour = "Median", shape = "Median"),
                   size = 2.8, na.rm = TRUE) +
        { if (isTRUE(input$show_labels))
          geom_text(aes(x = Median, label = sprintf("%.1f", Median)),
                    hjust = -0.2, size = 3.2, colour = "#2C7BE5", na.rm = TRUE)
        } +
        scale_fill_manual(name = "Keterangan", values = c("Rentang valid (min_ok–max_ok)" = "#E8F1FD")) +
        scale_color_manual(name = "Keterangan",
                           values = c("Min–Maks"="grey40","IQR (Q1–Q3)"="#2C7BE5","Median"="#2C7BE5")) +
        scale_shape_manual(name = "Keterangan", values = c("Median" = 16)) +
        guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2), shape = guide_legend(order = 2)) +
        labs(
          title    = paste0(lbl, " — Ringkasan Numerik"),
          subtitle = paste0("N=", row$Non_missing[1],
                            " · missing ", sprintf("%.1f%%", 100*miss_pct),
                            " · invalid ", sprintf("%.1f%%", 100*inval_pct)),
          x = "Nilai", y = NULL
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top",
              legend.title = element_text(face = "bold"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank())
    })
    
    output$clin_box <- renderPlot({
      qt <- quality_tables(); clin_tbl <- qt$clin; rules <- data$rules()
      req(nrow(clin_tbl) > 0, input$clin_var)
      
      row <- clin_tbl %>% dplyr::filter(Variable == input$clin_var)
      req(nrow(row) > 0)
      lbl <- pretty_var(input$clin_var)
      miss_pct  <- ifelse(row$Rows[1] > 0, row$Missing[1] / row$Rows[1], NA_real_)
      inval_pct <- ifelse(row$Non_missing[1] > 0, row$`Invalid (non-missing)`[1] / row$Non_missing[1], NA_real_)
      vr <- if (!is.null(rules)) rules %>% dplyr::filter(Variable == input$clin_var) else tibble()
      
      df_plot <- tibble::tibble(
        x     = lbl,
        ymin  = row$Min, lower = row$Q1, middle = row$Median, upper = row$Q3, ymax = row$Max
      )
      
      ggplot(df_plot, aes(x = x)) +
        { if (nrow(vr))
          geom_rect(aes(ymin = vr$min_ok[1], ymax = vr$max_ok[1],
                        xmin = 0.6, xmax = 1.4,
                        fill = "Rentang valid (min_ok–max_ok)"),
                    inherit.aes = FALSE)
        } +
        geom_boxplot(
          aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
          stat = "identity", width = 0.5, fill = "#2C7BE5", alpha = 0.35,
          colour = "#2C7BE5", linewidth = 0.7, outlier.shape = NA
        ) +
        geom_point(aes(y = middle, colour = "Median", shape = "Median"), size = 2.8) +
        { if (isTRUE(input$show_labels))
          geom_text(aes(y = middle, label = sprintf("%.1f", middle)),
                    vjust = -0.8, size = 3.2, colour = "#2C7BE5")
        } +
        scale_fill_manual(name = "Keterangan", values = c("Rentang valid (min_ok–max_ok)" = "#E8F1FD")) +
        scale_color_manual(name = "Keterangan", values = c("Median"="#2C7BE5")) +
        scale_shape_manual(name = "Keterangan", values = c("Median" = 16)) +
        labs(
          title    = paste0(lbl, " — Boxplot Vertikal"),
          subtitle = paste0(
            "N=", row$Non_missing[1],
            " · missing ", sprintf("%.1f%%", 100 * miss_pct),
            " · invalid ", sprintf("%.1f%%", 100 * inval_pct),
            if (nrow(vr)) paste0(" · rentang valid [", vr$min_ok[1], "–", vr$max_ok[1], "]") else ""
          ),
          x = NULL, y = "Nilai"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title.position = "plot")
    })
    
    # ===============================
    # D) EXTRA INFORMATION (tables after plots)
    # ===============================
    # Build outlier logs from RAW df (pre-clean), using friendly aliases provided by apply_schema
    build_outlier_logs <- reactive({
      df <- get_raw_df_for_scope(); req(df)
      rules <- data$rules();        req(rules)
      
      # Identifier columns — use what exists
      id_cols <- intersect(c("Tanggal","poliruangan","ID_Kunjungan","ID Kunjungan","id_kunjungan","No RM","no_rm"), names(df))
      
      clin_cols <- intersect(rules$Variable, names(df))
      if (!length(clin_cols)) return(list(outlier_log = tibble(), outlier_counts = tibble(), dropped_rows = tibble()))
      
      df_id <- df %>% mutate(row_id = dplyr::row_number())
      
      outlier_log <- df_id %>%
        dplyr::select(row_id, dplyr::any_of(id_cols), dplyr::all_of(clin_cols)) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(clin_cols), names_to = "Variable", values_to = "raw") %>%
        dplyr::mutate(parsed = safe_num(raw)) %>%
        dplyr::left_join(rules, by = "Variable") %>%
        dplyr::mutate(outlier = !is.na(parsed) & (parsed < min_ok | parsed > max_ok)) %>%
        dplyr::filter(outlier) %>%
        dplyr::transmute(row_id, !!!rlang::syms(id_cols),
                         Variable, raw_value = raw)
      
      outlier_counts <- outlier_log %>%
        dplyr::group_by(Variable, dplyr::across(dplyr::any_of("poliruangan"))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(n))
      
      # Strict drop log (rows that would be removed by strict filtering)
      # Use keep_in_range to create out_<var> flags; fall back to local if not found
      keep_in_range_local <- function(df_in, rules) {
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
      tf <- if (exists("keep_in_range", mode = "function")) keep_in_range(df_id, rules) else keep_in_range_local(df_id, rules)
      flag_cols <- grep("^out_", names(tf), value = TRUE)
      
      dropped_rows <- tf %>%
        dplyr::mutate(any_outlier = if (length(flag_cols)) rowSums(dplyr::across(dplyr::all_of(flag_cols), ~ .x %in% TRUE)) > 0L else FALSE) %>%
        dplyr::filter(any_outlier) %>%
        dplyr::select(row_id, dplyr::any_of(id_cols), dplyr::all_of(flag_cols)) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(flag_cols), names_to = "flag", values_to = "is_out") %>%
        dplyr::filter(is_out) %>%
        dplyr::mutate(Variable = sub("^out_", "", flag)) %>%
        dplyr::select(-flag, -is_out) %>%
        dplyr::left_join(outlier_log, by = c("row_id","Variable"))
      
      list(
        outlier_log = outlier_log,
        outlier_counts = outlier_counts,
        dropped_rows = dropped_rows
      )
    })
    
    output$outlier_log_table <- renderUI({
      logs <- build_outlier_logs()
      # show first 100 rows for readability
      df <- if (nrow(logs$outlier_log) > 100) head(logs$outlier_log, 100) else logs$outlier_log
      cap <- if (nrow(logs$outlier_log) > 100)
        sprintf("Outlier: nilai yang dihapus (menampilkan 100 dari %d baris)", nrow(logs$outlier_log))
      else "Outlier: nilai yang dihapus"
      kable_or_dt(df, caption = cap)
    })
    
    output$outlier_counts_table <- renderUI({
      logs <- build_outlier_logs()
      kable_or_dt(logs$outlier_counts, caption = "Outlier — ringkasan jumlah per variabel/unit",
                  align = c("l","l","r"))
    })
    
    output$dropped_rows_table <- renderUI({
      logs <- build_outlier_logs()
      df <- if (nrow(logs$dropped_rows) > 100) head(logs$dropped_rows, 100) else logs$dropped_rows
      cap <- if (nrow(logs$dropped_rows) > 100)
        sprintf("Baris yang di-drop oleh strict filter (menampilkan 100 dari %d baris)", nrow(logs$dropped_rows))
      else "Baris yang di-drop oleh strict filter"
      kable_or_dt(df, caption = cap)
    })
  })
}
