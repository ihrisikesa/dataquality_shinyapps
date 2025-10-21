# R/mod_heaping_debug.R — scope-aware + "All variables" + Step-6 raw + robust pivot

mod_heaping_debug_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Heaping Debug & Export"),
    fluidRow(
      column(3, checkboxInput(ns("debug_show"), "Show debug tables", TRUE)),
      column(3, numericInput(ns("debug_n"), "Rows to show", 12, min = 1)),
      column(
        3,
        selectizeInput(
          ns("debug_var"),
          "Focus variable",
          choices  = list("— All variables —" = "__ALL__"),
          selected = "__ALL__",
          multiple = FALSE
        )
      ),
      column(3, dateInput(ns("debug_date"), "Focus date", value = Sys.Date(), format = "yyyy-mm-dd"))
    ),
    fluidRow(
      column(4, checkboxInput(ns("use_debug_date"), "Filter by focus date", FALSE)),
      column(4, checkboxInput(ns("show_latest_only"), "Show latest date only", FALSE)),
      column(4, checkboxInput(ns("export_xlsx"), "Prepare Excel export", TRUE))
    ),
    fluidRow(
      column(4, downloadButton(ns("dl_xlsx"), "Download Excel (.xlsx)")),
      column(4, downloadButton(ns("dl_plot"), "Download Plot (.png)"))
    ),
    tags$hr(),
    h5("Final Heaping Table"),
    div(class = "text-muted small", textOutput(ns("hint"))),
    DT::DTOutput(ns("tbl_pivot")),
    plotOutput(ns("plot_heaping"), height = 380),
    conditionalPanel(
      sprintf("input['%s']", ns("debug_show")),
      tags$hr(),
      h5("Step 6 — preferred_digits_sum (raw before pivot)"),
      DT::DTOutput(ns("tbl_pref_raw")),
      tags$hr(),
      h5("Step 1 — NA Summary"),
      DT::DTOutput(ns("tbl_na_scan")),
      h5("Step 1 — Audit (Original → NA after parse)"),
      DT::DTOutput(ns("tbl_audit")),
      h5("Step 2 — df_long (preview)"),
      DT::DTOutput(ns("tbl_df_long")),
      h5("Step 3 — Integer counts"),
      DT::DTOutput(ns("tbl_int_counts")),
      h5("Step 3 — Integer preferred summary"),
      DT::DTOutput(ns("tbl_int_full")),
      h5("Step 4 — Decimal counts"),
      DT::DTOutput(ns("tbl_dec_counts")),
      h5("Step 5 — Combined"),
      DT::DTOutput(ns("tbl_combined"))
    )
  )
}

mod_heaping_debug_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ALL_SENTINEL <- "__ALL__"
    
    is_integerish <- function(x, tol = 1e-8) is.finite(x) & abs(x - round(x)) < tol
    parse_num <- function(x) {
      s <- as.character(x)
      s <- gsub("[^0-9,.-]", "", s)
      s <- gsub(",", ".", s, fixed = TRUE)
      suppressWarnings(as.numeric(s))
    }
    
    # ---------- scope-aware data ----------
    data_active_r <- reactive({
      if (!is.null(data$ACTIVE) && is.function(data$ACTIVE)) {
        dm <- tryCatch(data$ACTIVE(), error = function(e) NULL)
        if (is.data.frame(dm) && nrow(dm)) return(dm)
      }
      fr <- tryCatch(if (is.function(data$frames)) data$frames() else NULL,
                     error = function(e) NULL)
      if (!is.null(fr)) {
        for (nm in c("active", "current", "month")) {
          if (!is.null(fr[[nm]])) {
            dm <- if (is.function(fr[[nm]])) tryCatch(fr[[nm]](), error = function(e) NULL) else fr[[nm]]
            if (is.data.frame(dm) && nrow(dm)) return(dm)
          }
        }
      }
      if (!is.null(data$data_month) && is.function(data$data_month)) {
        dm <- tryCatch(data$data_month(), error = function(e) NULL)
        if (is.data.frame(dm) && nrow(dm)) return(dm)
      }
      dm <- get0("data_month", ifnotfound = NULL)
      if (is.function(dm)) tryCatch(dm(), error = function(e) NULL) else dm
    })
    
    vars_to_check_r <- reactive({
      if (!is.null(data$vars_to_check)) {
        tryCatch(data$vars_to_check(), error = function(e) NULL)
      } else {
        get0("vars_to_check", ifnotfound =
               c("Berat Badan","Tinggi","Lingkar Perut","Sistole","Diastole","Detak Nadi","Nafas","Suhu"))
      }
    })
    
    # ---------- UI choices (always include ALL) ----------
    observeEvent(list(vars_to_check_r(), data_active_r()), {
      vt <- vars_to_check_r(); vt <- if (is.null(vt)) character(0) else vt
      df <- data_active_r()
      present <- intersect(vt, if (is.data.frame(df)) names(df) else character(0))
      if (!length(present)) present <- vt
      if (!length(present)) present <- c("Berat Badan","Tinggi","Lingkar Perut","Sistole","Diastole","Detak Nadi","Nafas","Suhu")
      
      choices_list <- c(list("— All variables —" = ALL_SENTINEL),
                        stats::setNames(as.list(present), present))
      
      sel_prev <- isolate(input$debug_var)
      sel <- if (!is.null(sel_prev) && sel_prev %in% c(ALL_SENTINEL, present)) sel_prev else ALL_SENTINEL
      
      updateSelectizeInput(session, "debug_var", choices = choices_list, selected = sel, server = TRUE)
    }, ignoreInit = FALSE)
    
    # Snap focus date to latest available in current scope
    observeEvent(data_active_r(), {
      df <- data_active_r()
      if (!is.data.frame(df) || !"Tanggal" %in% names(df)) return()
      dd <- df$Tanggal
      if (inherits(dd, "POSIXt")) dd <- as.Date(dd) else dd <- suppressWarnings(as.Date(dd))
      dd <- dd[!is.na(dd)]
      if (!length(dd)) return()
      updateDateInput(session, "debug_date", min = min(dd), max = max(dd), value = max(dd))
    }, ignoreInit = FALSE)
    
    # ---------- main calc ----------
    calc <- reactive({
      status <- c()
      
      df0 <- data_active_r()
      if (!is.data.frame(df0) || !nrow(df0)) {
        return(list(
          status_msg = "No rows available in current scope.",
          na_scan = tibble::tibble(), audit = tibble::tibble(), df_long = tibble::tibble(),
          int_counts = tibble::tibble(), int_full = tibble::tibble(),
          dec_counts = tibble::tibble(), combined = tibble::tibble(),
          preferred_raw = tibble::tibble(),
          pivot_with_avg = tibble::tibble(Variabel = character(0)), plotheaping = NULL
        ))
      }
      
      vt <- unique(vars_to_check_r() %||% character(0))
      
      df0 <- df0 |>
        dplyr::mutate(
          Tanggal = dplyr::case_when(
            inherits(Tanggal, "Date")   ~ Tanggal,
            inherits(Tanggal, "POSIXt") ~ as.Date(Tanggal),
            TRUE                        ~ suppressWarnings(as.Date(Tanggal))
          )
        ) |>
        dplyr::filter(!is.na(Tanggal))
      
      status <- c(status, sprintf("Using %d rows across %d date(s).",
                                  nrow(df0), length(unique(df0$Tanggal))))
      
      # choose vars
      clin_vars <- intersect(vt, names(df0))
      if (!length(clin_vars)) {
        ignore_cols <- c("Tanggal","Poli/Ruangan","Perawat / Bidan / Nutrisionist / Sanitarian")
        cand <- setdiff(names(df0), ignore_cols)
        num_like <- vapply(df0[cand], function(x) sum(!is.na(parse_num(x))), integer(1))
        clin_vars <- names(num_like)[num_like > 0]
      }
      clin_vars2 <- setdiff(clin_vars, c("Nafas","Suhu"))
      if (!length(clin_vars2)) {
        return(list(
          status_msg = "No usable numeric columns found.",
          na_scan = tibble::tibble(), audit = tibble::tibble(), df_long = tibble::tibble(),
          int_counts = tibble::tibble(), int_full = tibble::tibble(),
          dec_counts = tibble::tibble(), combined = tibble::tibble(),
          preferred_raw = tibble::tibble(),
          pivot_with_avg = tibble::tibble(Variabel = character(0)), plotheaping = NULL
        ))
      }
      
      originals <- dplyr::select(
        df0,
        dplyr::any_of(c("Tanggal", "Perawat / Bidan / Nutrisionist / Sanitarian")),
        dplyr::any_of(clin_vars)
      )
      
      df_base <- df0 |>
        dplyr::mutate(dplyr::across(dplyr::all_of(clin_vars), parse_num))
      
      na_scan <- purrr::map_dfr(clin_vars, function(v) {
        x <- df_base[[v]]
        tibble::tibble(
          Variabel = v,
          n = length(x),
          n_nonNA = sum(!is.na(x)),
          n_NA = sum(is.na(x)),
          prop_NA = round(mean(is.na(x)) * 100, 2),
          min = suppressWarnings(min(x, na.rm = TRUE)),
          max = suppressWarnings(max(x, na.rm = TRUE))
        )
      })
      
      audit <- originals |>
        tidyr::pivot_longer(cols = dplyr::all_of(clin_vars), names_to = "Variabel", values_to = "Original") |>
        dplyr::mutate(Parsed = parse_num(Original)) |>
        dplyr::filter(is.na(Parsed) & !is.na(Original) & Original != "") |>
        dplyr::select(Tanggal,
                      dplyr::any_of(c("Perawat / Bidan / Nutrisionist / Sanitarian")),
                      Variabel, Original) |>
        dplyr::distinct()
      
      df_long <- df_base |>
        dplyr::select(
          Tanggal,
          dplyr::any_of(c("Perawat / Bidan / Nutrisionist / Sanitarian")),
          dplyr::all_of(clin_vars2)
        ) |>
        tidyr::pivot_longer(cols = dplyr::all_of(clin_vars2), names_to = "Variabel", values_to = "Value") |>
        dplyr::filter(!is.na(Value)) |>
        dplyr::mutate(
          is_int          = is_integerish(Value),
          int_last_digit  = dplyr::if_else(is_int, floor(abs(Value)) %% 10, NA_real_),
          frac_last_tenth = dplyr::if_else(!is_int, round((Value %% 1), 1), NA_real_)
        )
      
      # apply Focus variable filter ONLY if not ALL
      sel_var <- input$debug_var
      if (!is.null(sel_var) && nzchar(sel_var) && sel_var != ALL_SENTINEL) {
        df_long <- dplyr::filter(df_long, Variabel == sel_var)
      }
      
      # optional date filter
      if (isTRUE(input$use_debug_date)) {
        sel_date <- suppressWarnings(as.Date(input$debug_date))
        avail_dates <- unique(df_base$Tanggal)
        if (!is.na(sel_date) && length(avail_dates) && sel_date %in% avail_dates) {
          df_long <- dplyr::filter(df_long, Tanggal == sel_date)
          status <- c(status, paste0("Date filter applied: ", format(sel_date)))
        }
      }
      
      if (!nrow(df_long)) {
        return(list(
          status_msg = paste(status, collapse = " • "),
          na_scan = na_scan, audit = audit, df_long = df_long,
          int_counts = tibble::tibble(), int_full = tibble::tibble(),
          dec_counts = tibble::tibble(), combined = tibble::tibble(),
          preferred_raw = tibble::tibble(),
          pivot_with_avg = tibble::tibble(Variabel = character(0)), plotheaping = NULL
        ))
      }
      
      value_prop_threshold <- 0.15
      
      denoms <- df_long |>
        dplyr::group_by(Tanggal, Variabel) |>
        dplyr::summarise(Total_n = dplyr::n(), .groups = "drop")
      
      int_counts <- df_long |>
        dplyr::filter(is_int) |>
        dplyr::count(Tanggal, Variabel, int_last_digit, name = "n") |>
        dplyr::left_join(denoms, by = c("Tanggal","Variabel")) |>
        dplyr::mutate(prop = n / Total_n, prop_pct = round(prop * 100, 1))
      
      int_full <- int_counts |>
        dplyr::group_by(Tanggal, Variabel) |>
        dplyr::summarise(
          Int_Preferred_Digits      = paste0(sort(int_last_digit[prop >= value_prop_threshold]), collapse = ", "),
          Int_Preferred_Digits_Prop = paste0(sprintf("%.1f%%", (prop * 100)[prop >= value_prop_threshold]), collapse = ", "),
          Int_pref_n                = sum(n[prop >= value_prop_threshold], na.rm = TRUE),
          Int_Max_Last_Digit_Prop   = if (dplyr::n() > 0) max(prop) * 100 else NA_real_,
          .groups = "drop"
        ) |>
        dplyr::mutate(
          Int_Preferred_Digits      = ifelse(Int_Preferred_Digits == "", "-", Int_Preferred_Digits),
          Int_Preferred_Digits_Prop = ifelse(Int_Preferred_Digits_Prop == "", "-", Int_Preferred_Digits_Prop)
        )
      
      dec_counts <- df_long |>
        dplyr::filter(!is_int, !is.na(frac_last_tenth), frac_last_tenth != 0) |>
        dplyr::count(Tanggal, Variabel, frac_last_tenth, name = "n") |>
        dplyr::left_join(denoms, by = c("Tanggal","Variabel")) |>
        dplyr::mutate(prop = n / Total_n, prop_pct = round(prop * 100, 1))
      
      dec_full <- dec_counts |>
        dplyr::group_by(Tanggal, Variabel) |>
        dplyr::summarise(
          Dec_Preferred_Tenths      = paste0(sort(frac_last_tenth[prop >= value_prop_threshold]), collapse = ", "),
          Dec_Preferred_Tenths_Prop = paste0(sprintf("%.1f%%", (prop * 100)[prop >= value_prop_threshold]), collapse = ", "),
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
      
      # -------- Step 6: preferred_digits_sum (RAW) + Latest-only filter ----------
      preferred_digits_sum <- combined %>%
        dplyr::transmute(
          Tanggal,
          Variabel,
          label = ifelse(is.na(Total_Preferred_Prop),
                         "-",
                         sprintf("%.2f%% (%d/%d)", Total_Preferred_Prop, Total_pref_n, Total_n)),
          pct = Total_Preferred_Prop,
          num = Total_pref_n,
          den = Total_n
        )
      
      if (isTRUE(input$show_latest_only) && nrow(preferred_digits_sum)) {
        latest_date <- max(preferred_digits_sum$Tanggal, na.rm = TRUE)
        preferred_digits_sum <- dplyr::filter(preferred_digits_sum, Tanggal == latest_date)
        status <- c(status, paste0("Latest-only mode (", format(latest_date), ")."))
      }
      
      # ---------- robust pivot (fill -, keep column order) + Average ----------
      pivot_with_avg <- NULL
      if (nrow(preferred_digits_sum)) {
        preferred_digits_sum <- preferred_digits_sum %>%
          dplyr::mutate(Tanggal_chr = format(Tanggal, "%Y-%m-%d"))
        
        date_levels <- preferred_digits_sum %>%
          dplyr::distinct(Tanggal, Tanggal_chr) %>%
          dplyr::arrange(Tanggal) %>%
          dplyr::pull(Tanggal_chr)
        
        pivot_labels <- preferred_digits_sum %>%
          dplyr::select(Variabel, Tanggal_chr, label) %>%
          tidyr::pivot_wider(
            names_from = Tanggal_chr,
            values_from = label,
            values_fill = "-"
          ) %>%
          dplyr::select(Variabel, dplyr::all_of(date_levels))
        
        avg_row <- preferred_digits_sum %>%
          dplyr::group_by(Tanggal_chr) %>%
          dplyr::summarise(
            num = sum(num, na.rm = TRUE),
            den = sum(den, na.rm = TRUE),
            pct = ifelse(den > 0, round(100 * num / den, 2), NA_real_),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Variabel = "Average",
            label = ifelse(is.na(pct), "-", sprintf("%.2f%% (%d/%d)", pct, num, den))
          ) %>%
          dplyr::select(Variabel, Tanggal_chr, label) %>%
          tidyr::pivot_wider(
            names_from = Tanggal_chr,
            values_from = label,
            values_fill = "-"
          ) %>%
          dplyr::select(Variabel, dplyr::all_of(date_levels))
        
        pivot_with_avg <- dplyr::bind_rows(pivot_labels, avg_row) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      } else {
        pivot_with_avg <- tibble::tibble(Variabel = character(0))
      }
      
      # ---------- plot ----------
      plotheaping <- NULL
      plot_data <- combined %>%
        dplyr::select(Tanggal, Variabel, Total_Preferred_Prop, Total_pref_n, Total_n) %>%
        dplyr::filter(!is.na(Total_Preferred_Prop))
      
      if (nrow(plot_data)) {
        avg_data <- plot_data %>%
          dplyr::filter(Total_pref_n > 0, Total_n > 0) %>%
          dplyr::group_by(Tanggal) %>%
          dplyr::summarise(
            num_sum = sum(Total_pref_n, na.rm = TRUE),
            den_sum = sum(Total_n,       na.rm = TRUE),
            Average_Prop = dplyr::if_else(den_sum > 0, 100 * num_sum / den_sum, NA_real_),
            .groups = "drop"
          )
        
        var_levels <- sort(unique(plot_data$Variabel))
        pal <- stats::setNames(scales::hue_pal()(length(var_levels)), var_levels)
        dmin <- min(plot_data$Tanggal); dmax <- max(plot_data$Tanggal)
        
        plotheaping <- ggplot2::ggplot(
          plot_data,
          ggplot2::aes(x = Tanggal, y = Total_Preferred_Prop, color = Variabel, group = Variabel)
        ) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 2) +
          ggplot2::geom_line(
            data = avg_data,
            ggplot2::aes(Tanggal, Average_Prop, color = "Rata-rata", linetype = "Rata-rata", group = 1),
            linewidth = 1.2, inherit.aes = FALSE
          ) +
          ggplot2::labs(
            title = paste0("Daily Trends in Heaping — ", get0("puskesmas_name", ifnotfound = "")),
            y = "Total Preferred Digits (%)", x = "Date", color = NULL, linetype = NULL
          ) +
          ggplot2::scale_color_manual(values = c(pal, "Rata-rata" = "black"), breaks = c("Rata-rata", var_levels)) +
          ggplot2::scale_linetype_manual(values = c("Rata-rata" = "dashed"), breaks = "Rata-rata") +
          ggplot2::scale_x_date(breaks = seq(dmin, dmax, by = "1 day"), date_labels = "%Y-%m-%d", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
          ggplot2::coord_cartesian(ylim = c(0, 100)) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         legend.position = "bottom",
                         legend.direction = "horizontal",
                         legend.box = "horizontal")
      }
      
      list(
        status_msg = paste(status, collapse = " • "),
        na_scan = na_scan,
        audit = audit,
        df_long = if (nrow(df_long)) head(df_long, input$debug_n) else df_long,
        int_counts = int_counts,
        int_full = int_full,
        dec_counts = dec_counts,
        combined = combined,
        preferred_raw = preferred_digits_sum,   # <-- Step-6 raw for UI + Excel
        pivot_with_avg = pivot_with_avg,
        plotheaping = plotheaping
      )
    })
    
    output$hint <- renderText(calc()$status_msg)
    
    output$tbl_pivot <- DT::renderDT({
      x <- calc()$pivot_with_avg
      req(!is.null(x))
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 50))
    })
    
    output$plot_heaping <- renderPlot({ calc()$plotheaping })
    
    output$tbl_pref_raw  <- DT::renderDT(DT::datatable(calc()$preferred_raw, options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_na_scan   <- DT::renderDT(DT::datatable(calc()$na_scan,      options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_audit     <- DT::renderDT(DT::datatable(calc()$audit,        options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_df_long   <- DT::renderDT(DT::datatable(calc()$df_long,      options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_int_counts<- DT::renderDT(DT::datatable(calc()$int_counts |> dplyr::arrange(dplyr::desc(n)),
                                                       options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_int_full  <- DT::renderDT(DT::datatable(calc()$int_full,     options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_dec_counts<- DT::renderDT(DT::datatable(calc()$dec_counts |> dplyr::arrange(dplyr::desc(n)),
                                                       options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    output$tbl_combined  <- DT::renderDT(DT::datatable(calc()$combined |> dplyr::arrange(Tanggal, Variabel),
                                                       options = list(scrollX = TRUE, pageLength = input$debug_n), rownames = FALSE))
    
    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("heaping_debug_", format(Sys.Date()), ".xlsx"),
      content = function(file) {
        if (!isTRUE(input$export_xlsx)) { writeBin(charToRaw("Excel export disabled"), file); return(invisible()) }
        wb <- openxlsx::createWorkbook()
        safe_sheet_name <- function(x) { x <- gsub("[\\\\/:*?\\[\\]]", "_", x); substr(x, 1, 31) }
        add_sheet_unique <- function(wb, base) {
          nm <- safe_sheet_name(base); orig <- nm; i <- 2
          while (nm %in% names(wb$worksheets)) { nm <- safe_sheet_name(paste0(orig, "_", i)); i <- i + 1 }
          openxlsx::addWorksheet(wb, nm); nm
        }
        write_df_sheet <- function(wb, nm, df) {
          sheet <- add_sheet_unique(wb, nm)
          if (is.null(df)) df <- tibble::tibble()
          openxlsx::writeData(wb, sheet, df, withFilter = TRUE)
          if (ncol(df)) {
            openxlsx::freezePane(wb, sheet, firstRow = TRUE)
            openxlsx::setColWidths(wb, sheet, cols = 1:ncol(df), widths = "auto")
          }
        }
        res <- calc()
        write_df_sheet(wb, "Step1_NA_Summary",      res$na_scan)
        write_df_sheet(wb, "Step1_Audit",           res$audit)
        write_df_sheet(wb, "Step2_df_long",         res$df_long)
        write_df_sheet(wb, "Step3_int_counts",      res$int_counts)
        write_df_sheet(wb, "Step3_int_full",        res$int_full)
        write_df_sheet(wb, "Step4_dec_counts",      res$dec_counts)
        write_df_sheet(wb, "Step5_Combined",        res$combined)
        write_df_sheet(wb, "Step6_preferred_raw",   res$preferred_raw)  # <-- your Step-6 raw
        write_df_sheet(wb, "Step6_pivot_with_avg",  res$pivot_with_avg)
        if (!is.null(res$plotheaping)) {
          tmp_png <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp_png, res$plotheaping, width = 10, height = 6, dpi = 150)
          sheet_nm <- add_sheet_unique(wb, "Step7_Plot")
          openxlsx::insertImage(wb, sheet = sheet_nm, file = tmp_png,
                                startCol = 2, startRow = 2, width = 10, height = 6, units = "in")
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    output$dl_plot <- downloadHandler(
      filename = function() paste0("heaping_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
      content = function(file) {
        p <- calc()$plotheaping; req(!is.null(p))
        ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)
      }
    )
  })
}
