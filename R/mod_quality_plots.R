# R/mod_quality_plots.R

# Quality plots module
# Depends on: summarize_var(), vars_to_check, vars_to_check_adm, fmt_pct (from utils_rules.R)
# Uses data module contract: data$ACTIVE(), data$rules(), data$frames()

mod_quality_plots_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Quality Plots",
    fluidRow(
      column(12,
             h5("Admin — Kelengkapan (Non-missing %)"),
             plotOutput(ns("adm_bar"), height = 350),
             br(),
             h5("Admin — Komposisi Status (%)"),
             plotOutput(ns("adm_stack"), height = 450)
      )
    ),
    hr(),
    fluidRow(
      column(4,
             uiOutput(ns("clin_selector")),
             checkboxInput(ns("show_labels"), "Show numeric labels", value = TRUE)
      ),
      column(8,
             h5(textOutput(ns("clin_title"))),
             plotOutput(ns("clin_interval"), height = 300),
             br(),
             plotOutput(ns("clin_box"), height = 320)
      )
    )
  )
}

mod_quality_plots_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    suppressPackageStartupMessages({
      library(dplyr); library(tidyr); library(ggplot2); library(stringr); library(tibble)
    })
    
    pretty_var <- function(x){
      keep_upper <- c("NIK","BPJS","ID","UGD","IGD","KIA","ANC","MTBS","BMI","SPO2")
      x <- stringr::str_replace_all(x, "[._]+", " ")
      x <- stringr::str_replace_all(x, stringr::regex("\\bTgl\\.?\\b", ignore_case = TRUE), "Tanggal")
      x <- stringr::str_replace_all(x, stringr::regex("\\bNo\\b",      ignore_case = TRUE), "No.")
      x <- stringr::str_squish(x)
      ifelse(toupper(x) %in% keep_upper, toupper(x), tools::toTitleCase(tolower(x)))
    }
    
    # Build adm_tbl / clin_tbl from current ACTIVE() df
    quality_tables <- reactive({
      x <- data$ACTIVE(); req(x)
      
      # 🔑 Ensure friendly aliases ("NIK", "No Telp", etc.) exist
      x <- apply_schema(x)
      
      # ---- Fallback aliases (in case schema mapping didn't find them) ----
      # NIK: prefer masked string; otherwise (as a last resort) short hash preview
      if (!("NIK" %in% names(x))) {
        if ("nik_masked" %in% names(x)) x[["NIK"]] <- x$nik_masked
        else if ("nik_hash" %in% names(x)) x[["NIK"]] <- substr(x$nik_hash, 1, 8)
      }
      # No Telp: prefer masked phone
      if (!("No Telp" %in% names(x))) {
        if ("phone_masked" %in% names(x)) x[["No Telp"]] <- x$phone_masked
        else if ("phone_hash" %in% names(x)) x[["No Telp"]] <- substr(x$phone_hash, 1, 8)
      }
      # Nama Pasien: use presence only (no raw names shown)
      # If exporter created 'name_present' (TRUE/FALSE), turn that into a presence-only string column
      if (!("Nama Pasien" %in% names(x))) {
        if ("name_present" %in% names(x)) {
          x[["Nama Pasien"]] <- ifelse(!is.na(x$name_present) & x$name_present, "present", NA_character_)
        } else if ("nama_pasien" %in% names(x)) {
          # fallback (dev/local only): treat any non-empty value as present (we do NOT display the actual names)
          nm <- as.character(x$nama_pasien)
          x[["Nama Pasien"]] <- ifelse(!is.na(nm) & nzchar(trimws(nm)), "present", NA_character_)
        }
      }
      
      rules <- data$rules()
      ref_date <- as.Date(isolate(data$frames()$resolved_date %||% Sys.Date()))
      
      # Include admin targets explicitly to ensure they show up when present
      vars_target_adm <- unique(c(vars_to_check_adm, "NIK", "Nama Pasien", "No Telp"))
      adm_present  <- intersect(vars_target_adm, names(x))
      clin_present <- intersect(vars_to_check,   names(x))
      
      adm_tbl <- purrr::map_dfr(adm_present,  ~ summarize_var(x, .x, "Admin",   rules, ref_date)) %>%
        dplyr::select(-Role, -Min, -Q1, -Median, -Q3, -Max) %>%
        dplyr::arrange(Variable)
      
      clin_tbl <- purrr::map_dfr(clin_present, ~ summarize_var(x, .x, "Clinical", rules, ref_date)) %>%
        dplyr::select(-Role) %>%
        dplyr::arrange(Variable)
      
      list(adm = adm_tbl, clin = clin_tbl)
    })
    
    # ---- Admin plots ----
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
        geom_text(aes(label = sprintf("%.1f%%", 100*non_missing_rate)),
                  hjust = -0.05, size = 4) +
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
        geom_text(aes(label = ifelse(Rate >= LABEL_THRESH, sprintf("%.1f%%", 100*Rate), "")),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        labs(title = "Admin — Komposisi Status (%)", x = "Persentase", y = NULL) +
        theme_minimal(base_size = 11)
    })
    
    # ---- Clinical plots ----
    output$clin_selector <- renderUI({
      qt <- quality_tables(); clin_tbl <- qt$clin
      vars <- unique(clin_tbl$Variable)
      selectInput(ns("clin_var"), "Clinical variable", choices = vars, selected = head(vars, 1))
    })
    
    output$clin_title <- renderText({
      req(input$clin_var)
      paste0(pretty_var(input$clin_var), " — Ringkasan")
    })
    
    # Interval summary
    output$clin_interval <- renderPlot({
      qt <- quality_tables(); clin_tbl <- qt$clin; rules <- data$rules()
      req(nrow(clin_tbl) > 0, input$clin_var)
      
      row <- clin_tbl %>% dplyr::filter(Variable == input$clin_var)
      req(nrow(row) > 0)
      lbl <- pretty_var(input$clin_var)
      miss_pct  <- ifelse(row$Rows[1] > 0, row$Missing[1] / row$Rows[1], NA_real_)
      inval_pct <- ifelse(row$Non_missing[1] > 0, row$`Invalid (non-missing)`[1] / row$Non_missing[1], NA_real_)
      vr <- rules %>% dplyr::filter(Variable == input$clin_var)
      
      gg <- ggplot(row, aes(y = 0)) +
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
        scale_fill_manual(name = "Keterangan",
                          values = c("Rentang valid (min_ok–max_ok)" = "#E8F1FD")) +
        scale_color_manual(name = "Keterangan",
                           values = c("Min–Maks"="grey40","IQR (Q1–Q3)"="#2C7BE5","Median"="#2C7BE5")) +
        scale_shape_manual(name = "Keterangan", values = c("Median" = 16)) +
        guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2), shape = guide_legend(order = 2)) |
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
      
      gg
    })
    
    # Boxplot summary
    output$clin_box <- renderPlot({
      qt <- quality_tables(); clin_tbl <- qt$clin; rules <- data$rules()
      req(nrow(clin_tbl) > 0, input$clin_var)
      
      row <- clin_tbl %>% dplyr::filter(Variable == input$clin_var)
      req(nrow(row) > 0)
      lbl <- pretty_var(input$clin_var)
      miss_pct  <- ifelse(row$Rows[1] > 0, row$Missing[1] / row$Rows[1], NA_real_)
      inval_pct <- ifelse(row$Non_missing[1] > 0, row$`Invalid (non-missing)`[1] / row$Non_missing[1], NA_real_)
      vr <- rules %>% dplyr::filter(Variable == input$clin_var)
      
      df_plot <- tibble(
        x     = lbl,
        ymin  = row$Min,
        lower = row$Q1,
        middle= row$Median,
        upper = row$Q3,
        ymax  = row$Max
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
        scale_fill_manual(name = "Keterangan",
                          values = c("Rentang valid (min_ok–max_ok)" = "#E8F1FD")) +
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
  })
}
