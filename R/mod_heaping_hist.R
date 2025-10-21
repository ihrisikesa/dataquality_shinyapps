# R/mod_heaping_hist.R
# Heaping histogram module (single-variable, highly configurable)
# Contract: expects a `data` adapter with:
#   data$ACTIVE() -> reactive data.frame (already schema-applied / cleaned)
#   data$rules()  -> reactive data.frame or NULL (columns: Variable, min_ok, max_ok)
#   data$frames() -> reactive list with window metadata (resolved_date, range_start, range_end, etc.)

mod_heaping_hist_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Heaping histogram",
    fluidRow(
      column(
        width = 3,
        uiOutput(ns("var_picker")),
        numericInput(ns("binwidth"), "Bin width", value = 1, min = 0.1, step = 0.1),
        radioButtons(ns("clamp"), "X-axis clamp",
                     choices = c("Data range" = "data",
                                 "Quantiles" = "quantile",
                                 "Rule limits" = "rules"),
                     selected = "data"),
        sliderInput(ns("q"), "Quantile window (when clamp = Quantiles)",
                    min = 0, max = 100, value = c(1, 99), step = 1, post = "%"),
        checkboxInput(ns("show_band"), "Shade valid rule band", value = TRUE),
        checkboxInput(ns("show_median"), "Show median line", value = TRUE),
        checkboxInput(ns("show_mean"),   "Show mean line (dotted)", value = FALSE),
        hr(),
        numericInput(ns("breaks_step"), "X tick every … units (optional)", value = 5, min = 1, step = 1),
        checkboxInput(ns("breaks_all_values"), "Tick all distinct values (warning: dense)", value = FALSE),
        sliderInput(ns("x_angle"), "X label angle", min = 0, max = 90, value = 45, step = 5),
        hr(),
        numericInput(ns("top_n"), "Annotate top N bins", value = 5, min = 0, step = 1),
        numericInput(ns("top_digits"), "Annotation % digits", value = 1, min = 0, step = 1),
        checkboxInput(ns("annot_box"), "Use label box", value = TRUE),
        sliderInput(ns("annot_y"), "Annotation Y position (fraction of max)", min = 0.5, max = 0.99, value = 0.92, step = 0.01)
      ),
      column(
        width = 9,
        plotOutput(ns("hist"), height = 520)
      )
    )
  )
}

mod_heaping_hist_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    suppressPackageStartupMessages({
      library(dplyr); library(tidyr); library(stringr); library(readr)
      library(ggplot2)
    })
    ns <- session$ns
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # ---------- helpers ----------
    safe_num <- function(x) {
      s <- as.character(x)
      s <- ifelse(stringr::str_detect(s, ",") & !stringr::str_detect(s, "\\."),
                  stringr::str_replace_all(s, ",", "."), s)
      readr::parse_number(s)
    }
    fd_binwidth <- function(x) {
      x <- x[is.finite(x)]
      n <- length(x); if (n < 2) return(NA_real_)
      bw <- 2 * IQR(x) / (n^(1/3))
      if (!is.finite(bw) || bw <= 0) {
        rng <- diff(range(x))
        if (!is.finite(rng) || rng <= 0) return(NA_real_)
        rng / 30
      } else bw
    }
    paste_segs <- function(..., sep = " | ") {
      segs <- c(...)
      segs <- segs[!is.na(segs) & nzchar(segs)]
      if (!length(segs)) "" else paste(segs, collapse = sep)
    }
    infer_pusk <- function(df, col = "puskesmas_name") {
      if (!col %in% names(df)) return(NA_character_)
      vals <- unique(na.omit(df[[col]]))
      if (!length(vals)) return(NA_character_)
      if (length(vals) == 1) as.character(vals) else "Multiple"
    }
    infer_period <- function(df, date_cols = c("day","Tanggal","tanggal"), fmt = "%Y-%m-%d") {
      dcol <- intersect(date_cols, names(df))[1]
      if (is.na(dcol)) return(NA_character_)
      d <- df[[dcol]]
      if (inherits(d, "POSIXt")) d <- as.Date(d)
      if (!inherits(d, "Date"))  d <- suppressWarnings(as.Date(d))
      d <- d[!is.na(d)]
      if (!length(d)) return(NA_character_)
      rng <- range(d, na.rm = TRUE)
      if (identical(rng[1], rng[2])) format(rng[1], fmt) else paste(format(rng[1], fmt), "–", format(rng[2], fmt))
    }
    
    # ---------- variable choices ----------
    output$var_picker <- renderUI({
      x <- data$ACTIVE(); req(x)
      rules <- data$rules()
      
      # Prefer variables from valid_rules, else attempt numeric-like cols
      vars <- if (!is.null(rules)) {
        intersect(rules$Variable, names(x))
      } else {
        nms <- names(x)
        # crude numeric-like detector using sample
        keep <- vapply(nms, function(nm) {
          v <- x[[nm]]
          is.numeric(v) || any(grepl("\\d", head(as.character(v %||% ""), 100L)))
        }, logical(1))
        nms[keep]
      }
      selectInput(ns("var"), "Variable", choices = vars, selected = head(vars, 1))
    })
    
    # ---------- the plot ----------
    output$hist <- renderPlot({
      df <- data$ACTIVE(); req(df, input$var)
      rules <- data$rules()
      
      # numeric vector
      val <- safe_num(df[[input$var]])
      val <- val[is.finite(val)]
      req(length(val) > 0)
      
      # clamp window
      clamp <- input$clamp
      vr <- if (!is.null(rules)) rules %>% dplyr::filter(Variable == input$var) else tibble::tibble()
      if (clamp == "rules" && nrow(vr) == 1) {
        xlim <- c(vr$min_ok[1], vr$max_ok[1])
      } else if (clamp == "quantile") {
        qs  <- sort(pmin(pmax(input$q, 0), 100)) / 100
        qq  <- stats::quantile(val, probs = qs, na.rm = TRUE)
        xlim <- c(qq[[1]], qq[[2]])
      } else {
        rng <- range(val, na.rm = TRUE)
        xlim <- c(rng[1], rng[2])
      }
      # small padding
      span <- diff(xlim); pad <- 0.03
      xlim <- xlim + c(-1, 1) * (span * pad)
      
      # choose bins
      binwidth <- input$binwidth
      if (is.null(binwidth) || !is.finite(binwidth) || binwidth <= 0) {
        bw_est <- fd_binwidth(val)
        binwidth <- if (is.finite(bw_est) && bw_est > 0) bw_est else 1
      }
      # aligned breaks
      start <- floor(xlim[1] / binwidth) * binwidth
      end   <- ceiling(xlim[2] / binwidth) * binwidth
      breaks_seq <- seq(start, end + binwidth, by = binwidth)
      
      # keep only within xlim for plotting
      dv <- tibble::tibble(Value = val) %>% dplyr::filter(Value >= xlim[1], Value <= xlim[2])
      
      # histogram counts (to compute top-N)
      h <- hist(dv$Value, breaks = breaks_seq, plot = FALSE, right = FALSE, include.lowest = TRUE)
      counts <- h$counts; brks <- h$breaks
      N <- sum(counts); ymax <- if (length(counts)) max(counts) else 1
      
      # annotation text for top-N
      top_text <- NULL
      if (isTRUE(input$top_n > 0) && N > 0 && length(counts)) {
        ord <- order(counts, decreasing = TRUE)
        ord <- ord[counts[ord] > 0][seq_len(min(input$top_n, length(ord)))]
        top_text <- paste(vapply(ord, function(i) {
          left <- brks[i]; right <- brks[i+1]
          lab  <- if (abs(binwidth - 1) < 1e-9) sprintf("%.0f", left) else sprintf("%.0f–%.0f", left, right)
          pct  <- round(100 * counts[i] / N, input$top_digits)
          paste0(lab, ": ", counts[i], " (", format(pct, nsmall = input$top_digits), "%)")
        }, character(1)), collapse = "\n")
      }
      
      # x breaks
      xb_vals <- NULL; xb_fun <- NULL
      if (isTRUE(input$breaks_all_values)) {
        xb_vals <- sort(unique(dv$Value))
      } else if (!is.null(input$breaks_step) && is.finite(input$breaks_step) && input$breaks_step > 0) {
        xb_vals <- seq(floor(xlim[1]), ceiling(xlim[2]), by = input$breaks_step)
      } else {
        if (requireNamespace("scales", quietly = TRUE)) {
          xb_fun <- scales::breaks_extended(6)
        } else {
          xb_fun <- function(x) pretty(x, 6)
        }
      }
      
      # title bits
      pusk_label   <- infer_pusk(df)
      period_label <- infer_period(df)
      title_text   <- paste_segs(
        paste0("Variabel: ", input$var),
        if (!is.na(pusk_label))  paste0("Puskesmas: ", pusk_label)  else NA,
        if (!is.na(period_label)) paste0("Periode: ", period_label) else NA
      )
      subtitle_text <- paste_segs(
        paste0("N=", N),
        if (nrow(vr) == 1) paste0("Rentang valid [", vr$min_ok[1], "–", vr$max_ok[1], "]") else NA,
        paste0("Bin size = ", format(round(binwidth, 2), trim = TRUE))
      )
      
      # plot
      p <- ggplot(dv, aes(x = Value)) +
        { if (isTRUE(input$show_band) && nrow(vr) == 1)
          annotate("rect",
                   xmin = max(xlim[1], vr$min_ok[1]), xmax = min(xlim[2], vr$max_ok[1]),
                   ymin = 0, ymax = Inf, fill = "#E8F1FD") } +
        geom_histogram(binwidth = binwidth, boundary = breaks_seq[1],
                       fill = "#69b3a2", color = "#e9ecef", alpha = 0.9,
                       closed = "left") +
        {
          if (!is.null(xb_vals))
            scale_x_continuous(limits = xlim, breaks = xb_vals, minor_breaks = NULL)
          else
            scale_x_continuous(limits = xlim, breaks = xb_fun)
        } +
        labs(title = title_text, subtitle = subtitle_text, x = input$var, y = "Frekuensi") +
        theme_minimal(base_size = 12)
      
      if (!is.null(input$x_angle)) {
        p <- p + theme(axis.text.x = element_text(angle = input$x_angle, hjust = 1, vjust = 1))
      }
      if (exists("guide_axis")) {
        p <- p + guides(x = guide_axis(check.overlap = TRUE))
      }
      
      # annotation label
      if (!is.null(top_text) && nzchar(top_text)) {
        y_annot <- ymax * (input$annot_y %||% 0.92)
        if (isTRUE(input$annot_box)) {
          p <- p + annotate("label",
                            x = xlim[2], y = y_annot, hjust = 1, vjust = 1,
                            label = top_text, size = 3.2,
                            label.size = 0, fill = "white", alpha = 0.8)
        } else {
          p <- p + annotate("text",
                            x = xlim[2], y = y_annot, hjust = 1, vjust = 1,
                            label = top_text, size = 3.2)
        }
      }
      
      # reference lines
      if (isTRUE(input$show_median))
        p <- p + geom_vline(xintercept = stats::median(dv$Value, na.rm = TRUE),
                            linetype = "dashed", linewidth = 0.6, colour = "#2C7BE5")
      if (isTRUE(input$show_mean))
        p <- p + geom_vline(xintercept = mean(dv$Value, na.rm = TRUE),
                            linetype = "dotted", linewidth = 0.6, colour = "grey40")
      
      p
    })
  })
}
