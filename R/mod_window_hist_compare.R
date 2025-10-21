# R/mod_window_hist_compare.R
# Compare histograms for two windows (Current vs Previous) with shared bins/x-axis
# Expects a `data` adapter with:
#   data$frames() -> reactive list containing either:
#       data_range_curr_clean / data_range_prev_clean  (preferred)
#       or data_range_curr / data_range_prev           (raw; we'll clean if helpers exist)
#   data$rules()  -> reactive data.frame or NULL (cols: Variable, min_ok, max_ok)

mod_window_hist_compare_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Window comparison (hist)",
    fluidRow(
      column(
        width = 3,
        uiOutput(ns("var_picker")),
        numericInput(ns("binwidth"), "Bin width", value = 1, min = 0.1, step = 0.1),
        radioButtons(ns("clamp"), "X-axis clamp",
                     choices = c("Data range (both)" = "data",
                                 "Quantiles (both)"  = "quantile",
                                 "Rule limits"       = "rules"),
                     selected = "data"),
        sliderInput(ns("q"), "Quantile window (when clamp = Quantiles)",
                    min = 0, max = 100, value = c(1, 99), step = 1, post = "%"),
        checkboxInput(ns("show_band"), "Shade valid rule band", value = TRUE),
        checkboxInput(ns("show_median"), "Show median line", value = TRUE),
        checkboxInput(ns("show_mean"),   "Show mean line (dotted)", value = FALSE),
        hr(),
        numericInput(ns("breaks_step"), "X tick every … units (optional)", value = 5, min = 1, step = 1),
        checkboxInput(ns("breaks_all_values"), "Tick all distinct values (dense)", value = FALSE),
        sliderInput(ns("x_angle"), "X label angle", min = 0, max = 90, value = 45, step = 5),
        hr(),
        numericInput(ns("top_n"), "Annotate top N bins", value = 5, min = 0, step = 1),
        numericInput(ns("top_digits"), "Annotation % digits", value = 1, min = 0, step = 1),
        checkboxInput(ns("annot_box"), "Use label box", value = TRUE),
        sliderInput(ns("annot_y"), "Annotation Y position (fraction of max)", min = 0.5, max = 0.99, value = 0.92, step = 0.01),
        hr(),
        checkboxInput(ns("lock_y"), "Lock Y-axes (same max)", value = TRUE)
      ),
      column(
        width = 9,
        plotOutput(ns("hist_compare"), height = 800)
      )
    )
  )
}

mod_window_hist_compare_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    suppressPackageStartupMessages({
      library(dplyr); library(tidyr); library(stringr); library(readr)
      library(ggplot2)
    })
    ns <- session$ns
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # ---- helpers ----
    safe_num <- function(x) {
      s <- as.character(x)
      s <- ifelse(stringr::str_detect(s, ",") & !stringr::str_detect(s, "\\."),
                  stringr::str_replace_all(s, ",", "."), s)
      readr::parse_number(s)
    }
    infer_period <- function(df, date_cols = c("Tanggal","day","tanggal"), fmt = "%Y-%m-%d") {
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
    pretty_ps <- function(fr) if (is.na(fr)) "" else fr
    paste_segs <- function(..., sep = " | ") {
      segs <- c(...); segs <- segs[!is.na(segs) & nzchar(segs)]
      if (!length(segs)) "" else paste(segs, collapse = sep)
    }
    
    # Prefer cleaned frames if available; else try to apply schema + keep_in_range (if those helpers exist)
    get_pair_frames <- reactive({
      fr <- data$frames(); req(fr)
      rules_df <- data$rules()  # << data frame or NULL
      
      has_clean <- all(c("data_range_curr_clean","data_range_prev_clean") %in% names(fr))
      if (has_clean) {
        list(curr = fr$data_range_curr_clean, prev = fr$data_range_prev_clean,
             meta = list(
               curr_label = paste0("Current: ", pretty_ps(infer_period(fr$data_range_curr_clean))),
               prev_label = paste0("Previous: ", pretty_ps(infer_period(fr$data_range_prev_clean)))
             ))
      } else {
        curr <- fr$data_range_curr; prev <- fr$data_range_prev
        if (exists("apply_schema", mode = "function")) {
          curr <- apply_schema(curr); prev <- apply_schema(prev)
        }
        if (exists("keep_in_range", mode = "function")) {
          curr <- keep_in_range(curr, rules_df); prev <- keep_in_range(prev, rules_df)
        }
        list(curr = curr, prev = prev,
             meta = list(
               curr_label = paste0("Current: ", pretty_ps(infer_period(curr))),
               prev_label = paste0("Previous: ", pretty_ps(infer_period(prev)))
             ))
      }
    })
    
    # Variable choices = present in BOTH frames; prefer those listed in valid_rules
    output$var_picker <- renderUI({
      pr <- get_pair_frames(); req(pr$curr, pr$prev)
      rules_df <- data$rules()
      nms_both <- intersect(names(pr$curr), names(pr$prev))
      if (!is.null(rules_df)) {
        vars <- intersect(rules_df$Variable, nms_both)
      } else {
        keep <- vapply(nms_both, function(nm) {
          v1 <- pr$curr[[nm]]; v2 <- pr$prev[[nm]]
          is.numeric(v1) || is.numeric(v2) ||
            any(grepl("\\d", head(as.character(v1 %||% ""), 50))) ||
            any(grepl("\\d", head(as.character(v2 %||% ""), 50)))
        }, logical(1))
        vars <- nms_both[keep]
      }
      selectInput(ns("var"), "Variable", choices = vars, selected = head(vars, 1))
    })
    
    output$hist_compare <- renderPlot({
      pr <- get_pair_frames(); req(pr$curr, pr$prev, input$var)
      rules_df <- data$rules()
      
      # numeric vectors
      x_curr <- safe_num(pr$curr[[input$var]])
      x_prev <- safe_num(pr$prev[[input$var]])
      x_curr <- x_curr[is.finite(x_curr)]
      x_prev <- x_prev[is.finite(x_prev)]
      req(length(x_curr) + length(x_prev) > 0)
      
      # clamp window (combined)
      clamp <- input$clamp
      vr <- if (!is.null(rules_df)) dplyr::filter(rules_df, Variable == input$var) else tibble::tibble()
      if (clamp == "rules" && nrow(vr) == 1) {
        xlim <- c(vr$min_ok[1], vr$max_ok[1])
      } else if (clamp == "quantile") {
        qs <- sort(pmin(pmax(input$q, 0), 100)) / 100
        both <- c(x_curr, x_prev)
        qq <- stats::quantile(both, probs = qs, na.rm = TRUE)
        xlim <- c(qq[[1]], qq[[2]])
      } else {
        rng <- range(c(x_curr, x_prev), na.rm = TRUE)
        xlim <- c(rng[1], rng[2])
      }
      span <- diff(xlim); pad <- 0.03
      xlim <- xlim + c(-1, 1) * (span * pad)
      
      # shared breaks
      binwidth <- input$binwidth
      if (is.null(binwidth) || !is.finite(binwidth) || binwidth <= 0) binwidth <- 1
      start <- floor(xlim[1] / binwidth) * binwidth
      end   <- ceiling(xlim[2] / binwidth) * binwidth
      brks  <- seq(start, end + binwidth, by = binwidth)
      
      # restrict to window & counts
      dvC <- tibble::tibble(Value = x_curr) %>% dplyr::filter(Value >= xlim[1], Value <= xlim[2])
      dvP <- tibble::tibble(Value = x_prev) %>% dplyr::filter(Value >= xlim[1], Value <= xlim[2])
      
      hC <- hist(dvC$Value, breaks = brks, plot = FALSE, right = FALSE, include.lowest = TRUE)
      hP <- hist(dvP$Value, breaks = brks, plot = FALSE, right = FALSE, include.lowest = TRUE)
      NC <- sum(hC$counts); NP <- sum(hP$counts)
      yC <- if (length(hC$counts)) max(hC$counts) else 1
      yP <- if (length(hP$counts)) max(hP$counts) else 1
      yMax <- if (isTRUE(input$lock_y)) max(yC, yP) else NA
      
      mk_top_text <- function(h) {
        counts <- h$counts; br <- h$breaks; N <- sum(counts)
        if (isTRUE(input$top_n > 0) && N > 0 && length(counts)) {
          ord <- order(counts, decreasing = TRUE)
          ord <- ord[counts[ord] > 0][seq_len(min(input$top_n, length(ord)))]
          paste(vapply(ord, function(i) {
            left <- br[i]; right <- br[i+1]
            lab  <- if (abs(binwidth - 1) < 1e-9) sprintf("%.0f", left) else sprintf("%.0f–%.0f", left, right)
            pct  <- round(100 * counts[i] / N, input$top_digits)
            paste0(lab, ": ", counts[i], " (", format(pct, nsmall = input$top_digits), "%)")
          }, character(1)), collapse = "\n")
        } else ""
      }
      txtC <- mk_top_text(hC); txtP <- mk_top_text(hP)
      
      # x breaks
      xb_vals <- NULL; xb_fun <- NULL
      if (isTRUE(input$breaks_all_values)) {
        xb_vals <- sort(unique(c(dvC$Value, dvP$Value)))
      } else if (!is.null(input$breaks_step) && is.finite(input$breaks_step) && input$breaks_step > 0) {
        xb_vals <- seq(floor(xlim[1]), ceiling(xlim[2]), by = input$breaks_step)
      } else {
        if (requireNamespace("scales", quietly = TRUE)) xb_fun <- scales::breaks_extended(6) else xb_fun <- function(x) pretty(x, 6)
      }
      
      # titles
      meta <- get_pair_frames()$meta
      ttlC <- paste_segs(paste0("Variabel: ", input$var), meta$curr_label)
      ttlP <- paste_segs(paste0("Variabel: ", input$var), meta$prev_label)
      subC <- paste_segs(paste0("N=", NC), if (nrow(vr) == 1) paste0("Rule [", vr$min_ok[1], "–", vr$max_ok[1], "]") else NA,
                         paste0("Bin size = ", format(round(binwidth, 2), trim = TRUE)))
      subP <- paste_segs(paste0("N=", NP), if (nrow(vr) == 1) paste0("Rule [", vr$min_ok[1], "–", vr$max_ok[1], "]") else NA,
                         paste0("Bin size = ", format(round(binwidth, 2), trim = TRUE)))
      
      mk_plot <- function(dv, ttl, sub, ymax) {
        p <- ggplot(dv, aes(x = Value)) +
          { if (isTRUE(input$show_band) && nrow(vr) == 1)
            annotate("rect",
                     xmin = max(xlim[1], vr$min_ok[1]), xmax = min(xlim[2], vr$max_ok[1]),
                     ymin = 0, ymax = Inf, fill = "#E8F1FD") } +
          geom_histogram(binwidth = binwidth, boundary = brks[1],
                         fill = "#69b3a2", color = "#e9ecef", alpha = 0.9, closed = "left") +
          {
            if (!is.null(xb_vals)) scale_x_continuous(limits = xlim, breaks = xb_vals, minor_breaks = NULL)
            else                   scale_x_continuous(limits = xlim, breaks = xb_fun)
          } +
          labs(title = ttl, subtitle = sub, x = input$var, y = "Frekuensi") +
          theme_minimal(base_size = 12)
        if (!is.na(ymax)) p <- p + coord_cartesian(ylim = c(0, ymax))
        if (!is.null(input$x_angle)) {
          p <- p + theme(axis.text.x = element_text(angle = input$x_angle, hjust = 1, vjust = 1))
        }
        if (exists("guide_axis")) p <- p + guides(x = guide_axis(check.overlap = TRUE))
        if (isTRUE(input$show_median))
          p <- p + geom_vline(xintercept = stats::median(dv$Value, na.rm = TRUE),
                              linetype = "dashed", linewidth = 0.6, colour = "#2C7BE5")
        if (isTRUE(input$show_mean))
          p <- p + geom_vline(xintercept = mean(dv$Value, na.rm = TRUE),
                              linetype = "dotted", linewidth = 0.6, colour = "grey40")
        p
      }
      
      p_top <- mk_plot(dvC, ttlC, subC, if (isTRUE(input$lock_y)) yMax else NA)
      p_bot <- mk_plot(dvP, ttlP, subP, if (isTRUE(input$lock_y)) yMax else NA)
      
      # annotations
      add_annot <- function(p, txt, ymax_here) {
        if (!is.null(txt) && nzchar(txt)) {
          y_annot <- (ymax_here %||% 1) * (input$annot_y %||% 0.92)
          if (isTRUE(input$annot_box)) {
            p + annotate("label", x = xlim[2], y = y_annot, hjust = 1, vjust = 1,
                         label = txt, size = 3.2, label.size = 0, fill = "white", alpha = 0.8)
          } else {
            p + annotate("text",  x = xlim[2], y = y_annot, hjust = 1, vjust = 1,
                         label = txt, size = 3.2)
          }
        } else p
      }
      p_top <- add_annot(p_top, txtC, if (isTRUE(input$lock_y)) yMax else yC)
      p_bot <- add_annot(p_bot, txtP, if (isTRUE(input$lock_y)) yMax else yP)
      
      if (requireNamespace("patchwork", quietly = TRUE)) {
        (p_top / p_bot) + patchwork::plot_layout(heights = c(1, 1))
      } else if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::grid.arrange(p_top, p_bot, ncol = 1)
      } else {
        print(p_top); p_bot
      }
    })
  })
}
