# R/mod_plot_records.R
# Variables list only (no raw data), fixed 4 columns

mod_table_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Variables",
    fluidRow(
      column(
        width = 12,
        tags$p("Daftar kolom & tipe untuk dataset aktif. Raw data tidak ditampilkan."),
        DT::DTOutput(ns("vars_tbl"))
      )
    )
  )
}

mod_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    suppressPackageStartupMessages({
      library(dplyr); library(stringr); library(tibble); library(DT)
    })
    ns <- session$ns
    
    # Fallback pretty_var if not provided globally
    if (!exists("pretty_var", mode = "function")) {
      pretty_var <- function(x){
        keep_upper <- c("NIK","BPJS","ID","UGD","IGD","KIA","ANC","MTBS","BMI","SPO2")
        x <- stringr::str_replace_all(x, "[._]+", " ")
        x <- stringr::str_replace_all(x, stringr::regex("\\bTgl\\.?\\b", ignore_case = TRUE), "Tanggal")
        x <- stringr::str_replace_all(x, stringr::regex("\\bNo\\b",      ignore_case = TRUE), "No.")
        x <- stringr::str_squish(x)
        ifelse(toupper(x) %in% keep_upper, toupper(x), tools::toTitleCase(tolower(x)))
      }
    }
    
    # Simple type detector
    rich_type <- function(x){
      if (inherits(x, "Date")) "date"
      else if (inherits(x, "POSIXt")) "datetime"
      else if (is.numeric(x)) "numeric"
      else if (is.logical(x)) "logical"
      else if (is.factor(x) || is.character(x)) "character"
      else class(x)[1]
    }
    
    # Build multi-column table WITH index numbers (fixed n_cols = 4)
    as_columns_with_type_index <- function(names_vec, types_vec, n_cols = 4,
                                           idx_label = "No ", name_label = "Kolom ", type_label = "Tipe ") {
      stopifnot(length(names_vec) == length(types_vec))
      n <- length(names_vec)
      if (!n) return(tibble::tibble())
      
      n_rows <- ceiling(n / n_cols)
      pad_n  <- n_rows * n_cols - n
      
      names_pad <- c(names_vec, rep("", pad_n))
      types_pad <- c(types_vec, rep("", pad_n))
      idx_pad   <- c(seq_len(n), rep(NA_integer_, pad_n))
      
      out <- list()
      for (j in seq_len(n_cols)) {
        ix <- ((j - 1) * n_rows + 1):(j * n_rows)
        out[[paste0(idx_label,  j)]] <- ifelse(is.na(idx_pad[ix]), "", idx_pad[ix])
        out[[paste0(name_label, j)]] <- names_pad[ix]
        out[[paste0(type_label, j)]] <- types_pad[ix]
      }
      tibble::as_tibble(out)
    }
    
    vars_table <- reactive({
      x <- data$ACTIVE(); req(x)
      df <- if (is.data.frame(x)) x else if (is.list(x) && !is.null(x$df)) x$df else NULL
      req(is.data.frame(df), ncol(df) > 0)
      
      vars        <- colnames(df)
      types       <- vapply(df, rich_type, character(1))
      vars_pretty <- pretty_var(vars)
      
      as_columns_with_type_index(vars_pretty, types, n_cols = 4)
    })
    
    output$vars_tbl <- DT::renderDT({
      tbl <- vars_table(); req(nrow(tbl) > 0)
      # Right-align all "No " columns
      no_cols_idx <- which(grepl("^No\\s", names(tbl))) - 1  # DT is 0-based
      DT::datatable(
        tbl,
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE,
          autoWidth = TRUE, dom = "t",
          columnDefs = list(list(className = "dt-right", targets = no_cols_idx))
        )
      )
    })
  })
}
