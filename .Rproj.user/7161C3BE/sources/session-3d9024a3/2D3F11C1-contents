# app.R
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(ggplot2)
})

# Match the actual file name (case-sensitive on Linux/macOS)
source("global.R")   # defines: con, clin_map, adm_map, get_puskesmas_choices(), sources R/*.R

# Small, safe coalescer for UI text
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) {
    if (!is.null(a) && length(a) > 0 && !(is.character(a) && identical(a, ""))) a else b00
  }
}

# Local "today" helper (avoid UTC surprises on shinyapps.io)
local_today <- function(tz = Sys.getenv("APP_TZ", "Asia/Makassar")) {
  as.Date(format(Sys.time(), tz = tz))
}
TODAY <- local_today()

ui <- page_fluid(
  theme = bs_theme(bootswatch = "cosmo"),
  titlePanel("Laporan Monitoring Kualitas Data"),
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h4("Controls"),
      textInput("district", "District", value = "Lombok Barat"),
      selectizeInput("puskesmas", "Puskesmas", choices = NULL, multiple = FALSE),
      radioButtons(
        "data_scope", "Data scope",
        choices = list(
          "Selected day"                 = "daily",
          "Week"                         = "week",
          "Month"                        = "month",
          "Range (current only)"         = "range",
          "Range pair (current vs prev)" = "range_pair"
        ),
        selected = "daily"
      ),
      
      dateInput(
        "selected_date", "Selected date",
        value = TODAY - 1,                  # <-- yesterday
        format = "yyyy-mm-dd", weekstart = 1
      ),
      dateRangeInput(
        "date_range", "Date range",
        start = TODAY - 6,                  # last 7 days
        end   = TODAY,
        format = "yyyy-mm-dd"
      ),
      actionButton("run", "Run", class = "btn btn-primary w-100")
    ),
    card(
      card_header(h4(textOutput("subtitle"))),
      tabsetPanel(
        mod_plot_records_ui("vars"),
        mod_quality_plots_ui("qplots"),
        mod_heaping_hist_ui("heap"),
        mod_window_hist_compare_ui("histcmp"),
        mod_heaping_trend_ui("heaptrend"),
        mod_quality_summary_ui("qsum"),
        tabPanel("Heaping Debug & Export", mod_heaping_debug_ui("heapdebug"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Populate Puskesmas once; handle empty/error case gracefully
  observe({
    choices_chr <- tryCatch(get_puskesmas_choices(con), error = function(e) character(0))
    # label with names if present, otherwise use values as labels
    lab <- if (is.null(names(choices_chr))) choices_chr else names(choices_chr)
    choices_list <- setNames(as.list(choices_chr), lab)
    
    if (length(choices_list)) {
      updateSelectizeInput(session, "puskesmas",
                           choices = choices_list,
                           selected = choices_chr[[1]],
                           server = TRUE)
    } else {
      updateSelectizeInput(session, "puskesmas",
                           choices = list("(no facilities found)" = ""),
                           server = TRUE)
    }
  })
  
  
  
  # Central data provider (server-only module)
  data <- mod_data_server(
    id              = "data",
    con             = con,
    puskesmas_r     = reactive(input$puskesmas),
    selected_date_r = reactive(input$selected_date),
    date_range_r    = reactive(input$date_range),
    scope_r         = reactive(input$data_scope),
    rules_file_r    = reactive(NULL),
    run_r           = reactive(input$run)
  )
  
  output$subtitle <- renderText({
    req(data$label())
    paste0(
      "Facility: ", (input$puskesmas %||% "—"),
      " | District: ", (input$district %||% "—"),
      " | ", data$label()
    )
  })
  
  # Wire feature modules
  mod_plot_records_server("vars",           data)
  mod_quality_plots_server("qplots",        data)
  mod_heaping_hist_server("heap",           data)
  mod_window_hist_compare_server("histcmp", data)
  mod_heaping_trend_server("heaptrend",     data)
  mod_quality_summary_server("qsum",        data)
  mod_heaping_debug_server("heapdebug",     data)
}

shinyApp(ui, server)
