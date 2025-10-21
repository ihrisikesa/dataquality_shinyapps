mod_data_server <- function(id, con, puskesmas_r, selected_date_r, date_range_r, scope_r, rules_file_r, run_r) {
  moduleServer(id, function(input, output, session) {

    core_tbl <- reactive({
      req(puskesmas_r())
      core_tbl_for_pkm(con, puskesmas_r())
    })

    avail_days <- reactive({
      core_tbl() |> dplyr::filter(!is.na(day)) |> dplyr::distinct(day) |> dplyr::arrange(day) |> dplyr::collect() |> dplyr::pull(day)
    })

    resolved_date <- reactive({
      req(avail_days())
      sel <- as.Date(selected_date_r()); ad <- avail_days()
      if (!length(ad)) return(NA_Date_)
      if (sel %in% ad) return(sel)
      prev <- ad[ad <= sel]; next <- ad[ad >= sel]
      prev_day <- if (length(prev)) max(prev) else as.Date(NA)
      next_day <- if (length(next)) min(next) else as.Date(NA)
      if (!is.na(prev_day) && !is.na(next_day)) {
        if (abs(as.numeric(sel - prev_day)) <= abs(as.numeric(next_day - sel))) prev_day else next_day
      } else if (!is.na(prev_day)) prev_day else if (!is.na(next_day)) next_day else as.Date(NA)
    })

    frames <- eventReactive(run_r(), {
      ct <- core_tbl(); rd <- resolved_date()

      latest_file_for_day <- ct |>
        dplyr::filter(day == !!rd) |>
        dplyr::mutate(file_ts_fallback = coalesce(file_ts, tanggal_ts)) |>
        dplyr::filter(!is.na(file_ts_fallback)) |>
        dplyr::group_by(file_name) |>
        dplyr::summarise(file_ts_fallback = max(file_ts_fallback), .groups = "drop") |>
        dplyr::slice_max(order_by = file_ts_fallback, n = 1, with_ties = FALSE) |>
        dplyr::collect()

      data_df <- ct |> dplyr::filter(day == !!rd, file_name == !!latest_file_for_day$file_name) |> dplyr::collect()

      r <- as.Date(date_range_r()[1]); e <- as.Date(date_range_r()[2])
      rng <- sort(c(r, e)); r <- rng[1]; e <- rng[2]
      len_days <- as.integer(e - r) + 1L
      prev_end <- r - 1; prev_start <- prev_end - (len_days - 1L)

      latest_curr <- latest_file_per_day_between(ct, r, e)
      latest_prev <- latest_file_per_day_between(ct, prev_start, prev_end)

      data_range_curr <- ct |> dplyr::inner_join(latest_curr |> dplyr::select(day, file_name), by = c("day","file_name")) |> dplyr::mutate(period="Current") |> dplyr::collect()
      data_range_prev <- ct |> dplyr::inner_join(latest_prev |> dplyr::select(day, file_name), by = c("day","file_name")) |> dplyr::mutate(period="Previous") |> dplyr::collect()

      week_start <- lubridate::floor_date(as.Date(rd), unit = "week", week_start = 1)
      week_end   <- week_start + lubridate::days(6)
      latest_week <- latest_file_per_day_between(ct, week_start, week_end)
      data_week <- ct |> dplyr::inner_join(latest_week |> dplyr::select(day, file_name), by = c("day","file_name")) |> dplyr::collect()

      month_start <- as.Date(sprintf("%04d-%02d-01", lubridate::year(rd), lubridate::month(rd)))
      month_end   <- as.Date(lubridate::ceiling_date(month_start, "month") - 1)
      latest_month <- latest_file_per_day_between(ct, month_start, month_end)
      data_month <- ct |> dplyr::inner_join(latest_month |> dplyr::select(day, file_name), by = c("day","file_name")) |> dplyr::collect()

      list(
        data_df = data_df,
        data_range_curr = data_range_curr,
        data_range_prev = data_range_prev,
        data_week = data_week,
        data_month = data_month,
        resolved_date = rd, range_start = r, range_end = e,
        prev_start = prev_start, prev_end = prev_end,
        week_start = week_start, week_end = week_end,
        month_start = month_start, month_end = month_end
      )
    }, ignoreInit = FALSE)

    rules <- reactive(valid_rules_from_upload(rules_file_r()))
    af <- reactive({
      fr <- frames(); rw <- rules()
      list(
        daily      = keep_in_range(apply_schema(fr$data_df),        rw),
        range_curr = keep_in_range(apply_schema(fr$data_range_curr), rw),
        range_prev = keep_in_range(apply_schema(fr$data_range_prev), rw),
        week       = keep_in_range(apply_schema(fr$data_week),       rw),
        month      = keep_in_range(apply_schema(fr$data_month),      rw),
        meta       = fr
      )
    })

    label <- reactive({
      m <- af()$meta; sc <- scope_r()
      switch(sc,
        "range" = sprintf("Current window: %s → %s", fmt_date(m$range_start), fmt_date(m$range_end)),
        "range_pair" = sprintf("Current: %s → %s | Previous: %s → %s",
                               fmt_date(m$range_start), fmt_date(m$range_end),
                               fmt_date(m$prev_start),  fmt_date(m$prev_end)),
        "week"  = sprintf("Week: %s → %s",  fmt_date(m$week_start),  fmt_date(m$week_end)),
        "month" = sprintf("Month: %s → %s", fmt_date(m$month_start), fmt_date(m$month_end)),
        sprintf("Selected day: %s", fmt_date(m$resolved_date))
      )
    })

    ACTIVE <- reactive({
      a <- af(); sc <- scope_r()
      switch(sc,
        "range" = a$range_curr,
        "range_pair" = dplyr::bind_rows(dplyr::mutate(a$range_curr, period="Current"),
                                        dplyr::mutate(a$range_prev, period="Previous")),
        "week"  = a$week,
        "month" = a$month,
        a$daily
      )
    })

    list(
      ACTIVE = ACTIVE, frames = frames, label = label,
      rules = rules, scope = scope_r
    )
  })
}
