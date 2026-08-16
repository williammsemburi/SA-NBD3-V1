# Highcharter builders for the NBD3 results report ----------------------------

nbd_hc_theme <- function(config = NULL) {
  colours <- if (!is.null(config)) {
    unname(unlist(config$labels$models$colours, use.names = FALSE))
  } else {
    c("#0071BC", "#D48A16", "#1B998B", "#7A5195", "#7C858E")
  }
  highcharter::hc_theme(
    colors = colours,
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Lato, Aptos, Inter, Segoe UI, Arial, sans-serif")
    ),
    title = list(style = list(color = "#14324a", fontSize = "16px", fontWeight = "600")),
    subtitle = list(style = list(color = "#66727d", fontSize = "12px")),
    xAxis = list(
      lineColor = "#d9dee3", tickColor = "#d9dee3", gridLineColor = "#f0f2f4",
      labels = list(style = list(color = "#52697d", fontSize = "11px")),
      title = list(style = list(color = "#52697d", fontWeight = "500"))
    ),
    yAxis = list(
      lineColor = "#d9dee3", gridLineColor = "#eceff1",
      labels = list(style = list(color = "#52697d", fontSize = "11px")),
      title = list(style = list(color = "#52697d", fontWeight = "500"))
    ),
    legend = list(
      itemStyle = list(color = "#29465b", fontWeight = "500", fontSize = "11px"),
      itemHoverStyle = list(color = "#0071bc")
    ),
    tooltip = list(
      backgroundColor = "rgba(255,255,255,0.98)", borderColor = "#d9dee3",
      borderRadius = 8, shadow = TRUE,
      style = list(color = "#183b56", fontSize = "12px")
    ),
    credits = list(enabled = FALSE)
  )
}


nbd_hc_animation_enabled <- function() {
  isTRUE(getOption("nbd3.highcharts.animation", TRUE))
}

nbd_hc_animation <- function(duration = 500L, defer = 0L) {
  if (!nbd_hc_animation_enabled()) return(FALSE)
  list(
    duration = as.integer(duration),
    defer = as.integer(defer),
    easing = "easeInOutSine"
  )
}

hc_export_data_available <- function() {
  nzchar(system.file(
    "htmlwidgets/lib/highcharts/modules/export-data.js",
    package = "highcharter"
  ))
}

finish_nbd_hc <- function(
    hc,
    config = NULL,
    height = 440,
    filename = "nbd3-chart") {
  if (is.null(hc)) return(NULL)
  hc <- highcharter::hc_add_theme(hc, nbd_hc_theme(config))
  hc <- highcharter::hc_chart(
    hc,
    animation = nbd_hc_animation(duration = 300L)
  )
  hc <- highcharter::hc_credits(hc, enabled = FALSE)
  if (hc_export_data_available()) {
    hc <- highcharter::hc_add_dependency(hc, "modules/export-data.js")
    hc <- highcharter::hc_exporting(
      hc,
      enabled = TRUE,
      filename = filename,
      buttons = list(contextButton = list(menuItems = list(
        "downloadPNG", "downloadSVG", "separator", "downloadCSV", "downloadXLS"
      )))
    )
  } else {
    hc <- highcharter::hc_exporting(hc, enabled = TRUE, filename = filename)
  }
  hc <- highcharter::hc_size(hc, height = height)
  hc$x$fonts <- character(0)
  hc$x$hc_opts$exporting$fallbackToExportServer <- FALSE
  hc$x$hc_opts$lang <- list(thousandsSep = ",", decimalPoint = ".")
  hc
}

hc_points_xy <- function(x, y) {
  Map(function(a, b) list(x = as.numeric(a), y = as.numeric(b)), x, y)
}

hc_points_range <- function(x, low, high) {
  Map(
    function(a, b, c) list(x = as.numeric(a), low = as.numeric(b), high = as.numeric(c)),
    x, low, high
  )
}

empty_nbd_chart <- function(message = "No data are available for this selection.", height = 360) {
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line", height = height) |>
    highcharter::hc_title(
      text = message,
      align = "center",
      verticalAlign = "middle",
      style = list(color = "#66727d", fontSize = "13px", fontWeight = "500")
    ) |>
    highcharter::hc_xAxis(visible = FALSE) |>
    highcharter::hc_yAxis(visible = FALSE) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(enabled = FALSE)
  finish_nbd_hc(hc, height = height, filename = "no-data")
}

series_colour <- function(series, config, index = 1L) {
  colours <- unlist(config$labels$models$colours, use.names = TRUE)
  if (series %in% names(colours)) return(unname(colours[[series]]))
  fallback <- c("#0071BC", "#1B998B", "#D48A16", "#B23A48", "#7A5195", "#7C858E")
  fallback[[1L + ((index - 1L) %% length(fallback))]]
}

series_dash <- function(series, config) {
  styles <- unlist(config$labels$models$line_types, use.names = TRUE)
  if (series %in% names(styles)) unname(styles[[series]]) else "Solid"
}

hc_nbd_lines <- function(
    data,
    x = "year",
    y = "estimate",
    series = "model",
    lower = NULL,
    upper = NULL,
    title = NULL,
    subtitle = NULL,
    x_title = NULL,
    y_title = NULL,
    config,
    decimals = 1,
    suffix = "",
    height = 440,
    filename = "nbd3-trend",
    series_order = NULL,
    markers = FALSE,
    zero_line = FALSE) {
  x_col <- as.character(x)[[1L]]
  y_col <- as.character(y)[[1L]]
  series_col <- as.character(series)[[1L]]
  lower_col <- if (is.null(lower)) NULL else as.character(lower)[[1L]]
  upper_col <- if (is.null(upper)) NULL else as.character(upper)[[1L]]

  required_columns <- c(x_col, y_col, series_col)
  xdt <- data.table::as.data.table(data.table::copy(data))
  if (!nrow(xdt) || !all(required_columns %in% names(xdt))) {
    return(empty_nbd_chart(height = height))
  }

  finite_rows <- is.finite(xdt[[x_col]]) & is.finite(xdt[[y_col]])
  xdt <- xdt[finite_rows]
  if (!nrow(xdt)) return(empty_nbd_chart(height = height))

  plot_lines <- if (isTRUE(zero_line)) {
    list(list(value = 0, color = "#9aa2a9", width = 1, dashStyle = "ShortDash"))
  } else NULL

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line", zoomType = "x", spacing = c(12, 12, 8, 10)) |>
    highcharter::hc_title(text = title) |>
    highcharter::hc_subtitle(text = subtitle) |>
    highcharter::hc_xAxis(
      title = list(text = x_title),
      crosshair = list(width = 1, color = "#9fb3c3", dashStyle = "ShortDot")
    ) |>
    highcharter::hc_yAxis(title = list(text = y_title), plotLines = plot_lines) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      valueDecimals = decimals,
      valueSuffix = suffix
    ) |>
    highcharter::hc_plotOptions(series = list(
      animation = nbd_hc_animation(duration = 550L),
      animationLimit = 10000L,
      lineWidth = 2.5,
      marker = list(enabled = markers, radius = 3),
      states = list(inactive = list(opacity = 0.18), hover = list(lineWidthPlus = 1))
    )) |>
    highcharter::hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")

  values <- unique(as.character(xdt[[series_col]]))
  values <- values[!is.na(values) & nzchar(values)]
  if (!is.null(series_order)) {
    values <- c(intersect(series_order, values), setdiff(values, series_order))
  }

  for (i in seq_along(values)) {
    name <- values[[i]]
    d <- xdt[xdt[[series_col]] == name]
    data.table::setorderv(d, x_col)
    line_id <- paste0("nbd-line-", i)

    hc <- highcharter::hc_add_series(
      hc,
      id = line_id,
      name = name,
      type = "line",
      data = hc_points_xy(d[[x_col]], d[[y_col]]),
      color = series_colour(name, config, i),
      dashStyle = series_dash(name, config),
      connectNulls = FALSE,
      animation = nbd_hc_animation(
        duration = 550L,
        defer = 45L * (i - 1L)
      ),
      zIndex = 2
    )

    has_interval_columns <- !is.null(lower_col) && !is.null(upper_col) &&
      all(c(lower_col, upper_col) %in% names(d))
    if (has_interval_columns) {
      interval_rows <- is.finite(d[[lower_col]]) & is.finite(d[[upper_col]])
      if (any(interval_rows)) {
        band <- d[interval_rows]
        hc <- highcharter::hc_add_series(
          hc,
          name = paste0(name, " interval"),
          type = "arearange",
          data = hc_points_range(
            band[[x_col]], band[[lower_col]], band[[upper_col]]
          ),
          color = series_colour(name, config, i),
          fillOpacity = 0.10,
          lineWidth = 0,
          linkedTo = line_id,
          showInLegend = FALSE,
          animation = nbd_hc_animation(
            duration = 350L,
            defer = 70L + 45L * (i - 1L)
          ),
          zIndex = 0
        )
      }
    }
  }
  finish_nbd_hc(hc, config, height, filename)
}

hc_nbd_bar <- function(
    data,
    category,
    value,
    title = NULL,
    subtitle = NULL,
    axis_title = NULL,
    config = NULL,
    colour = "#0071BC",
    decimals = 1,
    suffix = "",
    height = 430,
    filename = "nbd3-ranking") {
  category_col <- as.character(category)[[1L]]
  value_col <- as.character(value)[[1L]]
  x <- data.table::as.data.table(data.table::copy(data))
  if (!nrow(x) || !all(c(category_col, value_col) %in% names(x))) {
    return(empty_nbd_chart(height = height))
  }
  x <- x[is.finite(x[[value_col]])]
  data.table::setorderv(x, value_col)
  if (!nrow(x)) return(empty_nbd_chart(height = height))
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "bar") |>
    highcharter::hc_title(text = title) |>
    highcharter::hc_subtitle(text = subtitle) |>
    highcharter::hc_xAxis(categories = as.character(x[[category_col]]), title = list(text = NULL)) |>
    highcharter::hc_yAxis(title = list(text = axis_title), gridLineWidth = 1) |>
    highcharter::hc_add_series(
      name = axis_title %||% "Estimate",
      data = as.numeric(x[[value_col]]),
      color = colour,
      showInLegend = FALSE,
      borderWidth = 0,
      dataLabels = list(
        enabled = TRUE,
        format = paste0("{point.y:,.", decimals, "f}", suffix),
        style = list(textOutline = "none", fontWeight = "500")
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = paste0("<b>{point.y:,.", decimals, "f}", suffix, "</b>")
    ) |>
    highcharter::hc_plotOptions(
      bar = list(borderRadius = 3, pointPadding = 0.06, groupPadding = 0.08),
      series = list(
        animation = nbd_hc_animation(duration = 500L, defer = 80L),
        animationLimit = 10000L
      )
    )
  finish_nbd_hc(hc, config, height, filename)
}


hc_nbd_columns <- function(
    data,
    category,
    value,
    title = NULL,
    subtitle = NULL,
    axis_title = NULL,
    config = NULL,
    colour = "#0071BC",
    decimals = 1,
    suffix = "",
    height = 430,
    filename = "nbd3-columns") {
  category_col <- as.character(category)[[1L]]
  value_col <- as.character(value)[[1L]]
  x <- data.table::as.data.table(data.table::copy(data))
  if (!nrow(x) || !all(c(category_col, value_col) %in% names(x))) {
    return(empty_nbd_chart(height = height))
  }
  x <- x[is.finite(x[[value_col]])]
  if (!nrow(x)) return(empty_nbd_chart(height = height))

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "column") |>
    highcharter::hc_title(text = title) |>
    highcharter::hc_subtitle(text = subtitle) |>
    highcharter::hc_xAxis(
      categories = as.character(x[[category_col]]),
      title = list(text = NULL),
      labels = list(rotation = -45)
    ) |>
    highcharter::hc_yAxis(title = list(text = axis_title), min = 0) |>
    highcharter::hc_add_series(
      name = axis_title %||% "Estimate",
      data = as.numeric(x[[value_col]]),
      color = colour,
      showInLegend = FALSE,
      borderWidth = 0
    ) |>
    highcharter::hc_tooltip(
      pointFormat = paste0("<b>{point.y:,.", decimals, "f}", suffix, "</b>")
    ) |>
    highcharter::hc_plotOptions(
      column = list(borderRadius = 2, pointPadding = 0.04, groupPadding = 0.08),
      series = list(
        animation = nbd_hc_animation(duration = 500L, defer = 80L),
        animationLimit = 10000L
      )
    )
  finish_nbd_hc(hc, config, height, filename)
}


hc_nbd_heatmap <- function(
    data,
    x,
    y,
    value,
    title,
    subtitle = NULL,
    value_label = "Difference",
    config = NULL,
    height = 500,
    filename = "nbd3-heatmap") {
  x_col <- as.character(x)[[1L]]
  y_col <- as.character(y)[[1L]]
  value_col <- as.character(value)[[1L]]
  d <- data.table::as.data.table(data.table::copy(data))
  if (!nrow(d) || !all(c(x_col, y_col, value_col) %in% names(d))) {
    return(empty_nbd_chart(height = height))
  }
  d <- d[is.finite(d[[value_col]])]
  if (!nrow(d)) return(empty_nbd_chart(height = height))
  x_categories <- sort(unique(as.character(d[[x_col]])))
  y_categories <- unique(as.character(d[[y_col]]))
  d$x_index <- match(as.character(d[[x_col]]), x_categories) - 1L
  d$y_index <- match(as.character(d[[y_col]]), y_categories) - 1L
  points <- Map(
    function(ix, iy, z) list(x = as.integer(ix), y = as.integer(iy), value = as.numeric(z)),
    d$x_index, d$y_index, d[[value_col]]
  )
  maximum <- max(abs(d[[value_col]]), na.rm = TRUE)
  if (!is.finite(maximum) || maximum <= 0) maximum <- 1
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "heatmap", zoomType = "xy") |>
    highcharter::hc_title(text = title) |>
    highcharter::hc_subtitle(text = subtitle) |>
    highcharter::hc_xAxis(categories = x_categories, title = list(text = NULL)) |>
    highcharter::hc_yAxis(categories = y_categories, title = list(text = NULL), reversed = TRUE) |>
    highcharter::hc_colorAxis(
      min = -maximum,
      max = maximum,
      stops = list(
        list(0, "#B23A48"),
        list(0.5, "#F7F8F9"),
        list(1, "#0071BC")
      )
    ) |>
    highcharter::hc_add_series(
      data = points,
      name = value_label,
      borderWidth = 0.5,
      borderColor = "#ffffff",
      animation = nbd_hc_animation(duration = 450L, defer = 60L)
    ) |>
    highcharter::hc_tooltip(pointFormat = paste0("<b>", value_label, ": {point.value:,.2f}</b>"))
  finish_nbd_hc(hc, config, height, filename)
}


hc_injury_composition <- function(
    source_data,
    model_data,
    config,
    title = "Injury composition: surveys and smoothed interpolation",
    subtitle = paste(
      "Diamonds show survey compositions on the final injury-envelope weighting basis;",
      "smooth lines show the fixed linear interpolation and triangular moving average and are not constrained to cross the points."
    ),
    height = 460) {
  source <- data.table::as.data.table(data.table::copy(source_data))
  model <- data.table::as.data.table(data.table::copy(model_data))
  if (!nrow(model)) return(empty_nbd_chart(height = height))

  group_colours <- c(
    "Transport injuries" = "#0071BC",
    "Other unintentional injuries" = "#7C858E",
    "Self-harm" = "#D48A16",
    "Interpersonal violence" = "#B23A48"
  )
  survey_years <- sort(unique(source$year[is.finite(source$year)]))
  survey_lines <- lapply(survey_years, function(survey_year) {
    list(
      value = as.numeric(survey_year),
      color = "#aeb7bf",
      width = 1,
      dashStyle = "Dot",
      zIndex = 1,
      label = list(
        text = as.character(survey_year),
        rotation = 0,
        y = 13,
        style = list(color = "#7c858e", fontSize = "10px")
      )
    )
  })

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line", zoomType = "x") |>
    highcharter::hc_title(text = title) |>
    highcharter::hc_subtitle(text = subtitle) |>
    highcharter::hc_xAxis(
      title = list(text = "Year"),
      tickInterval = 2,
      plotLines = survey_lines
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "Share of injury deaths"),
      min = 0,
      max = 1
    ) |>
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 3) |>
    highcharter::hc_plotOptions(
      series = list(
        animation = nbd_hc_animation(duration = 550L),
        animationLimit = 10000L,
        lineWidth = 2.5
      )
    ) |>
    highcharter::hc_legend(
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    )

  groups <- intersect(names(group_colours), unique(model$broad_group))
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    d <- model[broad_group == group][order(year)]
    hc <- highcharter::hc_add_series(
      hc,
      name = group,
      type = "line",
      data = hc_points_xy(d$year, d$fraction),
      color = group_colours[[group]],
      marker = list(enabled = FALSE),
      animation = nbd_hc_animation(
        duration = 550L,
        defer = 90L * (i - 1L)
      ),
      zIndex = 2
    )

    if (nrow(source) && "survey_fraction" %in% names(source)) {
      p <- source[
        broad_group == group & is.finite(survey_fraction)
      ][order(year)]
      if (nrow(p)) {
        hc <- highcharter::hc_add_series(
          hc,
          name = paste0(group, " survey"),
          type = "scatter",
          data = hc_points_xy(p$year, p$survey_fraction),
          color = group_colours[[group]],
          marker = list(
            radius = 6,
            symbol = "diamond",
            fillColor = "#ffffff",
            lineColor = group_colours[[group]],
            lineWidth = 2
          ),
          showInLegend = FALSE,
          animation = nbd_hc_animation(
            duration = 350L,
            defer = 300L + 90L * (i - 1L)
          ),
          zIndex = 5
        )
      }
    }
  }

  finish_nbd_hc(hc, config, height, "injury-composition")
}
