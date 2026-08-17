# Report UI/server helper functions.
# Generated from the working NBD3 research report; do not rebuild at app startup.

format_value <- function(x, measure, digits = 1L) {
  if (!length(x) || is.na(x) || !is.finite(x)) return("—")
  if (identical(measure, "fraction")) {
    return(scales::percent(x, accuracy = 0.1))
  }
  format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

metric_decimals <- function(measure) {
  if (identical(measure, "deaths")) 0 else 2
}

safe_default <- function(values, preferred = NULL) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(character())
  if (!is.null(preferred) && as.character(preferred) %in% values) {
    return(as.character(preferred))
  }
  values[[1L]]
}

nbd_cached_reactive <- function(
    key_reactive,
    value_function,
    millis = 200L) {
  debounced_key <- shiny::debounce(key_reactive, millis = as.integer(millis))
  value <- shiny::reactive({
    value_function(debounced_key())
  })
  shiny::bindCache(
    value,
    debounced_key(),
    ui_cache_version_key,
    cache = "app"
  )
}

named_choices <- function(data, value, label) {
  data <- data.table::as.data.table(data.table::copy(data))
  if (!all(c(value, label) %in% names(data))) return(stats::setNames(character(), character()))
  data <- unique(data[, c(value, label), with = FALSE])
  keep <- !is.na(data[[value]]) & !is.na(data[[label]]) &
    nzchar(as.character(data[[value]])) & nzchar(as.character(data[[label]]))
  data <- data[keep]
  stats::setNames(as.character(data[[value]]), as.character(data[[label]]))
}

humanise <- function(x) tools::toTitleCase(gsub("_", " ", as.character(x)))

format_count <- function(x) {
  if (!length(x) || is.na(x) || !is.finite(x)) return("—")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_table_value <- function(x, measure = NULL, digits = 1L) {
  vapply(
    x,
    function(value) {
      if (is.na(value) || !is.finite(value)) return("—")
      if (!is.null(measure) && identical(measure, "fraction")) {
        return(scales::percent(value, accuracy = 0.1))
      }
      format(
        round(value, digits),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE,
        nsmall = if (digits > 0L) digits else 0L
      )
    },
    character(1)
  )
}

nbd_kable_html <- function(
    data,
    caption = NULL,
    align = NULL,
    escape = TRUE,
    table_class = "nbd-kable",
    wrap_class = "nbd-kable-wrap",
    empty_message = "No data are available for this selection.") {
  x <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(x)) {
    x <- data.frame(Note = empty_message, check.names = FALSE)
  }
  if (is.null(align)) {
    align <- rep("l", ncol(x))
  }
  table_html <- knitr::kable(
    x,
    format = "html",
    escape = escape,
    align = align,
    caption = caption,
    table.attr = paste0('class="', table_class, '"')
  )
  htmltools::HTML(paste0(
    '<div class="', wrap_class, '">',
    as.character(table_html),
    '</div>'
  ))
}

# Compact report catalogues ---------------------------------------------------
#
# The query cache contains every supported combination, so the live app only
# needs the distinct geography, sex, age and cause catalogues. Avoid restoring
# the million-row cross-product availability table in each hosted worker.

nbd_result_geographies <- function(types = NULL) {
  out <- data.table::as.data.table(data.table::copy(result_geography_catalog))
  if (length(types)) out <- out[geography_type %in% as.character(types)]
  out[]
}

nbd_result_sexes <- function() {
  data.table::as.data.table(data.table::copy(result_sex_catalog))
}

nbd_result_ages <- function() {
  data.table::as.data.table(data.table::copy(result_age_catalog))
}

nbd_result_causes <- function(domain_name = NULL) {
  out <- data.table::as.data.table(data.table::copy(result_cause_catalog))
  if (!is.null(domain_name) && length(domain_name)) {
    out <- out[domain %in% as.character(domain_name)]
  }
  out[]
}

nbd_model_comparison_domain_slug <- function(x) {
  out <- tolower(gsub("[^a-z0-9]+", "_", as.character(x)))
  out <- gsub("(^_+|_+$)", "", out)
  ifelse(nzchar(out), out, "unknown")
}

nbd_read_model_comparison_domain <- function(runtime, domain_name) {
  info <- runtime$comparison_store
  if (is.null(info) || is.null(info$root) || !dir.exists(info$root)) {
    return(data.table::data.table())
  }
  slug <- nbd_model_comparison_domain_slug(domain_name)
  key <- paste0("domain", gsub("[^a-z0-9]", "", slug))
  if (!is.null(info$cache) && isTRUE(info$cache$exists(key))) {
    return(data.table::copy(info$cache$get(key)))
  }
  path <- file.path(info$root, paste0("domain=", slug), "part-0.parquet")
  if (!file.exists(path)) return(data.table::data.table())
  out <- data.table::as.data.table(arrow::read_parquet(path))
  data.table::setindexv(
    out,
    intersect(
      c("geography_code", "sex_code", "age_id", "series_id", "year"),
      names(out)
    )
  )
  if (!is.null(info$cache)) info$cache$set(key, data.table::copy(out))
  out[]
}

collect_model_comparison_slice <- function(
    runtime,
    domain_name,
    geography_code,
    sex_code,
    age_id,
    series_id) {
  d <- nbd_read_model_comparison_domain(runtime, domain_name)
  if (!nrow(d)) return(d)
  target_geography <- as.integer(geography_code)
  target_sex <- as.integer(sex_code)
  target_age <- as.character(age_id)
  target_series <- as.character(series_id)
  d[
    geography_code == target_geography &
      sex_code == target_sex &
      age_id == target_age &
      series_id == target_series
  ]
}

comparison_panel_ui <- function(prefix, domain_name, default_series) {
  available <- comparison_catalog[get("domain") == domain_name]
  geographies <- unique(available[, .(
    geography_type, geography_code, geography
  )])[order(geography_type, geography_code)]
  sexes <- unique(available[, .(sex_code, sex)])[order(sex_code)]
  ages <- unique(available[, .(
    age_id, age_label, age_sort_order
  )])[order(age_sort_order)]
  series <- unique(available[, .(
    series_id, series_label, series_sort_order
  )])[order(series_sort_order)]

  ids <- paste0(prefix, c("_geography", "_sex", "_age", "_series"))
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        htmltools::div(
          class = "control-panel",
          shiny::selectizeInput(
            ids[[1L]], "Geography",
            choices = named_choices(geographies, "geography_code", "geography"),
            selected = safe_default(geographies$geography_code, 10L),
            multiple = FALSE
          ),
          shiny::selectizeInput(
            ids[[2L]], "Sex",
            choices = named_choices(sexes, "sex_code", "sex"),
            selected = safe_default(sexes$sex_code, 3L),
            multiple = FALSE
          ),
          shiny::selectizeInput(
            ids[[3L]], "Age group",
            choices = named_choices(ages, "age_id", "age_label"),
            selected = safe_default(ages$age_id, "age_all"),
            multiple = FALSE
          ),
          shiny::selectizeInput(
            ids[[4L]], "Series",
            choices = named_choices(series, "series_id", "series_label"),
            selected = safe_default(series$series_id, default_series),
            multiple = FALSE
          ),
          htmltools::p(
            class = "control-hint",
            "All available models are shown together. Click a legend item to hide or restore a series."
          )
        )
      ),
      shiny::column(
        9,
        htmltools::div(
          class = "report-chart",
          highcharter::highchartOutput(paste0(prefix, "_chart"), height = "500px")
        )
      )
    ),
    shiny::uiOutput(paste0(prefix, "_note")),
    htmltools::tags$details(
      class = "appendix-detail",
      htmltools::tags$summary("View and download the annual model values"),
      htmltools::div(
        class = "report-table",
        shiny::uiOutput(paste0(prefix, "_table")),
        shiny::downloadLink(
          paste0(prefix, "_download"),
          "Download comparison data",
          class = "download-link"
        )
      )
    )
  )
}

register_comparison_panel <- function(prefix, domain_name, default_series) {
  ids <- list(
    geography = paste0(prefix, "_geography"),
    sex = paste0(prefix, "_sex"),
    age = paste0(prefix, "_age"),
    series = paste0(prefix, "_series"),
    chart = paste0(prefix, "_chart"),
    note = paste0(prefix, "_note"),
    table = paste0(prefix, "_table"),
    download = paste0(prefix, "_download")
  )

  selection_key <- shiny::reactive({
    shiny::req(
      input[[ids$geography]], input[[ids$sex]],
      input[[ids$age]], input[[ids$series]]
    )
    list(
      panel = prefix,
      geography = as.integer(input[[ids$geography]]),
      sex = as.integer(input[[ids$sex]]),
      age = as.character(input[[ids$age]]),
      series = as.character(input[[ids$series]])
    )
  })

  selected_data <- nbd_cached_reactive(
    selection_key,
    function(key) {
      d <- collect_model_comparison_slice(
        runtime = runtime,
        domain_name = domain_name,
        geography_code = key$geography,
        sex_code = key$sex,
        age_id = key$age,
        series_id = key$series
      )
      nbd_rows <- d[model == "NBD3-R"]
      if (nrow(nbd_rows) && !is.null(runtime$full_uncertainty)) {
        dynamic <- tryCatch(
          collect_full_comparison_uncertainty(runtime, nbd_rows),
          error = function(error) {
            warning(
              "Model-comparison uncertainty could not be calculated: ",
              conditionMessage(error),
              call. = FALSE
            )
            data.table::data.table()
          }
        )
        if (nrow(dynamic)) {
          join_key <- c(
            "geography_type", "geography_code", "sex_code", "year",
            "age_id", "series_id", "measure"
          )
          dynamic <- dynamic[, c(join_key, "lower", "upper"), with = FALSE]
          static_external <- d[model != "NBD3-R"]
          revised_nbd <- merge(
            nbd_rows[, c("lower", "upper") := NULL],
            dynamic,
            by = join_key,
            all.x = TRUE,
            sort = FALSE
          )
          d <- data.table::rbindlist(
            list(static_external, revised_nbd),
            use.names = TRUE,
            fill = TRUE
          )
        }
      }
      d[order(
        year,
        factor(
          model,
          levels = as.character(unlist(
            config$labels$models$order,
            use.names = FALSE
          ))
        )
      )]
    },
    millis = 120L
  )

  output[[ids$chart]] <- highcharter::renderHighchart({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(empty_nbd_chart(height = 500))
    selected_measure <- unique(d$measure)[[1L]]
    d[, `:=`(
      plot_estimate = estimate,
      plot_lower = lower,
      plot_upper = upper
    )]
    suffix <- ""
    decimals <- metric_decimals(selected_measure)
    y_title <- measure_label(selected_measure, config)
    if (identical(selected_measure, "fraction")) {
      d[, `:=`(
        plot_estimate = 100 * plot_estimate,
        plot_lower = 100 * plot_lower,
        plot_upper = 100 * plot_upper
      )]
      suffix <- "%"
      decimals <- 1
      y_title <- "Percent of all-cause deaths"
    }
    hc_nbd_lines(
      d,
      y = "plot_estimate",
      series = "model",
      lower = "plot_lower",
      upper = "plot_upper",
      title = unique(d$series_label)[[1L]],
      subtitle = paste(unique(d$geography), unique(d$sex), unique(d$age_label), sep = " — "),
      x_title = "Year",
      y_title = y_title,
      config = config,
      decimals = decimals,
      suffix = suffix,
      height = 500,
      filename = paste0(prefix, "-model-comparison"),
      series_order = as.character(unlist(config$labels$models$order, use.names = FALSE))
    )
  })

  output[[ids$note]] <- shiny::renderUI({
    d <- selected_data()
    if (!nrow(d)) {
      return(htmltools::div(
        class = "narrative-box warning",
        "No model series are available for the selected cell."
      ))
    }
    model_names <- ordered_model_names(unique(d$model), config)
    interval_models <- ordered_model_names(
      unique(d[is.finite(lower) & is.finite(upper), model]),
      config
    )
    interval_text <- if (length(interval_models)) {
      paste0(
        "Uncertainty bands are available for: ",
        paste(interval_models, collapse = ", "), "."
      )
    } else {
      "No uncertainty limits are supplied for this exact selection."
    }
    htmltools::div(
      class = "narrative-box",
      htmltools::strong("Models shown: "),
      paste(model_names, collapse = ", "),
      htmltools::br(),
      interval_text,
      htmltools::br(),
      htmltools::span(
        class = "provisional-note",
        paste0(
          "NBD3-R intervals come from ", uncertainty_draw_label,
          "; external intervals are retained as supplied."
        )
      )
    )
  })

  output[[ids$table]] <- shiny::renderUI({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(nbd_kable_html(data.frame()))
    selected_measure <- unique(d$measure)[[1L]]
    digits <- if (identical(selected_measure, "fraction")) {
      1L
    } else {
      metric_decimals(selected_measure)
    }
    display <- d[, .(
      Model = model,
      Geography = geography,
      Sex = sex,
      Year = year,
      Age = age_label,
      Series = series_label,
      Measure = measure,
      Estimate = estimate,
      Lower = lower,
      Upper = upper,
      Source = source
    )]
    display[, `:=`(
      Estimate = format_table_value(Estimate, selected_measure, digits),
      Lower = format_table_value(Lower, selected_measure, digits),
      Upper = format_table_value(Upper, selected_measure, digits)
    )]
    nbd_kable_html(
      display,
      caption = "Annual estimates and uncertainty limits for the selected model comparison",
      align = c("l", "l", "l", "r", "l", "l", "l", "r", "r", "r", "l")
    )
  })

  output[[ids$download]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "-model-comparison.csv"),
    content = function(file) data.table::fwrite(selected_data(), file)
  )

  invisible(TRUE)
}

final_panel_ui <- function(prefix, geography_type_name, domain_name, default_series) {
  geographies <- nbd_result_geographies(geography_type_name)[, .(
    geography_code, geography
  )][order(geography_code)]
  sexes <- nbd_result_sexes()[order(sex_code)]
  ages <- nbd_result_ages()[order(age_sort_order)]
  causes <- nbd_result_causes(domain_name)[, .(
    series_id, series_label, hierarchy, series_sort_order
  )][order(series_sort_order)]
  grouped_causes <- lapply(
    split(causes, causes$hierarchy),
    function(x) named_choices(x, "series_id", "series_label")
  )
  names(grouped_causes) <- humanise(names(grouped_causes))

  ids <- paste0(prefix, c(
    "_geographies", "_sex", "_cause", "_age", "_measure", "_years"
  ))
  shiny::tagList(
    htmltools::div(
      class = "control-panel final-control-panel",
      shiny::fluidRow(
        shiny::column(4, shiny::selectizeInput(
          ids[[1L]], "Geographies",
          choices = named_choices(geographies, "geography_code", "geography"),
          selected = as.character(geographies$geography_code),
          multiple = TRUE
        )),
        shiny::column(2, shiny::selectizeInput(
          ids[[2L]], "Sex",
          choices = named_choices(sexes, "sex_code", "sex"),
          selected = safe_default(sexes$sex_code, 3L),
          multiple = FALSE
        )),
        shiny::column(6, shiny::selectizeInput(
          ids[[3L]], "Cause",
          choices = grouped_causes,
          selected = safe_default(causes$series_id, default_series),
          multiple = FALSE
        ))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::selectizeInput(
          ids[[4L]], "Age group",
          choices = named_choices(ages[age_id != "asr_all"], "age_id", "age_label"),
          selected = safe_default(ages$age_id, "age_all"),
          multiple = FALSE
        )),
        shiny::column(4, shiny::selectInput(
          ids[[5L]], "Measure",
          choices = measure_choices,
          selected = "crude_rate"
        )),
        shiny::column(4, shiny::sliderInput(
          ids[[6L]], "Years",
          min = as.integer(config$labels$build$start_year),
          max = as.integer(config$labels$build$end_year),
          value = c(
            as.integer(config$labels$build$start_year),
            as.integer(config$labels$build$end_year)
          ),
          sep = ""
        ))
      )
    ),
    htmltools::div(
      class = "report-chart wide-report-chart",
      highcharter::highchartOutput(paste0(prefix, "_chart"), height = "520px")
    ),
    shiny::uiOutput(paste0(prefix, "_note")),
    htmltools::tags$details(
      class = "appendix-detail",
      htmltools::tags$summary("View and download the selected NBD3-R values"),
      htmltools::div(
        class = "report-table",
        shiny::uiOutput(paste0(prefix, "_table")),
        shiny::downloadLink(
          paste0(prefix, "_download"),
          "Download selected NBD3-R data",
          class = "download-link"
        )
      )
    )
  )
}

register_final_panel <- function(prefix, geography_type_name, domain_name, default_series) {
  ids <- list(
    geographies = paste0(prefix, "_geographies"),
    sex = paste0(prefix, "_sex"),
    cause = paste0(prefix, "_cause"),
    age = paste0(prefix, "_age"),
    measure = paste0(prefix, "_measure"),
    years = paste0(prefix, "_years"),
    chart = paste0(prefix, "_chart"),
    note = paste0(prefix, "_note"),
    table = paste0(prefix, "_table"),
    download = paste0(prefix, "_download")
  )

  age_catalog <- nbd_result_ages()[order(age_sort_order)]

  shiny::observeEvent(input[[ids$measure]], {
    selected_measure <- input[[ids$measure]] %||% "crude_rate"
    choices <- if (identical(selected_measure, "asr")) {
      age_catalog[age_id == "asr_all"]
    } else if (identical(selected_measure, "crude_rate")) {
      age_catalog[age_id != "asr_all" & age_id != "age_0"]
    } else {
      age_catalog[age_id != "asr_all"]
    }
    preferred <- if (identical(selected_measure, "asr")) "asr_all" else "age_all"
    shiny::updateSelectizeInput(
      session,
      ids$age,
      choices = named_choices(choices, "age_id", "age_label"),
      selected = safe_default(choices$age_id, preferred),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  selected_data <- shiny::reactive({
    shiny::req(
      input[[ids$geographies]], input[[ids$sex]], input[[ids$cause]],
      input[[ids$age]], input[[ids$measure]], input[[ids$years]]
    )
    selected_geography_codes <- suppressWarnings(as.integer(unlist(
      input[[ids$geographies]], recursive = TRUE, use.names = FALSE
    )))
    selected_geography_codes <- unique(selected_geography_codes[!is.na(selected_geography_codes)])
    selected_sex_code <- suppressWarnings(as.integer(unlist(
      input[[ids$sex]], recursive = TRUE, use.names = FALSE
    )))[[1L]]
    selected_years <- suppressWarnings(as.integer(unlist(
      input[[ids$years]], recursive = TRUE, use.names = FALSE
    )))
    selected_years <- selected_years[!is.na(selected_years)]

    d <- collect_cause_rates(
      runtime$cause_rates_dataset,
      models = "NBD3-R",
      geography_type = geography_type_name,
      geography_codes = selected_geography_codes,
      sex_code = selected_sex_code,
      series_id = input[[ids$cause]],
      age_id = input[[ids$age]],
      year_range = selected_years,
      measure = input[[ids$measure]]
    )
    if (!nrow(d)) return(d)
    d <- attach_exact_cause_uncertainty(d)
    d[, line_name := geography]
    d[order(geography_code, year)]
  })

  output[[ids$chart]] <- highcharter::renderHighchart({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(empty_nbd_chart(height = 520))
    hc_nbd_lines(
      d,
      y = "estimate",
      series = "line_name",
      lower = "lower",
      upper = "upper",
      title = unique(d$series_label)[[1L]],
      subtitle = paste(unique(d$sex), unique(d$age_label), sep = " — "),
      x_title = "Year",
      y_title = measure_label(input[[ids$measure]], config),
      config = config,
      decimals = metric_decimals(input[[ids$measure]]),
      height = 520,
      filename = paste0(prefix, "-nbd3-r-trend")
    )
  })

  output[[ids$note]] <- shiny::renderUI({
    d <- selected_data()
    if (!nrow(d)) {
      return(htmltools::div(
        class = "narrative-box warning",
        "No NBD3-R results are available for this selection."
      ))
    }
    interval_rows <- d[is.finite(lower) & is.finite(upper), .N]
    total_rows <- nrow(d)
    interval_message <- if (interval_rows > 0L) {
      paste0(
        format(interval_rows, big.mark = ","), " of ",
        format(total_rows, big.mark = ","),
        " plotted cells have 95% uncertainty intervals from ",
        uncertainty_draw_label, "."
      )
    } else {
      paste(
        "No full-grid interval was available for this cell. Confirm that the",
        "current uncertainty run completed all province and population-group",
        "full-draw files for Male, Female and Person."
      )
    }
    htmltools::div(
      class = if (interval_rows > 0L) "narrative-box success" else "narrative-box",
      htmltools::strong("NBD3-R implementation. "),
      interval_message,
      htmltools::br(),
      htmltools::span(
        class = "provisional-note",
        paste(
          "Intervals combine under-reporting, injury-envelope, injury",
          "survey-composition, HIV fitted-parameter, and redistribution uncertainty in one joint run."
        )
      )
    )
  })

  output[[ids$table]] <- shiny::renderUI({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(nbd_kable_html(data.frame()))
    selected_measure <- input[[ids$measure]]
    digits <- metric_decimals(selected_measure)
    display <- d[, .(
      Geography = geography,
      Sex = sex,
      Year = year,
      Age = age_label,
      Cause = series_label,
      Measure = measure,
      Estimate = estimate,
      Lower = lower,
      Upper = upper,
      `Draws used` = n_draws
    )]
    display[, `:=`(
      Estimate = format_table_value(Estimate, selected_measure, digits),
      Lower = format_table_value(Lower, selected_measure, digits),
      Upper = format_table_value(Upper, selected_measure, digits),
      `Draws used` = vapply(
        `Draws used`,
        function(value) if (is.na(value)) "—" else format(value, big.mark = ","),
        character(1)
      )
    )]
    nbd_kable_html(
      display,
      caption = "Selected NBD3-R estimates and 95% uncertainty intervals",
      align = c("l", "l", "r", "l", "l", "l", "r", "r", "r", "r")
    )
  })

  output[[ids$download]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "-nbd3-r.csv"),
    content = function(file) data.table::fwrite(selected_data(), file)
  )

  invisible(TRUE)
}

resolve_broad_age_profile_causes <- function() {
  causes <- data.table::copy(broad_age_profile_causes)
  available_ids <- unique(as.character(result_cause_catalog$series_id))
  if (all(causes$series_id %in% available_ids)) return(causes)

  # Stable label-based fallback for repositories whose report IDs were
  # regenerated while retaining the same cause hierarchy.
  fallback_patterns <- c(
    "communicable.*maternal.*perinatal|comm/mat/peri/nutr",
    "hiv/aids.*tuberculosis|hiv/aids.*tb",
    "non-communicable",
    "^injuries$"
  )
  for (i in seq_len(nrow(causes))) {
    if (causes$series_id[[i]] %in% available_ids) next
    hit <- result_cause_catalog[
      grepl(
        fallback_patterns[[i]],
        series_label,
        ignore.case = TRUE,
        perl = TRUE
      )
    ]
    if (nrow(hit)) causes$series_id[[i]] <- hit$series_id[[1L]]
  }
  causes
}

broad_age_profile_panel_chart <- function(
    data,
    panel_year,
    y_max,
    show_y_title = FALSE,
    show_x_title = FALSE,
    config = NULL,
    height = 355) {
  d <- data.table::as.data.table(data.table::copy(data))
  if (!nrow(d)) return(empty_nbd_chart(height = height))

  causes <- resolve_broad_age_profile_causes()[order(stack_order)]
  total_deaths <- sum(d$estimate, na.rm = TRUE)
  panel_title <- paste0(
    panel_year,
    " (N=",
    format(round(total_deaths), big.mark = ",", scientific = FALSE),
    ")"
  )

  complete_grid <- data.table::CJ(
    age_id = broad_age_profile_ages$age_id,
    series_id = causes$series_id,
    unique = TRUE
  )
  d <- merge(
    complete_grid,
    d[, .(age_id, series_id, estimate)],
    by = c("age_id", "series_id"),
    all.x = TRUE,
    sort = FALSE
  )
  d[!is.finite(estimate), estimate := 0]
  d <- merge(d, broad_age_profile_ages, by = "age_id", all.x = TRUE)
  d <- merge(d, causes, by = "series_id", all.x = TRUE)
  data.table::setorder(d, stack_order, age_order)

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(
      type = "column",
      spacing = c(8, 8, 8, 8),
      marginTop = 42
    ) |>
    highcharter::hc_title(
      text = panel_title,
      align = "left",
      x = 4,
      y = 15,
      style = list(
        color = "#30343b",
        fontSize = "13px",
        fontWeight = "600"
      )
    ) |>
    highcharter::hc_xAxis(
      categories = broad_age_profile_ages$age_label,
      title = list(text = if (show_x_title) "Age group (years)" else NULL),
      labels = list(
        rotation = -45,
        style = list(fontSize = "10px", color = "#4d535a")
      ),
      tickLength = 4,
      lineWidth = 1,
      lineColor = "#4d535a",
      tickColor = "#4d535a"
    ) |>
    highcharter::hc_yAxis(
      min = 0,
      max = y_max,
      tickAmount = 6,
      reversedStacks = FALSE,
      title = list(text = if (show_y_title) "Number of deaths" else NULL),
      gridLineColor = "#d8d8d8",
      gridLineWidth = 1,
      labels = list(style = list(fontSize = "10px", color = "#4d535a"))
    ) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      headerFormat = "<b>{point.key}</b><br/>",
      pointFormat = paste0(
        '<span style="color:{series.color}">●</span> ',
        '{series.name}: <b>{point.y:,.0f}</b><br/>'
      ),
      footerFormat = "<b>Total: {point.stackTotal:,.0f}</b>"
    ) |>
    highcharter::hc_plotOptions(
      column = list(
        stacking = "normal",
        borderColor = "#4b4b4b",
        borderWidth = 0.6,
        pointPadding = 0.02,
        groupPadding = 0.05
      ),
      series = list(
        animation = nbd_hc_animation(duration = 520L),
        animationLimit = 10000L,
        states = list(inactive = list(opacity = 0.22))
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE)

  for (i in seq_len(nrow(causes))) {
    cause <- causes[i]
    values <- d[series_id == cause$series_id][order(age_order)]$estimate
    hc <- highcharter::hc_add_series(
      hc,
      type = "column",
      name = cause$legend_label,
      data = as.numeric(values),
      color = cause$colour,
      borderColor = "#4b4b4b",
      borderWidth = 0.6,
      showInLegend = FALSE,
      animation = nbd_hc_animation(
        duration = 520L,
        defer = 70L * (i - 1L)
      )
    )
  }

  finish_nbd_hc(
    hc,
    config = config,
    height = height,
    filename = paste0("broad-cause-age-", panel_year)
  )
}

broad_age_profile_legend <- function() {
  causes <- resolve_broad_age_profile_causes()[order(legend_order)]
  htmltools::div(
    class = "broad-age-profile-legend",
    style = paste(
      "display:flex;flex-wrap:wrap;justify-content:center;",
      "gap:0.75rem 1.35rem;margin:0.2rem 0 1rem 0;",
      "color:#29465b;font-size:0.92rem;font-weight:500;"
    ),
    lapply(seq_len(nrow(causes)), function(i) {
      htmltools::span(
        class = "broad-age-profile-legend-item",
        style = "display:inline-flex;align-items:center;gap:0.42rem;",
        htmltools::span(
          class = "broad-age-profile-swatch",
          style = paste0(
            "display:inline-block;width:14px;height:14px;",
            "border:1px solid #4b4b4b;background:",
            causes$colour[[i]], ";"
          )
        ),
        causes$legend_label[[i]]
      )
    })
  )
}

broad_age_profile_panel_ui <- function(prefix = "result_broad_age") {
  ids <- paste0(prefix, c("_scope", "_geography", "_sex"))
  sexes <- nbd_result_sexes()[order(sex_code)]
  province_choices <- geography_choices_for_scope("province")

  shiny::tagList(
    htmltools::div(
      class = "control-panel research-control-panel",
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(
          ids[[1L]], "Population set",
          choices = c(
            "South Africa or province" = "province",
            "South Africa or population group" = "population_group"
          ),
          selected = "province"
        )),
        shiny::column(6, shiny::selectizeInput(
          ids[[2L]], "Geography or population group",
          choices = province_choices,
          selected = make_geography_key("national", 10L),
          multiple = FALSE
        )),
        shiny::column(3, shiny::selectizeInput(
          ids[[3L]], "Sex",
          choices = named_choices(sexes, "sex_code", "sex"),
          selected = safe_default(sexes$sex_code, 3L),
          multiple = FALSE
        ))
      )
    ),
    htmltools::div(
      class = "broad-age-profile-grid",
      shiny::fluidRow(
        shiny::column(
          6,
          htmltools::div(
            class = "report-chart broad-age-profile-panel",
            highcharter::highchartOutput(
              paste0(prefix, "_chart_1997"),
              height = "355px"
            )
          )
        ),
        shiny::column(
          6,
          htmltools::div(
            class = "report-chart broad-age-profile-panel",
            highcharter::highchartOutput(
              paste0(prefix, "_chart_2005"),
              height = "355px"
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          htmltools::div(
            class = "report-chart broad-age-profile-panel",
            highcharter::highchartOutput(
              paste0(prefix, "_chart_2012"),
              height = "355px"
            )
          )
        ),
        shiny::column(
          6,
          htmltools::div(
            class = "report-chart broad-age-profile-panel",
            highcharter::highchartOutput(
              paste0(prefix, "_chart_2019"),
              height = "355px"
            )
          )
        )
      )
    ),
    broad_age_profile_legend(),
    shiny::uiOutput(paste0(prefix, "_note")),
    htmltools::tags$details(
      class = "appendix-detail",
      htmltools::tags$summary(
        "View and download the broad-cause age profile"
      ),
      htmltools::div(
        class = "report-table",
        shiny::uiOutput(paste0(prefix, "_table")),
        shiny::downloadLink(
          paste0(prefix, "_download"),
          "Download broad-cause age profile",
          class = "download-link"
        )
      )
    )
  )
}

register_broad_age_profile_panel <- function(prefix = "result_broad_age") {
  ids <- list(
    scope = paste0(prefix, "_scope"),
    geography = paste0(prefix, "_geography"),
    sex = paste0(prefix, "_sex"),
    note = paste0(prefix, "_note"),
    table = paste0(prefix, "_table"),
    download = paste0(prefix, "_download")
  )
  chart_ids <- stats::setNames(
    paste0(prefix, "_chart_", broad_age_profile_years),
    broad_age_profile_years
  )

  shiny::observeEvent(input[[ids$scope]], {
    scope <- input[[ids$scope]] %||% "province"
    choices <- geography_choices_for_scope(scope)
    selected <- make_geography_key("national", 10L)
    if (!selected %in% unname(choices)) selected <- unname(choices)[[1L]]
    shiny::updateSelectizeInput(
      session,
      ids$geography,
      choices = choices,
      selected = selected,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  selection_key <- shiny::reactive({
    shiny::req(input[[ids$geography]], input[[ids$sex]])
    list(
      panel = prefix,
      geography = as.character(input[[ids$geography]]),
      sex = as.integer(input[[ids$sex]])
    )
  })

  selected_data <- nbd_cached_reactive(
    selection_key,
    function(key) {
      causes <- resolve_broad_age_profile_causes()
      pieces <- lapply(
        seq_len(nrow(broad_age_profile_ages)),
        function(i) {
          age <- broad_age_profile_ages[i]
          d <- collect_result_slice(
            geography_keys = key$geography,
            sex_code = key$sex,
            series_ids = causes$series_id,
            age_id = age$age_id,
            year_range = range(broad_age_profile_years),
            measure = "deaths"
          )
          if (!nrow(d)) return(data.table::data.table())
          d <- d[year %in% broad_age_profile_years]
          d[, `:=`(
            age_id = age$age_id,
            age_label = age$age_label,
            age_order = age$age_order
          )]
          d
        }
      )
      d <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
      if (!nrow(d)) return(d)
      d <- merge(
        d,
        causes[, .(
          series_id,
          broad_label = series_label,
          colour,
          stack_order,
          legend_order
        )],
        by = "series_id",
        all.x = TRUE,
        sort = FALSE
      )
      data.table::setorder(d, year, age_order, stack_order)
      d
    },
    millis = 250L
  )

  y_max <- shiny::reactive({
    d <- selected_data()
    if (!nrow(d)) return(NULL)
    totals <- d[, .(age_total = sum(estimate, na.rm = TRUE)),
                by = .(year, age_id)]
    maximum <- max(totals$age_total, na.rm = TRUE)
    if (!is.finite(maximum) || maximum <= 0) return(NULL)
    magnitude <- 10^floor(log10(maximum))
    step <- if (maximum / magnitude < 2) {
      magnitude / 5
    } else if (maximum / magnitude < 5) {
      magnitude / 2
    } else {
      magnitude
    }
    ceiling(maximum / step) * step
  })

  for (panel_year in broad_age_profile_years) {
    local({
      year_value <- panel_year
      output[[chart_ids[[as.character(year_value)]]]] <-
        highcharter::renderHighchart({
          d <- data.table::copy(selected_data())
          ymax <- y_max()
          if (!nrow(d) || is.null(ymax)) {
            return(empty_nbd_chart(height = 355))
          }
          broad_age_profile_panel_chart(
            d[year == year_value],
            panel_year = year_value,
            y_max = ymax,
            show_y_title = year_value %in% c(1997L, 2012L),
            show_x_title = year_value %in% c(2012L, 2019L),
            config = config,
            height = 355
          )
        })
    })
  }

  output[[ids$note]] <- shiny::renderUI({
    d <- selected_data()
    if (!nrow(d)) {
      return(htmltools::div(
        class = "narrative-box warning",
        "No broad-cause age profile is available for this selection."
      ))
    }
    htmltools::div(
      class = "narrative-box",
      htmltools::strong("Broad cause composition by age. "),
      paste0(
        "The four panels show deaths in 1997, 2005, 2012, and 2019 for ",
        unique(d$geography)[[1L]], " — ", unique(d$sex)[[1L]],
        ". The four stacked groups sum to the displayed all-cause total in ",
        "each age group."
      )
    )
  })

  output[[ids$table]] <- shiny::renderUI({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(nbd_kable_html(data.frame()))
    display <- d[, .(
      Geography = geography,
      Sex = sex,
      Year = year,
      `Age group` = age_label,
      `Broad cause group` = broad_label,
      Deaths = round(estimate)
    )]
    nbd_kable_html(
      display,
      caption = paste(
        "Deaths by broad cause group and age group for the four selected years"
      ),
      align = c("l", "l", "r", "l", "l", "r")
    )
  })

  output[[ids$download]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "-broad-cause-age-profile.csv"),
    content = function(file) data.table::fwrite(selected_data(), file)
  )

  invisible(TRUE)
}

series_colour <- function(series, config, index = 1L) {
  colours <- unlist(config$labels$models$colours, use.names = TRUE)
  if (series %in% names(colours)) return(unname(colours[[series]]))
  report_fallback_colours[[1L + ((index - 1L) %% length(report_fallback_colours))]]
}

read_optional_report_table <- function(path) {
  if (!length(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(data.table::data.table())
  }
  tryCatch({
    extension <- tolower(tools::file_ext(path))
    if (identical(extension, "parquet")) {
      data.table::as.data.table(arrow::read_parquet(path))
    } else {
      data.table::fread(path)
    }
  }, error = function(error) {
    warning("Could not read optional report input: ", path, ". ", conditionMessage(error))
    data.table::data.table()
  })
}

find_uncertainty_report_root <- function() {
  configured_name <- uncertainty_config$run$output_name %||% ""
  configured_path <- file.path(root, "output", "uncertainty", configured_name)
  if (nzchar(configured_name) && dir.exists(configured_path)) {
    return(configured_path)
  }
  base <- file.path(root, "output", "uncertainty")
  if (!dir.exists(base)) return(NA_character_)
  candidates <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  candidates <- candidates[file.exists(file.path(
    candidates, "completeness_weighted_cells.parquet"
  ))]
  if (!length(candidates)) return(NA_character_)
  candidates[[which.max(file.info(candidates)$mtime)]]
}

cause_choices_for_hierarchy <- function(hierarchy = "all", grouped = FALSE) {
  hierarchy_value <- as.character(hierarchy)[[1L]]
  causes <- data.table::copy(result_cause_catalog)
  if (!identical(hierarchy_value, "all")) {
    causes <- result_cause_catalog[get("hierarchy") == hierarchy_value]
  }
  if (!nrow(causes)) return(character())
  if (!grouped || !identical(hierarchy_value, "all")) {
    return(named_choices(causes, "series_id", "series_label"))
  }
  groups <- split(causes, causes$hierarchy)
  groups <- groups[names(groups) %in% result_hierarchies]
  out <- lapply(groups, named_choices, value = "series_id", label = "series_label")
  names(out) <- unname(report_hierarchy_labels[names(out)])
  out
}

default_cause_ids <- function(hierarchy = "all", multiple = FALSE) {
  available <- result_cause_catalog$series_id
  preferred <- if (multiple) {
    if (identical(hierarchy, "all")) {
      c("za_171", "za_2", "za_1")
    } else if (identical(hierarchy, "broad")) {
      c("za_172", "za_168", "za_169", "za_170", "za_171")
    } else if (identical(hierarchy, "group")) {
      c("za_150", "za_158", "za_166", "za_167")
    } else if (identical(hierarchy, "detailed")) {
      c("za_2", "za_1", "za_126", "za_139")
    } else {
      character()
    }
  } else {
    if (identical(hierarchy, "detailed")) "za_2" else "za_172"
  }
  selected <- preferred[preferred %in% available]
  if (length(selected)) return(selected)
  hierarchy_value <- as.character(hierarchy)[[1L]]
  causes <- if (identical(hierarchy_value, "all")) {
    result_cause_catalog
  } else {
    result_cause_catalog[get("hierarchy") == hierarchy_value]
  }
  if (!nrow(causes)) return(character())
  if (multiple) head(causes$series_id, 5L) else causes$series_id[[1L]]
}

make_geography_key <- function(type, code) paste(type, as.integer(code), sep = "::")

parse_geography_key <- function(key) {
  pieces <- strsplit(as.character(key), "::", fixed = TRUE)[[1L]]
  if (length(pieces) != 2L) return(list(type = NA_character_, code = NA_integer_))
  list(type = pieces[[1L]], code = suppressWarnings(as.integer(pieces[[2L]])))
}

geography_choices_for_scope <- function(scope = "province") {
  types <- if (identical(scope, "population_group")) {
    c("national", "population_group")
  } else {
    c("national", "province")
  }
  geographies <- nbd_result_geographies(types)[, .(
    geography_type, geography_code, geography
  )]
  geographies[, key := make_geography_key(geography_type, geography_code)]
  geographies[, label := geography]
  geographies[, type_order := match(
    geography_type, c("national", "province", "population_group")
  )]
  data.table::setorder(geographies, type_order, geography_code)
  stats::setNames(geographies$key, geographies$label)
}

attach_exact_cause_uncertainty <- function(data) {
  d <- data.table::as.data.table(data.table::copy(data))
  if (!nrow(d)) return(d)
  key <- c(
    "geography_type", "geography_code", "sex_code", "year",
    "age_id", "series_id", "measure"
  )
  d[, `:=`(
    lower = NA_real_, upper = NA_real_, n_draws = NA_integer_,
    uncertainty_source = NA_character_
  )]

  dynamic <- tryCatch(
    collect_full_cause_uncertainty(runtime, d),
    error = function(error) {
      warning(
        "Full-grid uncertainty could not be calculated: ",
        conditionMessage(error),
        call. = FALSE
      )
      data.table::data.table()
    }
  )
  if (nrow(dynamic)) {
    dynamic <- dynamic[, c(key, "lower", "upper", "n_draws", "source"),
                       with = FALSE]
    data.table::setnames(dynamic, "source", "uncertainty_source")
    d[, c("lower", "upper", "n_draws", "uncertainty_source") := NULL]
    return(merge(d, dynamic, by = key, all.x = TRUE, sort = FALSE))
  }

  # Backward-compatible fallback for an older compact uncertainty run.
  if (!nrow(cause_uncertainty)) return(d)
  u <- cause_uncertainty[
    geography_type %in% unique(d$geography_type) &
      geography_code %in% unique(d$geography_code) &
      sex_code %in% unique(d$sex_code) &
      year %in% unique(d$year) &
      age_id %in% unique(d$age_id) &
      series_id %in% unique(d$series_id) &
      measure %in% unique(d$measure),
    c(key, "lower", "upper", "n_draws", "source"),
    with = FALSE
  ]
  if (!nrow(u)) return(d)
  data.table::setnames(u, "source", "uncertainty_source")
  d[, c("lower", "upper", "n_draws", "uncertainty_source") := NULL]
  merge(d, u, by = key, all.x = TRUE, sort = FALSE)
}

collect_result_slice <- function(
    geography_keys,
    sex_code,
    series_ids,
    age_id,
    year_range,
    measure) {
  geography_keys <- unique(as.character(geography_keys))
  series_ids <- unique(as.character(series_ids))

  # Production fast path: deterministic values and uncertainty limits are read
  # together from one small sex/age partition. No raw uncertainty draw is
  # scanned during the Shiny session.
  fast <- collect_fast_ui_results(
    runtime = runtime,
    geography_keys = geography_keys,
    sex_code = sex_code,
    series_ids = series_ids,
    age_id = age_id,
    year_range = year_range,
    measure = measure
  )
  if (nrow(fast)) return(fast)

  pieces <- lapply(geography_keys, function(geography_key) {
    geography <- parse_geography_key(geography_key)
    if (is.na(geography$type) || is.na(geography$code)) {
      return(data.table::data.table())
    }
    data.table::rbindlist(lapply(series_ids, function(series_id) {
      collect_cause_rates(
        runtime$cause_rates_dataset,
        models = "NBD3-R",
        geography_type = geography$type,
        geography_codes = geography$code,
        sex_code = sex_code,
        series_id = series_id,
        age_id = age_id,
        year_range = year_range,
        measure = measure
      )
    }), use.names = TRUE, fill = TRUE)
  })
  d <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  attach_exact_cause_uncertainty(d)
}

update_result_age_choices <- function(input_id, selected_measure) {
  ages <- nbd_result_ages()[order(age_sort_order)]
  choices <- if (identical(selected_measure, "asr")) {
    ages[age_id == "asr_all"]
  } else if (identical(selected_measure, "crude_rate")) {
    # Neonatal deaths are available, but the current population input has no
    # separate neonatal denominator. The combined under-one rate is supported.
    ages[age_id != "asr_all" & age_id != "age_0"]
  } else {
    ages[age_id != "asr_all"]
  }
  preferred <- if (identical(selected_measure, "asr")) "asr_all" else "age_all"
  shiny::updateSelectizeInput(
    session,
    input_id,
    choices = named_choices(choices, "age_id", "age_label"),
    selected = safe_default(choices$age_id, preferred),
    server = TRUE
  )
}

result_geography_panel_ui <- function(prefix = "result_geography") {
  ids <- paste0(prefix, c(
    "_scope", "_geographies", "_hierarchy", "_cause", "_sex",
    "_age", "_measure", "_years"
  ))
  ages <- nbd_result_ages()[order(age_sort_order)]
  sexes <- nbd_result_sexes()[order(sex_code)]
  shiny::tagList(
    htmltools::div(
      class = "control-panel research-control-panel",
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(
          ids[[1L]], "Comparison set",
          choices = c(
            "South Africa and provinces" = "province",
            "South Africa and population groups" = "population_group"
          ),
          selected = "province"
        )),
        shiny::column(5, shiny::selectizeInput(
          ids[[2L]], "Geographies",
          choices = geography_choices_for_scope("province"),
          selected = unname(geography_choices_for_scope("province")),
          multiple = TRUE
        )),
        shiny::column(4, shiny::selectInput(
          ids[[3L]], "Cause hierarchy",
          choices = result_hierarchy_choices,
          selected = "broad"
        ))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::selectizeInput(
          ids[[4L]], "Cause or aggregate",
          choices = cause_choices_for_hierarchy("broad"),
          selected = default_cause_ids("broad", multiple = FALSE),
          multiple = FALSE
        )),
        shiny::column(2, shiny::selectizeInput(
          ids[[5L]], "Sex",
          choices = named_choices(sexes, "sex_code", "sex"),
          selected = safe_default(sexes$sex_code, 3L),
          multiple = FALSE
        )),
        shiny::column(3, shiny::selectizeInput(
          ids[[6L]], "Age group",
          choices = named_choices(ages[age_id != "asr_all"], "age_id", "age_label"),
          selected = safe_default(ages$age_id, "age_all"),
          multiple = FALSE
        )),
        shiny::column(3, shiny::selectInput(
          ids[[7L]], "Measure",
          choices = measure_choices,
          selected = "crude_rate"
        ))
      ),
      shiny::sliderInput(
        ids[[8L]], "Years",
        min = as.integer(config$labels$build$start_year),
        max = as.integer(config$labels$build$end_year),
        value = c(
          as.integer(config$labels$build$start_year),
          as.integer(config$labels$build$end_year)
        ),
        sep = ""
      )
    ),
    htmltools::div(
      class = "report-chart wide-report-chart",
      highcharter::highchartOutput(paste0(prefix, "_chart"), height = "540px")
    ),
    shiny::uiOutput(paste0(prefix, "_note")),
    htmltools::tags$details(
      class = "appendix-detail",
      htmltools::tags$summary("View and download the selected geography comparison"),
      htmltools::div(
        class = "report-table",
        shiny::uiOutput(paste0(prefix, "_table")),
        shiny::downloadLink(
          paste0(prefix, "_download"),
          "Download selected geography comparison",
          class = "download-link"
        )
      )
    )
  )
}

register_result_geography_panel <- function(prefix = "result_geography") {
  ids <- list(
    scope = paste0(prefix, "_scope"),
    geographies = paste0(prefix, "_geographies"),
    hierarchy = paste0(prefix, "_hierarchy"),
    cause = paste0(prefix, "_cause"),
    sex = paste0(prefix, "_sex"),
    age = paste0(prefix, "_age"),
    measure = paste0(prefix, "_measure"),
    years = paste0(prefix, "_years"),
    chart = paste0(prefix, "_chart"),
    note = paste0(prefix, "_note"),
    table = paste0(prefix, "_table"),
    download = paste0(prefix, "_download")
  )

  shiny::observeEvent(input[[ids$scope]], {
    choices <- geography_choices_for_scope(input[[ids$scope]] %||% "province")
    shiny::updateSelectizeInput(
      session, ids$geographies,
      choices = choices,
      selected = unname(choices),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[ids$hierarchy]], {
    hierarchy <- input[[ids$hierarchy]] %||% "broad"
    shiny::updateSelectizeInput(
      session, ids$cause,
      choices = cause_choices_for_hierarchy(hierarchy, grouped = identical(hierarchy, "all")),
      selected = default_cause_ids(hierarchy, multiple = FALSE),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[ids$measure]], {
    update_result_age_choices(ids$age, input[[ids$measure]] %||% "crude_rate")
  }, ignoreInit = FALSE)

  selection_key <- shiny::reactive({
    shiny::req(
      input[[ids$geographies]], input[[ids$cause]], input[[ids$sex]],
      input[[ids$age]], input[[ids$measure]], input[[ids$years]]
    )
    list(
      panel = prefix,
      geographies = as.character(input[[ids$geographies]]),
      cause = as.character(input[[ids$cause]]),
      sex = as.integer(input[[ids$sex]]),
      age = as.character(input[[ids$age]]),
      measure = as.character(input[[ids$measure]]),
      years = as.integer(input[[ids$years]])
    )
  })

  selected_data <- nbd_cached_reactive(
    selection_key,
    function(key) {
      d <- collect_result_slice(
        geography_keys = key$geographies,
        sex_code = key$sex,
        series_ids = key$cause,
        age_id = key$age,
        year_range = key$years,
        measure = key$measure
      )
      if (nrow(d)) {
        d[, line_name := geography]
        data.table::setorder(d, geography_type, geography_code, year)
      }
      d
    },
    millis = 250L
  )

  output[[ids$chart]] <- highcharter::renderHighchart({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(empty_nbd_chart(height = 540))
    hc_nbd_lines(
      d,
      y = "estimate",
      series = "line_name",
      lower = "lower",
      upper = "upper",
      title = unique(d$series_label)[[1L]],
      subtitle = paste(unique(d$sex), unique(d$age_label), sep = " — "),
      x_title = "Year",
      y_title = measure_label(input[[ids$measure]], config),
      config = config,
      decimals = metric_decimals(input[[ids$measure]]),
      height = 540,
      filename = paste0(prefix, "-geography-comparison")
    )
  })

  output[[ids$note]] <- shiny::renderUI({
    d <- selected_data()
    if (!nrow(d)) {
      return(htmltools::div(
        class = "narrative-box warning",
        "No NBD3-R result is available for this selection."
      ))
    }
    interval_rows <- d[is.finite(lower) & is.finite(upper), .N]
    htmltools::div(
      class = if (interval_rows) "narrative-box success" else "narrative-box",
      htmltools::strong("Geography comparison. "),
      paste0(
        data.table::uniqueN(d$line_name), " series are displayed; ",
        interval_rows, " of ", nrow(d),
        " plotted cells have exact 95% intervals from ", uncertainty_draw_label, "."
      )
    )
  })

  output[[ids$table]] <- shiny::renderUI({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(nbd_kable_html(data.frame()))
    measure <- input[[ids$measure]]
    digits <- metric_decimals(measure)
    display <- d[, .(
      Geography = geography,
      Sex = sex,
      Year = year,
      Age = age_label,
      Cause = series_label,
      Measure = measure,
      Estimate = estimate,
      Lower = lower,
      Upper = upper,
      `Draws used` = n_draws
    )]
    display[, `:=`(
      Estimate = format_table_value(Estimate, measure, digits),
      Lower = format_table_value(Lower, measure, digits),
      Upper = format_table_value(Upper, measure, digits),
      `Draws used` = ifelse(is.na(`Draws used`), "—", format(`Draws used`, big.mark = ","))
    )]
    nbd_kable_html(
      display,
      caption = "Selected NBD3-R values by geography or population group",
      align = c("l", "l", "r", "l", "l", "l", "r", "r", "r", "r")
    )
  })

  output[[ids$download]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "-geography-comparison.csv"),
    content = function(file) data.table::fwrite(selected_data(), file)
  )
  invisible(TRUE)
}

result_cause_panel_ui <- function(prefix = "result_cause") {
  ids <- paste0(prefix, c(
    "_scope", "_geography", "_hierarchy", "_causes", "_sex",
    "_age", "_measure", "_years"
  ))
  ages <- nbd_result_ages()[order(age_sort_order)]
  sexes <- nbd_result_sexes()[order(sex_code)]
  province_choices <- geography_choices_for_scope("province")
  shiny::tagList(
    htmltools::div(
      class = "control-panel research-control-panel",
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(
          ids[[1L]], "Population set",
          choices = c(
            "South Africa or province" = "province",
            "South Africa or population group" = "population_group"
          ),
          selected = "province"
        )),
        shiny::column(4, shiny::selectizeInput(
          ids[[2L]], "Geography or population group",
          choices = province_choices,
          selected = make_geography_key("national", 10L),
          multiple = FALSE
        )),
        shiny::column(5, shiny::selectInput(
          ids[[3L]], "Cause hierarchy",
          choices = result_hierarchy_choices,
          selected = "all"
        ))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::selectizeInput(
          ids[[4L]], "Causes or aggregates",
          choices = cause_choices_for_hierarchy("all", grouped = TRUE),
          selected = default_cause_ids("all", multiple = TRUE),
          multiple = TRUE
        )),
        shiny::column(2, shiny::selectizeInput(
          ids[[5L]], "Sex",
          choices = named_choices(sexes, "sex_code", "sex"),
          selected = safe_default(sexes$sex_code, 3L),
          multiple = FALSE
        )),
        shiny::column(2, shiny::selectizeInput(
          ids[[6L]], "Age group",
          choices = named_choices(ages[age_id != "asr_all"], "age_id", "age_label"),
          selected = safe_default(ages$age_id, "age_all"),
          multiple = FALSE
        )),
        shiny::column(2, shiny::selectInput(
          ids[[7L]], "Measure",
          choices = measure_choices,
          selected = "crude_rate"
        ))
      ),
      shiny::sliderInput(
        ids[[8L]], "Years",
        min = as.integer(config$labels$build$start_year),
        max = as.integer(config$labels$build$end_year),
        value = c(
          as.integer(config$labels$build$start_year),
          as.integer(config$labels$build$end_year)
        ),
        sep = ""
      )
    ),
    htmltools::div(
      class = "report-chart wide-report-chart",
      highcharter::highchartOutput(paste0(prefix, "_chart"), height = "540px")
    ),
    shiny::uiOutput(paste0(prefix, "_note")),
    htmltools::tags$details(
      class = "appendix-detail",
      htmltools::tags$summary("View and download the selected cause comparison"),
      htmltools::div(
        class = "report-table",
        shiny::uiOutput(paste0(prefix, "_table")),
        shiny::downloadLink(
          paste0(prefix, "_download"),
          "Download selected cause comparison",
          class = "download-link"
        )
      )
    )
  )
}

register_result_cause_panel <- function(prefix = "result_cause") {
  ids <- list(
    scope = paste0(prefix, "_scope"),
    geography = paste0(prefix, "_geography"),
    hierarchy = paste0(prefix, "_hierarchy"),
    causes = paste0(prefix, "_causes"),
    sex = paste0(prefix, "_sex"),
    age = paste0(prefix, "_age"),
    measure = paste0(prefix, "_measure"),
    years = paste0(prefix, "_years"),
    chart = paste0(prefix, "_chart"),
    note = paste0(prefix, "_note"),
    table = paste0(prefix, "_table"),
    download = paste0(prefix, "_download")
  )

  shiny::observeEvent(input[[ids$scope]], {
    scope <- input[[ids$scope]] %||% "province"
    choices <- geography_choices_for_scope(scope)
    selected <- if (identical(scope, "population_group")) {
      unname(choices)[[1L]]
    } else {
      make_geography_key("national", 10L)
    }
    shiny::updateSelectizeInput(
      session, ids$geography,
      choices = choices,
      selected = selected,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[ids$hierarchy]], {
    hierarchy <- input[[ids$hierarchy]] %||% "all"
    shiny::updateSelectizeInput(
      session, ids$causes,
      choices = cause_choices_for_hierarchy(hierarchy, grouped = identical(hierarchy, "all")),
      selected = default_cause_ids(hierarchy, multiple = TRUE),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[ids$measure]], {
    update_result_age_choices(ids$age, input[[ids$measure]] %||% "crude_rate")
  }, ignoreInit = FALSE)

  selection_key <- shiny::reactive({
    shiny::req(
      input[[ids$geography]], input[[ids$causes]], input[[ids$sex]],
      input[[ids$age]], input[[ids$measure]], input[[ids$years]]
    )
    list(
      panel = prefix,
      geography = as.character(input[[ids$geography]]),
      causes = as.character(input[[ids$causes]]),
      sex = as.integer(input[[ids$sex]]),
      age = as.character(input[[ids$age]]),
      measure = as.character(input[[ids$measure]]),
      years = as.integer(input[[ids$years]])
    )
  })

  selected_data <- nbd_cached_reactive(
    selection_key,
    function(key) {
      d <- collect_result_slice(
        geography_keys = key$geography,
        sex_code = key$sex,
        series_ids = key$causes,
        age_id = key$age,
        year_range = key$years,
        measure = key$measure
      )
      if (nrow(d)) {
        label_map <- unique(d[, .(series_id, series_label, hierarchy)])
        label_map[, line_name := series_label]
        if (anyDuplicated(label_map$line_name)) {
          label_map[, line_name := paste0(
            series_label, " [", humanise(hierarchy), "]"
          )]
        }
        d <- merge(
          d,
          label_map[, .(series_id, line_name)],
          by = "series_id",
          all.x = TRUE,
          sort = FALSE
        )
        data.table::setorder(d, series_id, year)
      }
      d
    },
    millis = 250L
  )

  output[[ids$chart]] <- highcharter::renderHighchart({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(empty_nbd_chart(height = 540))
    hc_nbd_lines(
      d,
      y = "estimate",
      series = "line_name",
      lower = "lower",
      upper = "upper",
      title = unique(d$geography)[[1L]],
      subtitle = paste(unique(d$sex), unique(d$age_label), sep = " — "),
      x_title = "Year",
      y_title = measure_label(input[[ids$measure]], config),
      config = config,
      decimals = metric_decimals(input[[ids$measure]]),
      height = 540,
      filename = paste0(prefix, "-cause-comparison")
    )
  })

  output[[ids$note]] <- shiny::renderUI({
    d <- selected_data()
    if (!nrow(d)) {
      return(htmltools::div(
        class = "narrative-box warning",
        "No NBD3-R result is available for this selection."
      ))
    }
    interval_rows <- d[is.finite(lower) & is.finite(upper), .N]
    htmltools::div(
      class = if (interval_rows) "narrative-box success" else "narrative-box",
      htmltools::strong("Cause comparison. "),
      paste0(
        data.table::uniqueN(d$series_id), " causes or aggregates are displayed; ",
        interval_rows, " of ", nrow(d),
        " plotted cells have exact 95% intervals from ", uncertainty_draw_label, "."
      )
    )
  })

  output[[ids$table]] <- shiny::renderUI({
    d <- data.table::copy(selected_data())
    if (!nrow(d)) return(nbd_kable_html(data.frame()))
    measure <- input[[ids$measure]]
    digits <- metric_decimals(measure)
    display <- d[, .(
      Geography = geography,
      Sex = sex,
      Year = year,
      Age = age_label,
      Hierarchy = humanise(hierarchy),
      Cause = series_label,
      Measure = measure,
      Estimate = estimate,
      Lower = lower,
      Upper = upper,
      `Draws used` = n_draws
    )]
    display[, `:=`(
      Estimate = format_table_value(Estimate, measure, digits),
      Lower = format_table_value(Lower, measure, digits),
      Upper = format_table_value(Upper, measure, digits),
      `Draws used` = ifelse(is.na(`Draws used`), "—", format(`Draws used`, big.mark = ","))
    )]
    nbd_kable_html(
      display,
      caption = "Selected NBD3-R causes and aggregates within one population",
      align = c("l", "l", "r", "l", "l", "l", "l", "r", "r", "r", "r")
    )
  })

  output[[ids$download]] <- shiny::downloadHandler(
    filename = function() paste0(prefix, "-cause-comparison.csv"),
    content = function(file) data.table::fwrite(selected_data(), file)
  )
  invisible(TRUE)
}
