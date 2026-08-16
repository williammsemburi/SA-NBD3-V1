# Query-optimised uncertainty cache for the interactive report ----------------
#
# The analytical archive retains one complete Parquet file per uncertainty
# draw. Those files are the correct research record, but scanning all of them
# during a live Shiny session is unnecessarily expensive. The offline cache
# builder converts the full draw archive into compact interval tables
# partitioned by the fields used most often by the report. This module reads one
# small partition per selection and keeps recently used partitions in a bounded
# in-process cache.

nbd_parse_geography_key <- function(key) {
  pieces <- strsplit(as.character(key), "::", fixed = TRUE)[[1L]]
  if (length(pieces) != 2L) {
    return(list(type = NA_character_, code = NA_integer_))
  }
  list(
    type = pieces[[1L]],
    code = suppressWarnings(as.integer(pieces[[2L]]))
  )
}

nbd_fast_ui_cache_paths <- function(config) {
  root <- file.path(config$labels$paths$derived_dir, "ui_uncertainty_cache")
  list(
    root = root,
    manifest = file.path(root, "manifest.yml"),
    province = file.path(root, "province"),
    population_group = file.path(root, "population_group"),
    comparison = file.path(root, "model_comparison_uncertainty.parquet")
  )
}

open_fast_ui_cache_runtime <- function(config) {
  viz_require_packages(c("arrow", "cachem", "data.table", "yaml"))
  paths <- nbd_fast_ui_cache_paths(config)
  if (!file.exists(paths$manifest) || !dir.exists(paths$province)) {
    return(NULL)
  }

  manifest <- yaml::read_yaml(paths$manifest)
  n_draws <- suppressWarnings(as.integer(manifest$n_draws %||% NA_integer_))
  if (!is.finite(n_draws) || n_draws < 2L) {
    stop(
      "The Shiny uncertainty-cache manifest has an invalid n_draws value.",
      call. = FALSE
    )
  }

  comparison <- if (file.exists(paths$comparison)) {
    data.table::as.data.table(arrow::read_parquet(paths$comparison))
  } else {
    data.table::data.table()
  }

  partition_cache_mb <- suppressWarnings(as.numeric(
    Sys.getenv("NBD3_PARTITION_CACHE_MB", "64")
  ))
  if (!is.finite(partition_cache_mb) || partition_cache_mb < 16) {
    partition_cache_mb <- 64
  }

  info <- list(
    mode = "summary_cache",
    manifest = manifest,
    paths = paths,
    n_draws = n_draws,
    comparison = comparison,
    partition_cache = cachem::cache_mem(
      max_size = partition_cache_mb * 1024^2
    )
  )

  # Warm the four partitions used by the default report views. This moves a
  # handful of small Parquet reads into application startup, before the first
  # user interaction, and makes the initial all-age Person charts feel as fast
  # as later selections. The behaviour can be disabled for diagnostics with
  # NBD3_PREWARM_UI_CACHE=false.
  prewarm <- as_flag(Sys.getenv("NBD3_PREWARM_UI_CACHE", "true"), TRUE)
  if (isTRUE(prewarm)) {
    for (scope in c("province", "population_group")) {
      for (age_id in c("age_all", "asr_all")) {
        try(
          nbd_fast_ui_read_partition(
            info,
            storage_scope = scope,
            sex_code = 3L,
            age_id = age_id
          ),
          silent = TRUE
        )
      }
    }
  }

  info
}

nbd_fast_ui_partition_cache_key <- function(
    storage_scope,
    sex_code,
    age_id) {
  # cachem accepts only lowercase alphanumeric cache keys. The human-readable
  # partition identifier contains separators and underscores (for example,
  # province|3|asr_all), so convert each component to a collision-resistant
  # alphanumeric token before calling cachem$exists(), $get(), or $set().
  encode_component <- function(x) {
    raw <- charToRaw(enc2utf8(as.character(x)[[1L]]))
    paste(sprintf("%02x", as.integer(raw)), collapse = "")
  }

  paste0(
    "s", encode_component(storage_scope),
    "x", as.integer(sex_code),
    "a", encode_component(age_id)
  )
}

nbd_fast_ui_partition_path <- function(
    info,
    storage_scope,
    sex_code,
    age_id) {
  root <- if (identical(storage_scope, "province")) {
    info$paths$province
  } else if (identical(storage_scope, "population_group")) {
    info$paths$population_group
  } else {
    stop("Unknown Shiny-cache storage scope: ", storage_scope, call. = FALSE)
  }
  file.path(
    root,
    paste0("sex_code=", as.integer(sex_code)),
    paste0("age_id=", as.character(age_id)),
    "part-0.parquet"
  )
}

nbd_fast_ui_read_partition <- function(
    info,
    storage_scope,
    sex_code,
    age_id) {
  key <- nbd_fast_ui_partition_cache_key(storage_scope, sex_code, age_id)
  if (isTRUE(info$partition_cache$exists(key))) {
    return(data.table::copy(info$partition_cache$get(key)))
  }

  path <- nbd_fast_ui_partition_path(
    info,
    storage_scope,
    sex_code,
    age_id
  )
  if (!file.exists(path)) return(data.table::data.table())

  out <- data.table::as.data.table(arrow::read_parquet(path))
  # sex_code and age_id are encoded in the partition path and deliberately not
  # duplicated inside each Parquet file.
  sex_value <- as.integer(sex_code)
  age_value <- as.character(age_id)
  out[, `:=`(
    sex_code = sex_value,
    age_id = age_value
  )]
  data.table::setindexv(
    out,
    intersect(
      c("geography_type", "geography_code", "series_id", "year"),
      names(out)
    )
  )
  info$partition_cache$set(key, data.table::copy(out))
  out[]
}

nbd_fast_ui_filter_partition <- function(
    data,
    geography_types,
    geography_codes,
    series_ids,
    years) {
  if (!nrow(data)) return(data.table::data.table())
  geography_types <- unique(as.character(geography_types))
  geography_codes <- unique(as.integer(geography_codes))
  series_ids <- unique(as.character(series_ids))
  years <- sort(unique(as.integer(years)))
  years <- years[is.finite(years)]
  if (!length(years)) return(data.table::data.table())

  data[
    geography_type %in% geography_types &
      geography_code %in% geography_codes &
      series_id %in% series_ids &
      year >= min(years) & year <= max(years)
  ]
}

collect_fast_ui_results <- function(
    runtime,
    geography_keys,
    sex_code,
    series_ids,
    age_id,
    year_range,
    measure) {
  info <- runtime$full_uncertainty
  if (is.null(info) || !identical(info$mode, "summary_cache")) {
    return(data.table::data.table())
  }

  geography_keys <- unique(as.character(geography_keys))
  parsed <- lapply(geography_keys, nbd_parse_geography_key)
  geography_types <- vapply(parsed, `[[`, character(1), "type")
  geography_codes <- as.integer(vapply(parsed, `[[`, numeric(1), "code"))
  valid <- !is.na(geography_types) & nzchar(geography_types) &
    !is.na(geography_codes)
  geography_types <- geography_types[valid]
  geography_codes <- geography_codes[valid]
  if (!length(geography_codes)) return(data.table::data.table())

  years <- seq.int(min(as.integer(year_range)), max(as.integer(year_range)))
  pieces <- list()

  province_index <- geography_types %in% c("national", "province")
  if (any(province_index)) {
    partition <- nbd_fast_ui_read_partition(
      info,
      storage_scope = "province",
      sex_code = sex_code,
      age_id = age_id
    )
    pieces[[length(pieces) + 1L]] <- nbd_fast_ui_filter_partition(
      partition,
      geography_types = geography_types[province_index],
      geography_codes = geography_codes[province_index],
      series_ids = series_ids,
      years = years
    )
  }

  population_index <- geography_types == "population_group"
  if (any(population_index)) {
    partition <- nbd_fast_ui_read_partition(
      info,
      storage_scope = "population_group",
      sex_code = sex_code,
      age_id = age_id
    )
    pieces[[length(pieces) + 1L]] <- nbd_fast_ui_filter_partition(
      partition,
      geography_types = "population_group",
      geography_codes = geography_codes[population_index],
      series_ids = series_ids,
      years = years
    )
  }

  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) return(out)

  measure <- as.character(measure)[[1L]]
  if (identical(measure, "deaths")) {
    out[, `:=`(
      estimate = as.numeric(deaths),
      lower = as.numeric(lower_deaths),
      upper = as.numeric(upper_deaths),
      draw_mean = as.numeric(draw_mean_deaths),
      draw_median = as.numeric(draw_median_deaths),
      draw_sd = as.numeric(draw_sd_deaths)
    )]
  } else if (identical(measure, "crude_rate")) {
    out[, `:=`(
      estimate = as.numeric(crude_rate),
      lower = safe_ratio(lower_deaths, population, 1e5),
      upper = safe_ratio(upper_deaths, population, 1e5),
      draw_mean = safe_ratio(draw_mean_deaths, population, 1e5),
      draw_median = safe_ratio(draw_median_deaths, population, 1e5),
      draw_sd = safe_ratio(draw_sd_deaths, population, 1e5)
    )]
  } else if (identical(measure, "asr")) {
    out[, `:=`(
      estimate = as.numeric(asr),
      lower = as.numeric(lower_asr),
      upper = as.numeric(upper_asr),
      draw_mean = as.numeric(draw_mean_asr),
      draw_median = as.numeric(draw_median_asr),
      draw_sd = as.numeric(draw_sd_asr)
    )]
  } else {
    stop("Unsupported fast-cache measure: ", measure, call. = FALSE)
  }

  out[, `:=`(
    model = "NBD3-R",
    measure = measure,
    unit = measure_unit(measure),
    source = "NBD3-R deployment cache"
  )]

  required <- c(
    "model", "geography_type", "geography_code", "geography",
    "sex_code", "sex", "year", "age_id", "age_label", "series_id",
    "series_label", "domain", "hierarchy", "cause_type", "population",
    "estimate", "lower", "upper", "draw_mean", "draw_median", "draw_sd",
    "n_draws", "uncertainty_source", "measure", "unit",
    "series_sort_order", "age_sort_order", "source"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) out[, (missing) := NA]
  out <- out[, ..required]
  data.table::setorder(
    out,
    geography_type,
    geography_code,
    series_sort_order,
    year
  )
  out[]
}

collect_fast_cause_uncertainty <- function(runtime, point_rows) {
  d <- data.table::as.data.table(data.table::copy(point_rows))
  if (!nrow(d)) return(data.table::data.table())

  unique_value <- function(x, label) {
    values <- unique(x)
    if (length(values) != 1L) {
      stop("Fast uncertainty query requires one ", label, ".", call. = FALSE)
    }
    values[[1L]]
  }

  geography_keys <- unique(paste(d$geography_type, d$geography_code, sep = "::"))
  out <- collect_fast_ui_results(
    runtime = runtime,
    geography_keys = geography_keys,
    sex_code = unique_value(d$sex_code, "sex"),
    series_ids = unique(d$series_id),
    age_id = unique_value(d$age_id, "age group"),
    year_range = range(d$year),
    measure = unique_value(d$measure, "measure")
  )
  if (!nrow(out)) return(out)

  out[, .(
    geography_type, geography_code, sex_code, year, age_id, series_id,
    measure, lower, upper, draw_mean, draw_median, draw_sd, n_draws,
    source = uncertainty_source
  )]
}

collect_fast_comparison_uncertainty <- function(runtime, point_rows) {
  info <- runtime$full_uncertainty
  d <- data.table::as.data.table(data.table::copy(point_rows))
  if (is.null(info) || !identical(info$mode, "summary_cache") ||
      !nrow(d) || !nrow(info$comparison)) {
    return(data.table::data.table())
  }

  key <- c(
    "geography_type", "geography_code", "sex_code", "year",
    "age_id", "series_id", "measure"
  )
  u <- info$comparison[
    geography_type %in% unique(d$geography_type) &
      geography_code %in% unique(d$geography_code) &
      sex_code %in% unique(d$sex_code) &
      year %in% unique(d$year) &
      age_id %in% unique(d$age_id) &
      series_id %in% unique(d$series_id) &
      measure %in% unique(d$measure)
  ]
  if (!nrow(u)) return(u)
  unique(u, by = key)
}

report_method_asset <- function(config, filename, fallback = NULL) {
  candidate <- file.path(config$labels$paths$derived_dir, "methods", filename)
  if (file.exists(candidate)) return(candidate)
  fallback
}

# Preserve original functions for an explicit local-development fallback.
nbd_load_viz_runtime_data_dynamic <- load_viz_runtime_data
nbd_collect_full_cause_uncertainty_dynamic <- collect_full_cause_uncertainty
nbd_collect_full_comparison_uncertainty_dynamic <-
  collect_full_comparison_uncertainty
nbd_load_injury_report_inputs_dynamic <- load_injury_report_inputs

load_viz_runtime_data <- function(config = read_viz_config(), open_rates = TRUE) {
  runtime <- nbd_load_viz_runtime_data_dynamic(config, open_rates = FALSE)
  runtime$config <- config
  runtime$paths$cause_rates <- derived_path(config, "cause_rates.parquet")

  if (!isTRUE(open_rates)) {
    runtime$full_uncertainty <- NULL
    runtime$fast_ui_cache <- NULL
    return(runtime)
  }

  viz_require_packages(c("arrow", "dplyr"))
  runtime$cause_rates_dataset <- arrow::open_dataset(
    runtime$paths$cause_rates,
    format = "parquet"
  )

  fast <- open_fast_ui_cache_runtime(config)
  runtime$fast_ui_cache <- fast
  runtime$full_uncertainty <- if (!is.null(fast)) {
    fast
  } else {
    allow_raw <- as_flag(
      Sys.getenv("NBD3_ALLOW_RAW_UI_DRAWS", "false"),
      default = FALSE
    )
    if (isTRUE(allow_raw)) {
      warning(
        "The deployment cache is absent. Falling back to raw uncertainty ",
        "draws because NBD3_ALLOW_RAW_UI_DRAWS=true. First queries may be ",
        "slow; do not use this mode on shinyapps.io.",
        call. = FALSE
      )
      open_full_uncertainty_runtime(config)
    } else {
      warning(
        "The query-optimised uncertainty cache is absent. Point estimates ",
        "will load, but full-grid intervals are disabled. Run ",
        "Rscript dev/build_shiny_ui_cache.R before publishing the app.",
        call. = FALSE
      )
      NULL
    }
  }
  runtime
}

collect_full_cause_uncertainty <- function(runtime, point_rows) {
  info <- runtime$full_uncertainty
  if (!is.null(info) && identical(info$mode, "summary_cache")) {
    return(collect_fast_cause_uncertainty(runtime, point_rows))
  }
  nbd_collect_full_cause_uncertainty_dynamic(runtime, point_rows)
}

collect_full_comparison_uncertainty <- function(runtime, point_rows) {
  info <- runtime$full_uncertainty
  if (!is.null(info) && identical(info$mode, "summary_cache")) {
    return(collect_fast_comparison_uncertainty(runtime, point_rows))
  }
  nbd_collect_full_comparison_uncertainty_dynamic(runtime, point_rows)
}

load_injury_report_inputs <- function(config = read_viz_config()) {
  original <- nbd_load_injury_report_inputs_dynamic(config)
  asset <- function(filename, current) {
    path <- report_method_asset(config, filename, fallback = NA_character_)
    if (!is.na(path) && file.exists(path)) read_optional_table(path) else current
  }
  original$source_fractions <- asset(
    "03_injury_source_fractions.parquet", original$source_fractions
  )
  original$model_fractions <- asset(
    "03_injury_fractions.parquet", original$model_fractions
  )
  original$survey_model_comparison <- asset(
    "03_injury_survey_model_comparison.parquet",
    original$survey_model_comparison
  )
  original$diagnostics <- asset(
    "03_injury_model_diagnostics.csv", original$diagnostics
  )
  original$final_estimates <- asset(
    "03_final_injuries.parquet", original$final_estimates
  )
  original
}
