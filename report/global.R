# Global startup for the Shiny-enabled NBD3 R Markdown report.
# Loaded once per R worker.

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  shiny.sanitize.errors = TRUE
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L ||
      (length(x) == 1L && is.na(x))) y else x
}

nbd_report_locate_dirs <- function() {
  source_path <- tryCatch(
    normalizePath(sys.frame(1L)$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  candidates <- unique(c(
    if (!is.na(source_path)) dirname(source_path) else character(),
    normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    normalizePath(file.path(getwd(), "report"), winslash = "/",
                  mustWork = FALSE)
  ))
  report_dir <- candidates[
    file.exists(file.path(candidates, "nbd3_results_report.Rmd"))
  ][1L]
  if (!length(report_dir) || is.na(report_dir)) {
    stop("Could not locate report/nbd3_results_report.Rmd.", call. = FALSE)
  }
  list(
    report_dir = normalizePath(report_dir, winslash = "/", mustWork = TRUE),
    root = normalizePath(dirname(report_dir), winslash = "/", mustWork = TRUE)
  )
}

startup_started <- proc.time()[[3L]]
dirs <- nbd_report_locate_dirs()
nbd_report_dir <- dirs$report_dir
nbd_report_root <- dirs$root
rm(dirs)

required_packages <- c(
  "arrow", "cachem", "data.table", "dplyr", "highcharter", "htmltools",
  "knitr", "rmarkdown", "scales", "shiny", "yaml"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing report package(s): ", paste(missing_packages, collapse = ", "),
    ". Run Rscript install_packages.R.",
    call. = FALSE
  )
}

for (helper in c("report_data.R", "report_cache.R", "report_charts.R")) {
  source(file.path(nbd_report_dir, "R", helper), local = .GlobalEnv)
}

state_path <- file.path(nbd_report_dir, "data", "report_state.rds")
if (!file.exists(state_path)) {
  stop(
    "Prepared report state is missing: ", state_path,
    ". Run Rscript build_report_state.R.",
    call. = FALSE
  )
}

nbd_report_state <- readRDS(state_path)
if (!is.list(nbd_report_state) ||
    !identical(as.integer(nbd_report_state$schema_version), 7L) ||
    !is.list(nbd_report_state$objects) ||
    length(nbd_report_state$objects) < 25L) {
  stop(
    "report_state.rds is incomplete or incompatible. Run ",
    "Rscript apply_shiny_deploy_startup_fix.R.",
    call. = FALSE
  )
}

config <- read_viz_config(nbd_report_root)
nbd_report_state$objects$config <- config
runtime <- list(
  config = config,
  comparisons = data.table::data.table(),
  cause_uncertainty = data.table::data.table(),
  catalogs = nbd_report_state$objects$catalog,
  paths = list(),
  report_database = NULL,
  full_uncertainty = NULL,
  cause_rates_dataset = NULL,
  comparison_store = NULL
)

comparison_relative <- nbd_report_state$objects$comparison_store$relative_path %||%
  file.path("report", "data", "model_comparison")
comparison_root <- normalizePath(
  file.path(nbd_report_root, comparison_relative),
  winslash = "/",
  mustWork = FALSE
)
if (dir.exists(comparison_root)) {
  comparison_cache_mb <- suppressWarnings(as.numeric(
    Sys.getenv("NBD3_COMPARISON_CACHE_MB", "64")
  ))
  if (!is.finite(comparison_cache_mb) || comparison_cache_mb < 16) {
    comparison_cache_mb <- 64
  }
  runtime$comparison_store <- list(
    root = comparison_root,
    manifest = nbd_report_state$objects$comparison_store$manifest,
    cache = cachem::cache_mem(max_size = comparison_cache_mb * 1024^2)
  )
} else {
  stop(
    "Prepared model-comparison store is missing: ", comparison_root,
    ". Run Rscript apply_shiny_deploy_startup_fix.R.",
    call. = FALSE
  )
}

if (!nzchar(Sys.getenv("NBD3_PREWARM_UI_CACHE", ""))) {
  Sys.setenv(NBD3_PREWARM_UI_CACHE = "false")
}

# Prefer the compact publication database if present, otherwise use the exact
# sex/age summary cache. Do not open the large cause-rate dataset when either
# query-optimised store is available.
if (exists("open_report_database_runtime", mode = "function")) {
  runtime$report_database <- tryCatch(
    open_report_database_runtime(config),
    error = function(e) NULL
  )
}

if (is.null(runtime$report_database) &&
    exists("open_fast_ui_cache_runtime", mode = "function")) {
  runtime$full_uncertainty <- tryCatch(
    open_fast_ui_cache_runtime(config),
    error = function(e) {
      warning(
        "Could not open the optimized UI cache: ", conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

if (is.null(runtime$report_database) && is.null(runtime$full_uncertainty)) {
  cause_rates_path <- if (exists("derived_path", mode = "function")) {
    derived_path(config, "cause_rates.parquet")
  } else {
    ""
  }
  if (nzchar(cause_rates_path) && file.exists(cause_rates_path)) {
    runtime$paths$cause_rates <- cause_rates_path
    runtime$cause_rates_dataset <- arrow::open_dataset(
      cause_rates_path, format = "parquet"
    )
  }
}

nbd_report_state$objects$ui_cache_version_key <- if (
    !is.null(runtime$full_uncertainty) &&
      identical(runtime$full_uncertainty$mode, "summary_cache")) {
  paste(
    runtime$full_uncertainty$manifest$cache_version %||% "cache",
    runtime$full_uncertainty$manifest$built_at %||% "unknown",
    runtime$full_uncertainty$n_draws,
    sep = "|"
  )
} else {
  nbd_report_state$objects$ui_cache_version_key %||% "point-only"
}

report_cache_mb <- suppressWarnings(as.numeric(
  Sys.getenv("NBD3_REPORT_CACHE_MB", "64")
))
if (!is.finite(report_cache_mb) || report_cache_mb < 16) report_cache_mb <- 64
shiny::shinyOptions(
  cache = cachem::cache_mem(max_size = report_cache_mb * 1024^2)
)

nbd_report_runtime <- runtime
startup_elapsed <- proc.time()[[3L]] - startup_started
message(sprintf(
  "NBD3 report worker state loaded in %.2f seconds (%.1f MB on disk).",
  startup_elapsed,
  file.info(state_path)$size / 1024^2
))
rm(runtime, config, required_packages, missing_packages, state_path,
   startup_started, startup_elapsed)
