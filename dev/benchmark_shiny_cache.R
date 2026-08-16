#!/usr/bin/env Rscript

# Measure cold and warm query latency of the publication uncertainty cache.

locate_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "_targets.R")) &&
        file.exists(file.path(current, "report", "R", "report_cache.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the SA-NBD3 repository root.", call. = FALSE)
    }
    current <- parent
  }
}

for (package in c("arrow", "cachem", "data.table", "dplyr", "yaml")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Missing package: ", package, call. = FALSE)
  }
}

root <- locate_root()
setwd(root)
source(file.path(root, "report", "R", "report_data.R"), local = FALSE)
source(file.path(root, "report", "R", "report_cache.R"), local = FALSE)
config <- read_viz_config(root)
runtime <- load_viz_runtime_data(config, open_rates = TRUE)
if (is.null(runtime$full_uncertainty) ||
    !identical(runtime$full_uncertainty$mode, "summary_cache")) {
  stop("The query-optimised Shiny cache is not available.", call. = FALSE)
}

queries <- list(
  national_all_cause = list(
    geography_keys = "national::10", sex_code = 3L,
    series_ids = "za_172", age_id = "age_all",
    year_range = c(1997L, 2019L), measure = "deaths"
  ),
  provinces_hiv = list(
    geography_keys = paste0("province::", 1:9), sex_code = 3L,
    series_ids = "za_2", age_id = "age_all",
    year_range = c(1997L, 2019L), measure = "crude_rate"
  ),
  population_groups_injury = list(
    geography_keys = paste0("population_group::", 11:14), sex_code = 3L,
    series_ids = "za_171", age_id = "age_all",
    year_range = c(1997L, 2019L), measure = "deaths"
  ),
  female_asr = list(
    geography_keys = "national::10", sex_code = 2L,
    series_ids = c("za_2", "za_1", "za_126", "za_139"),
    age_id = "asr_all", year_range = c(1997L, 2019L), measure = "asr"
  )
)

results <- list()
for (query_name in names(queries)) {
  query <- queries[[query_name]]
  for (iteration in 1:5) {
    start <- proc.time()[[3L]]
    value <- do.call(
      collect_fast_ui_results,
      c(list(runtime = runtime), query)
    )
    elapsed <- proc.time()[[3L]] - start
    results[[length(results) + 1L]] <- data.table::data.table(
      query = query_name,
      iteration = iteration,
      cache_state = if (iteration == 1L) "cold" else "warm",
      rows = nrow(value),
      elapsed_seconds = elapsed
    )
  }
}

out <- data.table::rbindlist(results)
path <- file.path(root, "output", "tables", "shiny_cache_benchmark.csv")
dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(out, path)
print(out)
message("Benchmark written: ", path)
