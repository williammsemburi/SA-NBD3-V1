# Build the compact state loaded once per hosted Shiny worker.
#
# Large result grids and annual model-comparison rows remain in Parquet files.
# The serialized state contains only small catalogues and static methods tables.

nbd_state_serializable <- function(object) {
  if (is.function(object) || is.environment(object) ||
      typeof(object) %in% c("externalptr", "weakref")) {
    return(FALSE)
  }
  isTRUE(tryCatch({
    serialize(object, NULL, version = 3)
    TRUE
  }, error = function(e) FALSE))
}

nbd_state_size_bytes <- function(object) {
  as.numeric(utils::object.size(object))
}

nbd_comparison_store_domain <- function(x) {
  out <- tolower(gsub("[^a-z0-9]+", "_", as.character(x)))
  out <- gsub("(^_+|_+$)", "", out)
  ifelse(nzchar(out), out, "unknown")
}

nbd_write_model_comparison_store <- function(comparison_data, root) {
  required <- c(
    "domain", "geography_type", "geography_code", "geography",
    "sex_code", "sex", "year", "age_id", "age_label",
    "age_sort_order", "series_id", "series_label", "series_sort_order",
    "measure", "model", "estimate", "lower", "upper", "source"
  )
  missing <- setdiff(required, names(comparison_data))
  if (length(missing)) {
    stop(
      "Model-comparison data are missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x <- data.table::as.data.table(data.table::copy(
    comparison_data[, required, with = FALSE]
  ))
  x[, `:=`(
    domain = as.character(domain),
    geography_type = as.character(geography_type),
    geography_code = as.integer(geography_code),
    geography = as.character(geography),
    sex_code = as.integer(sex_code),
    sex = as.character(sex),
    year = as.integer(year),
    age_id = as.character(age_id),
    age_label = as.character(age_label),
    age_sort_order = as.integer(age_sort_order),
    series_id = as.character(series_id),
    series_label = as.character(series_label),
    series_sort_order = as.integer(series_sort_order),
    measure = as.character(measure),
    model = as.character(model),
    estimate = as.numeric(estimate),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    source = as.character(source)
  )]
  x <- x[!is.na(domain) & nzchar(domain)]
  if (!nrow(x)) {
    stop("Model-comparison data contain no valid rows.", call. = FALSE)
  }
  data.table::setorder(
    x, domain, geography_type, geography_code, sex_code,
    age_sort_order, series_sort_order, model, year
  )

  target <- file.path(root, "report", "data", "model_comparison")
  temp <- paste0(target, ".tmp-", Sys.getpid())
  if (dir.exists(temp)) unlink(temp, recursive = TRUE, force = TRUE)
  dir.create(temp, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    if (dir.exists(temp)) unlink(temp, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  domains <- sort(unique(x$domain))
  manifest_rows <- vector("list", length(domains))
  for (i in seq_along(domains)) {
    domain_value <- domains[[i]]
    domain_slug <- nbd_comparison_store_domain(domain_value)
    domain_dir <- file.path(temp, paste0("domain=", domain_slug))
    dir.create(domain_dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(domain_dir, "part-0.parquet")
    part <- x[domain == domain_value]
    arrow::write_parquet(
      as.data.frame(part),
      path,
      compression = "zstd"
    )
    manifest_rows[[i]] <- data.frame(
      domain = domain_value,
      domain_slug = domain_slug,
      rows = nrow(part),
      file = file.path(paste0("domain=", domain_slug), "part-0.parquet"),
      size_bytes = as.numeric(file.info(path)$size),
      stringsAsFactors = FALSE
    )
  }

  manifest <- data.table::rbindlist(manifest_rows, use.names = TRUE, fill = TRUE)
  utils::write.csv(
    manifest,
    file.path(temp, "manifest.csv"),
    row.names = FALSE,
    na = ""
  )

  if (dir.exists(target)) unlink(target, recursive = TRUE, force = TRUE)
  if (!file.rename(temp, target)) {
    stop("Could not install the model-comparison Parquet store.", call. = FALSE)
  }

  list(
    relative_path = file.path("report", "data", "model_comparison"),
    manifest = as.data.frame(manifest),
    row_count = nrow(x),
    domain_count = length(domains),
    columns = required
  )
}

build_nbd_report_state <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

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

  helper_files <- file.path(
    root, "report", "R",
    c("report_data.R", "report_cache.R", "report_charts.R", "report_panels.R")
  )
  source_file <- file.path(root, "report", "R", "report_state_source.R")
  required_files <- c(helper_files, source_file)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files)) {
    stop(
      "Report-state source file(s) are missing:\n- ",
      paste(missing_files, collapse = "\n- "),
      call. = FALSE
    )
  }

  build_env <- new.env(parent = globalenv())
  build_env$root <- root
  for (path in helper_files) {
    sys.source(path, envir = build_env)
  }

  old_prewarm <- Sys.getenv("NBD3_PREWARM_UI_CACHE", unset = NA_character_)
  Sys.setenv(NBD3_PREWARM_UI_CACHE = "false")
  on.exit({
    if (is.na(old_prewarm)) {
      Sys.unsetenv("NBD3_PREWARM_UI_CACHE")
    } else {
      Sys.setenv(NBD3_PREWARM_UI_CACHE = old_prewarm)
    }
  }, add = TRUE)

  sys.source(source_file, envir = build_env)

  if (!exists("comparison_data", envir = build_env, inherits = FALSE)) {
    stop("Report preparation did not create comparison_data.", call. = FALSE)
  }
  comparison_store <- nbd_write_model_comparison_store(
    get("comparison_data", envir = build_env, inherits = FALSE),
    root
  )

  required_objects <- c(
    "config", "ui_cache_version_key", "comparison_catalog",
    "cause_uncertainty", "catalog", "injury_broad", "injury_level_audit",
    "uncertainty_draw_n", "uncertainty_draw_label", "model_names_available",
    "measure_choices", "injury_source_table", "injury_broad_fit_table",
    "injury_flagged_fit_table", "completeness_time_data",
    "completeness_province_data", "injury_anchor_data",
    "result_availability", "result_geography_catalog", "result_sex_catalog",
    "result_age_catalog", "result_cause_catalog", "report_hierarchy_labels",
    "result_hierarchies", "result_hierarchy_choices",
    "report_fallback_colours", "broad_age_profile_years",
    "broad_age_profile_ages", "broad_age_profile_causes"
  )

  missing_objects <- required_objects[
    !vapply(required_objects, exists, logical(1), envir = build_env,
            inherits = FALSE)
  ]
  if (length(missing_objects)) {
    stop(
      "Report-state preparation did not create required object(s): ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  objects <- mget(required_objects, envir = build_env, inherits = FALSE)
  objects$comparison_store <- comparison_store
  objects$cause_uncertainty <- data.table::data.table()
  objects$result_availability <- data.table::data.table()

  serializable <- vapply(objects, nbd_state_serializable, logical(1))
  if (!all(serializable)) {
    stop(
      "Report-state object(s) could not be serialized: ",
      paste(names(objects)[!serializable], collapse = ", "),
      call. = FALSE
    )
  }

  object_sizes <- vapply(objects, nbd_state_size_bytes, numeric(1))
  oversized <- names(object_sizes)[object_sizes > 32 * 1024^2]
  if (length(oversized)) {
    stop(
      "Refusing to create a slow hosted-worker state. Object(s) exceed 32 MiB: ",
      paste(
        paste0(oversized, " (", round(object_sizes[oversized] / 1024^2, 1),
               " MiB)"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  source_files <- unique(c(
    file.path(root, "report", "nbd3_results_report.Rmd"),
    required_files,
    file.path(root, "report", "config", "labels.yml"),
    file.path(root, "report", "config", "cause_comparison_map.csv"),
    file.path(root, "report", "config", "age_comparison_map.csv"),
    file.path(root, "output", "report-data", "ui_uncertainty_cache",
              "manifest.yml"),
    file.path(root, "report", "data", "model_comparison", "manifest.csv")
  ))
  source_files <- source_files[file.exists(source_files)]
  source_signature <- paste(unname(tools::md5sum(source_files)), collapse = "|")

  structure(
    list(
      schema_version = 7L,
      built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      r_version = as.character(getRversion()),
      source_signature = source_signature,
      object_sizes = object_sizes,
      objects = objects
    ),
    class = "nbd3_report_state"
  )
}
