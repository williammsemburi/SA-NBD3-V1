#!/usr/bin/env Rscript

# Build a query-optimised interval cache for the interactive NBD3 report.
#
# The analytical uncertainty archive remains one Parquet file per draw. This
# script reads those files offline, maps every draw to the published report
# hierarchy, derives all supported ages and ASRs, and writes compact partitioned
# summaries. The live Shiny report then reads one small partition instead of
# scanning the full draw archive.

options(stringsAsFactors = FALSE, scipen = 999)

locate_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "_targets.R")) &&
        file.exists(file.path(current, "report", "R", "report_data.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the SA-NBD3 repository root.", call. = FALSE)
    }
    current <- parent
  }
}

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package, "' is required. Run Rscript install_packages.R.",
      call. = FALSE
    )
  }
}

for (package in c(
  "arrow", "data.table", "dplyr", "Matrix", "matrixStats", "yaml"
)) require_package(package)

root <- locate_root()
setwd(root)
source(file.path(root, "report", "R", "report_data.R"), local = FALSE)
config <- read_viz_config(root)
info <- open_full_uncertainty_runtime(config)
if (is.null(info)) {
  stop(
    "No complete full-grid uncertainty draw store was found. Complete the ",
    "full-ui uncertainty run before building the Shiny cache.",
    call. = FALSE
  )
}

cache_version <- "1.0.1"
cache_root <- file.path(root, "output", "report-data", "ui_uncertainty_cache")
work_root <- file.path(root, "output", "report-data", ".ui_uncertainty_work")
overwrite <- as_flag(Sys.getenv("NBD3_UI_CACHE_OVERWRITE", "false"))
keep_work <- as_flag(Sys.getenv("NBD3_UI_CACHE_KEEP_WORK", "false"))
block_rows <- suppressWarnings(as.integer(
  Sys.getenv("NBD3_UI_CACHE_BLOCK_ROWS", "5000")
))
if (!is.finite(block_rows) || block_rows < 500L) block_rows <- 5000L

if (dir.exists(cache_root) && overwrite) unlink(cache_root, recursive = TRUE)
if (dir.exists(work_root) && overwrite) unlink(work_root, recursive = TRUE)
if (file.exists(file.path(cache_root, "manifest.yml")) && !overwrite) {
  stop(
    "A completed Shiny cache already exists: ", cache_root,
    "\nSet NBD3_UI_CACHE_OVERWRITE=true to rebuild it.",
    call. = FALSE
  )
}
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
dir.create(work_root, recursive = TRUE, showWarnings = FALSE)

message("SA-NBD3 Shiny deployment-cache builder")
message("Project root: ", root)
message("Draws: ", format(info$n_draws, big.mark = ","))
message("Cache output: ", cache_root)
message(
  "The builder processes one geography store and sex at a time. ",
  "Its largest temporary file is expected to require approximately 12 GB."
)

age_rows <- config$age_map[
  include_explorer %in% TRUE & metric_type == "standard"
][order(sort_order)]
age_specs <- lapply(seq_len(nrow(age_rows)), function(index) {
  list(
    age_id = as.character(age_rows$age_id[[index]]),
    age_label = as.character(age_rows$age_label[[index]]),
    sort_order = as.integer(age_rows$sort_order[[index]]),
    codes = full_uncertainty_expand_database_ages(
      parse_integer_codes(age_rows$database_age_codes[[index]])
    ),
    measure = "deaths"
  )
})
age_specs[[length(age_specs) + 1L]] <- list(
  age_id = "asr_all",
  age_label = "ASR All ages",
  sort_order = 50L,
  codes = 0:19,
  measure = "asr"
)

comparison_age_rows <- config$age_map[
  include_comparison %in% TRUE & metric_type == "standard"
][order(sort_order)]
comparison_age_specs <- lapply(seq_len(nrow(comparison_age_rows)), function(index) {
  list(
    age_id = as.character(comparison_age_rows$age_id[[index]]),
    age_label = as.character(comparison_age_rows$age_label[[index]]),
    sort_order = as.integer(comparison_age_rows$sort_order[[index]]),
    codes = full_uncertainty_expand_database_ages(
      parse_integer_codes(comparison_age_rows$database_age_codes[[index]])
    )
  )
})

explorer_meta <- config$cause_map[
  include_explorer %in% TRUE
][order(sort_order)]
series_ids <- as.character(explorer_meta$series_id)
comparison_meta <- config$cause_map[
  include_comparison %in% TRUE
][order(sort_order)]
comparison_series_ids <- as.character(comparison_meta$series_id)

mapping_long <- full_uncertainty_source_mapping(config, comparison = FALSE)
comparison_mapping_long <- full_uncertainty_source_mapping(
  config, comparison = TRUE
)

asr_weights <- unique(info$asr_factors[
  age %in% 1:18 & is.finite(F) & F > 0,
  .(age = as.integer(age), F = as.numeric(F))
])
if (!identical(sort(asr_weights$age), 1:18)) {
  stop("ASR factors must contain one positive value for ages 1:18.",
       call. = FALSE)
}
weight_for <- function(age_code) {
  value <- asr_weights[age == as.integer(age_code), F]
  if (length(value) != 1L || !is.finite(value[[1L]]) || value[[1L]] <= 0) {
    stop("Missing or invalid ASR weight for standard age ", age_code, ".",
         call. = FALSE)
  }
  as.numeric(value[[1L]])
}

cause_rates_path <- derived_path(config, "cause_rates.parquet")
if (!file.exists(cause_rates_path)) {
  stop("Point-estimate cause-rate table not found: ", cause_rates_path,
       call. = FALSE)
}
point_dataset <- arrow::open_dataset(cause_rates_path, format = "parquet")

build_sparse_mapping <- function(long, source_ids, target_ids) {
  x <- data.table::as.data.table(data.table::copy(long))
  x <- x[
    source_cause_id %in% source_ids & series_id %in% target_ids &
      is.finite(weight) & weight != 0
  ]
  Matrix::sparseMatrix(
    i = match(x$source_cause_id, source_ids),
    j = match(x$series_id, target_ids),
    x = as.numeric(x$weight),
    dims = c(length(source_ids), length(target_ids)),
    dimnames = list(source_ids, target_ids)
  )
}

read_draw_slice <- function(path, geo_column, sex_code, source_ids = NULL) {
  columns <- c(
    geo_column, "Sex", "DeathYear", "cause_id",
    full_uncertainty_age_columns()
  )
  x <- data.table::as.data.table(arrow::read_parquet(
    path,
    col_select = columns,
    as_data_frame = TRUE
  ))
  x <- x[Sex == as.integer(sex_code)]
  if (!nrow(x)) stop("No rows found for sex ", sex_code, " in ", path,
                     call. = FALSE)
  if (is.null(source_ids)) source_ids <- sort(unique(as.character(x$cause_id)))
  x[, source_order__ := match(as.character(cause_id), source_ids)]
  if (x[is.na(source_order__), .N]) {
    stop("A full draw contains an unexpected cause_id: ", path, call. = FALSE)
  }
  data.table::setorderv(x, c("source_order__", geo_column, "DeathYear"))
  units <- unique(x[source_order__ == 1L, .(
    geography_code_raw = as.integer(get(geo_column)),
    year = as.integer(DeathYear)
  )])
  data.table::setorder(units, geography_code_raw, year)
  expected_unit_key <- paste(units$geography_code_raw, units$year, sep = "|")
  observed_unit_key <- paste(
    as.integer(x[[geo_column]]), as.integer(x$DeathYear), sep = "|"
  )
  expected_all <- rep(expected_unit_key, times = length(source_ids))
  if (!identical(observed_unit_key, expected_all)) {
    stop(
      "The full draw grid is not identical across causes in ", path, ".",
      call. = FALSE
    )
  }
  if (nrow(x) != nrow(units) * length(source_ids)) {
    stop("The full draw has an incomplete cause/geography/year grid: ", path,
         call. = FALSE)
  }
  list(data = x, units = units, source_ids = source_ids)
}

point_population_matrix <- function(storage_scope, sex_code, units) {
  base_age_map <- data.table::rbindlist(lapply(0:19, function(code) {
    rows <- config$age_map[
      include_explorer %in% TRUE &
        vapply(database_age_codes, function(value) {
          parsed <- tryCatch(
            parse_integer_codes(value),
            error = function(e) integer()
          )
          length(parsed) == 1L && identical(parsed, as.integer(code))
        }, logical(1))
    ]
    if (!nrow(rows)) return(NULL)
    data.table::data.table(
      database_age = as.integer(code),
      age_id = as.character(rows$age_id[[1L]])
    )
  }), use.names = TRUE, fill = TRUE)
  geo_types <- if (identical(storage_scope, "province")) {
    c("national", "province")
  } else {
    "population_group"
  }
  age_ids <- unique(base_age_map$age_id)
  query <- dplyr::filter(
    point_dataset,
    .data$model == "NBD3-R",
    .data$sex_code == .env$sex_code,
    .data$series_id %in% .env$series_ids,
    .data$age_id %in% .env$age_ids
  )
  query <- dplyr::select(
    query,
    dplyr::all_of(c(
      "geography_type", "geography_code", "year", "age_id",
      "series_id", "population"
    ))
  )
  out <- data.table::as.data.table(dplyr::collect(query))
  out <- out[geography_type %in% geo_types]
  if (identical(storage_scope, "population_group")) {
    out[, geography_code_raw := as.integer(geography_code) - 10L]
  } else {
    out[, geography_code_raw := as.integer(geography_code)]
  }
  out <- merge(out, base_age_map, by = "age_id", all.x = TRUE, sort = FALSE)
  out[, unit_key__ := paste(geography_code_raw, year, sep = "|")]
  out[, series_unit_key__ := paste(series_id, unit_key__, sep = "|")]
  unit_key <- paste(units$geography_code_raw, units$year, sep = "|")
  expected_key <- unlist(lapply(series_ids, function(series_id) {
    paste(series_id, unit_key, sep = "|")
  }), use.names = FALSE)
  n_units <- nrow(units)
  n_series <- length(series_ids)
  populations <- vector("list", 20L)
  for (code in 0:19) {
    values <- out[database_age == code]
    if (values[, anyDuplicated(series_unit_key__)]) {
      stop(
        "Point populations are duplicated for database age ", code, ".",
        call. = FALSE
      )
    }
    selected <- as.numeric(values$population[
      match(expected_key, values$series_unit_key__)
    ])
    if (any(!is.finite(selected))) {
      stop(
        "Point populations are incomplete for database age ", code, ".",
        call. = FALSE
      )
    }
    populations[[code + 1L]] <- matrix(
      selected,
      nrow = n_units,
      ncol = n_series,
      byrow = FALSE,
      dimnames = list(NULL, series_ids)
    )
  }
  populations
}

point_partition <- function(storage_scope, sex_code, age_id) {
  geo_types <- if (identical(storage_scope, "province")) {
    c("national", "province")
  } else {
    "population_group"
  }
  query <- dplyr::filter(
    point_dataset,
    .data$model == "NBD3-R",
    .data$sex_code == .env$sex_code,
    .data$age_id == .env$age_id
  )
  columns <- c(
    "geography_type", "geography_code", "geography",
    "sex_code", "sex", "year", "age_id", "age_label",
    "series_id", "series_label", "domain", "hierarchy", "cause_type",
    "deaths", "population", "crude_rate", "asr",
    "series_sort_order", "age_sort_order"
  )
  query <- dplyr::select(query, dplyr::all_of(columns))
  out <- data.table::as.data.table(dplyr::collect(query))
  out <- out[
    geography_type %in% geo_types & series_id %in% series_ids
  ]
  out[]
}

map_draw <- function(slice, mapping, target_ids) {
  x <- slice$data
  n_units <- nrow(slice$units)
  n_source <- length(slice$source_ids)
  mapped <- vector("list", 20L)
  age_columns <- full_uncertainty_age_columns()
  for (index in seq_along(age_columns)) {
    raw <- matrix(
      as.numeric(x[[age_columns[[index]]]]),
      nrow = n_units,
      ncol = n_source,
      byrow = FALSE
    )
    mapped[[index]] <- as.matrix(raw %*% mapping)
    if (!identical(ncol(mapped[[index]]), length(target_ids))) {
      stop("Cause mapping produced an unexpected number of series.",
           call. = FALSE)
    }
  }
  mapped
}

flatten_matrix <- function(x) as.numeric(x)

asr_vector <- function(mapped, populations, n_series) {
  n_units <- nrow(mapped[[1L]])
  numerator <- matrix(0, nrow = n_units, ncol = n_series)
  denominator <- matrix(0, nrow = n_units, ncol = n_series)

  under5_population <- populations[[2L]] + populations[[3L]]
  under5_deaths <- mapped[[1L]] + mapped[[2L]] + mapped[[3L]]
  valid <- is.finite(under5_population) & under5_population > 0
  weight <- weight_for(1L)
  numerator[valid] <- numerator[valid] +
    weight * under5_deaths[valid] / under5_population[valid]
  denominator[valid] <- denominator[valid] + weight

  for (database_age in 3:19) {
    population <- populations[[database_age + 1L]]
    deaths <- mapped[[database_age + 1L]]
    valid <- is.finite(population) & population > 0
    weight <- weight_for(database_age - 1L)
    numerator[valid] <- numerator[valid] +
      weight * deaths[valid] / population[valid]
    denominator[valid] <- denominator[valid] + weight
  }
  out <- matrix(NA_real_, nrow = n_units, ncol = n_series)
  valid <- is.finite(denominator) & denominator > 0
  out[valid] <- 1e5 * numerator[valid] / denominator[valid]
  as.numeric(out)
}

age_vector <- function(mapped, codes) {
  indexes <- as.integer(codes) + 1L
  result <- mapped[[indexes[[1L]]]]
  if (length(indexes) > 1L) {
    for (index in indexes[-1L]) result <- result + mapped[[index]]
  }
  flatten_matrix(result)
}

comparison_vectors <- function(mapped, raw_slice, comparison_specs) {
  n_units <- nrow(raw_slice$units)
  n_source <- length(raw_slice$source_ids)
  all_causes_index <- match("all_causes", raw_slice$source_ids)
  if (is.na(all_causes_index)) {
    stop("The full draw is missing the all_causes source series.", call. = FALSE)
  }
  raw_age <- lapply(full_uncertainty_age_columns(), function(column) {
    matrix(
      as.numeric(raw_slice$data[[column]]),
      nrow = n_units,
      ncol = n_source,
      byrow = FALSE
    )[, all_causes_index]
  })
  values <- vector("list", length(comparison_specs))
  for (age_index in seq_along(comparison_specs)) {
    codes <- comparison_specs[[age_index]]$codes
    numerator <- age_vector(mapped, codes)
    denominator_unit <- Reduce(`+`, raw_age[as.integer(codes) + 1L])
    denominator <- rep(
      denominator_unit,
      times = length(comparison_series_ids)
    )
    measure <- rep(
      as.character(comparison_meta$measure),
      each = n_units
    )
    fraction <- measure == "fraction"
    numerator[fraction] <- safe_ratio(
      numerator[fraction], denominator[fraction], 1
    )
    values[[age_index]] <- numerator
  }
  values
}

prepare_binary <- function(path, bytes_per_draw) {
  if (!file.exists(path)) return(0L)
  size <- as.numeric(file.info(path)$size)
  completed <- floor(size / bytes_per_draw)
  expected_size <- completed * bytes_per_draw
  if (size != expected_size) {
    file.truncate(path, expected_size)
  }
  as.integer(completed)
}

append_vectors <- function(path, vectors) {
  connection <- file(path, open = "ab")
  on.exit(close(connection), add = TRUE)
  for (vector in vectors) {
    if (any(!is.finite(vector))) {
      stop("A deployment-cache draw vector contains non-finite values.",
           call. = FALSE)
    }
    writeBin(as.double(vector), connection, size = 8L, endian = "little")
  }
  invisible(path)
}

read_binary_block <- function(
    path,
    n_draws,
    cells_per_draw,
    start_cell,
    count) {
  matrix_out <- matrix(NA_real_, nrow = count, ncol = n_draws)
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  for (draw in seq_len(n_draws)) {
    offset <- ((draw - 1) * cells_per_draw + (start_cell - 1)) * 8
    seek(connection, where = offset, origin = "start", rw = "read")
    values <- readBin(
      connection, what = "numeric", n = count,
      size = 8L, endian = "little"
    )
    if (length(values) != count) {
      stop("The temporary cache cube is truncated at draw ", draw, ".",
           call. = FALSE)
    }
    matrix_out[, draw] <- values
  }
  matrix_out
}

summarise_binary_segment <- function(
    path,
    n_draws,
    cells_per_draw,
    start_cell,
    n_cells,
    block_rows) {
  lower <- upper <- means <- medians <- sds <- numeric(n_cells)
  starts <- seq.int(1L, n_cells, by = block_rows)
  for (block_index in seq_along(starts)) {
    local_start <- starts[[block_index]]
    count <- min(block_rows, n_cells - local_start + 1L)
    values <- read_binary_block(
      path, n_draws, cells_per_draw,
      start_cell + local_start - 1L, count
    )
    range_values <- matrixStats::rowQuantiles(
      values,
      probs = c(0.025, 0.975),
      type = 8,
      na.rm = FALSE,
      drop = FALSE
    )
    index <- local_start:(local_start + count - 1L)
    lower[index] <- range_values[, 1L]
    upper[index] <- range_values[, 2L]
    means[index] <- matrixStats::rowMeans2(values)
    medians[index] <- matrixStats::rowMedians(values)
    sds[index] <- matrixStats::rowSds(values)
    rm(values, range_values)
  }
  list(
    lower = lower,
    upper = upper,
    draw_mean = means,
    draw_median = medians,
    draw_sd = sds
  )
}

write_partition <- function(data, storage_scope, sex_code, age_id) {
  path <- file.path(
    cache_root,
    storage_scope,
    paste0("sex_code=", as.integer(sex_code)),
    paste0("age_id=", as.character(age_id)),
    "part-0.parquet"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- data.table::as.data.table(data.table::copy(data))
  payload[, c("sex_code", "age_id") := NULL]
  arrow::write_parquet(
    payload,
    path,
    compression = "zstd",
    chunk_size = 65536L
  )
  path
}

copy_method_assets <- function() {
  target <- file.path(root, "output", "report-data", "methods")
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  uncertainty_root <- info$output_root
  candidates <- c(
    file.path(root, "data", "derived", "03_injury_source_fractions.parquet"),
    file.path(root, "data", "derived", "03_injury_fractions.parquet"),
    file.path(root, "data", "derived", "03_injury_survey_model_comparison.parquet"),
    file.path(root, "data", "derived", "03_final_injuries.parquet"),
    file.path(root, "data", "derived", "04_injury_envelope_anchors.parquet"),
    file.path(root, "data", "derived", "04_completeness_scalars.parquet"),
    file.path(root, "data", "derived", "04_investigation_subpopulation_pre_injury_envelope.parquet"),
    file.path(root, "data", "derived", "04_investigation_subpopulation.parquet"),
    file.path(root, "output", "tables", "03_injury_model_diagnostics.csv"),
    file.path(root, "output", "tables", "03_injury_survey_reference_audit.csv"),
    file.path(root, "output", "tables", "03_injury_survey_level_uncertainty.csv"),
    file.path(root, "output", "tables", "03_injury_survey_province_uncertainty.csv"),
    file.path(root, "output", "tables", "03_injury_survey_cause_uncertainty.csv"),
    file.path(uncertainty_root, "completeness_weighted_cells.parquet"),
    file.path(uncertainty_root, "completeness_by_province.csv")
  )
  candidates <- unique(candidates[file.exists(candidates)])
  for (path in candidates) {
    file.copy(path, file.path(target, basename(path)), overwrite = TRUE)
  }
  invisible(target)
}

comparison_parts <- character()
build_log <- list()

stores <- list(
  province = list(
    files = info$province_files,
    geo_column = "Death_Prov"
  ),
  population_group = list(
    files = info$population_files,
    geo_column = "Popgroup"
  )
)

for (storage_scope in names(stores)) {
  store <- stores[[storage_scope]]
  if (!length(store$files)) next
  for (sex_code in 1:3) {
    marker <- file.path(
      work_root,
      paste0(storage_scope, "_sex_", sex_code, ".complete")
    )
    completed_partition_dir <- file.path(
      cache_root,
      storage_scope,
      paste0("sex_code=", sex_code)
    )
    if (file.exists(marker) && dir.exists(completed_partition_dir) &&
        !overwrite) {
      message("Skipping completed cache section: ", storage_scope,
              ", sex ", sex_code)
      comparison_part <- file.path(
        work_root,
        paste0(storage_scope, "_sex_", sex_code, "_comparison.parquet")
      )
      if (file.exists(comparison_part)) comparison_parts <- c(
        comparison_parts, comparison_part
      )
      next
    }

    message("\nBuilding ", storage_scope, ", sex ", sex_code, "...")
    first <- read_draw_slice(
      store$files[[1L]], store$geo_column, sex_code
    )
    source_ids <- first$source_ids
    mapping <- build_sparse_mapping(mapping_long, source_ids, series_ids)
    comparison_mapping <- build_sparse_mapping(
      comparison_mapping_long, source_ids, comparison_series_ids
    )
    units <- first$units
    n_units <- nrow(units)
    n_series <- length(series_ids)
    n_comparison_series <- length(comparison_series_ids)
    base_cells <- n_units * n_series
    comparison_base_cells <- n_units * n_comparison_series
    cells_per_draw <- base_cells * length(age_specs)
    comparison_cells_per_draw <- comparison_base_cells *
      length(comparison_age_specs)
    cube_path <- file.path(
      work_root, paste0(storage_scope, "_sex_", sex_code, ".bin")
    )
    comparison_cube_path <- file.path(
      work_root,
      paste0(storage_scope, "_sex_", sex_code, "_comparison.bin")
    )
    completed_main <- prepare_binary(cube_path, cells_per_draw * 8)
    completed_comparison <- prepare_binary(
      comparison_cube_path, comparison_cells_per_draw * 8
    )
    completed <- min(completed_main, completed_comparison)
    if (file.exists(cube_path)) {
      file.truncate(cube_path, completed * cells_per_draw * 8)
    }
    if (file.exists(comparison_cube_path)) {
      file.truncate(
        comparison_cube_path,
        completed * comparison_cells_per_draw * 8
      )
    }
    populations <- point_population_matrix(
      storage_scope, sex_code, units
    )

    # Verify the draw-level ASR implementation against the deterministic point
    # database before processing the stochastic archive. This catches weight,
    # age-alignment, and series-specific denominator errors immediately.
    point_full_path <- file.path(
      info$output_root,
      if (identical(storage_scope, "province")) {
        "full_point_report.parquet"
      } else {
        "population_full_point_report.parquet"
      }
    )
    if (!file.exists(point_full_path)) {
      stop("Full-grid point report not found: ", point_full_path, call. = FALSE)
    }
    point_slice <- read_draw_slice(
      point_full_path,
      store$geo_column,
      sex_code,
      source_ids = source_ids
    )
    if (!identical(point_slice$units, units)) {
      stop("The point and draw unit grids differ for ASR validation.",
           call. = FALSE)
    }
    point_mapped <- map_draw(point_slice, mapping, series_ids)
    point_calculated <- asr_vector(point_mapped, populations, n_series)
    point_key <- data.table::data.table(
      series_id = rep(series_ids, each = n_units),
      geography_code_raw = rep(units$geography_code_raw, times = n_series),
      year = rep(units$year, times = n_series),
      calculated_asr = point_calculated
    )
    if (identical(storage_scope, "province")) {
      point_key[, `:=`(
        geography_type = data.table::fifelse(
          geography_code_raw == 10L, "national", "province"
        ),
        geography_code = geography_code_raw
      )]
    } else {
      point_key[, `:=`(
        geography_type = "population_group",
        geography_code = geography_code_raw + 10L
      )]
    }
    point_key[, geography_code_raw := NULL]
    point_reference <- point_partition(storage_scope, sex_code, "asr_all")
    point_check <- merge(
      point_reference[, .(
        geography_type, geography_code, year, series_id,
        point_asr = as.numeric(asr)
      )],
      point_key,
      by = c("geography_type", "geography_code", "year", "series_id"),
      all = TRUE,
      sort = FALSE
    )
    point_check[, relative_error := abs(calculated_asr - point_asr) /
      pmax(1, abs(point_asr))]
    max_asr_relative_error <- max(point_check$relative_error, na.rm = TRUE)
    if (!is.finite(max_asr_relative_error) || max_asr_relative_error > 1e-8) {
      worst <- point_check[order(-relative_error)][1L]
      stop(
        "Draw-level ASR calculation does not reproduce the deterministic ASR. ",
        "Scope=", storage_scope, ", sex=", sex_code,
        ", max relative error=", signif(max_asr_relative_error, 6),
        ", series=", worst$series_id,
        ", geography=", worst$geography_code,
        ", year=", worst$year, ".",
        call. = FALSE
      )
    }
    rm(point_slice, point_mapped, point_calculated, point_key,
       point_reference, point_check)

    if (completed < info$n_draws) {
      for (draw_index in seq.int(completed + 1L, info$n_draws)) {
        slice <- if (draw_index == 1L) {
          first
        } else {
          read_draw_slice(
            store$files[[draw_index]],
            store$geo_column,
            sex_code,
            source_ids = source_ids
          )
        }
        if (!identical(slice$units, units)) {
          stop("The full draw unit grid changes in draw ", draw_index, ".",
               call. = FALSE)
        }
        mapped <- map_draw(slice, mapping, series_ids)
        main_vectors <- lapply(age_specs, function(spec) {
          if (identical(spec$measure, "asr")) {
            asr_vector(mapped, populations, n_series)
          } else {
            age_vector(mapped, spec$codes)
          }
        })
        append_vectors(cube_path, main_vectors)

        mapped_comparison <- map_draw(
          slice, comparison_mapping, comparison_series_ids
        )
        comparison_values <- comparison_vectors(
          mapped_comparison, slice, comparison_age_specs
        )
        append_vectors(comparison_cube_path, comparison_values)
        rm(slice, mapped, mapped_comparison, main_vectors, comparison_values)
        if (draw_index %% 25L == 0L || draw_index == info$n_draws) {
          message("  cache draw files processed: ", draw_index, "/", info$n_draws)
          invisible(gc(verbose = FALSE))
        }
      }
    }

    base_key <- data.table::data.table(
      series_id = rep(series_ids, each = n_units),
      geography_code_raw = rep(units$geography_code_raw, times = n_series),
      year = rep(units$year, times = n_series)
    )
    if (identical(storage_scope, "province")) {
      base_key[, `:=`(
        geography_type = data.table::fifelse(
          geography_code_raw == 10L, "national", "province"
        ),
        geography_code = geography_code_raw
      )]
    } else {
      base_key[, `:=`(
        geography_type = "population_group",
        geography_code = geography_code_raw + 10L
      )]
    }
    base_key[, geography_code_raw := NULL]

    for (age_index in seq_along(age_specs)) {
      spec <- age_specs[[age_index]]
      message("  summarising ", spec$age_id, "...")
      stats <- summarise_binary_segment(
        cube_path,
        info$n_draws,
        cells_per_draw,
        start_cell = (age_index - 1L) * base_cells + 1L,
        n_cells = base_cells,
        block_rows = block_rows
      )
      summary <- data.table::copy(base_key)
      if (identical(spec$measure, "asr")) {
        summary[, `:=`(
          lower_asr = stats$lower,
          upper_asr = stats$upper,
          draw_mean_asr = stats$draw_mean,
          draw_median_asr = stats$draw_median,
          draw_sd_asr = stats$draw_sd,
          lower_deaths = NA_real_,
          upper_deaths = NA_real_,
          draw_mean_deaths = NA_real_,
          draw_median_deaths = NA_real_,
          draw_sd_deaths = NA_real_
        )]
      } else {
        summary[, `:=`(
          lower_deaths = stats$lower,
          upper_deaths = stats$upper,
          draw_mean_deaths = stats$draw_mean,
          draw_median_deaths = stats$draw_median,
          draw_sd_deaths = stats$draw_sd,
          lower_asr = NA_real_,
          upper_asr = NA_real_,
          draw_mean_asr = NA_real_,
          draw_median_asr = NA_real_,
          draw_sd_asr = NA_real_
        )]
      }
      points <- point_partition(storage_scope, sex_code, spec$age_id)
      summary <- merge(
        points,
        summary,
        by = c(
          "geography_type", "geography_code", "year", "series_id"
        ),
        all.x = TRUE,
        sort = FALSE
      )
      summary[, `:=`(
        n_draws = as.integer(info$n_draws),
        uncertainty_source = paste0(
          "NBD3 joint uncertainty; ",
          format(info$n_draws, big.mark = ",", scientific = FALSE),
          " draws"
        )
      )]
      bad <- summary[
        (is.finite(lower_deaths) & is.finite(upper_deaths) &
           lower_deaths > upper_deaths) |
          (is.finite(lower_asr) & is.finite(upper_asr) &
             lower_asr > upper_asr)
      ]
      if (nrow(bad)) {
        stop("The Shiny cache contains reversed uncertainty intervals.",
             call. = FALSE)
      }
      write_partition(summary, storage_scope, sex_code, spec$age_id)
      rm(summary, points, stats)
      invisible(gc(verbose = FALSE))
    }

    comparison_key <- data.table::data.table(
      series_id = rep(comparison_series_ids, each = n_units),
      geography_code_raw = rep(
        units$geography_code_raw,
        times = n_comparison_series
      ),
      year = rep(units$year, times = n_comparison_series)
    )
    comparison_key[, measure := rep(
      as.character(comparison_meta$measure),
      each = n_units
    )]
    if (identical(storage_scope, "province")) {
      comparison_key[, `:=`(
        geography_type = data.table::fifelse(
          geography_code_raw == 10L, "national", "province"
        ),
        geography_code = geography_code_raw
      )]
    } else {
      comparison_key[, `:=`(
        geography_type = "population_group",
        geography_code = geography_code_raw + 10L
      )]
    }
    comparison_key[, geography_code_raw := NULL]
    comparison_results <- vector("list", length(comparison_age_specs))
    for (age_index in seq_along(comparison_age_specs)) {
      spec <- comparison_age_specs[[age_index]]
      stats <- summarise_binary_segment(
        comparison_cube_path,
        info$n_draws,
        comparison_cells_per_draw,
        start_cell = (age_index - 1L) * comparison_base_cells + 1L,
        n_cells = comparison_base_cells,
        block_rows = block_rows
      )
      part <- data.table::copy(comparison_key)
      part[, `:=`(
        sex_code = as.integer(sex_code),
        age_id = spec$age_id,
        lower = stats$lower,
        upper = stats$upper,
        draw_mean = stats$draw_mean,
        draw_median = stats$draw_median,
        draw_sd = stats$draw_sd,
        n_draws = as.integer(info$n_draws),
        source = paste0(
          "NBD3 joint uncertainty; ",
          format(info$n_draws, big.mark = ",", scientific = FALSE),
          " draws"
        )
      )]
      comparison_results[[age_index]] <- part
    }
    comparison_result <- data.table::rbindlist(
      comparison_results, use.names = TRUE, fill = TRUE
    )
    comparison_part <- file.path(
      work_root,
      paste0(storage_scope, "_sex_", sex_code, "_comparison.parquet")
    )
    arrow::write_parquet(
      comparison_result,
      comparison_part,
      compression = "zstd"
    )
    comparison_parts <- c(comparison_parts, comparison_part)

    build_log[[length(build_log) + 1L]] <- data.table::data.table(
      storage_scope = storage_scope,
      sex_code = sex_code,
      units = n_units,
      source_causes = length(source_ids),
      report_series = n_series,
      cache_rows = n_units * n_series * length(age_specs)
    )
    file.create(marker)
    if (!keep_work) {
      unlink(cube_path)
      unlink(comparison_cube_path)
    }
    invisible(gc(verbose = FALSE))
  }
}

comparison_parts <- unique(comparison_parts[file.exists(comparison_parts)])
if (length(comparison_parts)) {
  comparison <- data.table::rbindlist(lapply(comparison_parts, function(path) {
    data.table::as.data.table(arrow::read_parquet(path))
  }), use.names = TRUE, fill = TRUE)
  data.table::setorder(
    comparison,
    geography_type, geography_code, sex_code, age_id, series_id, year
  )
  arrow::write_parquet(
    comparison,
    file.path(cache_root, "model_comparison_uncertainty.parquet"),
    compression = "zstd",
    chunk_size = 65536L
  )
}

copy_method_assets()

build_table <- data.table::rbindlist(build_log, use.names = TRUE, fill = TRUE)
data.table::fwrite(
  build_table,
  file.path(cache_root, "build_summary.csv")
)

cache_files <- list.files(cache_root, recursive = TRUE, full.names = TRUE)
cache_files <- cache_files[file.info(cache_files)$isdir %in% FALSE]
cache_bytes <- sum(file.info(cache_files)$size, na.rm = TRUE)
manifest <- list(
  cache_version = cache_version,
  built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  uncertainty_output_name = as.character(info$profile$run$output_name),
  uncertainty_engine = as.character(info$profile$run$engine_version %||% "1.8"),
  n_draws = as.integer(info$n_draws),
  cause_series = length(series_ids),
  age_outputs = length(age_specs),
  sexes = 3L,
  cache_files = length(cache_files),
  cache_bytes = as.numeric(cache_bytes),
  cache_gib = as.numeric(cache_bytes / 1024^3),
  crude_rate_intervals = "derived from death intervals and fixed point-estimate populations",
  asr_intervals = "calculated inside each draw with the deterministic ASR weight schedule before summarisation",
  asr_formula_version = "1.0.1",
  raw_draw_runtime_required = FALSE
)
yaml::write_yaml(manifest, file.path(cache_root, "manifest.yml"))

if (!keep_work) unlink(work_root, recursive = TRUE)
message("\nShiny deployment cache completed.")
message("Files: ", length(cache_files))
message("Size: ", round(cache_bytes / 1024^3, 3), " GiB")
message("Manifest: ", file.path(cache_root, "manifest.yml"))
