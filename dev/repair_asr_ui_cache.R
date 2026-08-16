#!/usr/bin/env Rscript

# Repair only the all-age ASR uncertainty partitions in the precomputed Shiny
# cache. This script does not rerun point estimates, uncertainty models, or the
# non-ASR cache partitions.
#
# The original cache builder used a data.table expression in which the function
# argument `age` was shadowed by the ASR-factor column of the same name. As a
# result, every standard age received the first ASR weight. This script uses an
# explicit age_code argument, validates the draw-level formula against the
# deterministic ASR, and atomically replaces only six `asr_all` partitions.

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
    "full-ui uncertainty run before repairing the ASR cache.",
    call. = FALSE
  )
}

cache_root <- file.path(root, "output", "report-data", "ui_uncertainty_cache")
manifest_path <- file.path(cache_root, "manifest.yml")
if (!file.exists(manifest_path)) {
  stop(
    "The precomputed Shiny cache is missing: ", manifest_path,
    "\nRun Rscript dev/build_shiny_ui_cache.R first.",
    call. = FALSE
  )
}

work_root <- file.path(root, "output", "report-data", ".asr_cache_repair")
if (dir.exists(work_root)) unlink(work_root, recursive = TRUE, force = TRUE)
dir.create(work_root, recursive = TRUE, showWarnings = FALSE)
on.exit({
  if (dir.exists(work_root)) unlink(work_root, recursive = TRUE, force = TRUE)
}, add = TRUE)

block_rows <- suppressWarnings(as.integer(
  Sys.getenv("NBD3_ASR_REPAIR_BLOCK_ROWS", "5000")
))
if (!is.finite(block_rows) || block_rows < 500L) block_rows <- 5000L

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

explorer_meta <- config$cause_map[
  include_explorer %in% TRUE
][order(sort_order)]
series_ids <- as.character(explorer_meta$series_id)
mapping_long <- full_uncertainty_source_mapping(config, comparison = FALSE)

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

read_draw_all <- function(path, geo_column, source_ids = NULL) {
  columns <- c(
    geo_column, "Sex", "DeathYear", "cause_id",
    full_uncertainty_age_columns()
  )
  x <- data.table::as.data.table(arrow::read_parquet(
    path,
    col_select = columns,
    as_data_frame = TRUE
  ))
  if (is.null(source_ids)) {
    source_ids <- sort(unique(as.character(x$cause_id)))
  }
  x[, source_order__ := match(as.character(cause_id), source_ids)]
  if (x[is.na(source_order__), .N]) {
    stop("A full draw contains an unexpected cause_id: ", path, call. = FALSE)
  }
  list(data = x, source_ids = source_ids)
}

slice_draw_sex <- function(all_draw, geo_column, sex_code) {
  x <- all_draw$data[Sex == as.integer(sex_code)]
  if (!nrow(x)) {
    stop("No rows found for sex ", sex_code, ".", call. = FALSE)
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
  expected_all <- rep(expected_unit_key, times = length(all_draw$source_ids))
  if (!identical(observed_unit_key, expected_all)) {
    stop("The full draw grid is not identical across causes.", call. = FALSE)
  }
  if (nrow(x) != nrow(units) * length(all_draw$source_ids)) {
    stop("The full draw has an incomplete cause/geography/year grid.",
         call. = FALSE)
  }
  list(data = x, units = units, source_ids = all_draw$source_ids)
}

base_age_ids <- c(
  "age_0", "age_1", "age_1_4", "age_5_9", "age_10_14",
  "age_15_19", "age_20_24", "age_25_29", "age_30_34",
  "age_35_39", "age_40_44", "age_45_49", "age_50_54",
  "age_55_59", "age_60_64", "age_65_69", "age_70_74",
  "age_75_79", "age_80_84", "age_85_plus"
)

partition_path <- function(storage_scope, sex_code, age_id) {
  file.path(
    cache_root,
    storage_scope,
    paste0("sex_code=", as.integer(sex_code)),
    paste0("age_id=", age_id),
    "part-0.parquet"
  )
}

population_matrices_from_cache <- function(
    storage_scope,
    sex_code,
    units,
    series_ids) {
  unit_key <- paste(units$geography_code_raw, units$year, sep = "|")
  expected_key <- unlist(lapply(series_ids, function(series_id) {
    paste(series_id, unit_key, sep = "|")
  }), use.names = FALSE)
  n_units <- nrow(units)
  n_series <- length(series_ids)
  result <- vector("list", length(base_age_ids))
  for (index in seq_along(base_age_ids)) {
    path <- partition_path(storage_scope, sex_code, base_age_ids[[index]])
    if (!file.exists(path)) {
      stop("Base-age cache partition not found: ", path, call. = FALSE)
    }
    x <- data.table::as.data.table(arrow::read_parquet(path))
    x <- x[series_id %in% series_ids]
    if (identical(storage_scope, "population_group")) {
      x[, geography_code_raw := as.integer(geography_code) - 10L]
    } else {
      x[, geography_code_raw := as.integer(geography_code)]
    }
    x[, unit_key__ := paste(geography_code_raw, year, sep = "|")]
    x[, series_unit_key__ := paste(series_id, unit_key__, sep = "|")]
    if (x[, anyDuplicated(series_unit_key__)]) {
      stop("Duplicate series/population rows in cache partition: ", path,
           call. = FALSE)
    }
    values <- as.numeric(x$population[match(expected_key, x$series_unit_key__)])
    if (any(!is.finite(values))) {
      stop("Series-specific population denominators are incomplete in: ", path,
           call. = FALSE)
    }
    result[[index]] <- matrix(
      values,
      nrow = n_units,
      ncol = n_series,
      byrow = FALSE,
      dimnames = list(NULL, series_ids)
    )
  }
  result
}

raw_age_matrix <- function(slice, columns) {
  n_units <- nrow(slice$units)
  n_source <- length(slice$source_ids)
  out <- matrix(0, nrow = n_units, ncol = n_source)
  for (column in columns) {
    out <- out + matrix(
      as.numeric(slice$data[[column]]),
      nrow = n_units,
      ncol = n_source,
      byrow = FALSE
    )
  }
  out
}

asr_vector <- function(slice, mapping, populations, n_series) {
  n_units <- nrow(slice$units)
  numerator <- matrix(0, nrow = n_units, ncol = n_series)
  denominator <- matrix(0, nrow = n_units, ncol = n_series)

  # Standard age 1 combines neonatal, post-neonatal and age 1-4 deaths. The
  # neonatal population is structural zero, so the denominator uses age 1 and
  # age 2 populations exactly as in calculate_asr(). Population is retained by
  # report series so Person estimates for female- and male-specific causes use
  # the same denominators as the deterministic database.
  population <- populations[[2L]] + populations[[3L]]
  valid <- is.finite(population) & population > 0
  weight <- weight_for(1L)
  mapped <- as.matrix(
    raw_age_matrix(slice, c("age_0", "age_1", "age_2")) %*% mapping
  )
  numerator[valid] <- numerator[valid] + weight * mapped[valid] / population[valid]
  denominator[valid] <- denominator[valid] + weight

  # Database ages 3:19 map to standard ages 2:18.
  for (database_age in 3:19) {
    population <- populations[[database_age + 1L]]
    valid <- is.finite(population) & population > 0
    weight <- weight_for(database_age - 1L)
    raw <- raw_age_matrix(slice, paste0("age_", database_age))
    mapped <- as.matrix(raw %*% mapping)
    numerator[valid] <- numerator[valid] + weight * mapped[valid] / population[valid]
    denominator[valid] <- denominator[valid] + weight
  }

  result <- matrix(NA_real_, nrow = n_units, ncol = n_series)
  valid <- is.finite(denominator) & denominator > 0
  result[valid] <- 1e5 * numerator[valid] / denominator[valid]
  as.numeric(result)
}

append_vector <- function(path, values) {
  if (any(!is.finite(values))) {
    stop("An ASR repair draw contains non-finite values.", call. = FALSE)
  }
  connection <- file(path, open = "ab")
  on.exit(close(connection), add = TRUE)
  writeBin(as.double(values), connection, size = 8L, endian = "little")
  invisible(path)
}

read_binary_block <- function(
    path,
    n_draws,
    cells_per_draw,
    start_cell,
    count) {
  output <- matrix(NA_real_, nrow = count, ncol = n_draws)
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  for (draw in seq_len(n_draws)) {
    offset <- ((draw - 1L) * cells_per_draw + start_cell - 1L) * 8
    seek(connection, where = offset, origin = "start", rw = "read")
    values <- readBin(
      connection, what = "numeric", n = count,
      size = 8L, endian = "little"
    )
    if (length(values) != count) {
      stop("The ASR repair matrix is truncated at draw ", draw, ".",
           call. = FALSE)
    }
    output[, draw] <- values
  }
  output
}

summarise_binary <- function(path, n_draws, n_cells, block_rows) {
  lower <- upper <- means <- medians <- sds <- numeric(n_cells)
  starts <- seq.int(1L, n_cells, by = block_rows)
  for (block_index in seq_along(starts)) {
    start <- starts[[block_index]]
    count <- min(block_rows, n_cells - start + 1L)
    values <- read_binary_block(path, n_draws, n_cells, start, count)
    ranges <- matrixStats::rowQuantiles(
      values,
      probs = c(0.025, 0.975),
      type = 8,
      na.rm = FALSE,
      drop = FALSE
    )
    index <- start:(start + count - 1L)
    lower[index] <- ranges[, 1L]
    upper[index] <- ranges[, 2L]
    means[index] <- matrixStats::rowMeans2(values)
    medians[index] <- matrixStats::rowMedians(values)
    sds[index] <- matrixStats::rowSds(values)
    rm(values, ranges)
    if (block_index %% 10L == 0L || block_index == length(starts)) {
      message("    summary blocks: ", block_index, "/", length(starts))
    }
  }
  list(
    lower = lower,
    upper = upper,
    draw_mean = means,
    draw_median = medians,
    draw_sd = sds
  )
}

base_key <- function(storage_scope, units) {
  n_units <- nrow(units)
  out <- data.table::data.table(
    series_id = rep(series_ids, each = n_units),
    geography_code_raw = rep(units$geography_code_raw, times = length(series_ids)),
    year = rep(units$year, times = length(series_ids))
  )
  if (identical(storage_scope, "province")) {
    out[, `:=`(
      geography_type = data.table::fifelse(
        geography_code_raw == 10L, "national", "province"
      ),
      geography_code = geography_code_raw
    )]
  } else {
    out[, `:=`(
      geography_type = "population_group",
      geography_code = geography_code_raw + 10L
    )]
  }
  out[, geography_code_raw := NULL]
  out[]
}

read_point_partition <- function(storage_scope, sex_code) {
  path <- partition_path(storage_scope, sex_code, "asr_all")
  if (!file.exists(path)) stop("ASR cache partition not found: ", path,
                              call. = FALSE)
  data.table::as.data.table(arrow::read_parquet(path))
}

write_asr_partition_atomic <- function(data, storage_scope, sex_code) {
  target <- partition_path(storage_scope, sex_code, "asr_all")
  temporary <- paste0(target, ".tmp-", Sys.getpid())
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  payload <- data.table::as.data.table(data.table::copy(data))
  payload[, c("sex_code", "age_id") := NULL]
  arrow::write_parquet(
    payload,
    temporary,
    compression = "zstd",
    chunk_size = 65536L
  )
  if (file.exists(target)) unlink(target)
  if (!file.rename(temporary, target)) {
    stop("Could not replace ASR cache partition: ", target, call. = FALSE)
  }
  target
}

stores <- list(
  province = list(
    files = info$province_files,
    point = file.path(info$output_root, "full_point_report.parquet"),
    geo_column = "Death_Prov"
  ),
  population_group = list(
    files = info$population_files,
    point = file.path(info$output_root, "population_full_point_report.parquet"),
    geo_column = "Popgroup"
  )
)

validation_rows <- list()
message("SA-NBD3 ASR cache repair")
message("Project root: ", root)
message("Draws: ", format(info$n_draws, big.mark = ","))
message("Only the six asr_all partitions will be replaced.")

for (storage_scope in names(stores)) {
  store <- stores[[storage_scope]]
  if (!length(store$files)) next
  if (!file.exists(store$point)) {
    stop("Point full-grid report not found: ", store$point, call. = FALSE)
  }

  message("\nReading ", storage_scope, " draw layout...")
  first_all <- read_draw_all(store$files[[1L]], store$geo_column)
  source_ids <- first_all$source_ids
  mapping <- build_sparse_mapping(mapping_long, source_ids, series_ids)
  n_series <- length(series_ids)

  slices <- lapply(1:3, function(sex_code) {
    slice_draw_sex(first_all, store$geo_column, sex_code)
  })
  populations <- lapply(1:3, function(sex_code) {
    population_matrices_from_cache(
      storage_scope, sex_code, slices[[sex_code]]$units, series_ids
    )
  })
  binary_paths <- vapply(1:3, function(sex_code) {
    file.path(work_root, paste0(storage_scope, "_sex_", sex_code, ".bin"))
  }, character(1))
  unlink(binary_paths[file.exists(binary_paths)])

  # Validate the corrected formula against the deterministic point ASR before
  # spending time on the stochastic draws.
  point_all <- read_draw_all(store$point, store$geo_column, source_ids)
  for (sex_code in 1:3) {
    point_slice <- slice_draw_sex(point_all, store$geo_column, sex_code)
    calculated <- asr_vector(
      point_slice, mapping, populations[[sex_code]], n_series
    )
    key <- base_key(storage_scope, point_slice$units)
    calculated_table <- data.table::copy(key)
    calculated_table[, calculated_asr := calculated]
    point_partition <- read_point_partition(storage_scope, sex_code)
    point_partition[, `:=`(
      sex_code = as.integer(sex_code),
      age_id = "asr_all"
    )]
    check <- merge(
      point_partition[, .(
        geography_type, geography_code, year, series_id,
        point_asr = as.numeric(asr)
      )],
      calculated_table,
      by = c("geography_type", "geography_code", "year", "series_id"),
      all = TRUE,
      sort = FALSE
    )
    check[, absolute_error := abs(calculated_asr - point_asr)]
    check[, relative_error := absolute_error / pmax(1, abs(point_asr))]
    max_relative <- max(check$relative_error, na.rm = TRUE)
    max_absolute <- max(check$absolute_error, na.rm = TRUE)
    validation_rows[[length(validation_rows) + 1L]] <- data.table::data.table(
      storage_scope = storage_scope,
      sex_code = sex_code,
      validation_type = "deterministic_formula",
      n_rows = nrow(check),
      max_absolute_error = max_absolute,
      max_relative_error = max_relative
    )
    if (!is.finite(max_relative) || max_relative > 1e-8) {
      bad <- check[order(-relative_error)][1L]
      stop(
        "Corrected ASR formula does not reproduce the deterministic ASR. ",
        "Scope=", storage_scope, ", sex=", sex_code,
        ", max relative error=", signif(max_relative, 6),
        ". First worst series=", bad$series_id,
        ", geography=", bad$geography_code,
        ", year=", bad$year, ".",
        call. = FALSE
      )
    }
  }
  rm(point_all)

  message("Computing corrected ASRs from the joint draws...")
  for (draw_index in seq_len(info$n_draws)) {
    all_draw <- if (draw_index == 1L) {
      first_all
    } else {
      read_draw_all(store$files[[draw_index]], store$geo_column, source_ids)
    }
    for (sex_code in 1:3) {
      slice <- slice_draw_sex(all_draw, store$geo_column, sex_code)
      values <- asr_vector(
        slice, mapping, populations[[sex_code]], n_series
      )
      append_vector(binary_paths[[sex_code]], values)
      rm(slice, values)
    }
    rm(all_draw)
    if (draw_index %% 25L == 0L || draw_index == info$n_draws) {
      message("  ASR draw files processed: ", draw_index, "/", info$n_draws)
      invisible(gc(verbose = FALSE))
    }
  }

  for (sex_code in 1:3) {
    units <- slices[[sex_code]]$units
    n_cells <- nrow(units) * n_series
    message("  summarising ", storage_scope, ", sex ", sex_code, "...")
    stats <- summarise_binary(
      binary_paths[[sex_code]], info$n_draws, n_cells, block_rows
    )
    summary <- base_key(storage_scope, units)
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

    points <- read_point_partition(storage_scope, sex_code)
    points[, `:=`(
      sex_code = as.integer(sex_code),
      age_id = "asr_all"
    )]
    replace_columns <- c(
      "lower_asr", "upper_asr", "draw_mean_asr",
      "draw_median_asr", "draw_sd_asr",
      "lower_deaths", "upper_deaths", "draw_mean_deaths",
      "draw_median_deaths", "draw_sd_deaths", "n_draws",
      "uncertainty_source"
    )
    points[, (intersect(replace_columns, names(points))) := NULL]
    out <- merge(
      points,
      summary,
      by = c("geography_type", "geography_code", "year", "series_id"),
      all.x = TRUE,
      sort = FALSE
    )
    out[, `:=`(
      n_draws = as.integer(info$n_draws),
      uncertainty_source = paste0(
        "NBD3 joint uncertainty; ",
        format(info$n_draws, big.mark = ",", scientific = FALSE),
        " draws"
      ),
      sex_code = as.integer(sex_code),
      age_id = "asr_all"
    )]
    if (out[
      !is.finite(lower_asr) | !is.finite(upper_asr) |
        lower_asr > upper_asr,
      .N
    ]) {
      stop("The repaired ASR partition contains invalid intervals.",
           call. = FALSE)
    }
    write_asr_partition_atomic(out, storage_scope, sex_code)

    total_check <- out[series_id == "za_172", .(
      point_asr = as.numeric(asr),
      draw_median_asr,
      lower_asr,
      upper_asr,
      point_outside_ui = asr < lower_asr | asr > upper_asr,
      median_to_point = draw_median_asr / asr
    )]
    validation_rows[[length(validation_rows) + 1L]] <- data.table::data.table(
      storage_scope = storage_scope,
      sex_code = sex_code,
      validation_type = "total_series_interval",
      n_rows = nrow(total_check),
      max_absolute_error = max(
        abs(total_check$draw_median_asr - total_check$point_asr),
        na.rm = TRUE
      ),
      max_relative_error = max(
        abs(total_check$median_to_point - 1), na.rm = TRUE
      ),
      point_outside_count = sum(total_check$point_outside_ui, na.rm = TRUE)
    )
    rm(stats, summary, points, out, total_check)
    unlink(binary_paths[[sex_code]])
    invisible(gc(verbose = FALSE))
  }
}

validation <- data.table::rbindlist(
  validation_rows, use.names = TRUE, fill = TRUE
)
dir.create(file.path(root, "output", "tables"), recursive = TRUE,
           showWarnings = FALSE)
data.table::fwrite(
  validation,
  file.path(root, "output", "tables", "asr_cache_validation.csv")
)

manifest <- yaml::read_yaml(manifest_path)
manifest$cache_version <- "1.0.1"
manifest$asr_formula_version <- "1.0.1"
manifest$asr_repaired_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
manifest$asr_point_max_relative_error <- max(
  validation[validation_type == "deterministic_formula"]$max_relative_error,
  na.rm = TRUE
)
manifest$asr_intervals <- paste(
  "calculated inside each draw with the deterministic ASR weight schedule",
  "before type-8 summarisation"
)
cache_files <- list.files(cache_root, recursive = TRUE, full.names = TRUE)
cache_files <- cache_files[file.exists(cache_files) & !dir.exists(cache_files)]
cache_bytes <- sum(file.info(cache_files)$size, na.rm = TRUE)
manifest$cache_files <- length(cache_files)
manifest$cache_bytes <- as.numeric(cache_bytes)
manifest$cache_gib <- as.numeric(cache_bytes / 1024^3)
yaml::write_yaml(manifest, manifest_path)

message("\nASR cache repair completed successfully.")
message("Updated partitions: 6")
message("Validation: output/tables/asr_cache_validation.csv")
message("Manifest: ", manifest_path)
