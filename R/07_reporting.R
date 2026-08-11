# ==============================================================================
# 07_reporting: Uncertainty-to-report integration
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Draw aggregation and report interval construction
# ------------------------------------------------------------------------------

# Draw-based uncertainty integration for the collaborator report --------------
#
# This file is downstream of the validated point-estimate and uncertainty
# engines. It does not recalculate point estimates. It converts the stored
# joint draws into the exact broad comparison series used by the report, checks
# those point values against NBD3-R, and then attaches lower and upper limits to
# matching NBD3-R rows only. Rows supplied for NBD2, NBD3-Stata, THEMBISA, and
# GBD2023 are checked before and after the join and are never rewritten.

nbd_integrated_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required. Run source('install_packages.R').",
         call. = FALSE)
  }
  invisible(TRUE)
}

nbd_integrated_is_absolute <- function(path) {
  grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\\\\\)", path)
}

nbd_integrated_resolve_path <- function(root, path) {
  if (!nbd_integrated_is_absolute(path)) path <- file.path(root, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

# The uncertainty engine writes one validated Parquet file per completed draw.
# These per-draw files are the canonical reporting source. Retaining all 214
# analysis causes makes a single consolidated uncertainty_draws.parquet large
# enough to exceed Parquet/Thrift deserialisation limits on some Arrow builds.
# Report construction therefore reads the completed draw files one at a time
# and keeps only the derived report series in an in-memory numeric matrix.

nbd_uncertainty_draw_files <- function(paths) {
  draw_dir <- if (!is.null(paths$draw_dir) && length(paths$draw_dir)) {
    as.character(paths$draw_dir[[1L]])
  } else {
    file.path(paths$scenario_root, "draws")
  }
  if (!dir.exists(draw_dir)) return(character())

  files <- list.files(
    draw_dir,
    pattern = "^draw_[0-9]+[.]parquet$",
    full.names = TRUE
  )
  if (!length(files)) return(character())

  draw_ids <- suppressWarnings(as.integer(sub(
    "^draw_([0-9]+)[.]parquet$",
    "\\1",
    basename(files)
  )))
  if (anyNA(draw_ids)) {
    stop(
      "Could not parse one or more uncertainty draw filenames in ",
      draw_dir, ".",
      call. = FALSE
    )
  }
  order_index <- order(draw_ids)
  files <- files[order_index]
  draw_ids <- draw_ids[order_index]

  expected_draws <- suppressWarnings(as.integer(paths$config$run$n_draws))
  if (length(expected_draws) == 1L && !is.na(expected_draws)) {
    if (length(files) != expected_draws) {
      stop(
        "Expected ", expected_draws,
        " completed uncertainty draw files in ", draw_dir,
        "; found ", length(files), ".",
        call. = FALSE
      )
    }
    expected_ids <- seq_len(expected_draws)
    if (!identical(draw_ids, expected_ids)) {
      missing_ids <- setdiff(expected_ids, draw_ids)
      extra_ids <- setdiff(draw_ids, expected_ids)
      stop(
        "The uncertainty draw directory is not a complete 1:",
        expected_draws, " sequence.",
        if (length(missing_ids)) paste0(
          " Missing draw(s): ", paste(missing_ids, collapse = ", "), "."
        ) else "",
        if (length(extra_ids)) paste0(
          " Unexpected draw(s): ", paste(extra_ids, collapse = ", "), "."
        ) else "",
        call. = FALSE
      )
    }
  }

  files
}


nbd_population_uncertainty_draw_files <- function(paths) {
  if (is.null(paths$population_draw_dir) ||
      !length(paths$population_draw_dir)) {
    return(character())
  }
  population_paths <- paths
  population_paths$draw_dir <- paths$population_draw_dir
  nbd_uncertainty_draw_files(population_paths)
}

nbd_read_uncertainty_draw_file <- function(path, expected_draw_id) {
  nbd_integrated_require("arrow")
  nbd_integrated_require("data.table")

  out <- data.table::as.data.table(
    arrow::read_parquet(path, as_data_frame = TRUE)
  )
  required_columns <- c(
    "scenario", "draw_id", "Death_Prov", "Sex", "DeathYear",
    "age_group", "cause_id", "Deaths"
  )
  missing_columns <- setdiff(required_columns, names(out))
  if (length(missing_columns)) {
    stop(
      "Uncertainty draw file ", path,
      " is missing column(s): ",
      paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }

  observed_ids <- unique(as.integer(out$draw_id))
  if (
    length(observed_ids) != 1L || is.na(observed_ids) ||
    !identical(observed_ids, as.integer(expected_draw_id))
  ) {
    stop(
      "Draw identifier mismatch in ", path,
      ": expected ", expected_draw_id,
      " but the file contains ", paste(observed_ids, collapse = ", "), ".",
      call. = FALSE
    )
  }
  out[, ..required_columns]
}


nbd_read_population_uncertainty_draw_file <- function(
    path,
    expected_draw_id) {
  nbd_integrated_require("arrow")
  nbd_integrated_require("data.table")

  out <- data.table::as.data.table(
    arrow::read_parquet(path, as_data_frame = TRUE)
  )
  required_columns <- c(
    "scenario", "draw_id", "Popgroup", "Sex", "DeathYear",
    "age_group", "cause_id", "Deaths"
  )
  missing_columns <- setdiff(required_columns, names(out))
  if (length(missing_columns)) {
    stop(
      "Population-group uncertainty draw file ", path,
      " is missing column(s): ",
      paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }
  observed_ids <- unique(as.integer(out$draw_id))
  if (
    length(observed_ids) != 1L || is.na(observed_ids) ||
    !identical(observed_ids, as.integer(expected_draw_id))
  ) {
    stop(
      "Draw identifier mismatch in ", path,
      ": expected ", expected_draw_id,
      " but the file contains ", paste(observed_ids, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (out[!Popgroup %in% 1:4, .N]) {
    stop("Population-group uncertainty contains an invalid Popgroup code.",
         call. = FALSE)
  }
  out[, ..required_columns]
}

nbd_read_uncertainty_summary <- function(paths) {
  nbd_integrated_require("data.table")
  parquet_error <- NULL
  if (file.exists(paths$summary)) {
    result <- tryCatch(
      {
        nbd_integrated_require("arrow")
        data.table::as.data.table(
          arrow::read_parquet(paths$summary, as_data_frame = TRUE)
        )
      },
      error = function(error) {
        parquet_error <<- conditionMessage(error)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  if (!is.null(paths$summary_csv) && file.exists(paths$summary_csv)) {
    if (!is.null(parquet_error)) {
      message(
        "The Parquet uncertainty summary could not be read (",
        parquet_error,
        "); using uncertainty_summary.csv instead."
      )
    }
    return(data.table::fread(paths$summary_csv))
  }

  stop(
    "No readable uncertainty summary was found. Expected ", paths$summary,
    if (!is.null(paths$summary_csv)) paste0(" or ", paths$summary_csv) else "",
    ".",
    call. = FALSE
  )
}

nbd_identical_key_table <- function(reference, candidate, key_columns) {
  if (nrow(reference) != nrow(candidate)) return(FALSE)
  all(vapply(
    key_columns,
    function(column) identical(reference[[column]], candidate[[column]]),
    logical(1)
  ))
}

nbd_collect_uncertainty_draw_matrix <- function(
    paths,
    derive,
    key_columns,
    label,
    files = NULL,
    reader = nbd_read_uncertainty_draw_file) {
  nbd_integrated_require("data.table")
  if (is.null(files)) files <- nbd_uncertainty_draw_files(paths)
  if (!length(files)) {
    stop(
      "The report requires the completed per-draw Parquet files in ",
      file.path(paths$scenario_root, "draws"),
      ". The consolidated uncertainty_draws.parquet is deliberately not ",
      "used because it exceeded the Arrow Thrift size limit.",
      call. = FALSE
    )
  }

  key_reference <- NULL
  value_matrix <- NULL
  required_derived <- c("draw_id", key_columns, "value")

  message(
    "Building ", label, " uncertainty from ", length(files),
    " completed per-draw files."
  )
  for (index in seq_along(files)) {
    draw <- reader(
      files[[index]],
      expected_draw_id = index
    )
    derived <- data.table::as.data.table(derive(draw))
    if (!nrow(derived)) {
      stop(
        "The derived ", label,
        " table contains no rows for draw ", index, ".",
        call. = FALSE
      )
    }
    missing_columns <- setdiff(required_derived, names(derived))
    if (length(missing_columns)) {
      stop(
        "The derived ", label,
        " draw table is missing column(s): ",
        paste(missing_columns, collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (derived[, data.table::uniqueN(draw_id)] != 1L ||
        !identical(unique(as.integer(derived$draw_id)), as.integer(index))) {
      stop(
        "The derived ", label,
        " table does not retain the expected draw_id ", index, ".",
        call. = FALSE
      )
    }
    if (derived[, data.table::uniqueN(.SD), .SDcols = key_columns] != nrow(derived)) {
      stop(
        "The derived ", label,
        " table is not unique on its report key for draw ", index, ".",
        call. = FALSE
      )
    }
    values <- as.numeric(derived$value)
    if (any(!is.finite(values))) {
      stop(
        "The derived ", label,
        " table contains non-finite values in draw ", index, ".",
        call. = FALSE
      )
    }

    data.table::setorderv(derived, key_columns)
    current_keys <- derived[, ..key_columns]
    if (is.null(key_reference)) {
      key_reference <- data.table::copy(current_keys)
      value_matrix <- matrix(
        NA_real_,
        nrow = nrow(derived),
        ncol = length(files)
      )
    } else if (!nbd_identical_key_table(
      key_reference,
      current_keys,
      key_columns
    )) {
      stop(
        "The report key for ", label,
        " differs between uncertainty draws; first detected in draw ",
        index, ".",
        call. = FALSE
      )
    }
    value_matrix[, index] <- as.numeric(derived$value)

    if (index == 1L || index %% 10L == 0L || index == length(files)) {
      message("  ", label, " draw files processed: ", index, "/", length(files))
    }
    rm(draw, derived, current_keys, values)
    if (index %% 10L == 0L) invisible(gc(verbose = FALSE))
  }

  list(
    keys = key_reference,
    values = value_matrix,
    n_draws = ncol(value_matrix)
  )
}

nbd_type8_quantile_from_sorted <- function(sorted_values, probability) {
  n <- ncol(sorted_values)
  if (n == 1L) return(as.numeric(sorted_values[, 1L]))
  h <- n * probability + (probability + 1) / 3
  j <- floor(h)
  g <- h - j
  if (j < 1L) return(as.numeric(sorted_values[, 1L]))
  if (j >= n) return(as.numeric(sorted_values[, n]))
  rows <- seq_len(nrow(sorted_values))
  lower <- sorted_values[cbind(rows, j)]
  upper <- sorted_values[cbind(rows, j + 1L)]
  as.numeric((1 - g) * lower + g * upper)
}

nbd_summarise_uncertainty_draw_matrix <- function(
    collected,
    chunk_size = 5000L) {
  nbd_integrated_require("data.table")
  values <- collected$values
  keys <- collected$keys
  n_draws <- as.integer(collected$n_draws)
  if (!is.matrix(values) || nrow(values) != nrow(keys)) {
    stop("The uncertainty draw matrix and report key are inconsistent.",
         call. = FALSE)
  }
  if (any(!is.finite(values))) {
    stop("The uncertainty draw matrix contains non-finite values.",
         call. = FALSE)
  }

  chunk_size <- max(1L, as.integer(chunk_size))
  starts <- seq.int(1L, nrow(values), by = chunk_size)
  pieces <- vector("list", length(starts))
  for (piece_index in seq_along(starts)) {
    first <- starts[[piece_index]]
    last <- min(nrow(values), first + chunk_size - 1L)
    rows <- first:last
    block <- values[rows, , drop = FALSE]
    means <- rowMeans(block)
    sum_squares <- NULL
    variances <- NULL
    if (n_draws > 1L) {
      sum_squares <- rowSums(block * block)
      variances <- pmax(
        0,
        (sum_squares - n_draws * means * means) / (n_draws - 1L)
      )
      standard_deviations <- sqrt(variances)
    } else {
      standard_deviations <- rep(NA_real_, length(rows))
    }
    sorted <- t(apply(block, 1L, sort, na.last = NA))
    if (!is.matrix(sorted)) {
      sorted <- matrix(sorted, nrow = length(rows), byrow = TRUE)
    }

    statistics <- data.table::data.table(
      n_draws = n_draws,
      draw_mean = means,
      draw_median = nbd_type8_quantile_from_sorted(sorted, 0.5),
      draw_sd = standard_deviations,
      lower = nbd_type8_quantile_from_sorted(sorted, 0.025),
      upper = nbd_type8_quantile_from_sorted(sorted, 0.975)
    )
    pieces[[piece_index]] <- cbind(
      data.table::copy(keys[rows]),
      statistics
    )
    rm(block, sorted, statistics, sum_squares, variances)
  }

  data.table::rbindlist(
    pieces,
    use.names = TRUE,
    fill = FALSE
  )
}



# The fast matrix summariser above uses a custom row-wise type-8 quantile
# implementation. The per-draw reader has already verified that every stored
# value is finite. If a platform-specific simplification edge case nevertheless
# produces a non-finite or reversed interval, recompute only those rows with
# stats::quantile(type = 8), which is the reference R implementation used by the
# uncertainty engine. The diagnostic file preserves both the original and the
# recomputed statistics; no stochastic draw is changed or discarded.
nbd_recompute_invalid_interval_rows <- function(
    summary,
    collected,
    key_columns,
    label,
    diagnostic_path = NULL) {
  nbd_integrated_require("data.table")
  x <- data.table::as.data.table(data.table::copy(summary))
  required <- c(
    key_columns,
    "n_draws", "draw_mean", "draw_median", "draw_sd", "lower", "upper"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "The ", label, " uncertainty summary is missing column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.matrix(collected$values) || nrow(collected$values) != nrow(x)) {
    stop(
      "The ", label,
      " uncertainty summary cannot be checked against its draw matrix.",
      call. = FALSE
    )
  }
  if (!nbd_identical_key_table(
    collected$keys,
    x[, ..key_columns],
    key_columns
  )) {
    stop(
      "The ", label,
      " uncertainty summary key no longer matches its draw matrix.",
      call. = FALSE
    )
  }

  invalid <- which(
    !is.finite(x$lower) |
      !is.finite(x$upper) |
      x$lower > x$upper
  )
  if (!length(invalid)) {
    if (!is.null(diagnostic_path) && file.exists(diagnostic_path)) {
      unlink(diagnostic_path)
    }
    return(list(
      data = x,
      diagnostics = data.table::data.table(),
      repaired_rows = 0L
    ))
  }

  raw <- collected$values[invalid, , drop = FALSE]
  exact <- t(vapply(
    seq_len(nrow(raw)),
    function(row_index) {
      values <- as.numeric(raw[row_index, ])
      if (any(!is.finite(values))) {
        return(rep(NA_real_, 5L))
      }
      quantiles <- stats::quantile(
        values,
        probs = c(0.025, 0.5, 0.975),
        names = FALSE,
        type = 8,
        na.rm = FALSE
      )
      c(
        mean(values),
        quantiles[[2L]],
        if (length(values) > 1L) stats::sd(values) else NA_real_,
        quantiles[[1L]],
        quantiles[[3L]]
      )
    },
    numeric(5L)
  ))
  exact <- data.table::as.data.table(exact)
  data.table::setnames(
    exact,
    c(
      "recomputed_draw_mean",
      "recomputed_draw_median",
      "recomputed_draw_sd",
      "recomputed_lower",
      "recomputed_upper"
    )
  )

  draw_min <- vapply(
    seq_len(nrow(raw)),
    function(row_index) min(raw[row_index, ]),
    numeric(1L)
  )
  draw_max <- vapply(
    seq_len(nrow(raw)),
    function(row_index) max(raw[row_index, ]),
    numeric(1L)
  )
  diagnostics <- cbind(
    data.table::copy(x[invalid, ..key_columns]),
    x[invalid, .(
      original_draw_mean = draw_mean,
      original_draw_median = draw_median,
      original_draw_sd = draw_sd,
      original_lower = lower,
      original_upper = upper
    )],
    data.table::data.table(
      draw_min = draw_min,
      draw_max = draw_max,
      finite_draws = rowSums(is.finite(raw)),
      total_draws = ncol(raw)
    ),
    exact
  )

  x[invalid, `:=`(
    draw_mean = exact$recomputed_draw_mean,
    draw_median = exact$recomputed_draw_median,
    draw_sd = exact$recomputed_draw_sd,
    lower = exact$recomputed_lower,
    upper = exact$recomputed_upper
  )]

  diagnostics[, repaired :=
    is.finite(recomputed_lower) &
      is.finite(recomputed_upper) &
      recomputed_lower <= recomputed_upper]

  if (!is.null(diagnostic_path)) {
    dir.create(dirname(diagnostic_path), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(diagnostics, diagnostic_path)
  }

  remaining <- which(
    !is.finite(x$lower) |
      !is.finite(x$upper) |
      x$lower > x$upper
  )
  if (length(remaining)) {
    stop(
      "The ", label, " uncertainty table still contains ",
      length(remaining), " invalid interval row(s) after exact type-8 ",
      "recomputation.",
      if (!is.null(diagnostic_path)) paste0(
        " Review ", diagnostic_path, "."
      ) else "",
      call. = FALSE
    )
  }

  message(
    "Recomputed ", length(invalid), " invalid ", label,
    " interval row(s) with stats::quantile(type = 8).",
    if (!is.null(diagnostic_path)) paste0(
      " Diagnostic: ", diagnostic_path
    ) else ""
  )
  list(
    data = x,
    diagnostics = diagnostics,
    repaired_rows = length(invalid)
  )
}

nbd_integrated_parse_codes <- function(value) {
  values <- trimws(unlist(strsplit(
    if (is.null(value) || length(value) == 0L || is.na(value)) "" else as.character(value),
    ";",
    fixed = TRUE
  )))
  values <- values[nzchar(values)]
  out <- suppressWarnings(as.integer(values))
  if (length(values) && anyNA(out)) {
    stop("Unable to parse code list: ", value, call. = FALSE)
  }
  sort(unique(out))
}

nbd_validate_uncertainty_report_mapping <- function(root) {
  nbd_integrated_require("data.table")
  cause_path <- file.path(root, "report", "config", "cause_comparison_map.csv")
  hierarchy_path <- file.path(root, "data", "lookups", "analysis_to_za.csv")
  age_path <- file.path(root, "report", "config", "age_comparison_map.csv")
  required <- c(cause_path, hierarchy_path, age_path)
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    stop(
      "Cannot verify uncertainty-to-report mappings. Missing:\n- ",
      paste(missing, collapse = "\n- "),
      call. = FALSE
    )
  }

  cause_map <- data.table::fread(cause_path, na.strings = c("", "NA"))
  hierarchy <- data.table::fread(hierarchy_path, na.strings = c("", "NA"))
  age_map <- data.table::fread(age_path, na.strings = c("", "NA"))
  required_cause_columns <- c("series_id", "za_codes", "measure")
  required_hierarchy_columns <- c("analysis_code", "za_code", "weight")
  required_age_columns <- c("age_id", "age_label")
  for (item in list(
    list(data = cause_map, columns = required_cause_columns, label = cause_path),
    list(data = hierarchy, columns = required_hierarchy_columns, label = hierarchy_path),
    list(data = age_map, columns = required_age_columns, label = age_path)
  )) {
    absent <- setdiff(item$columns, names(item$data))
    if (length(absent)) {
      stop(
        item$label, " is missing column(s): ", paste(absent, collapse = ", "),
        call. = FALSE
      )
    }
  }

  catalog <- nbd_uncertainty_series_catalog()
  direct_series <- c(
    "hiv_deaths", "transport_deaths", "unintentional_deaths",
    "interpersonal_deaths", "self_harm_deaths"
  )
  for (series_name in direct_series) {
    row <- cause_map[series_id == series_name & measure == "deaths"]
    if (nrow(row) != 1L) {
      stop(
        "Expected one death-series mapping for ", series_name,
        "; found ", nrow(row), ".",
        call. = FALSE
      )
    }
    za_codes <- nbd_integrated_parse_codes(row$za_codes[[1L]])
    analysis_codes <- sort(unique(as.integer(
      hierarchy[za_code %in% za_codes & weight > 0, analysis_code]
    )))
    expected <- paste0("nbd_", analysis_codes)
    observed <- sort(as.character(catalog$counts[[series_name]]))
    if (!identical(sort(expected), observed)) {
      stop(
        "The uncertainty definition for ", series_name,
        " does not reproduce analysis_to_za.csv. Expected ",
        paste(expected, collapse = ", "), "; observed ",
        paste(observed, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  injury_row <- cause_map[series_id == "injuries_deaths" & measure == "deaths"]
  if (nrow(injury_row) != 1L) {
    stop("Expected one injuries_deaths mapping.", call. = FALSE)
  }
  injury_za <- nbd_integrated_parse_codes(injury_row$za_codes[[1L]])
  expected_injury_codes <- sort(unique(as.integer(
    hierarchy[za_code %in% injury_za & weight > 0, analysis_code]
  )))
  engine_injury_codes <- if (exists("UC_INJURY_CODES", inherits = TRUE)) {
    sort(as.integer(get("UC_INJURY_CODES", inherits = TRUE)))
  } else {
    c(124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L,
      135L, 136L, 137L, 138L, 139L, 140L, 141L)
  }
  if (!identical(expected_injury_codes, engine_injury_codes)) {
    stop(
      "The uncertainty injury envelope does not reproduce the report hierarchy. ",
      "Expected analysis codes ", paste(expected_injury_codes, collapse = ", "),
      "; engine codes ", paste(engine_injury_codes, collapse = ", "), ".",
      call. = FALSE
    )
  }

  required_fraction_series <- names(catalog$fractions)
  missing_fraction_series <- setdiff(
    required_fraction_series,
    cause_map[measure == "fraction", series_id]
  )
  if (length(missing_fraction_series)) {
    stop(
      "The report map is missing uncertainty fraction series: ",
      paste(missing_fraction_series, collapse = ", "),
      call. = FALSE
    )
  }

  expected_ages <- data.table::data.table(
    age_id = c("age_under1", "age_under5", "age_all"),
    age_label = c("<1 year", "Under 5 years", "All ages")
  )
  observed_ages <- age_map[age_id %in% expected_ages$age_id, .(age_id, age_label)]
  data.table::setorder(expected_ages, age_id)
  data.table::setorder(observed_ages, age_id)
  age_mapping_ok <- identical(expected_ages$age_id, observed_ages$age_id) &&
    identical(expected_ages$age_label, observed_ages$age_label)
  if (!age_mapping_ok) {
    stop(
      "The uncertainty age mapping no longer matches age_comparison_map.csv.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

nbd_uncertainty_series_catalog <- function() {
  list(
    counts = list(
      hiv_all_causes_deaths = "all_causes",
      hiv_deaths = "nbd_2",
      injuries_deaths = "all_injuries",
      transport_deaths = c("nbd_124", "nbd_125"),
      unintentional_deaths = paste0(
        "nbd_",
        INJURY_OTHER_UNINTENTIONAL_CODES
      ),
      interpersonal_deaths = "nbd_141",
      self_harm_deaths = "nbd_140"
    ),
    fractions = c(
      hiv_fraction = "hiv_deaths",
      injuries_fraction = "injuries_deaths",
      transport_fraction = "transport_deaths",
      unintentional_fraction = "unintentional_deaths",
      interpersonal_fraction = "interpersonal_deaths",
      self_harm_fraction = "self_harm_deaths"
    ),
    denominator = "hiv_all_causes_deaths"
  )
}

nbd_uncertainty_age_map <- function() {
  nbd_integrated_require("data.table")
  data.table::data.table(
    age_group = c("under_1", "under_5", "all_ages"),
    age_id = c("age_under1", "age_under5", "age_all"),
    age_label = c("<1 year", "Under 5 years", "All ages")
  )
}

nbd_uncertainty_profile_paths <- function(
    root,
    uncertainty_config_path = file.path("config", "uncertainty_joint.yml"),
    scenario = "joint") {
  nbd_integrated_require("yaml")
  config_path <- nbd_integrated_resolve_path(root, uncertainty_config_path)
  if (!file.exists(config_path)) {
    stop("Uncertainty configuration not found: ", config_path, call. = FALSE)
  }
  profile <- yaml::read_yaml(config_path)
  output_name <- profile$run$output_name
  if (is.null(output_name) || length(output_name) != 1L ||
      is.na(output_name) || !nzchar(output_name)) {
    stop("run.output_name is missing from ", config_path, ".", call. = FALSE)
  }
  output_root <- file.path(root, "output", "uncertainty", output_name)
  scenario_root <- file.path(output_root, scenario)
  list(
    config = profile,
    config_path = config_path,
    output_root = output_root,
    scenario_root = scenario_root,
    draw_dir = file.path(scenario_root, "draws"),
    population_draw_dir = file.path(scenario_root, "population_draws"),
    population_point = file.path(output_root, "population_point_report.parquet"),
    draws = file.path(scenario_root, "uncertainty_draws.parquet"),
    summary = file.path(scenario_root, "uncertainty_summary.parquet"),
    summary_csv = file.path(scenario_root, "uncertainty_summary.csv"),
    diagnostics = file.path(scenario_root, "draw_diagnostics.csv"),
    convergence = file.path(scenario_root, "convergence_headlines.csv")
  )
}

nbd_derive_uncertainty_series <- function(
    source,
    value_column,
    include_draw_id = TRUE) {
  nbd_integrated_require("data.table")
  x <- data.table::as.data.table(data.table::copy(source))
  group_columns <- c(
    if (isTRUE(include_draw_id)) "draw_id",
    "Death_Prov", "Sex", "DeathYear", "age_group"
  )
  required <- c(group_columns, "cause_id", value_column)
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Uncertainty data are missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x <- x[age_group %in% nbd_uncertainty_age_map()$age_group]
  if (!nrow(x)) {
    stop("No uncertainty rows match the report age groups.", call. = FALSE)
  }

  base <- unique(x[cause_id == "all_causes", ..group_columns])
  if (!nrow(base)) {
    stop("The uncertainty output does not contain all_causes rows.", call. = FALSE)
  }

  catalog <- nbd_uncertainty_series_catalog()
  count_pieces <- lapply(names(catalog$counts), function(series_name) {
    cause_ids <- catalog$counts[[series_name]]
    values <- x[
      cause_id %in% cause_ids,
      .(value = sum(get(value_column), na.rm = TRUE)),
      by = eval(group_columns)
    ]
    values <- merge(base, values, by = group_columns, all.x = TRUE, sort = FALSE)
    values[is.na(value), value := 0]
    values[, `:=`(series_id = series_name, measure = "deaths")]
    values[]
  })
  counts <- data.table::rbindlist(count_pieces, use.names = TRUE, fill = TRUE)

  denominator <- counts[
    series_id == catalog$denominator,
    c(group_columns, "value"),
    with = FALSE
  ]
  data.table::setnames(denominator, "value", "denominator")

  fraction_pieces <- lapply(names(catalog$fractions), function(series_name) {
    numerator_name <- unname(catalog$fractions[[series_name]])
    numerator <- counts[
      series_id == numerator_name,
      c(group_columns, "value"),
      with = FALSE
    ]
    values <- merge(
      numerator,
      denominator,
      by = group_columns,
      all.x = TRUE,
      sort = FALSE
    )
    values[, value := data.table::fifelse(
      is.finite(denominator) & denominator > 0,
      value / denominator,
      NA_real_
    )]
    values[, denominator := NULL]
    values[, `:=`(series_id = series_name, measure = "fraction")]
    values[]
  })
  fractions <- data.table::rbindlist(
    fraction_pieces,
    use.names = TRUE,
    fill = TRUE
  )

  out <- data.table::rbindlist(
    list(counts, fractions),
    use.names = TRUE,
    fill = TRUE
  )
  out[, domain := data.table::fifelse(
    grepl("^hiv_", series_id),
    "hiv",
    "injury"
  )]
  out <- merge(
    out,
    nbd_uncertainty_age_map(),
    by = "age_group",
    all = FALSE,
    sort = FALSE
  )
  data.table::setnames(
    out,
    c("Death_Prov", "Sex", "DeathYear"),
    c("geography_code", "sex_code", "year")
  )
  data.table::setcolorder(
    out,
    c(
      if (isTRUE(include_draw_id)) "draw_id",
      "domain", "geography_code", "sex_code", "year",
      "age_group", "age_id", "age_label", "series_id", "measure", "value"
    )
  )
  out[]
}

nbd_build_uncertainty_intervals <- function(
    root,
    uncertainty_config_path = file.path("config", "uncertainty_joint.yml"),
    scenario = "joint") {
  nbd_integrated_require("arrow")
  nbd_integrated_require("data.table")
  nbd_validate_uncertainty_report_mapping(root)

  paths <- nbd_uncertainty_profile_paths(
    root = root,
    uncertainty_config_path = uncertainty_config_path,
    scenario = scenario
  )
  required_files <- c(paths$diagnostics)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files)) {
    stop(
      "Required uncertainty output file(s) are missing:\n- ",
      paste(missing_files, collapse = "\n- "),
      call. = FALSE
    )
  }

  scenario_name <- as.character(scenario)
  interval_keys <- c(
    "domain", "geography_code", "sex_code", "year",
    "age_group", "age_id", "age_label", "series_id", "measure"
  )
  collected <- nbd_collect_uncertainty_draw_matrix(
    paths = paths,
    derive = function(draw) {
      selected <- if (
        data.table::uniqueN(draw$scenario) == 1L &&
        identical(unique(as.character(draw$scenario)), scenario_name)
      ) {
        draw
      } else {
        draw[get("scenario") == scenario_name]
      }
      nbd_derive_uncertainty_series(
        selected,
        value_column = "Deaths",
        include_draw_id = TRUE
      )
    },
    key_columns = interval_keys,
    label = "comparison"
  )
  intervals <- nbd_summarise_uncertainty_draw_matrix(collected)
  rm(collected)
  invisible(gc(verbose = FALSE))

  summary_input <- nbd_read_uncertainty_summary(paths)
  summary_input <- summary_input[get("scenario") == scenario_name]
  point_source <- unique(summary_input[, .(
    Death_Prov, Sex, DeathYear, age_group, cause_id, point
  )])
  point_series <- nbd_derive_uncertainty_series(
    point_source,
    value_column = "point",
    include_draw_id = FALSE
  )

  points <- point_series[, .(point = value), by = eval(interval_keys)]
  intervals <- merge(
    intervals,
    points,
    by = interval_keys,
    all.x = TRUE,
    sort = FALSE
  )
  intervals[, `:=`(
    draw_minus_point = draw_mean - point,
    relative_draw_minus_point = data.table::fifelse(
      is.finite(point) & abs(point) > 0,
      (draw_mean - point) / abs(point),
      NA_real_
    )
  )]

  bad_intervals <- intervals[
    !is.finite(lower) | !is.finite(upper) | lower > upper
  ]
  if (nrow(bad_intervals)) {
    stop(
      "The uncertainty interval table contains ", nrow(bad_intervals),
      " invalid row(s).",
      call. = FALSE
    )
  }

  output_dir <- file.path(root, "output", "report-data")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  parquet_path <- file.path(output_dir, "nbd3_uncertainty_comparisons.parquet")
  csv_path <- file.path(output_dir, "nbd3_uncertainty_comparisons.csv")
  if (exists("write_parquet_dt", mode = "function", inherits = TRUE)) {
    write_parquet_dt(intervals, parquet_path)
  } else {
    arrow::write_parquet(intervals, parquet_path, compression = "zstd")
  }
  data.table::fwrite(intervals, csv_path)

  list(
    data = intervals,
    paths = paths,
    parquet = parquet_path,
    csv = csv_path
  )
}

# Detailed NBD3-R cause uncertainty for the final-results panels --------------
#
# The uncertainty engine stores compact draw outputs by analysis cause,
# province/national geography, person sex, year, and configured age group. The
# functions below translate only exactly reconstructible draw series to the ZA
# cause hierarchy used by the final NBD3-R database. A report series is omitted
# when any required analysis cause is absent from the stored draw catalogue.
# No interval is interpolated, imputed, or borrowed from another series.

nbd_build_cause_uncertainty_mapping <- function(root, available_cause_ids) {
  nbd_integrated_require("data.table")
  if (!exists("read_viz_config", mode = "function", inherits = TRUE)) {
    source(file.path(root, "report", "R", "report_data.R"), local = FALSE)
  }

  config <- read_viz_config(root)
  cause_map <- data.table::as.data.table(data.table::copy(
    config$cause_map[include_explorer %in% TRUE]
  ))
  hierarchy_path <- file.path(root, "data", "lookups", "analysis_to_za.csv")
  if (!file.exists(hierarchy_path)) {
    stop("Cause hierarchy not found: ", hierarchy_path, call. = FALSE)
  }
  hierarchy <- data.table::fread(hierarchy_path, na.strings = c("", "NA"))
  required_hierarchy <- c("analysis_code", "za_code", "weight")
  missing_hierarchy <- setdiff(required_hierarchy, names(hierarchy))
  if (length(missing_hierarchy)) {
    stop(
      "analysis_to_za.csv is missing column(s): ",
      paste(missing_hierarchy, collapse = ", "),
      call. = FALSE
    )
  }
  hierarchy[, `:=`(
    analysis_code = as.integer(analysis_code),
    za_code = as.integer(za_code),
    weight = as.numeric(weight)
  )]

  available_cause_ids <- sort(unique(as.character(available_cause_ids)))
  mapping_rows <- vector("list", nrow(cause_map))
  coverage_rows <- vector("list", nrow(cause_map))

  for (index in seq_len(nrow(cause_map))) {
    row <- cause_map[index]
    za_codes <- nbd_integrated_parse_codes(row$za_codes[[1L]])
    source_map <- data.table::data.table(
      source_cause_id = character(),
      weight = numeric()
    )
    reason <- ""

    if (length(za_codes) == 1L && identical(za_codes, 172L)) {
      source_map <- data.table::data.table(
        source_cause_id = "all_causes",
        weight = 1
      )
    } else if (length(za_codes) == 1L && identical(za_codes, 171L)) {
      source_map <- data.table::data.table(
        source_cause_id = "all_injuries",
        weight = 1
      )
    } else if (!length(za_codes)) {
      reason <- "No ZA cause code is configured."
    } else {
      selected <- hierarchy[
        za_code %in% za_codes & is.finite(weight) & weight > 0,
        .(weight = sum(weight)),
        by = analysis_code
      ]
      if (!nrow(selected)) {
        reason <- "No positive analysis-to-ZA mapping was found."
      } else if (selected[weight > 1 + 1e-12, .N]) {
        reason <- paste0(
          "The configured ZA codes overlap in the hierarchy for analysis code(s): ",
          paste(selected[weight > 1 + 1e-12, analysis_code], collapse = ", "),
          "."
        )
      } else {
        source_map <- selected[, .(
          source_cause_id = paste0("nbd_", analysis_code),
          weight = as.numeric(weight)
        )]
      }
    }

    required_ids <- sort(unique(source_map$source_cause_id))
    missing_ids <- setdiff(required_ids, available_cause_ids)
    supported <- length(required_ids) > 0L && !length(missing_ids) && !nzchar(reason)
    if (!supported && !nzchar(reason)) {
      reason <- paste0(
        "Required analysis cause(s) are not stored in the completed draw catalogue: ",
        paste(missing_ids, collapse = ", "),
        "."
      )
    }

    coverage_rows[[index]] <- data.table::data.table(
      series_id = as.character(row$series_id[[1L]]),
      series_label = as.character(row$display_name[[1L]]),
      domain = as.character(row$domain[[1L]]),
      hierarchy = as.character(row$hierarchy[[1L]]),
      cause_type = as.character(row$cause_type[[1L]]),
      series_sort_order = as.integer(row$sort_order[[1L]]),
      za_codes = paste(za_codes, collapse = ";"),
      required_cause_ids = paste(required_ids, collapse = ";"),
      missing_cause_ids = paste(missing_ids, collapse = ";"),
      supported = isTRUE(supported),
      reason = if (isTRUE(supported)) "Exact reconstruction available." else reason
    )

    if (isTRUE(supported)) {
      source_map[, `:=`(
        series_id = as.character(row$series_id[[1L]]),
        series_label = as.character(row$display_name[[1L]]),
        domain = as.character(row$domain[[1L]]),
        hierarchy = as.character(row$hierarchy[[1L]]),
        cause_type = as.character(row$cause_type[[1L]]),
        series_sort_order = as.integer(row$sort_order[[1L]])
      )]
      mapping_rows[[index]] <- source_map
    }
  }

  mapping <- data.table::rbindlist(mapping_rows, use.names = TRUE, fill = TRUE)
  coverage <- data.table::rbindlist(coverage_rows, use.names = TRUE, fill = TRUE)
  if (!nrow(mapping)) {
    stop(
      "None of the final NBD3-R cause series can be reconstructed from the ",
      "completed uncertainty draw catalogue.",
      call. = FALSE
    )
  }
  data.table::setorder(mapping, series_sort_order, series_id, source_cause_id)
  data.table::setorder(coverage, series_sort_order, series_id)
  list(mapping = mapping, coverage = coverage, config = config)
}

nbd_derive_cause_uncertainty_series <- function(
    source,
    mapping,
    value_column,
    include_draw_id = TRUE) {
  nbd_integrated_require("data.table")
  x <- data.table::as.data.table(data.table::copy(source))
  group_columns <- c(
    if (isTRUE(include_draw_id)) "draw_id",
    "Death_Prov", "Sex", "DeathYear", "age_group"
  )
  required <- c(group_columns, "cause_id", value_column)
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Cause uncertainty source is missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x <- x[age_group %in% nbd_uncertainty_age_map()$age_group]
  x <- x[cause_id %in% mapping$source_cause_id]
  if (!nrow(x)) {
    stop("No uncertainty draw rows match the final-cause mapping.", call. = FALSE)
  }
  joined <- merge(
    x,
    mapping,
    by.x = "cause_id",
    by.y = "source_cause_id",
    all = FALSE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  joined[, weighted_value := as.numeric(get(value_column)) * as.numeric(weight)]
  metadata_columns <- c("series_id", "series_sort_order")
  by_columns <- c(group_columns, metadata_columns)
  out <- joined[, .(value = sum(weighted_value, na.rm = TRUE)), by = eval(by_columns)]
  out[]
}


nbd_derive_population_cause_uncertainty_series <- function(
    source,
    mapping,
    value_column,
    include_draw_id = TRUE) {
  nbd_integrated_require("data.table")
  x <- data.table::as.data.table(data.table::copy(source))
  group_columns <- c(
    if (isTRUE(include_draw_id)) "draw_id",
    "Popgroup", "Sex", "DeathYear", "age_group"
  )
  required <- c(group_columns, "cause_id", value_column)
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Population-group cause uncertainty is missing column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  x <- x[age_group %in% nbd_uncertainty_age_map()$age_group]
  x <- x[cause_id %in% mapping$source_cause_id]
  if (!nrow(x)) {
    stop(
      "No population-group draw rows match the final-cause mapping.",
      call. = FALSE
    )
  }
  joined <- merge(
    x,
    mapping,
    by.x = "cause_id",
    by.y = "source_cause_id",
    all = FALSE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  joined[, weighted_value := as.numeric(get(value_column)) * as.numeric(weight)]
  by_columns <- c(group_columns, "series_id", "series_sort_order")
  joined[, .(value = sum(weighted_value, na.rm = TRUE)), by = eval(by_columns)]
}

nbd_build_cause_uncertainty_intervals <- function(
    root,
    uncertainty_config_path = file.path("config", "uncertainty_joint.yml"),
    scenario = "joint",
    relative_tolerance = 2e-7,
    absolute_tolerance = 1e-6) {
  nbd_integrated_require("arrow")
  nbd_integrated_require("data.table")
  if (!exists("read_viz_config", mode = "function", inherits = TRUE)) {
    source(file.path(root, "report", "R", "report_data.R"), local = FALSE)
  }

  paths <- nbd_uncertainty_profile_paths(
    root = root,
    uncertainty_config_path = uncertainty_config_path,
    scenario = scenario
  )
  scenario_name <- as.character(scenario)
  summary_input <- nbd_read_uncertainty_summary(paths)[
    get("scenario") == scenario_name
  ]
  if (!nrow(summary_input)) {
    stop("The selected uncertainty scenario contains no rows: ", scenario_name,
         call. = FALSE)
  }

  available_cause_ids <- unique(as.character(summary_input$cause_id))
  mapping_result <- nbd_build_cause_uncertainty_mapping(
    root = root,
    available_cause_ids = available_cause_ids
  )
  series_metadata <- unique(mapping_result$mapping[, .(
    series_id,
    series_sort_order,
    series_label,
    domain,
    hierarchy,
    cause_type
  )])
  if (series_metadata[, data.table::uniqueN(series_id)] != nrow(series_metadata)) {
    stop("Final-cause uncertainty metadata are not unique by series_id.",
         call. = FALSE)
  }

  output_dir <- file.path(root, "output", "report-data")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  repair_paths <- character()
  repaired_rows <- 0L

  # Province and national intervals ------------------------------------------
  interval_keys <- c(
    "Death_Prov", "Sex", "DeathYear", "age_group",
    "series_id", "series_sort_order"
  )
  collected <- nbd_collect_uncertainty_draw_matrix(
    paths = paths,
    derive = function(draw) {
      selected <- if (
        data.table::uniqueN(draw$scenario) == 1L &&
        identical(unique(as.character(draw$scenario)), scenario_name)
      ) {
        draw
      } else {
        draw[get("scenario") == scenario_name]
      }
      nbd_derive_cause_uncertainty_series(
        source = selected,
        mapping = mapping_result$mapping,
        value_column = "Deaths",
        include_draw_id = TRUE
      )
    },
    key_columns = interval_keys,
    label = "final-cause"
  )
  province_intervals <- nbd_summarise_uncertainty_draw_matrix(collected)
  province_repair_path <- file.path(
    output_dir,
    "final_cause_interval_repair.csv"
  )
  province_repair <- nbd_recompute_invalid_interval_rows(
    summary = province_intervals,
    collected = collected,
    key_columns = interval_keys,
    label = "final-cause",
    diagnostic_path = province_repair_path
  )
  province_intervals <- province_repair$data
  if (province_repair$repaired_rows > 0L) {
    repair_paths <- c(repair_paths, province_repair_path)
    repaired_rows <- repaired_rows + province_repair$repaired_rows
  }
  data.table::setnames(
    province_intervals,
    c("draw_mean", "draw_median", "draw_sd", "lower", "upper"),
    c(
      "draw_mean_deaths", "draw_median_deaths", "draw_sd_deaths",
      "lower_deaths", "upper_deaths"
    )
  )
  rm(collected)
  invisible(gc(verbose = FALSE))

  point_source <- unique(summary_input[, .(
    Death_Prov, Sex, DeathYear, age_group, cause_id, point
  )])
  point_series <- nbd_derive_cause_uncertainty_series(
    source = point_source,
    mapping = mapping_result$mapping,
    value_column = "point",
    include_draw_id = FALSE
  )
  points <- point_series[, .(point_deaths = value), by = eval(interval_keys)]
  province_intervals <- merge(
    province_intervals,
    points,
    by = interval_keys,
    all.x = TRUE,
    sort = FALSE
  )
  province_intervals <- merge(
    province_intervals,
    series_metadata,
    by = c("series_id", "series_sort_order"),
    all.x = TRUE,
    sort = FALSE
  )
  province_intervals <- merge(
    province_intervals,
    nbd_uncertainty_age_map(),
    by = "age_group",
    all = FALSE,
    sort = FALSE
  )
  data.table::setnames(
    province_intervals,
    c("Death_Prov", "Sex", "DeathYear"),
    c("geography_code", "sex_code", "year")
  )

  config <- mapping_result$config
  sex <- sex_catalog(config)
  province_geography <- geography_catalog(config)[
    geography_type %in% c("province", "national")
  ]
  province_intervals <- merge(
    province_intervals,
    province_geography,
    by = "geography_code",
    all.x = TRUE,
    sort = FALSE
  )
  province_intervals <- merge(
    province_intervals,
    sex,
    by = "sex_code",
    all.x = TRUE,
    sort = FALSE
  )

  # National population-group intervals -------------------------------------
  population_intervals <- data.table::data.table()
  include_population_groups <- !is.null(
    paths$config$reporting$include_population_groups
  ) && isTRUE(paths$config$reporting$include_population_groups)

  if (include_population_groups) {
    population_files <- nbd_population_uncertainty_draw_files(paths)
    if (!length(population_files)) {
      stop(
        "Population-group uncertainty was requested, but no completed draw ",
        "files were found in ", paths$population_draw_dir, ".",
        call. = FALSE
      )
    }
    if (!file.exists(paths$population_point)) {
      stop(
        "Population-group point report is missing: ",
        paths$population_point, ".",
        call. = FALSE
      )
    }

    population_keys <- c(
      "Popgroup", "Sex", "DeathYear", "age_group",
      "series_id", "series_sort_order"
    )
    population_collected <- nbd_collect_uncertainty_draw_matrix(
      paths = paths,
      derive = function(draw) {
        selected <- if (
          data.table::uniqueN(draw$scenario) == 1L &&
          identical(unique(as.character(draw$scenario)), scenario_name)
        ) {
          draw
        } else {
          draw[get("scenario") == scenario_name]
        }
        nbd_derive_population_cause_uncertainty_series(
          source = selected,
          mapping = mapping_result$mapping,
          value_column = "Deaths",
          include_draw_id = TRUE
        )
      },
      key_columns = population_keys,
      label = "population-group final-cause",
      files = population_files,
      reader = nbd_read_population_uncertainty_draw_file
    )
    population_intervals <- nbd_summarise_uncertainty_draw_matrix(
      population_collected
    )
    population_repair_path <- file.path(
      output_dir,
      "population_group_interval_repair.csv"
    )
    population_repair <- nbd_recompute_invalid_interval_rows(
      summary = population_intervals,
      collected = population_collected,
      key_columns = population_keys,
      label = "population-group final-cause",
      diagnostic_path = population_repair_path
    )
    population_intervals <- population_repair$data
    if (population_repair$repaired_rows > 0L) {
      repair_paths <- c(repair_paths, population_repair_path)
      repaired_rows <- repaired_rows + population_repair$repaired_rows
    }
    data.table::setnames(
      population_intervals,
      c("draw_mean", "draw_median", "draw_sd", "lower", "upper"),
      c(
        "draw_mean_deaths", "draw_median_deaths", "draw_sd_deaths",
        "lower_deaths", "upper_deaths"
      )
    )
    rm(population_collected)
    invisible(gc(verbose = FALSE))

    population_point <- data.table::as.data.table(
      arrow::read_parquet(paths$population_point, as_data_frame = TRUE)
    )
    required_population_point <- c(
      "Popgroup", "Sex", "DeathYear", "age_group", "cause_id", "Deaths"
    )
    missing_population_point <- setdiff(
      required_population_point,
      names(population_point)
    )
    if (length(missing_population_point)) {
      stop(
        "Population-group point report is missing column(s): ",
        paste(missing_population_point, collapse = ", "), ".",
        call. = FALSE
      )
    }
    population_point_series <- nbd_derive_population_cause_uncertainty_series(
      source = population_point,
      mapping = mapping_result$mapping,
      value_column = "Deaths",
      include_draw_id = FALSE
    )
    population_points <- population_point_series[
      , .(point_deaths = value),
      by = eval(population_keys)
    ]
    population_intervals <- merge(
      population_intervals,
      population_points,
      by = population_keys,
      all.x = TRUE,
      sort = FALSE
    )
    population_intervals <- merge(
      population_intervals,
      series_metadata,
      by = c("series_id", "series_sort_order"),
      all.x = TRUE,
      sort = FALSE
    )
    population_intervals <- merge(
      population_intervals,
      nbd_uncertainty_age_map(),
      by = "age_group",
      all = FALSE,
      sort = FALSE
    )
    data.table::setnames(
      population_intervals,
      c("Sex", "DeathYear"),
      c("sex_code", "year")
    )
    population_intervals[, geography_code := as.integer(Popgroup) + 10L]
    population_intervals[, Popgroup := NULL]
    population_geography <- geography_catalog(config)[
      geography_type == "population_group"
    ]
    population_intervals <- merge(
      population_intervals,
      population_geography,
      by = "geography_code",
      all.x = TRUE,
      sort = FALSE
    )
    population_intervals <- merge(
      population_intervals,
      sex,
      by = "sex_code",
      all.x = TRUE,
      sort = FALSE
    )
  }

  intervals <- data.table::rbindlist(
    list(province_intervals, population_intervals),
    use.names = TRUE,
    fill = TRUE
  )
  if (intervals[
    !is.finite(lower_deaths) | !is.finite(upper_deaths) |
      lower_deaths > upper_deaths,
    .N
  ]) {
    stop("The final-cause uncertainty table contains invalid intervals.",
         call. = FALSE)
  }
  if (intervals[
    is.na(geography_type) | is.na(geography) | is.na(sex),
    .N
  ]) {
    stop("Could not label one or more final-cause uncertainty cells.",
         call. = FALSE)
  }

  # Align draw points to the deterministic NBD3-R reporting table ------------
  rates_path <- derived_path(config, "cause_rates.parquet")
  if (!file.exists(rates_path)) {
    stop("The NBD3-R cause-rate table is missing: ", rates_path,
         call. = FALSE)
  }
  deterministic <- read_parquet_dt(rates_path)[
    model == "NBD3-R",
    .(
      geography_type, geography_code, geography,
      sex_code, sex, year, age_id, age_label,
      series_id, deterministic_series_label = series_label,
      deterministic_domain = domain,
      deterministic_hierarchy = hierarchy,
      deterministic_cause_type = cause_type,
      deaths, population, crude_rate
    )
  ]
  deterministic_key <- c(
    "geography_type", "geography_code", "sex_code", "year", "age_id",
    "series_id"
  )
  intervals <- merge(
    intervals,
    deterministic,
    by = deterministic_key,
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", "_deterministic")
  )
  unmatched <- intervals[!is.finite(deaths)]
  if (nrow(unmatched)) {
    first <- unmatched[1L]
    stop(
      "The final NBD3-R cause table is missing ", nrow(unmatched),
      " uncertainty cell(s). First unmatched cell: type=",
      first$geography_type, ", series=", first$series_id,
      ", geography=", first$geography_code, ", sex=", first$sex_code,
      ", year=", first$year, ", age=", first$age_id, ".",
      call. = FALSE
    )
  }

  alignment_scale <- pmax(1, abs(intervals$deaths), abs(intervals$point_deaths))
  mismatch <- intervals[
    !is.finite(point_deaths) |
      abs(deaths - point_deaths) > pmax(
        absolute_tolerance,
        relative_tolerance * alignment_scale
      )
  ]
  if (nrow(mismatch)) {
    first <- mismatch[1L]
    stop(
      "Final-cause uncertainty points do not align with the deterministic ",
      "NBD3-R table in ", nrow(mismatch), " row(s). First mismatch: type=",
      first$geography_type, ", series=", first$series_id,
      ", geography=", first$geography_code,
      ", sex=", first$sex_code, ", year=", first$year,
      ", age=", first$age_id, ", NBD3-R=", first$deaths,
      ", uncertainty point=", first$point_deaths, ".",
      call. = FALSE
    )
  }

  intervals[, `:=`(
    series_label = deterministic_series_label,
    domain = deterministic_domain,
    hierarchy = deterministic_hierarchy,
    cause_type = deterministic_cause_type,
    model = "NBD3-R",
    source = "NBD3 Version 1 joint uncertainty draws"
  )]

  death_rows <- intervals[, .(
    model, geography_type, geography_code, geography,
    sex_code, sex, year, age_id, age_label,
    series_id, series_label, domain, hierarchy, cause_type,
    measure = "deaths",
    unit = "deaths",
    estimate = deaths,
    point = point_deaths,
    draw_mean = draw_mean_deaths,
    draw_median = draw_median_deaths,
    draw_sd = draw_sd_deaths,
    lower = lower_deaths,
    upper = upper_deaths,
    n_draws,
    population,
    series_sort_order,
    source
  )]

  rate_scale <- as.numeric(config$labels$build$rate_scale)
  rate_rows <- intervals[is.finite(population) & population > 0, .(
    model, geography_type, geography_code, geography,
    sex_code, sex, year, age_id, age_label,
    series_id, series_label, domain, hierarchy, cause_type,
    measure = "crude_rate",
    unit = "per 100,000",
    estimate = crude_rate,
    point = point_deaths / population * rate_scale,
    draw_mean = draw_mean_deaths / population * rate_scale,
    draw_median = draw_median_deaths / population * rate_scale,
    draw_sd = draw_sd_deaths / population * rate_scale,
    lower = lower_deaths / population * rate_scale,
    upper = upper_deaths / population * rate_scale,
    n_draws,
    population,
    series_sort_order,
    source
  )]

  output <- data.table::rbindlist(
    list(death_rows, rate_rows),
    use.names = TRUE,
    fill = TRUE
  )
  output[, `:=`(
    draw_minus_point = draw_mean - point,
    draw_minus_estimate = draw_mean - estimate,
    relative_draw_minus_estimate = data.table::fifelse(
      is.finite(estimate) & abs(estimate) > 0,
      (draw_mean - estimate) / abs(estimate),
      NA_real_
    )
  )]
  output_key <- c(
    "model", "geography_type", "geography_code", "sex_code", "year",
    "age_id", "series_id", "measure"
  )
  assert_unique_rows(output, output_key, "final NBD3-R cause uncertainty")
  data.table::setorder(
    output,
    domain, series_sort_order, geography_type, geography_code,
    sex_code, age_id, year, measure
  )

  coverage_counts <- output[, .(
    interval_rows = .N,
    first_year = min(year),
    last_year = max(year),
    geography_types = paste(sort(unique(geography_type)), collapse = ";"),
    geography_cells = data.table::uniqueN(paste(geography_type, geography_code)),
    sex_cells = data.table::uniqueN(sex_code),
    age_cells = data.table::uniqueN(age_id),
    measures = paste(sort(unique(measure)), collapse = ";")
  ), by = .(series_id)]
  coverage <- merge(
    mapping_result$coverage,
    coverage_counts,
    by = "series_id",
    all.x = TRUE,
    sort = FALSE
  )
  coverage[is.na(interval_rows), `:=`(
    interval_rows = 0L,
    geography_types = "",
    geography_cells = 0L,
    sex_cells = 0L,
    age_cells = 0L,
    measures = ""
  )]
  data.table::setorder(coverage, series_sort_order, series_id)

  parquet_path <- file.path(output_dir, "nbd3_cause_uncertainty.parquet")
  csv_path <- file.path(output_dir, "nbd3_cause_uncertainty.csv")
  coverage_path <- file.path(output_dir, "nbd3_cause_uncertainty_coverage.csv")
  if (exists("write_parquet_dt", mode = "function", inherits = TRUE)) {
    write_parquet_dt(output, parquet_path)
  } else {
    arrow::write_parquet(output, parquet_path, compression = "zstd")
  }
  data.table::fwrite(output, csv_path)
  data.table::fwrite(coverage, coverage_path)

  list(
    data = output,
    coverage = coverage,
    parquet = parquet_path,
    csv = csv_path,
    coverage_path = coverage_path,
    interval_repair_path = if (length(repair_paths)) {
      paste(repair_paths, collapse = ";")
    } else {
      NA_character_
    },
    interval_repaired_rows = repaired_rows,
    supported_series = coverage[supported %in% TRUE & interval_rows > 0, .N],
    point_alignment_max_abs = max(
      abs(output[measure == "deaths", estimate - point]),
      na.rm = TRUE
    )
  )
}


nbd_compare_tables_exactly <- function(before, after, key, label) {
  nbd_integrated_require("data.table")
  x <- data.table::as.data.table(data.table::copy(before))
  y <- data.table::as.data.table(data.table::copy(after))
  if (!identical(names(x), names(y))) {
    stop(label, " changed column names or column order.", call. = FALSE)
  }
  if (nrow(x) != nrow(y)) {
    stop(label, " changed row count from ", nrow(x), " to ", nrow(y), ".",
         call. = FALSE)
  }
  data.table::setorderv(x, key)
  data.table::setorderv(y, key)
  for (column in names(x)) {
    comparison <- all.equal(
      x[[column]], y[[column]],
      tolerance = 0,
      check.attributes = FALSE
    )
    if (!isTRUE(comparison)) {
      stop(label, " changed column '", column, "': ", comparison, call. = FALSE)
    }
  }
  invisible(TRUE)
}

nbd_attach_uncertainty_to_report <- function(
    root,
    uncertainty_config_path = file.path("config", "uncertainty_joint.yml"),
    scenario = "joint",
    relative_tolerance = 2e-7,
    absolute_tolerance = 1e-6) {
  nbd_integrated_require("data.table")
  if (!exists("read_viz_config", mode = "function", inherits = TRUE)) {
    source(file.path(root, "report", "R", "report_data.R"), local = FALSE)
  }

  config <- read_viz_config(root)
  nbd3_path <- derived_path(config, "nbd3_r_comparisons.parquet")
  legacy_path <- derived_path(config, "legacy_comparisons.parquet")
  runtime_path <- derived_path(config, "viz_input_new.rds")
  required <- c(nbd3_path, legacy_path, runtime_path)
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    stop(
      "Build the standard report inputs before attaching uncertainty. Missing:\n- ",
      paste(missing, collapse = "\n- "),
      call. = FALSE
    )
  }

  legacy_parquet_md5_before <- unname(tools::md5sum(legacy_path))
  legacy_rda_path <- config$labels$paths$legacy_rda
  if (is.na(legacy_rda_path) || !file.exists(legacy_rda_path)) {
    stop("The supplied viz.input.Rda could not be resolved.", call. = FALSE)
  }
  legacy_rda_md5_before <- unname(tools::md5sum(legacy_rda_path))
  runtime_before <- readRDS(runtime_path)
  external_before <- data.table::as.data.table(data.table::copy(
    runtime_before$comparisons[model != "NBD3-R"]
  ))

  interval_result <- nbd_build_uncertainty_intervals(
    root = root,
    uncertainty_config_path = uncertainty_config_path,
    scenario = scenario
  )
  intervals <- data.table::as.data.table(data.table::copy(interval_result$data))
  interval_key <- c(
    "domain", "geography_code", "sex_code", "year",
    "age_id", "series_id", "measure"
  )
  interval_columns <- c(
    interval_key,
    "point", "draw_mean", "draw_median", "draw_sd", "lower", "upper", "n_draws"
  )
  interval_join <- intervals[, ..interval_columns]
  assert_unique_rows(interval_join, interval_key, "NBD3 uncertainty intervals")
  data.table::setnames(
    interval_join,
    c("lower", "upper"),
    c("uncertainty_lower", "uncertainty_upper")
  )

  nbd3_before <- read_parquet_dt(nbd3_path)
  nbd3_after <- merge(
    nbd3_before,
    interval_join,
    by = interval_key,
    all.x = TRUE,
    sort = FALSE
  )

  nbd3_after[, alignment_scale := pmax(
    1,
    abs(estimate),
    abs(point)
  )]
  point_mismatch <- nbd3_after[
    is.finite(point) & is.finite(estimate) &
      abs(estimate - point) > pmax(
        absolute_tolerance,
        relative_tolerance * alignment_scale
      )
  ]
  if (nrow(point_mismatch)) {
    first <- point_mismatch[1L]
    stop(
      "Uncertainty point estimates do not align with NBD3-R in ",
      nrow(point_mismatch), " row(s). First mismatch: series=",
      first$series_id, ", geography=", first$geography_code,
      ", sex=", first$sex_code, ", year=", first$year,
      ", age=", first$age_id, ", NBD3-R=", first$estimate,
      ", uncertainty point=", first$point, ".",
      call. = FALSE
    )
  }

  matched <- is.finite(nbd3_after$uncertainty_lower) &
    is.finite(nbd3_after$uncertainty_upper)
  matched_count <- sum(matched)
  if (!matched_count) {
    stop("No uncertainty rows matched the NBD3-R comparison table.", call. = FALSE)
  }
  nbd3_after[matched, `:=`(
    lower = uncertainty_lower,
    upper = uncertainty_upper
  )]

  original_estimates <- nbd3_before[, c(interval_key, "estimate"), with = FALSE]
  updated_estimates <- nbd3_after[, c(interval_key, "estimate"), with = FALSE]
  nbd_compare_tables_exactly(
    original_estimates,
    updated_estimates,
    key = interval_key,
    label = "NBD3-R point estimates"
  )

  added_columns <- c(
    "point", "draw_mean", "draw_median", "draw_sd", "uncertainty_lower",
    "uncertainty_upper", "n_draws", "alignment_scale"
  )
  nbd3_after[, (added_columns) := NULL]
  data.table::setcolorder(nbd3_after, comparison_columns())
  assert_unique_rows(nbd3_after, interval_key, "NBD3-R comparisons with uncertainty")
  write_parquet_dt(nbd3_after, nbd3_path)

  build_viz_input(config)
  cause_interval_result <- nbd_build_cause_uncertainty_intervals(
    root = root,
    uncertainty_config_path = uncertainty_config_path,
    scenario = scenario,
    relative_tolerance = relative_tolerance,
    absolute_tolerance = absolute_tolerance
  )
  # Rebuild once more after the final-cause interval file is written so the
  # same visualization run immediately loads province and population-group
  # uncertainty into viz_input_new.rds.
  build_viz_input(config)
  runtime_after <- readRDS(runtime_path)
  external_after <- data.table::as.data.table(data.table::copy(
    runtime_after$comparisons[model != "NBD3-R"]
  ))
  external_key <- c(
    "domain", "model", "geography_code", "sex_code", "year",
    "age_id", "series_id", "measure"
  )
  nbd_compare_tables_exactly(
    external_before,
    external_after,
    key = external_key,
    label = "External and historical comparison rows"
  )

  legacy_parquet_md5_after <- unname(tools::md5sum(legacy_path))
  if (!identical(legacy_parquet_md5_before, legacy_parquet_md5_after)) {
    stop("The standardised legacy comparison table changed during integration.",
         call. = FALSE)
  }
  legacy_rda_md5_after <- unname(tools::md5sum(legacy_rda_path))
  if (!identical(legacy_rda_md5_before, legacy_rda_md5_after)) {
    stop("The supplied viz.input.Rda changed during integration.", call. = FALSE)
  }

  coverage <- nbd3_after[, .(
    nbd3_rows = .N,
    interval_rows = sum(is.finite(lower) & is.finite(upper)),
    first_year = min(year),
    last_year = max(year)
  ), by = .(domain, series_id, series_label, age_id, age_label, sex_code)]
  data.table::setorder(coverage, domain, series_id, age_id, sex_code)
  coverage_path <- file.path(
    config$labels$paths$derived_dir,
    "uncertainty_coverage.csv"
  )
  data.table::fwrite(coverage, coverage_path)

  diagnostics <- data.table::fread(interval_result$paths$diagnostics)
  convergence <- if (file.exists(interval_result$paths$convergence)) {
    data.table::fread(interval_result$paths$convergence)
  } else {
    data.table::data.table()
  }
  validation <- data.table::data.table(
    check = c(
      "uncertainty interval rows created",
      "NBD3-R interval rows attached",
      "NBD3-R point estimates unchanged",
      "uncertainty points align with NBD3-R",
      "standardised legacy comparison table unchanged",
      "supplied viz.input.Rda unchanged",
      "NBD2, NBD3-Stata, THEMBISA, and GBD2023 rows unchanged",
      "final NBD3-R cause intervals created",
      "final-cause uncertainty points align with NBD3-R",
      "all completed draws valid"
    ),
    status = "PASS",
    detail = c(
      paste(format(nrow(intervals), big.mark = ","), "comparison interval rows"),
      paste(format(matched_count, big.mark = ","), "matched NBD3-R comparison rows"),
      paste(format(nrow(nbd3_after), big.mark = ","), "NBD3-R comparison rows checked"),
      paste("maximum allowed error uses relative", relative_tolerance,
            "and absolute", absolute_tolerance, "tolerances"),
      paste("MD5", legacy_parquet_md5_after),
      paste("MD5", legacy_rda_md5_after),
      paste(format(nrow(external_after), big.mark = ","), "rows checked"),
      paste(format(nrow(cause_interval_result$data), big.mark = ","),
            "cause interval rows across",
            cause_interval_result$supported_series, "exactly supported series"),
      paste("maximum absolute point difference",
            signif(cause_interval_result$point_alignment_max_abs, 6)),
      paste(format(nrow(diagnostics), big.mark = ","), "draw diagnostics rows")
    )
  )
  if ("valid" %in% names(diagnostics) && diagnostics[valid != TRUE, .N]) {
    stop("One or more uncertainty draws are marked invalid.", call. = FALSE)
  }
  validation_path <- file.path(
    config$labels$paths$derived_dir,
    "integrated_uncertainty_validation.csv"
  )
  data.table::fwrite(validation, validation_path)

  runtime_after$metadata$uncertainty <- list(
    configuration = interval_result$paths$config_path,
    output_root = interval_result$paths$output_root,
    scenario = scenario,
    interval_rows = nrow(intervals),
    attached_rows = matched_count,
    cause_interval_rows = nrow(cause_interval_result$data),
    cause_supported_series = cause_interval_result$supported_series,
    validation = validation_path,
    coverage = coverage_path,
    cause_coverage = cause_interval_result$coverage_path,
    built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  runtime_after$uncertainty <- list(
    intervals = interval_result$parquet,
    cause_intervals = cause_interval_result$parquet,
    diagnostics = interval_result$paths$diagnostics,
    convergence = interval_result$paths$convergence,
    coverage = coverage_path,
    cause_coverage = cause_interval_result$coverage_path,
    validation = validation_path
  )
  runtime_after$paths$uncertainty_intervals <- interval_result$parquet
  runtime_after$paths$cause_uncertainty <- cause_interval_result$parquet
  write_rds_atomic(runtime_after, runtime_path)

  invisible(list(
    runtime = runtime_path,
    intervals = interval_result$parquet,
    cause_intervals = cause_interval_result$parquet,
    coverage = coverage_path,
    cause_coverage = cause_interval_result$coverage_path,
    validation = validation_path,
    matched_rows = matched_count,
    cause_interval_rows = nrow(cause_interval_result$data),
    convergence = convergence
  ))
}
