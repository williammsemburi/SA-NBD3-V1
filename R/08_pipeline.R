# ==============================================================================
# 08_pipeline: Stage wrappers and comparison helpers
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Disk-oriented Stage 01-08 wrappers
# ------------------------------------------------------------------------------

# Disk-oriented stage wrappers -------------------------------------------------
#
# These functions are intentionally thin. Statistical and data-management logic
# lives in focused modules under R/, while each stage reads one canonical input,
# writes one canonical output and records diagnostics alongside it.

required_input_manifest <- function(cfg) {
  require_package("data.table")

  # `key` is a formal argument of data.table::data.table(), so creating a
  # column called `key` inside that constructor is interpreted as a request to
  # key the table on the supplied file names. Build it under a neutral name and
  # rename it explicitly; this is stable across data.table releases.
  manifest <- data.table::data.table(
    stage = c(
      "population", "population", "COD", "COD",
      "injury", "injury", "injury",
      "completeness", "completeness", "completeness", "completeness",
      "HIV", "YLL", "YLL", "database"
    ),
    input_key = c(
      "population_current", "population_workbook", "cod_raw", "icd_to_nbd_lookup",
      "nims_2000", "ims_2009", "famhis_2017", "completeness_child",
      "completeness_province", "npr", "investigation_parameters",
      "prevalence_raw", "analysis_to_za_lookup", "yll", "asr_factors"
    ),
    section = c(
      "raw", "raw", "raw", "lookups",
      "raw", "raw", "raw", "raw", "raw",
      "raw", "raw", "raw", "lookups", "raw", "raw"
    ),
    required = c(
      FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
      TRUE, TRUE, TRUE, TRUE
    ),
    note = c(
      "Preferred current population file; must cover all configured years.",
      "Historical fallback covering 1997 through population_workbook_end_year.",
      "Stats SA cause-of-death microdata.",
      "Bundled static rules generated from NBD215grProgram.do; age rules are applied in R.",
      "NIMS 2000 national injury survey; expanded spatially with IMS 2009 fractions.",
      "IMS 2009 injury survey and middle composition anchor.",
      "FAMHIS 2017 injury survey and final composition anchor.",
      "Child completeness multipliers.",
      "Province/sex/age adjusted death totals.",
      "National Population Register deaths.",
      "Optional columns appended to the national investigation dataset.",
      "Province ANC prevalence source; combined with Stage 01 population in R.",
      "Bundled many-to-many hierarchy generated from Analysis to ZA codes.do.",
      "Remaining life expectancy by age and sex.",
      "Age-standardisation weights."
    )
  )
  data.table::setnames(manifest, "input_key", "key")

  manifest[]
}

resolve_manifest_path <- function(cfg, section, key) {
  if (section == "raw") raw_file(cfg, key) else lookup_file(cfg, key)
}

check_lookup_ready <- function(path, required_columns) {
  tryCatch({
    table <- read_tabular(path)
    missing_columns <- setdiff(required_columns, names(table))
    if (length(missing_columns)) {
      return(list(
        ready = FALSE,
        issue = paste0("missing column(s): ", paste(missing_columns, collapse = ", "))
      ))
    }
    if (!nrow(table)) {
      return(list(ready = FALSE, issue = "template contains no mapping rows"))
    }
    list(ready = TRUE, issue = "")
  }, error = function(error) {
    list(ready = FALSE, issue = conditionMessage(error))
  })
}

check_required_inputs <- function(cfg, write_report = TRUE) {
  require_package("data.table")
  # Validate method settings before scanning production files so obsolete or
  # mistyped options fail during the input check rather than a long production run.
  invisible(injury_model_parameters(cfg))
  aggregation_backend <- cod_aggregation_backend(cfg)
  if (aggregation_backend == "collapse") require_package("collapse")
  manifest <- required_input_manifest(cfg)
  manifest[, path := mapply(
    function(section, key) resolve_manifest_path(cfg, section, key),
    section, key,
    USE.NAMES = FALSE
  )]
  manifest[, `:=`(
    exists = file.exists(path),
    ready = file.exists(path),
    issue = data.table::fifelse(file.exists(path), "", "file not found")
  )]

  lookup_contracts <- list(
    icd_to_nbd_lookup = c("icd10", "nbdcode"),
    analysis_to_za_lookup = c("analysis_code", "za_code", "weight", "hierarchy")
  )
  for (lookup_key in names(lookup_contracts)) {
    row <- which(manifest$key == lookup_key & manifest$exists)
    if (!length(row)) next
    result <- check_lookup_ready(
      manifest$path[[row[[1L]]]],
      lookup_contracts[[lookup_key]]
    )
    manifest[row, `:=`(ready = result$ready, issue = result$issue)]
  }

  # At least one population source is required. The full-year coverage check is
  # performed after the selected source has been read and standardised.
  population_ok <- any(
    manifest[key %in% c("population_current", "population_workbook"), ready]
  )
  incomplete <- manifest[required & !ready]
  if (!population_ok) {
    incomplete <- data.table::rbindlist(list(
      incomplete,
      manifest[key %in% c("population_current", "population_workbook")][1L]
    ), use.names = TRUE, fill = TRUE)
  }
  incomplete <- unique(incomplete, by = "key")

  if (isTRUE(write_report)) {
    write_csv_table(manifest, cfg, "input_status.csv")
  }
  if (nrow(incomplete)) {
    stop(
      "Required pipeline inputs are missing or incomplete:\n",
      paste0(
        "- ", incomplete$key, ": ", incomplete$path,
        " [", incomplete$issue, "]",
        collapse = "\n"
      ),
      "\nSee docs/INPUTS.md for schemas and provenance requirements.",
      call. = FALSE
    )
  }
  manifest[]
}


manifest_files <- function(manifest, keys) {
  require_package("data.table")
  x <- data.table::as.data.table(manifest)
  assert_has_columns(x, c("key", "path", "ready"), "input manifest")
  paths <- x[key %in% keys & ready == TRUE, path]
  paths <- unique(as.character(paths[file.exists(paths)]))
  if (!length(paths)) {
    stop(
      "No ready input files were found for key(s): ",
      paste(keys, collapse = ", "),
      call. = FALSE
    )
  }
  normalizePath(paths, winslash = "/", mustWork = TRUE)
}

stage_prepare_population <- function(cfg) {
  population <- prepare_population(cfg)
  write_stage_data(population, cfg, "01_population", stata_export = TRUE)
}

stage_group_cod_records <- function(cfg) {
  backend <- cod_aggregation_backend(cfg)
  threads <- as.integer(cfg$settings$n_threads_resolved %||% 1L)
  cod_progress(
    1, 5,
    paste0(
      "Read selected microdata columns, run record-level ICD verification, ",
      "then collapse to the maximum verified key using ",
      backend, " (", threads, " thread", if (threads == 1L) "" else "s", ")."
    )
  )

  source_path <- raw_file(cfg, "cod_raw", must_exist = TRUE)
  select_columns <- isTRUE(cfg$settings$cod_read_selected_columns %||% TRUE)
  raw <- run_timed_step(
    "  Reading COD microdata",
    read_cod_microdata(source_path, select_columns = select_columns)
  )
  message(
    "  Read ", format_count(nrow(raw)), " unit records and ",
    format_count(ncol(raw)), " required source columns."
  )

  lookup <- read_icd_to_nbd_lookup(cfg)
  verified <- run_timed_step(
    "  Applying ICD verification and final NBD classification",
    apply_icd10_verification(raw, lookup, copy = FALSE, progress = TRUE)
  )
  check_unmapped_live_births(verified, cfg)
  grouped <- run_timed_step(
    "  Collapsing verified records to the exact maximum COD key",
    collapse_verified_cod(verified, cfg, copy = FALSE)
  )
  message(
    "  ", format_count(nrow(verified)), " verified records -> ",
    format_count(nrow(grouped)), " grouped rows; represented deaths = ",
    format_count(sum(grouped$num)), "."
  )
  flush.console()

  # `raw` and `verified` refer to the same deliberately mutated data.table.
  # Release both bindings before serialising the grouped checkpoint so the
  # record-level columns are eligible for collection during the Parquet write.
  rm(raw, verified)
  invisible(gc(verbose = FALSE))

  path <- run_timed_step(
    "  Writing 02a grouped checkpoint",
    write_stage_data(
      grouped, cfg, "02a_cod_verified_groups", stata_export = FALSE
    )
  )
  cod_progress(1, 5, paste0("Checkpoint written: ", path))
  path
}

stage_prepare_cod_annual <- function(cfg, verified_group_path) {
  cod_progress(
    2, 5,
    paste0(
      "Apply exclusions and age grouping, calculate the targeted 2004 ",
      "correction, and collapse directly to annual cells."
    )
  )
  grouped <- read_tabular(verified_group_path)
  prepared <- run_timed_step(
    "  Filtering and deriving the analysis age groups",
    prepare_cod_analysis_groups(grouped)
  )
  message(
    "  ", format_count(nrow(grouped)), " verified groups -> ",
    format_count(nrow(prepared)), " retained prepared groups."
  )
  annual <- run_timed_step(
    "  Aggregating prepared groups directly to annual COD cells",
    aggregate_cod_annual_groups(prepared, cfg)
  )
  interpolation <- attr(annual, "interpolation_diagnostics")
  message(
    "  2004 ill-defined age 1-4 total: observed ",
    format_count(interpolation$observed_2004), ", corrected ",
    format_count(interpolation$corrected_2004), " across ",
    format_count(interpolation$corrected_cells), " non-zero cells."
  )
  message(
    "  ", format_count(nrow(prepared)), " prepared groups -> ",
    format_count(nrow(annual)), " annual pre-redistribution cells."
  )
  flush.console()

  rm(grouped, prepared)
  invisible(gc(verbose = FALSE))
  path <- run_timed_step(
    "  Writing 02b annual checkpoint",
    write_stage_data(
      annual, cfg, "02b_cod_annual_pre_redistribution", stata_export = FALSE
    )
  )
  cod_progress(2, 5, paste0("Checkpoint written: ", path))
  path
}

stage_redistribute_cod_sex <- function(cfg, annual_path) {
  cod_progress(3, 5, "Redistribute unknown sex on a compact three-column wide table.")
  annual <- read_tabular(annual_path)
  redistributed <- run_timed_step(
    "  Redistributing unknown sex",
    redistribute_cod_sex(annual, cfg)
  )
  message(
    "  ", format_count(nrow(annual)), " input cells -> ",
    format_count(nrow(redistributed)), " non-zero sex-specific cells."
  )
  path <- run_timed_step(
    "  Writing 02c sex checkpoint",
    write_stage_data(
      redistributed, cfg, "02c_cod_after_sex", stata_export = FALSE
    )
  )
  cod_progress(3, 5, paste0("Checkpoint written: ", path))
  path
}

stage_redistribute_cod_age <- function(cfg, sex_path) {
  cod_progress(4, 5, "Redistribute unknown age on a compact 21-column age matrix.")
  sex <- read_tabular(sex_path)
  redistributed <- run_timed_step(
    "  Redistributing unknown age",
    redistribute_cod_age(sex, cfg)
  )
  message(
    "  ", format_count(nrow(sex)), " sex-specific cells -> ",
    format_count(nrow(redistributed)), " non-zero age-specific cells."
  )
  path <- run_timed_step(
    "  Writing 02d age checkpoint",
    write_stage_data(
      redistributed, cfg, "02d_cod_after_age", stata_export = FALSE
    )
  )
  cod_progress(4, 5, paste0("Checkpoint written: ", path))
  path
}

stage_redistribute_cod_population <- function(cfg, age_path) {
  cod_progress(
    5, 5,
    "Redistribute unknown population group using current-year shares and the 1999-2000 reference for 1997-1998."
  )
  age <- read_tabular(age_path)
  population <- run_timed_step(
    "  Redistributing unknown population group",
    redistribute_cod_population_group(age, cfg)
  )
  clean <- finalize_clean_cod(population, copy = FALSE)
  message(
    "  ", format_count(nrow(age)), " age-specific cells -> ",
    format_count(nrow(clean)), " final non-zero COD cells."
  )
  path <- run_timed_step(
    "  Writing final Stage 02 checkpoint",
    write_stage_data(clean, cfg, "02_cod_clean", stata_export = TRUE)
  )
  cod_progress(5, 5, paste0("Stage 02 complete: ", path))
  path
}

# Convenience entry point for non-targets use. Production runs should use the
# separate file targets above so a failure in redistribution never repeats the
# record-level verification and first collapse.
stage_clean_cod <- function(cfg) {
  verified <- stage_group_cod_records(cfg)
  annual <- stage_prepare_cod_annual(cfg, verified)
  sex <- stage_redistribute_cod_sex(cfg, annual)
  age <- stage_redistribute_cod_age(cfg, sex)
  stage_redistribute_cod_population(cfg, age)
}

stage_prepare_injury_surveys <- function(cfg) {
  surveys <- prepare_injury_surveys(cfg)
  write_stage_data(surveys, cfg, "03_injury_surveys", stata_export = TRUE)
}

stage_estimate_injuries <- function(cfg, cod_path, survey_path) {
  cod <- read_tabular(cod_path)
  surveys <- read_tabular(survey_path)
  injury_cod <- split_cod_by_injury(cod)$injury
  model <- fit_injury_fraction_model(surveys, cfg)
  fractions <- model$fractions
  final <- apply_injury_fractions(injury_cod, fractions)
  diagnostics <- injury_model_diagnostics(
    surveys,
    fractions,
    cfg,
    fitted_model = model
  )

  source_fraction_path <- write_stage_data(
    model$source_fractions,
    cfg,
    "03_injury_source_fractions",
    stata_export = FALSE
  )
  logit_trend_path <- write_stage_data(
    model$logit_trends,
    cfg,
    "03_injury_logit_trends",
    stata_export = FALSE
  )
  fraction_path <- write_stage_data(
    fractions,
    cfg,
    "03_injury_fractions",
    stata_export = FALSE
  )
  diagnostic_path <- write_csv_table(
    diagnostics,
    cfg,
    "03_injury_model_diagnostics.csv"
  )
  survey_comparison_path <- write_stage_data(
    model$survey_model_comparison,
    cfg,
    "03_injury_survey_model_comparison",
    stata_export = FALSE
  )

  model_artifact_path <- derived_file(cfg, "03_injury_fraction_model.rds")
  dir.create(dirname(model_artifact_path), recursive = TRUE, showWarnings = FALSE)
  temporary_artifact <- paste0(model_artifact_path, ".tmp-", Sys.getpid())
  saveRDS(model$model_artifact, temporary_artifact, version = 3)
  if (file.exists(model_artifact_path)) unlink(model_artifact_path)
  if (!file.rename(temporary_artifact, model_artifact_path)) {
    stop("Could not finalise injury interpolation artifact: ", model_artifact_path,
         call. = FALSE)
  }

  final_path <- write_stage_data(
    final,
    cfg,
    "03_final_injuries",
    stata_export = TRUE
  )

  # Return every canonical Stage 03 product so {targets} monitors the audit
  # files as well as the downstream mortality output. Deleting or corrupting an
  # audit checkpoint therefore invalidates the target and triggers a rebuild.
  c(
    source_fractions = source_fraction_path,
    logit_trends = logit_trend_path,
    fractions = fraction_path,
    diagnostics = diagnostic_path,
    survey_model_comparison = survey_comparison_path,
    fraction_model = model_artifact_path,
    final_injuries = final_path
  )
}

stage_adjust_completeness <- function(cfg, cod_path, injury_path) {
  cod <- read_tabular(cod_path)
  natural <- split_cod_by_injury(cod)$natural
  injury <- read_tabular(injury_path)
  adjusted <- read_adjusted_deaths_by_province(cfg)
  scales <- derive_completeness_scalars(natural, injury, adjusted, cfg)
  complete <- apply_completeness_scalars(natural, injury, scales, cfg)
  npr <- prepare_npr(cfg)
  subpopulation <- apply_npr_adjustment(complete, npr, cfg)
  national <- build_national_investigation(subpopulation, cfg)

  write_stage_data(scales, cfg, "04_completeness_scalars", stata_export = TRUE)
  write_stage_data(national, cfg, "04_investigation_national", stata_export = TRUE)
  write_stage_data(subpopulation, cfg, "04_investigation_subpopulation", stata_export = TRUE)
}

stage_prepare_prevalence_population <- function(cfg, population_path) {
  source <- read_tabular(raw_file(cfg, "prevalence_raw", must_exist = TRUE))
  population <- read_tabular(population_path)
  panel <- prepare_prevalence_population(source, population, cfg)
  write_stage_data(panel, cfg, "05_prevalence_population", stata_export = TRUE)
}

stage_reallocate_hiv <- function(cfg, subpopulation_path, prevalence_path) {
  subpopulation <- read_tabular(subpopulation_path)
  prevalence <- read_prevalence_population(prevalence_path, cfg)
  result <- run_hiv_reallocation(subpopulation, prevalence, cfg)

  paths <- c(
    write_stage_data(
      result$data, cfg, "05_hiv_reallocated_long", stata_export = TRUE
    ),
    write_stage_data(
      result$trends, cfg, "05_hiv_background_trends", stata_export = FALSE
    ),
    write_stage_data(
      result$intercepts, cfg, "05_hiv_zero_prevalence_intercepts", stata_export = FALSE
    ),
    write_stage_data(
      result$misclassified, cfg, "05_hiv_misclassified_diagnostics", stata_export = FALSE
    ),
    write_stage_data(
      result$wide, cfg, "05_hiv_reallocated_wide", stata_export = TRUE
    )
  )

  covariance_path <- derived_file(cfg, "05_hiv_model_covariance.rds")
  dir.create(dirname(covariance_path), recursive = TRUE, showWarnings = FALSE)
  temporary_path <- paste0(covariance_path, ".tmp-", Sys.getpid())
  saveRDS(result$covariance_artifact, temporary_path, version = 3)
  if (file.exists(covariance_path)) unlink(covariance_path)
  if (!file.rename(temporary_path, covariance_path)) {
    stop("Could not finalise HIV covariance artifact: ", covariance_path,
         call. = FALSE)
  }

  c(paths, covariance_path)
}

stage_redistribute_garbage <- function(cfg, hiv_wide_path) {
  result <- run_garbage_redistribution(read_tabular(hiv_wide_path), cfg)
  write_stage_data(result$wide, cfg, "06_redistributed_analysis_wide", stata_export = TRUE)
  write_stage_data(result$long, cfg, "06_redistributed_analysis_long", stata_export = TRUE)
}

stage_calculate_yll <- function(cfg, redistributed_path) {
  redistributed <- read_tabular(redistributed_path)
  za_lookup <- read_analysis_to_za_lookup(cfg)
  schedule <- read_yll_schedule(cfg)
  result <- calculate_yll_outputs(
    redistributed,
    za_lookup,
    schedule,
    strict = isTRUE(cfg$settings$strict_checks)
  )
  write_stage_data(result$national, cfg, "07_deaths_yll_national", stata_export = TRUE)
  write_stage_data(result$all, cfg, "07_deaths_yll_all", stata_export = TRUE)
}

stage_build_database <- function(cfg, deaths_yll_path, population_path) {
  data <- read_tabular(deaths_yll_path)
  population <- read_tabular(population_path)
  factors <- read_asr_factors(cfg)
  database <- build_final_database(data, population, factors)
  stem <- paste0(
    "NBD_database_",
    as.integer(cfg$settings$start_year),
    "_",
    as.integer(cfg$settings$end_year)
  )
  parquet <- write_tabular(database, database_file(cfg, paste0(stem, ".parquet")))
  write_tabular(database, database_file(cfg, paste0(stem, ".csv")))
  if (isTRUE(cfg$settings$write_stata_exports)) {
    write_tabular(database, database_file(cfg, paste0(stem, ".dta")))
  }
  parquet
}

# ------------------------------------------------------------------------------
# Checkpoint comparison helpers
# ------------------------------------------------------------------------------

# Stata-to-R checkpoint comparison ---------------------------------------------

#' Compare equivalent Stata and R pipeline checkpoints
#'
#' Performs a full outer join on a declared key, reports whether each key is
#' present in both datasets, and calculates absolute and relative differences
#' for every requested measure. Presence flags are stored before the merge so a
#' row whose measures are all missing is not mistaken for a missing key.
#'
#' @param stata Data frame or data.table exported from the Stata pipeline.
#' @param r_output Equivalent output produced by the R pipeline.
#' @param keys Character vector defining the unique row key.
#' @param measures Numeric columns to compare.
#' @param absolute_tolerance Maximum acceptable absolute difference.
#' @param relative_tolerance Maximum acceptable relative difference.
#'
#' @return A data.table containing both versions, differences, match flags, and
#'   a `key_status` column (`both`, `stata_only`, or `r_only`).
compare_checkpoint_outputs <- function(
    stata,
    r_output,
    keys,
    measures,
    absolute_tolerance = 1e-8,
    relative_tolerance = 1e-7) {
  require_package("data.table")

  if (!length(keys)) stop("At least one key column is required.", call. = FALSE)
  if (!length(measures)) stop("At least one measure column is required.", call. = FALSE)
  if (length(intersect(keys, measures))) {
    stop("Key and measure columns must be distinct.", call. = FALSE)
  }
  if (!is.numeric(absolute_tolerance) || length(absolute_tolerance) != 1L ||
      is.na(absolute_tolerance) || absolute_tolerance < 0) {
    stop("`absolute_tolerance` must be one non-negative number.", call. = FALSE)
  }
  if (!is.numeric(relative_tolerance) || length(relative_tolerance) != 1L ||
      is.na(relative_tolerance) || relative_tolerance < 0) {
    stop("`relative_tolerance` must be one non-negative number.", call. = FALSE)
  }

  left <- data.table::as.data.table(data.table::copy(stata))
  right <- data.table::as.data.table(data.table::copy(r_output))
  assert_has_columns(left, c(keys, measures), "Stata checkpoint")
  assert_has_columns(right, c(keys, measures), "R checkpoint")
  assert_unique_key(left, keys, "Stata checkpoint")
  assert_unique_key(right, keys, "R checkpoint")

  non_numeric_left <- measures[!vapply(left[, ..measures], is.numeric, logical(1))]
  non_numeric_right <- measures[!vapply(right[, ..measures], is.numeric, logical(1))]
  if (length(non_numeric_left) || length(non_numeric_right)) {
    bad <- unique(c(non_numeric_left, non_numeric_right))
    stop(
      "Checkpoint measures must be numeric: ", paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  left <- left[, c(keys, measures), with = FALSE]
  right <- right[, c(keys, measures), with = FALSE]
  left[, .stata_present := TRUE]
  right[, .r_present := TRUE]

  data.table::setnames(left, measures, paste0(measures, "_stata"))
  data.table::setnames(right, measures, paste0(measures, "_r"))
  out <- merge(left, right, by = keys, all = TRUE, sort = FALSE)

  out[, key_status := data.table::fcase(
    !is.na(.stata_present) & !is.na(.r_present), "both",
    !is.na(.stata_present), "stata_only",
    !is.na(.r_present), "r_only",
    default = "neither"
  )]
  out[, c(".stata_present", ".r_present") := NULL]

  for (measure in measures) {
    stata_col <- paste0(measure, "_stata")
    r_col <- paste0(measure, "_r")
    absolute_col <- paste0(measure, "_abs_diff")
    relative_col <- paste0(measure, "_rel_diff")
    match_col <- paste0(measure, "_match")

    out[, (absolute_col) := abs(get(r_col) - get(stata_col))]

    # Scale relative differences by the larger magnitude. The absolute
    # tolerance is the denominator floor, which keeps comparisons around zero
    # stable and interpretable.
    out[, (relative_col) := get(absolute_col) / pmax(
      absolute_tolerance,
      abs(get(stata_col)),
      abs(get(r_col)),
      na.rm = TRUE
    )]

    # Two missing values are considered equal when the key exists in both
    # files. A missing value on only one side remains a mismatch.
    both_missing <- is.na(out[[stata_col]]) & is.na(out[[r_col]])
    within_tolerance <-
      !is.na(out[[absolute_col]]) &
      (out[[absolute_col]] <= absolute_tolerance |
         out[[relative_col]] <= relative_tolerance)
    out[, (match_col) := key_status == "both" & (both_missing | within_tolerance)]
  }

  match_columns <- paste0(measures, "_match")
  out[, all_measures_match :=
        key_status == "both" & rowSums(as.matrix(.SD), na.rm = TRUE) == length(match_columns),
      .SDcols = match_columns]

  data.table::setorderv(out, keys)
  out[]
}

#' Summarise a checkpoint comparison
#'
#' @param comparison Output from [compare_checkpoint_outputs()].
#' @return One-row data.table with key and measure agreement counts.
summarise_checkpoint_comparison <- function(comparison) {
  require_package("data.table")
  x <- data.table::as.data.table(comparison)
  assert_has_columns(x, c("key_status", "all_measures_match"), "checkpoint comparison")

  x[, .(
    rows = .N,
    keys_in_both = sum(key_status == "both"),
    keys_stata_only = sum(key_status == "stata_only"),
    keys_r_only = sum(key_status == "r_only"),
    rows_matching_all_measures = sum(all_measures_match, na.rm = TRUE),
    rows_failing_any_measure = sum(key_status == "both" & !all_measures_match, na.rm = TRUE)
  )]
}
