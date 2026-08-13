# Full development validation --------------------------------------------------
#
# This extended validation suite is retained for release review and debugging.
# It is not part of the normal collaborator-facing run. Execute with:
#   source("dev/full_validation.R")
#
# The script validates every stage output currently present and skips stages
# that have not yet been run. Set NBD3_HEAVY_CHECKS=true before sourcing to add
# the 02a/2004 reconstruction check. By default, any failed check stops the run
# after the complete validation report has been written.

locate_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "R", "00_core.R"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Run this script from inside the NBD3 project folder.", call. = FALSE)
    }
    current <- parent
  }
}

root <- locate_root()
setwd(root)
module_files <- sort(list.files(
  file.path(root, "R"),
  pattern = "^[0-9]{2}_.*\\.R$",
  full.names = TRUE
))
invisible(lapply(module_files, sys.source, envir = .GlobalEnv))
cfg <- read_project_config(root)
require_package("data.table")

RUN_HEAVY_CHECKS <- identical(
  tolower(Sys.getenv("NBD3_HEAVY_CHECKS", "false")),
  "true"
)
STOP_ON_FAILURE <- !identical(
  tolower(Sys.getenv("NBD3_STOP_ON_VALIDATION_FAILURE", "true")),
  "false"
)

validation_results <- data.table::data.table(
  stage = character(),
  check = character(),
  status = character(),
  detail = character()
)

add_result <- function(stage, check, status, detail = "") {
  row <- data.table::data.table(
    stage = as.character(stage),
    check = as.character(check),
    status = as.character(status),
    detail = as.character(detail)
  )
  validation_results <<- data.table::rbindlist(
    list(validation_results, row),
    use.names = TRUE
  )
  symbol <- switch(status, PASS = "[PASS]", FAIL = "[FAIL]", SKIP = "[SKIP]", "[INFO]")
  message(symbol, " ", stage, " - ", check, if (nzchar(detail)) paste0(": ", detail) else "")
  invisible(status)
}

run_check <- function(stage, check, fun) {
  tryCatch({
    detail <- fun()
    if (is.null(detail) || !length(detail)) detail <- ""
    add_result(stage, check, "PASS", paste(detail, collapse = "; "))
  }, error = function(e) {
    add_result(stage, check, "FAIL", conditionMessage(e))
  })
}

expect_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

format_number <- function(x, digits = 12L) {
  format(signif(as.numeric(x), digits), scientific = FALSE, trim = TRUE, big.mark = ",")
}

stage_path <- function(stem) derived_file(cfg, paste0(stem, ".parquet"))

load_stage <- function(stage, stem) {
  path <- stage_path(stem)
  if (!file.exists(path)) {
    add_result(stage, paste0("file ", basename(path)), "SKIP", "not yet created")
    return(NULL)
  }
  tryCatch({
    x <- read_tabular(path)
    size_mb <- file.info(path)$size / 1024^2
    add_result(
      stage,
      paste0("file ", basename(path)),
      "PASS",
      paste0(format_number(nrow(x)), " rows; ", sprintf("%.2f MB", size_mb))
    )
    x
  }, error = function(e) {
    add_result(stage, paste0("file ", basename(path)), "FAIL", conditionMessage(e))
    NULL
  })
}

check_columns <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  expect_true(!length(missing), paste0(label, " is missing: ", paste(missing, collapse = ", ")))
  paste0("required columns present (", length(columns), ")")
}

check_unique <- function(x, key, label) {
  assert_unique_key(x, key, label)
  paste0("unique on ", paste(key, collapse = " + "))
}

check_finite_nonnegative <- function(x, columns, label, tolerance = 1e-10) {
  for (column in columns) {
    bad_nonfinite <- x[!is.finite(get(column)), .N]
    bad_negative <- x[is.finite(get(column)) & get(column) < -tolerance, .N]
    expect_true(
      bad_nonfinite == 0L && bad_negative == 0L,
      paste0(
        label, " has ", bad_nonfinite, " non-finite and ", bad_negative,
        " negative value(s) in ", column
      )
    )
  }
  paste0("finite and non-negative: ", paste(columns, collapse = ", "))
}

check_allowed <- function(x, column, allowed, label) {
  observed <- sort(unique(x[[column]][!is.na(x[[column]])]))
  unexpected <- setdiff(observed, allowed)
  missing_n <- sum(is.na(x[[column]]))
  expect_true(
    !length(unexpected) && missing_n == 0L,
    paste0(
      label, " has unexpected ", column, " values: ",
      paste(c(unexpected, if (missing_n) "NA"), collapse = ", ")
    )
  )
  paste0(column, " levels valid")
}

compare_group_totals <- function(
    before,
    after,
    by_columns,
    before_value,
    after_value,
    label,
    tolerance = 1e-8) {
  b <- before[, .(before_total = sum(get(before_value), na.rm = TRUE)), by = by_columns]
  a <- after[, .(after_total = sum(get(after_value), na.rm = TRUE)), by = by_columns]
  z <- merge(b, a, by = by_columns, all = TRUE, sort = FALSE)
  z[is.na(before_total), before_total := 0]
  z[is.na(after_total), after_total := 0]
  z[, `:=`(
    absolute_difference = abs(before_total - after_total),
    scale = pmax(1, abs(before_total), abs(after_total))
  )]
  bad <- z[absolute_difference > tolerance * scale]
  max_difference <- if (nrow(z)) max(z$absolute_difference) else 0
  expect_true(
    !nrow(bad),
    paste0(
      label, " failed in ", nrow(bad), " group(s); maximum absolute difference = ",
      format_number(max_difference)
    )
  )
  paste0(
    format_number(nrow(z)), " groups; max absolute difference ",
    format_number(max_difference)
  )
}

compare_measure_tables <- function(
    expected,
    observed,
    key,
    measures,
    label,
    tolerance = 1e-8) {
  expected <- data.table::copy(expected)
  observed <- data.table::copy(observed)
  assert_unique_key(expected, key, paste0(label, " expected"))
  assert_unique_key(observed, key, paste0(label, " observed"))
  data.table::setnames(expected, measures, paste0(measures, "_expected"))
  data.table::setnames(observed, measures, paste0(measures, "_observed"))
  z <- merge(expected, observed, by = key, all = TRUE, sort = FALSE)

  worst <- 0
  bad_total <- 0L
  for (measure in measures) {
    e <- paste0(measure, "_expected")
    o <- paste0(measure, "_observed")
    z[is.na(get(e)), (e) := 0]
    z[is.na(get(o)), (o) := 0]
    difference <- abs(z[[e]] - z[[o]])
    scale <- pmax(1, abs(z[[e]]), abs(z[[o]]))
    bad_total <- bad_total + sum(difference > tolerance * scale)
    if (length(difference)) worst <- max(worst, difference)
  }
  expect_true(
    bad_total == 0L,
    paste0(
      label, " has ", bad_total,
      " measure mismatch(es); maximum absolute difference = ", format_number(worst)
    )
  )
  paste0(format_number(nrow(z)), " keys; max absolute difference ", format_number(worst))
}

# {targets} metadata -----------------------------------------------------------
if (requireNamespace("targets", quietly = TRUE) && dir.exists(file.path(root, "_targets"))) {
  run_check("targets", "completed target warnings and errors", function() {
    meta <- targets::tar_meta(
      fields = c(name, warnings, error, seconds),
      complete_only = TRUE
    )
    problems <- meta[!is.na(meta$warnings) | !is.na(meta$error), , drop = FALSE]
    if (nrow(problems)) {
      out <- table_file(cfg, "validation_target_messages.csv")
      data.table::fwrite(data.table::as.data.table(problems), out)
      warning_text <- paste0(
        nrow(problems), " target(s) have warning/error metadata; see ", out
      )
      # A warning is not automatically a failed numerical validation. Report it
      # as the check detail; an actual target error fails the check.
      if (any(!is.na(problems$error))) stop(warning_text, call. = FALSE)
      return(warning_text)
    }
    "no stored target warnings or errors"
  })
}

# Stage 01: population ---------------------------------------------------------
population <- load_stage("01", "01_population")
if (!is.null(population)) {
  pop_key <- c("Popgroup", "Sex", "DeathYear", "age5", "Death_Prov")
  run_check("01", "population schema", function() {
    check_columns(population, c(pop_key, "Pop"), "population")
  })
  run_check("01", "population key", function() check_unique(population, pop_key, "population"))
  run_check("01", "population values", function() {
    check_finite_nonnegative(population, "Pop", "population")
  })
  run_check("01", "population dimensions", function() {
    check_allowed(population, "Death_Prov", 1:9, "population")
    check_allowed(population, "Sex", 1:2, "population")
    check_allowed(population, "Popgroup", 1:4, "population")
    check_allowed(population, "age5", 2:20, "population")
    expected_years <- seq.int(cfg$settings$start_year, cfg$settings$end_year)
    observed_years <- sort(unique(as.integer(population$DeathYear)))
    missing_years <- setdiff(expected_years, observed_years)
    extra_years <- setdiff(observed_years, expected_years)
    expect_true(
      !length(missing_years),
      paste0("population is missing configured year(s): ", paste(missing_years, collapse = ", "))
    )
    expect_true(
      !length(extra_years),
      paste0("population output contains year(s) outside the configured range: ", paste(extra_years, collapse = ", "))
    )
    expected_rows <- length(expected_years) * 9L * 2L * 4L * 19L
    expect_true(
      nrow(population) == expected_rows,
      paste0("population has ", nrow(population), " rows; expected ", expected_rows)
    )
    paste0(
      "years ", min(expected_years), "-", max(expected_years),
      "; ages 2-20; ", format_number(expected_rows), " complete cells"
    )
  })
}

# Stage 02: COD cleaning and redistribution -----------------------------------
cod_02b <- load_stage("02", "02b_cod_annual_pre_redistribution")
cod_02c <- load_stage("02", "02c_cod_after_sex")
cod_02d <- load_stage("02", "02d_cod_after_age")
cod_final <- load_stage("02", "02_cod_clean")

annual_key <- c("Death_Prov", "Sex", "DeathYear", "nbdcode", "age5", "Popgroup")
if (!is.null(cod_02b)) {
  run_check("02", "02b annual key and values", function() {
    check_unique(cod_02b, annual_key, "02b annual COD")
    check_finite_nonnegative(cod_02b, "num", "02b annual COD")
  })
}
if (!is.null(cod_02c)) {
  run_check("02", "02c sex key and levels", function() {
    check_unique(cod_02c, annual_key, "02c COD after sex")
    check_finite_nonnegative(cod_02c, "num", "02c COD after sex")
    check_allowed(cod_02c, "Sex", 1:2, "02c COD after sex")
  })
}
if (!is.null(cod_02d)) {
  run_check("02", "02d age key and levels", function() {
    check_unique(cod_02d, annual_key, "02d COD after age")
    check_finite_nonnegative(cod_02d, "num", "02d COD after age")
    check_allowed(cod_02d, "age5", 1:20, "02d COD after age")
  })
}
if (!is.null(cod_final)) {
  run_check("02", "final COD key and values", function() {
    check_columns(cod_final, c(annual_key, "Deaths"), "final COD")
    check_unique(cod_final, annual_key, "final COD")
    check_finite_nonnegative(cod_final, "Deaths", "final COD")
    check_allowed(cod_final, "Death_Prov", 1:9, "final COD")
    check_allowed(cod_final, "Sex", 1:2, "final COD")
    check_allowed(cod_final, "Popgroup", 1:4, "final COD")
    check_allowed(cod_final, "age5", 1:20, "final COD")
    "unique, finite, non-negative, and no unknown demographic categories"
  })
}
if (!is.null(cod_02b) && !is.null(cod_02c)) {
  run_check("02", "unknown-sex conservation", function() {
    compare_group_totals(
      cod_02b, cod_02c,
      c("Death_Prov", "DeathYear", "nbdcode", "age5", "Popgroup"),
      "num", "num", "unknown-sex redistribution"
    )
  })
}
if (!is.null(cod_02c) && !is.null(cod_02d)) {
  run_check("02", "unknown-age conservation", function() {
    compare_group_totals(
      cod_02c, cod_02d,
      c("Death_Prov", "Sex", "DeathYear", "nbdcode", "Popgroup"),
      "num", "num", "unknown-age redistribution"
    )
  })
}
if (!is.null(cod_02d) && !is.null(cod_final)) {
  run_check("02", "unknown-population-group conservation", function() {
    compare_group_totals(
      cod_02d, cod_final,
      c("Death_Prov", "Sex", "DeathYear", "nbdcode", "age5"),
      "num", "Deaths", "unknown-population-group redistribution"
    )
  })
}

if (RUN_HEAVY_CHECKS) {
  cod_02a <- load_stage("02-heavy", "02a_cod_verified_groups")
  if (!is.null(cod_02a)) {
    verified_key <- c(
      "DeathType", "Death_Prov", "Sex", "DeathYear", "DeathMonth",
      "nbdcode", "age_", "age_u5", "Popgroup"
    )
    run_check("02-heavy", "02a exact maximum key", function() {
      check_unique(cod_02a, verified_key, "02a verified grouped COD")
      check_finite_nonnegative(cod_02a, "num", "02a verified grouped COD")
    })
    if (!is.null(cod_02b)) {
      run_check("02-heavy", "2004 cause-143 age-3 reconstruction", function() {
        prepared <- prepare_cod_analysis_groups(cod_02a)
        correction <- cod_2004_ill_defined_correction(prepared, cfg)
        observed <- cod_02b[
          DeathYear == 2004L & nbdcode == 143L & age5 == 3L,
          .(num = sum(num)),
          by = c("Death_Prov", "Sex", "DeathYear", "nbdcode", "age5", "Popgroup")
        ]
        compare_measure_tables(
          correction,
          observed,
          c("Death_Prov", "Sex", "DeathYear", "nbdcode", "age5", "Popgroup"),
          "num",
          "2004 interpolation"
        )
      })
    }
  }
} else {
  add_result(
    "02-heavy",
    "02a key and 2004 reconstruction",
    "SKIP",
    "set Sys.setenv(NBD3_HEAVY_CHECKS='true') before sourcing to run"
  )
}

# Stage 03: injury fractions and injury envelope -------------------------------
injury_surveys <- load_stage("03", "03_injury_surveys")
source_fractions <- load_stage("03", "03_injury_source_fractions")
logit_trends <- load_stage("03", "03_injury_logit_trends")
injury_fractions <- load_stage("03", "03_injury_fractions")
survey_model_comparison <- load_stage("03", "03_injury_survey_model_comparison")
final_injuries <- load_stage("03", "03_final_injuries")

injury_fraction_key <- c("Death_Prov", "Sex", "Popgroup", "age5", "year", "nbdcode")
if (!is.null(injury_surveys)) {
  run_check("03", "three injury survey anchors", function() {
    key <- c("Death_Prov", "Sex", "Popgroup", "age5", "nbdcode")
    check_unique(injury_surveys, key, "harmonised injury surveys")
    check_finite_nonnegative(
      injury_surveys,
      c("Inj2000", "Inj2009", "Inj2017", "EffN2000", "EffN2009", "EffN2017", "nims_spatial_share"),
      "harmonised injury surveys"
    )
    expect_true(
      injury_surveys[is.na(nims_spatial_status) | !nzchar(nims_spatial_status), .N] == 0L,
      "NIMS spatial provenance is missing"
    )
    expected_rows <- 9L * 2L * 4L * 20L * length(INJURY_CODES)
    expect_true(
      nrow(injury_surveys) == expected_rows,
      paste0("harmonised injury survey grid has ", nrow(injury_surveys),
             " rows; expected ", expected_rows)
    )
    nims_share_check <- injury_surveys[, .(
      share_sum = sum(nims_spatial_share)
    ), by = .(Sex, age5, nbdcode)]
    expect_true(
      all(abs(nims_share_check$share_sum - 1) <= 1e-10),
      "NIMS province-population-group shares do not sum to one"
    )
    "NIMS 2000, IMS 2009 and FAMHIS 2017 are present on the complete grid"
  })
}
if (!is.null(source_fractions)) {
  run_check("03", "source survey fractions and audit metadata", function() {
    check_unique(source_fractions, injury_fraction_key, "injury source fractions")
    check_finite_nonnegative(
      source_fractions,
      c("observed_count", "survey_count", "total_count", "survey_effective_n", "fraction"),
      "injury source fractions"
    )
    expect_true(
      identical(sort(unique(source_fractions$year)), c(2000L, 2009L, 2017L)),
      "injury source fractions do not contain the three configured anchors"
    )
    anchor_sums <- source_fractions[, .(fraction_sum = sum(fraction)), by = .(
      Death_Prov, Sex, Popgroup, age5, year
    )]
    max_error <- max(abs(anchor_sums$fraction_sum - 1))
    expect_true(max_error <= 1e-10, "source injury fractions do not sum to one")
    required_corrections <- source_fractions[
      (nbdcode %in% c(132L, 138L) & year == 2000L) |
        (nbdcode == 136L & year == 2017L)
    ]
    expect_true(
      nrow(required_corrections) > 0L &&
        required_corrections[is.na(correction_note) | !nzchar(correction_note), .N] == 0L,
      "source-specific correction provenance is incomplete"
    )
    paste0(
      "three anchors close; maximum closure error ", format_number(max_error),
      "; correction metadata retained for causes 132, 136 and 138"
    )
  })
}
if (!is.null(source_fractions)) {
  run_check("03", "source-specific correction formulas", function() {
    required <- c(
      "Death_Prov", "Sex", "Popgroup", "age5", "nbdcode", "year",
      "fraction_pre_correction", "fraction_corrected", "nims_source_zero",
      "correction_applied"
    )
    check_columns(source_fractions, required, "injury source correction audit")

    strata <- c("Death_Prov", "Sex", "Popgroup", "age5")
    key <- c(strata, "nbdcode")
    wide <- data.table::dcast(
      source_fractions,
      stats::as.formula(paste(paste(key, collapse = " + "), "~ year")),
      value.var = "fraction_pre_correction"
    )
    data.table::setnames(
      wide,
      c("2000", "2009", "2017"),
      c("p2000", "p2009", "p2017")
    )
    zero_status <- unique(source_fractions[, c(key, "nims_source_zero"), with = FALSE])
    check_unique(zero_status, key, "NIMS source-zero audit")
    wide <- merge(wide, zero_status, by = key, all.x = TRUE, sort = FALSE)
    wide[is.na(nims_source_zero), nims_source_zero := TRUE]

    tolerance <- 1e-14
    floor_value <- as.numeric(cfg$settings$injury_bias_correction_floor %||% 1e-5)
    wide[, `:=`(
      expected_2000 = p2000,
      expected_2009 = p2009,
      expected_2017 = p2017,
      expected_correction_2000 = FALSE,
      expected_correction_2017 = FALSE
    )]

    backward <- wide$nbdcode %in% c(132L, 138L) &
      wide$nims_source_zero & wide$p2009 > tolerance
    forward <- wide$nbdcode == 136L &
      wide$p2000 > tolerance & wide$p2009 > tolerance

    wide[backward, `:=`(
      expected_2000 = 2 * p2009 - p2017,
      expected_correction_2000 = TRUE
    )]
    wide[forward, `:=`(
      expected_2017 = 2 * p2009 - p2000,
      expected_correction_2017 = TRUE
    )]
    wide[nbdcode %in% c(132L, 138L), expected_2000 := pmax(expected_2000, floor_value)]
    wide[nbdcode == 136L, expected_2017 := pmax(expected_2017, floor_value)]

    expected <- data.table::melt(
      wide,
      id.vars = c(
        key,
        "expected_correction_2000",
        "expected_correction_2017"
      ),
      measure.vars = c("expected_2000", "expected_2009", "expected_2017"),
      variable.name = "year_field",
      value.name = "expected_fraction",
      variable.factor = FALSE
    )
    expected[, year := as.integer(sub("expected_", "", year_field))]
    expected[, year_field := NULL]
    expected[, expected_fraction := pmax(expected_fraction, 0)]
    expected[, expected_fraction := expected_fraction / sum(expected_fraction),
             by = c(strata, "year")]
    expected[, expected_correction := data.table::fifelse(
      year == 2000L,
      expected_correction_2000,
      data.table::fifelse(year == 2017L, expected_correction_2017, FALSE)
    )]

    observed <- source_fractions[, .(
      Death_Prov, Sex, Popgroup, age5, nbdcode, year,
      fraction_corrected,
      correction_applied
    )]
    audit <- merge(
      expected[, c(key, "year", "expected_fraction", "expected_correction"), with = FALSE],
      observed,
      by = c(key, "year"),
      all = TRUE,
      sort = FALSE
    )
    expect_true(
      audit[is.na(expected_fraction) | is.na(fraction_corrected), .N] == 0L,
      "correction formula audit has unmatched rows"
    )
    maximum_error <- max(abs(audit$expected_fraction - audit$fraction_corrected))
    expect_true(
      maximum_error <= 1e-12,
      paste0(
        "corrected source fractions do not reproduce the documented formulas; maximum error = ",
        format_number(maximum_error)
      )
    )
    flag_match <- !is.na(audit$expected_correction) &
      !is.na(audit$correction_applied) &
      as.logical(audit$expected_correction) == as.logical(audit$correction_applied)
    expect_true(
      all(flag_match),
      "stored correction flags do not match the documented activation conditions"
    )
    paste0(
      "causes 132, 136 and 138 reproduce the documented formulas; maximum error ",
      format_number(maximum_error)
    )
  })
}
if (!is.null(logit_trends)) {
  run_check("03", "fixed interpolation trajectory audit", function() {
    expect_true(
      "record_type" %in% names(logit_trends),
      "trajectory audit has no record_type"
    )
    survey_rows <- logit_trends[record_type == "Survey ALR observation"]
    trajectory_rows <- logit_trends[record_type == "Interpolation specification"]
    annual_rows <- logit_trends[
      record_type == "Linear ALR interpolation plus triangular moving average"
    ]
    expect_true(nrow(survey_rows) > 0L, "survey ALR audit is empty")
    expect_true(nrow(trajectory_rows) == 14L, "interpolation specification must contain 14 trajectories")
    expect_true(nrow(annual_rows) > 0L, "annual interpolation audit is empty")
    expect_true(
      survey_rows[!is.finite(log_ratio), .N] == 0L,
      "survey ALR observations contain non-finite values"
    )
    expect_true(
      annual_rows[
        !is.finite(log_ratio_linear) | !is.finite(log_ratio_smoothed),
        .N
      ] == 0L,
      "annual injury trajectories contain non-finite values"
    )
    required <- c(
      "method", "interpolation", "smoother", "tail_policy",
      "survey_years", "uncertainty", "fit_status"
    )
    check_columns(trajectory_rows, required, "fixed interpolation trajectory audit")
    expect_true(
      trajectory_rows[
        is.na(method) | !nzchar(method) |
          is.na(interpolation) | !nzchar(interpolation) |
          is.na(smoother) | !nzchar(smoother) |
          is.na(tail_policy) | !nzchar(tail_policy) |
          fit_status != "closed_form",
        .N
      ] == 0L,
      "one or more interpolation summaries are incomplete"
    )
    paste0(
      format_number(nrow(trajectory_rows)),
      " broad or within-group closed-form trajectories documented"
    )
  })
}
if (!is.null(injury_fractions)) {
  run_check("03", "modelled fraction panel", function() {
    validate_injury_fraction_panel(injury_fractions, cfg)
    expected_rows <- 9L * 2L * 4L * 20L *
      length(seq.int(cfg$settings$start_year, cfg$settings$end_year)) *
      length(INJURY_CODES)
    expect_true(
      nrow(injury_fractions) == expected_rows,
      paste0(
        "injury fraction row count is ", nrow(injury_fractions),
        "; expected ", expected_rows
      )
    )
    expected_period <- paste0(
      "Linear ALR interpolation + ",
      as.integer(cfg$settings$injury_smoothing_window %||% 5L),
      "-year triangular moving average"
    )
    expect_true(
      injury_fractions[is.na(period) | period != expected_period, .N] == 0L,
      "injury fraction panel does not identify the fixed smoothed interpolation"
    )
    paste0(
      format_number(expected_rows),
      " complete positive smoothed cause-fraction cells"
    )
  })

  run_check("03", "smooth annual injury trajectory audit", function() {
    key <- c("Death_Prov", "Sex", "Popgroup", "age5", "nbdcode")
    annual <- data.table::copy(injury_fractions)
    data.table::setorderv(annual, c(key, "year"))
    annual[, annual_change := cf_final - data.table::shift(cf_final), by = key]
    changes <- annual[!is.na(annual_change)]
    expect_true(nrow(changes) > 0L, "annual injury change audit is empty")
    expect_true(
      changes[!is.finite(annual_change), .N] == 0L,
      "annual injury changes contain non-finite values"
    )
    summary <- changes[, .(
      mean_absolute_change = mean(abs(annual_change)),
      p95_absolute_change = as.numeric(stats::quantile(
        abs(annual_change), 0.95, na.rm = TRUE
      )),
      maximum_absolute_change = max(abs(annual_change))
    ), by = year]
    data.table::fwrite(
      summary,
      table_file(cfg, "validation_03_year_to_year_fraction_changes.csv")
    )
    paste0(
      "complete smooth annual panel; year-to-year audit written; maximum ",
      "absolute fraction change ",
      format_number(max(summary$maximum_absolute_change))
    )
  })
}
if (!is.null(source_fractions) && !is.null(injury_fractions)) {
  run_check("03", "survey-to-model comparison", function() {
    comparison <- if (!is.null(survey_model_comparison)) {
      data.table::copy(survey_model_comparison)
    } else {
      injury_survey_model_comparison(source_fractions, injury_fractions)
    }
    required <- c(
      injury_fraction_key,
      "survey",
      "survey_fraction",
      "model_fraction",
      "difference",
      "absolute_difference"
    )
    check_columns(comparison, required, "survey-to-model comparison")
    check_unique(comparison, injury_fraction_key, "survey-to-model comparison")
    expect_true(
      comparison[
        !is.finite(survey_fraction) | !is.finite(model_fraction) |
          !is.finite(difference) | !is.finite(absolute_difference),
        .N
      ] == 0L,
      "survey-to-model comparison contains non-finite values"
    )
    expect_true(
      comparison[absolute_difference > 1e-12, .N] > 0L,
      "model estimates appear to be forced exactly to every survey observation"
    )
    groups <- injury_group_table(INJURY_CODES)
    comparison <- merge(
      comparison,
      groups[, .(nbdcode, broad_group)],
      by = "nbdcode",
      all.x = TRUE,
      sort = FALSE
    )
    summary <- comparison[, .(
      rows = .N,
      mean_absolute_difference = mean(absolute_difference),
      root_mean_squared_difference = sqrt(mean(difference^2)),
      p95_absolute_difference = as.numeric(stats::quantile(
        absolute_difference, 0.95, na.rm = TRUE
      )),
      maximum_absolute_difference = max(absolute_difference)
    ), by = .(survey, year, broad_group)]
    data.table::fwrite(
      summary,
      table_file(cfg, "validation_03_survey_model_comparison.csv")
    )
    paste0(
      "surveys retained as inputs rather than exact final constraints; ",
      "comparison audit written; maximum absolute difference ",
      format_number(max(summary$maximum_absolute_difference))
    )
  })
}
if (!is.null(final_injuries)) {
  final_injury_key <- c(
    "Death_Prov", "Sex", "DeathYear", "Popgroup", "age5", "nbdcode", "Inj"
  )
  run_check("03", "final injury output", function() {
    check_unique(final_injuries, final_injury_key, "final injury estimates")
    check_finite_nonnegative(final_injuries, "Deaths", "final injury estimates")
    check_allowed(final_injuries, "nbdcode", INJURY_CODES, "final injury estimates")
    expect_true(all(final_injuries$Inj == 1L), "final injury Inj flag is not uniformly 1")
    "unique, finite and limited to the 15 modelled injury causes"
  })
}
if (!is.null(cod_final) && !is.null(final_injuries)) {
  run_check("03", "injury-envelope conservation", function() {
    raw_envelope <- cod_final[
      nbdcode %in% RAW_INJURY_CODES,
      .(Deaths = sum(Deaths)),
      by = .(Death_Prov, Sex, DeathYear, Popgroup, age5)
    ]
    model_envelope <- final_injuries[, .(Deaths = sum(Deaths)), by = .(
      Death_Prov, Sex, DeathYear, Popgroup, age5
    )]
    compare_measure_tables(
      raw_envelope,
      model_envelope,
      c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age5"),
      "Deaths",
      "injury mortality envelope"
    )
  })
}

# Stage 04: completeness and NPR ----------------------------------------------
completeness_scalars <- load_stage("04", "04_completeness_scalars")
investigation_pre_injury_envelope <- load_stage(
  "04", "04_investigation_subpopulation_pre_injury_envelope"
)
injury_envelope_anchors <- load_stage("04", "04_injury_envelope_anchors")
injury_envelope_factors <- load_stage("04", "04_injury_envelope_factors")
injury_envelope_adjustment <- load_stage("04", "04_injury_envelope_adjustment")
investigation_national <- load_stage("04", "04_investigation_national")
investigation_subpopulation <- load_stage("04", "04_investigation_subpopulation")


if (!is.null(injury_envelope_anchors)) {
  run_check("04", "injury level and profile survey anchors", function() {
    key <- c("Death_Prov", "Sex", "age5", "survey_year")
    check_unique(injury_envelope_anchors, key, "injury level/profile anchors")
    check_finite_nonnegative(
      injury_envelope_anchors,
      c(
        "routine_total", "routine_injury_total", "survey_total",
        "survey_injury_total", "survey_effective_n", "profile_factor",
        "survey_level_total", "routine_level_total",
        "routine_adjustable_injury_total", "routine_fixed_injury_total",
        "target_adjustable_injury_total", "maximum_adjustable_injury_total",
        "derived_survey_level_total", "processed_survey_all_age_total",
        "point_level_ratio"
      ),
      "injury level/profile anchors"
    )
    expect_true(
      injury_envelope_anchors[
        !is.finite(raw_log_profile_scalar) |
          !is.finite(smoothed_log_profile_scalar) |
          !is.finite(point_log_level_ratio),
        .N
      ] == 0L,
      "the injury level/profile anchor log quantities contain non-finite values"
    )
    expect_true(
      setequal(unique(injury_envelope_anchors$survey_year), c(2009L, 2017L)),
      "injury anchors are not limited to IMS 2009 and FAMHIS 2017"
    )
    level <- unique(injury_envelope_anchors[, .(
      survey_year,
      survey_level_total,
      derived_survey_level_total,
      published_survey_level_total,
      published_survey_level_lower,
      published_survey_level_upper,
      processed_survey_all_age_total,
      processed_to_published_level_ratio,
      point_level_ratio
    )])
    check_unique(level, "survey_year", "survey-derived injury-level anchors")
    expect_true(
      max(abs(level$survey_level_total - level$derived_survey_level_total)) <=
        1e-10 * max(1, abs(level$derived_survey_level_total)),
      "the injury-level point anchors do not equal the empirical survey totals"
    )
    reference <- level[
      is.finite(published_survey_level_total) |
        is.finite(published_survey_level_lower) |
        is.finite(published_survey_level_upper)
    ]
    if (nrow(reference)) {
      expect_true(
        reference[
          !is.finite(published_survey_level_total) |
            !is.finite(published_survey_level_lower) |
            !is.finite(published_survey_level_upper) |
            published_survey_level_lower <= 0 |
            published_survey_level_total <= published_survey_level_lower |
            published_survey_level_upper <= published_survey_level_total,
          .N
        ] == 0L,
        "an external injury-level validation reference is incomplete or invalid"
      )
    }
    closure <- injury_envelope_anchors[calibration_active == TRUE, .(
      survey_share_sum = sum(survey_share),
      routine_share_sum = sum(routine_share)
    ), by = survey_year]
    expect_true(
      max(abs(closure$survey_share_sum - 1)) <= 1e-8 &&
        max(abs(closure$routine_share_sum - 1)) <= 1e-8,
      "the injury relative profiles do not sum to one"
    )
    expect_true(
      injury_envelope_anchors[
        survey_year == 2009L & age5 %in% 1:2,
        all(abs(profile_factor - 1) <= 1e-12)
      ],
      "the historical IMS infant relative profile is not fixed to one"
    )
    paste0(
      format_number(nrow(injury_envelope_anchors)),
      " anchor cells; empirical survey levels ",
      paste(format_number(level$survey_level_total), collapse = " and "),
      "; profile-factor range ",
      format_number(min(injury_envelope_anchors$profile_factor)), "-",
      format_number(max(injury_envelope_anchors$profile_factor))
    )
  })
}

if (!is.null(injury_envelope_factors)) {
  run_check("04", "injury level/profile annual time policy", function() {
    key <- c("Death_Prov", "Sex", "age5", "DeathYear")
    check_unique(injury_envelope_factors, key, "injury annual factors")
    check_finite_nonnegative(
      injury_envelope_factors,
      c("profile_factor", "point_level_ratio"),
      "injury annual factors"
    )
    ids <- c("Death_Prov", "Sex", "age5")
    ref09 <- injury_envelope_factors[DeathYear == 2009L, c(
      ids, "profile_factor", "point_level_ratio"
    ), with = FALSE]
    data.table::setnames(
      ref09,
      c("profile_factor", "point_level_ratio"),
      c("profile_2009", "level_2009")
    )
    ref17 <- injury_envelope_factors[DeathYear == 2017L, c(
      ids, "profile_factor", "point_level_ratio"
    ), with = FALSE]
    data.table::setnames(
      ref17,
      c("profile_factor", "point_level_ratio"),
      c("profile_2017", "level_2017")
    )
    pre <- merge(
      injury_envelope_factors[DeathYear <= 2009L],
      ref09,
      by = ids,
      all.x = TRUE,
      sort = FALSE
    )
    post <- merge(
      injury_envelope_factors[DeathYear >= 2017L],
      ref17,
      by = ids,
      all.x = TRUE,
      sort = FALSE
    )
    expect_true(
      max(abs(pre$profile_factor - pre$profile_2009), na.rm = TRUE) <= 1e-12 &&
        max(abs(pre$point_level_ratio - pre$level_2009), na.rm = TRUE) <= 1e-12,
      "the pre-2009 injury level/profile does not reproduce IMS 2009"
    )
    expect_true(
      max(abs(post$profile_factor - post$profile_2017), na.rm = TRUE) <= 1e-12 &&
        max(abs(post$point_level_ratio - post$level_2017), na.rm = TRUE) <= 1e-12,
      "the post-2017 injury level/profile does not reproduce FAMHIS 2017"
    )
    middle <- merge(
      injury_envelope_factors[DeathYear > 2009L & DeathYear < 2017L],
      ref09,
      by = ids,
      all.x = TRUE,
      sort = FALSE
    )
    middle <- merge(middle, ref17, by = ids, all.x = TRUE, sort = FALSE)
    middle[, weight_2017 := (DeathYear - 2009) / 8]
    middle[, `:=`(
      expected_profile = exp(
        (1 - weight_2017) * log(profile_2009) +
          weight_2017 * log(profile_2017)
      ),
      expected_level = exp(
        (1 - weight_2017) * log(level_2009) +
          weight_2017 * log(level_2017)
      )
    )]
    expect_true(
      max(abs(middle$profile_factor - middle$expected_profile), na.rm = TRUE) <= 1e-12 &&
        max(abs(middle$point_level_ratio - middle$expected_level), na.rm = TRUE) <= 1e-12,
      "the 2009-2017 injury level/profile is not log-linearly interpolated"
    )
    "IMS 2009 level/profile held through 2009; log transition; FAMHIS 2017 held thereafter"
  })
}

if (!is.null(injury_envelope_adjustment)) {
  run_check("04", "injury level/profile calibration and cell-total preservation", function() {
    cell <- c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age5")
    check_unique(injury_envelope_adjustment, cell, "injury calibration audit")
    check_finite_nonnegative(
      injury_envelope_adjustment,
      c(
        "total_before", "total_after", "injury_before", "injury_after",
        "natural_before", "natural_after", "profile_factor__",
        "level_ratio__", "effective_profile_factor__"
      ),
      "injury calibration audit"
    )
    scale <- pmax(1, abs(injury_envelope_adjustment$total_before))
    expect_true(
      max(abs(injury_envelope_adjustment$total_error) / scale, na.rm = TRUE) <= 1e-10,
      "injury calibration changes a population-group cell total"
    )
    annual <- injury_envelope_adjustment[, .(
      injury_after = sum(injury_after),
      target_total_injury = unique(target_total_injury_year)
    ), by = DeathYear]
    expect_true(
      max(abs(annual$injury_after - annual$target_total_injury), na.rm = TRUE) <= 1e-7,
      "the calibrated annual injury total does not reproduce the survey-level target"
    )
    expected_anchor <- unique(injury_envelope_anchors[, .(
      DeathYear = survey_year,
      expected_injury = survey_level_total
    )])
    anchor_check <- merge(
      annual[DeathYear %in% expected_anchor$DeathYear],
      expected_anchor,
      by = "DeathYear",
      all = TRUE,
      sort = FALSE
    )
    expect_true(
      nrow(anchor_check) == 2L &&
        max(abs(anchor_check$injury_after - anchor_check$expected_injury),
            na.rm = TRUE) <=
          1e-8 * max(1, abs(anchor_check$expected_injury)),
      "the calibrated 2009/2017 national injury totals do not reproduce the empirical survey anchors"
    )
    expect_true(
      injury_envelope_adjustment[
        injury_after < -1e-10 | natural_after < -1e-10 |
          injury_after > total_after + 1e-10,
        .N
      ] == 0L,
      "the injury level/profile calibration produced an invalid total"
    )
    paste0(
      "population-group cell totals preserved; annual injury range ",
      format_number(min(annual$injury_after)), "-",
      format_number(max(annual$injury_after))
    )
  })
}

if (!is.null(completeness_scalars)) {
  scalar_key <- c("Death_Prov", "Sex", "DeathYear", "age5")
  run_check("04", "completeness scalar integrity", function() {
    check_unique(completeness_scalars, scalar_key, "completeness scalars")
    check_finite_nonnegative(completeness_scalars, c("S1", "S2"), "completeness scalars")
    check_allowed(completeness_scalars, "Death_Prov", 1:9, "completeness scalars")
    check_allowed(completeness_scalars, "Sex", 1:2, "completeness scalars")
    check_allowed(completeness_scalars, "age5", 1:20, "completeness scalars")
    paste0(
      "S1 range ", format_number(min(completeness_scalars$S1)), "-",
      format_number(max(completeness_scalars$S1)), "; S2 range ",
      format_number(min(completeness_scalars$S2)), "-",
      format_number(max(completeness_scalars$S2))
    )
  })
  run_check("04", "post-freeze completeness scalars", function() {
    freeze <- as.integer(cfg$settings$completeness_freeze_year)
    ref <- completeness_scalars[DeathYear == freeze, .(
      Death_Prov, Sex, age5, S1_ref = S1, S2_ref = S2
    )]
    post <- completeness_scalars[DeathYear > freeze]
    z <- merge(post, ref, by = c("Death_Prov", "Sex", "age5"), all.x = TRUE, sort = FALSE)
    expect_true(z[is.na(S1_ref) | is.na(S2_ref), .N] == 0L, "freeze-year scalar is missing")
    max_diff <- max(c(abs(z$S1 - z$S1_ref), abs(z$S2 - z$S2_ref)), na.rm = TRUE)
    expect_true(max_diff <= 1e-12, "post-freeze S1/S2 values differ from the freeze year")
    paste0("all years after ", freeze, " reproduce the freeze-year scalars")
  })
}

if (!is.null(investigation_subpopulation)) {
  investigation_key <- c(
    "Death_Prov", "Sex", "DeathYear", "Popgroup", "age5", "nbdcode"
  )
  run_check("04", "subpopulation investigation output", function() {
    check_unique(
      investigation_subpopulation,
      investigation_key,
      "04 investigation subpopulation"
    )
    check_finite_nonnegative(
      investigation_subpopulation,
      "Deaths",
      "04 investigation subpopulation"
    )
    check_allowed(investigation_subpopulation, "Death_Prov", 1:9, "04 investigation")
    check_allowed(investigation_subpopulation, "Sex", 1:2, "04 investigation")
    check_allowed(investigation_subpopulation, "Popgroup", 1:4, "04 investigation")
    check_allowed(investigation_subpopulation, "age5", 1:20, "04 investigation")
    expect_true(
      investigation_subpopulation[nbdcode == 142L & Deaths > 1e-12, .N] == 0L,
      "cause 142 remains non-zero after its NPR-stage consolidation into cause 141"
    )
    "unique, finite, non-negative, and cause 142 consolidated"
  })
}

if (!is.null(investigation_national) && !is.null(investigation_subpopulation)) {
  run_check("04", "national investigation consistency", function() {
    expected <- investigation_subpopulation[, .(
      adjNBDcount = sum(Deaths)
    ), by = .(Sex, DeathYear, nbdcode, age5)]
    observed <- investigation_national[, .(
      adjNBDcount = sum(adjNBDcount)
    ), by = .(Sex, DeathYear, nbdcode, age5)]
    compare_measure_tables(
      expected,
      observed,
      c("Sex", "DeathYear", "nbdcode", "age5"),
      "adjNBDcount",
      "national investigation aggregation"
    )
  })
}

if (!is.null(cod_final) && !is.null(investigation_subpopulation)) {
  run_check("04", "completeness/NPR adjustment summary", function() {
    before <- cod_final[, .(registered_deaths = sum(Deaths)), by = DeathYear]
    after <- investigation_subpopulation[, .(adjusted_deaths = sum(Deaths)), by = DeathYear]
    summary <- merge(before, after, by = "DeathYear", all = TRUE, sort = TRUE)
    summary[, adjustment_ratio := adjusted_deaths / registered_deaths]
    output <- table_file(cfg, "validation_04_adjustment_ratios_by_year.csv")
    data.table::fwrite(summary, output)
    expect_true(
      summary[!is.finite(adjustment_ratio) | adjustment_ratio < 0, .N] == 0L,
      "invalid completeness/NPR adjustment ratio"
    )
    paste0(
      "annual ratios written to ", output, "; range ",
      format_number(min(summary$adjustment_ratio)), "-",
      format_number(max(summary$adjustment_ratio))
    )
  })
}

# Stage 05: prevalence and HIV reallocation -----------------------------------
prevalence_population <- load_stage("05", "05_prevalence_population")
hiv_long <- load_stage("05", "05_hiv_reallocated_long")
hiv_trends <- load_stage("05", "05_hiv_background_trends")
hiv_intercepts <- load_stage("05", "05_hiv_zero_prevalence_intercepts")
hiv_misclassified <- load_stage("05", "05_hiv_misclassified_diagnostics")
hiv_wide <- load_stage("05", "05_hiv_reallocated_wide")

if (!is.null(prevalence_population)) {
  prevalence_key <- c("Death_Prov", "Sex", "DeathYear", "age5")
  run_check("05", "prevalence-population panel", function() {
    check_unique(prevalence_population, prevalence_key, "prevalence-population panel")
    check_finite_nonnegative(prevalence_population, "ASSABase", "prevalence-population panel")
    expect_true(
      prevalence_population[
        ASSABase > 0 & (!is.finite(ANCPrev) | ANCPrev < 0),
        .N
      ] == 0L,
      "ANC prevalence is missing, non-finite or negative where population is positive"
    )
    check_allowed(prevalence_population, "Death_Prov", 1:10, "prevalence-population panel")
    check_allowed(prevalence_population, "Sex", 1:3, "prevalence-population panel")
    check_allowed(prevalence_population, "age5", 2:20, "prevalence-population panel")
    "unique panel with valid prevalence and population"
  })
  run_check("05", "post-2009 prevalence freeze", function() {
    ref <- prevalence_population[DeathYear == 2009L, .(
      Death_Prov, Sex, age5, ANCPrev_ref = ANCPrev
    )]
    post <- prevalence_population[DeathYear >= 2010L]
    z <- merge(post, ref, by = c("Death_Prov", "Sex", "age5"), all.x = TRUE, sort = FALSE)
    same <- (is.na(z$ANCPrev) & is.na(z$ANCPrev_ref)) |
      abs(z$ANCPrev - z$ANCPrev_ref) <= 1e-12
    same[is.na(same)] <- FALSE
    expect_true(all(same), "post-2009 ANC prevalence differs from the 2009 value")
    "all prevalence values from 2010 onward equal their 2009 anchor"
  })
}

if (!is.null(hiv_long)) {
  hiv_key <- c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup", "nbdcode")
  run_check("05", "HIV-reallocated long output", function() {
    check_unique(hiv_long, hiv_key, "HIV-reallocated long output")
    check_finite_nonnegative(hiv_long, "Deaths", "HIV-reallocated long output")
    expect_true(
      hiv_long[nbdcode %in% HIV_PSEUDO_CODES & Deaths > 1e-12, .N] == 0L,
      "HIV pseudo-causes remain non-zero in the reallocated output"
    )
    "unique, finite, non-negative, with pseudo-causes removed"
  })
}
if (!is.null(investigation_subpopulation) && !is.null(hiv_long)) {
  run_check("05", "HIV reallocation conservation", function() {
    compare_group_totals(
      investigation_subpopulation,
      hiv_long,
      c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup"),
      "Deaths", "Deaths", "HIV reallocation"
    )
  })
}
if (!is.null(hiv_misclassified)) {
  run_check("05", "misclassified-HIV accounting identity", function() {
    check_finite_nonnegative(
      hiv_misclassified,
      c("NBD", "NonHIV", "Misclassified"),
      "HIV misclassification diagnostics"
    )
    error <- abs(hiv_misclassified$NBD -
      hiv_misclassified$NonHIV - hiv_misclassified$Misclassified)
    scale <- pmax(1, abs(hiv_misclassified$NBD))
    expect_true(
      all(error <= 1e-8 * scale),
      "NBD != NonHIV + Misclassified in the HIV diagnostics"
    )
    paste0("maximum accounting error ", format_number(max(error)))
  })
}
if (!is.null(hiv_wide) && !is.null(hiv_long)) {
  run_check("05", "HIV wide/long equivalence", function() {
    cause_columns <- paste0("c", 1:214)
    check_columns(hiv_wide, cause_columns, "HIV wide output")
    wide_totals <- hiv_wide[, .(
      Deaths = rowSums(as.matrix(.SD), na.rm = TRUE)
    ), .SDcols = cause_columns]
    wide_totals <- cbind(
      hiv_wide[, .(Death_Prov, Sex, DeathYear, age5, Popgroup)],
      wide_totals
    )
    long_totals <- hiv_long[, .(Deaths = sum(Deaths)), by = .(
      Death_Prov, Sex, DeathYear, age5, Popgroup
    )]
    compare_measure_tables(
      long_totals,
      wide_totals,
      c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup"),
      "Deaths",
      "HIV wide/long totals"
    )
  })
}
if (!is.null(hiv_trends)) {
  run_check("05", "HIV background trends finite where fitted", function() {
    fitted <- hiv_trends[!is.na(model_status) & !model_status %in% c(
      "no_model", "excluded_legacy_compatibility"
    )]
    expect_true(fitted[!is.finite(g1), .N] == 0L, "a fitted HIV trend has non-finite g1")
    paste0(format_number(nrow(fitted)), " fitted trend rows")
  })
}
if (!is.null(hiv_intercepts)) {
  run_check("05", "HIV zero-prevalence intercepts", function() {
    expect_true(
      hiv_intercepts[!is.finite(all_cause_intercept) | all_cause_intercept < 0, .N] == 0L,
      "HIV zero-prevalence intercepts are non-finite or negative"
    )
    paste0("intercept range ", format_number(min(hiv_intercepts$all_cause_intercept)), "-",
      format_number(max(hiv_intercepts$all_cause_intercept)))
  })
}

# Stage 06: garbage and ill-defined redistribution ----------------------------
garbage_wide <- load_stage("06", "06_redistributed_analysis_wide")
garbage_long <- load_stage("06", "06_redistributed_analysis_long")

if (!is.null(garbage_long)) {
  garbage_key <- c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup", "nbdcode")
  run_check("06", "redistributed analysis long output", function() {
    check_unique(garbage_long, garbage_key, "garbage-redistributed long output")
    check_finite_nonnegative(garbage_long, "Count", "garbage-redistributed long output")
    check_allowed(garbage_long, "nbdcode", 1:214, "garbage-redistributed long output")
    "unique, finite and non-negative"
  })
  run_check("06", "redistributed garbage sources are zero", function() {
    redistributed_sources <- unique(c(
      176, 178, 210, 211, 152, 153, 154, 193, 209, 212, 191, 175,
      179, 180, 149, 192, 155, 156, 157, 162, 148, 194, 207, 195,
      167, 168, 169, 206, 184, 174, 182, 183, 170, 181, 186, 187,
      188, 208, 145, 147, 146, 204, 173, 205,
      143, 144, 163, 164, 166, 171, 172, 177, 185, 189, 190, 202, 203
    ))
    remaining <- garbage_long[
      nbdcode %in% redistributed_sources & abs(Count) > 1e-10,
      .N
    ]
    expect_true(remaining == 0L, paste0(remaining, " non-zero redistributed source cells remain"))
    paste0(length(redistributed_sources), " source codes are zero")
  })
}
if (!is.null(hiv_long) && !is.null(garbage_long)) {
  run_check("06", "garbage redistribution conservation", function() {
    compare_group_totals(
      hiv_long,
      garbage_long,
      c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup"),
      "Deaths", "Count", "garbage and ill-defined redistribution",
      tolerance = 1e-7
    )
  })
}
if (!is.null(garbage_wide) && !is.null(garbage_long)) {
  run_check("06", "garbage wide/long equivalence", function() {
    cause_columns <- paste0("c", 1:214)
    check_columns(garbage_wide, cause_columns, "garbage wide output")
    wide_totals <- garbage_wide[, .(
      Count = rowSums(as.matrix(.SD), na.rm = TRUE)
    ), .SDcols = cause_columns]
    wide_totals <- cbind(
      garbage_wide[, .(Death_Prov, Sex, DeathYear, age5, Popgroup)],
      wide_totals
    )
    long_totals <- garbage_long[, .(Count = sum(Count)), by = .(
      Death_Prov, Sex, DeathYear, age5, Popgroup
    )]
    compare_measure_tables(
      long_totals,
      wide_totals,
      c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup"),
      "Count",
      "garbage wide/long totals"
    )
  })
}

if (!is.null(hiv_wide) && !is.null(garbage_wide)) {
  run_check("06", "natural and injury envelope preservation", function() {
    before_plausibility <- stage06_apply_sequential_redistribution(
      data.table::copy(hiv_wide)
    )
    id_columns <- c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup")

    compare_envelope <- function(codes, label) {
      selected_columns <- cause_column(codes)
      expected <- data.table::copy(before_plausibility[, id_columns, with = FALSE])
      observed <- data.table::copy(garbage_wide[, id_columns, with = FALSE])
      expected[, total := rowSums(
        as.matrix(before_plausibility[, selected_columns, with = FALSE]),
        na.rm = TRUE
      )]
      observed[, total := rowSums(
        as.matrix(garbage_wide[, selected_columns, with = FALSE]),
        na.rm = TRUE
      )]
      compare_measure_tables(
        expected,
        observed,
        id_columns,
        "total",
        paste0(label, " plausibility envelope"),
        tolerance = 1e-7
      )
    }

    natural_detail <- compare_envelope(c(1:123, 150:214), "natural")
    injury_detail <- compare_envelope(INJURY_CODES, "injury")
    paste(natural_detail, injury_detail, sep = "; ")
  })

  run_check("06", "neonatal cause 140 plausibility rule", function() {
    remaining <- garbage_wide[age5 == 1L & c140 > 1e-10, .N]
    expect_true(remaining == 0L, paste0(remaining, " positive neonatal c140 row(s) remain"))
    "neonatal c140 is zero and its injury envelope is retained"
  })
}

fallback_path <- table_file(cfg, "06_biological_fallback_rows.csv")
if (file.exists(fallback_path)) {
  fallback_rows <- read_tabular(fallback_path)
  run_check("06", "biological reference-allocation audit", function() {
    required <- c(
      "row_index", "fallback_level", "donor_total", "valid_targets",
      "envelope", "Death_Prov", "Sex", "DeathYear", "age5",
      "Popgroup", "restored_deaths"
    )
    check_columns(fallback_rows, required, "biological reference-allocation audit")
    check_unique(fallback_rows, c("row_index", "envelope"), "biological reference allocations")
    check_finite_nonnegative(
      fallback_rows,
      c("donor_total", "valid_targets", "restored_deaths"),
      "biological reference allocations"
    )
    allowed <- c(
      "province_sex_year_age",
      "national_sex_year_age",
      "province_sex_age",
      "national_sex_age",
      "equal_valid_causes"
    )
    unexpected <- setdiff(unique(fallback_rows$fallback_level), allowed)
    expect_true(!length(unexpected), paste0(
      "unexpected biological allocation level(s): ",
      paste(unexpected, collapse = ", ")
    ))
    paste0(
      format_number(nrow(fallback_rows)), " row(s); restored deaths ",
      format_number(sum(fallback_rows$restored_deaths)), "; levels ",
      paste(sort(unique(fallback_rows$fallback_level)), collapse = ", ")
    )
  })
}



# Stage 07: ZA causes and YLLs -------------------------------------------------
deaths_yll_national <- load_stage("07", "07_deaths_yll_national")
deaths_yll_all <- load_stage("07", "07_deaths_yll_all")

if (!is.null(deaths_yll_all)) {
  yll_key <- c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age", "nbdcode")
  run_check("07", "deaths and YLL output", function() {
    check_unique(deaths_yll_all, yll_key, "deaths and YLL output")
    check_finite_nonnegative(
      deaths_yll_all,
      c("Count", "YLL00", "YLL03", "YLL015"),
      "deaths and YLL output"
    )
    check_allowed(deaths_yll_all, "Death_Prov", 1:10, "deaths and YLL output")
    check_allowed(deaths_yll_all, "Sex", 1:2, "deaths and YLL output")
    check_allowed(deaths_yll_all, "Popgroup", 1:4, "deaths and YLL output")
    check_allowed(deaths_yll_all, "age", 1:20, "deaths and YLL output")
    "unique, finite and non-negative"
  })
  run_check("07", "national rows equal provincial sum", function() {
    expected <- deaths_yll_all[Death_Prov %in% 1:9, .(
      Count = sum(Count),
      YLL00 = sum(YLL00),
      YLL03 = sum(YLL03),
      YLL015 = sum(YLL015)
    ), by = .(Sex, DeathYear, Popgroup, age, nbdcode)]
    observed <- deaths_yll_all[Death_Prov == 10L, .(
      Count = sum(Count),
      YLL00 = sum(YLL00),
      YLL03 = sum(YLL03),
      YLL015 = sum(YLL015)
    ), by = .(Sex, DeathYear, Popgroup, age, nbdcode)]
    compare_measure_tables(
      expected,
      observed,
      c("Sex", "DeathYear", "Popgroup", "age", "nbdcode"),
      c("Count", "YLL00", "YLL03", "YLL015"),
      "national deaths/YLL aggregation"
    )
  })
  run_check("07", "YLL arithmetic", function() {
    schedule <- read_yll_schedule(cfg)
    z <- merge(
      deaths_yll_all,
      schedule,
      by = c("Sex", "age"),
      all.x = TRUE,
      sort = FALSE
    )
    expect_true(z[is.na(L00) | is.na(L03) | is.na(L015), .N] == 0L, "YLL schedule is missing")
    differences <- c(
      abs(z$YLL00 - z$Count * z$L00),
      abs(z$YLL03 - z$Count * z$L03),
      abs(z$YLL015 - z$Count * z$L015)
    )
    scales <- c(
      pmax(1, abs(z$YLL00)),
      pmax(1, abs(z$YLL03)),
      pmax(1, abs(z$YLL015))
    )
    expect_true(all(differences <= 1e-10 * scales), "stored YLLs do not equal Count x remaining life expectancy")
    paste0("maximum YLL arithmetic difference ", format_number(max(differences)))
  })
}

if (!is.null(garbage_long) && !is.null(deaths_yll_all)) {
  run_check("07", "detailed ZA causes preserve analysis-cause deaths", function() {
    lookup <- read_analysis_to_za_lookup(cfg)
    detailed_codes <- unique(lookup[hierarchy == "detailed", za_code])
    expected <- garbage_long[, .(Count = sum(Count)), by = .(
      Death_Prov, Sex, DeathYear, Popgroup, age = age5
    )]
    observed <- deaths_yll_all[
      Death_Prov %in% 1:9 & nbdcode %in% detailed_codes,
      .(Count = sum(Count)),
      by = .(Death_Prov, Sex, DeathYear, Popgroup, age)
    ]
    compare_measure_tables(
      expected,
      observed,
      c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age"),
      "Count",
      "detailed ZA mapping"
    )
  })
}

if (!is.null(deaths_yll_national) && !is.null(deaths_yll_all)) {
  run_check("07", "national summary file consistency", function() {
    expected <- deaths_yll_all[Death_Prov == 10L, .(
      Count = sum(Count),
      YLL00 = sum(YLL00),
      YLL03 = sum(YLL03),
      YLL015 = sum(YLL015)
    ), by = .(DeathYear, Sex, age, nbdcode)]
    compare_measure_tables(
      expected,
      deaths_yll_national,
      c("DeathYear", "Sex", "age", "nbdcode"),
      c("Count", "YLL00", "YLL03", "YLL015"),
      "07 national summary"
    )
  })
}

# Stage 08: final database ------------------------------------------------------
database_stem <- paste0(
  "NBD_database_", cfg$settings$start_year, "_", cfg$settings$end_year
)
database_path_value <- database_file(cfg, paste0(database_stem, ".parquet"))
final_database <- NULL
if (!file.exists(database_path_value)) {
  add_result("08", paste0("file ", basename(database_path_value)), "SKIP", "not yet created")
} else {
  final_database <- tryCatch({
    x <- read_tabular(database_path_value)
    add_result(
      "08", paste0("file ", basename(database_path_value)), "PASS",
      paste0(format_number(nrow(x)), " rows; ", sprintf("%.2f MB", file.info(database_path_value)$size / 1024^2))
    )
    x
  }, error = function(e) {
    add_result("08", paste0("file ", basename(database_path_value)), "FAIL", conditionMessage(e))
    NULL
  })
}

if (!is.null(final_database)) {
  db_key <- c("Grouping", "Sex", "nbdcode", "DeathYear", "Age")
  db_measures <- c("Deaths", "YLL00", "YLL03", "YLL015", "Pop")
  run_check("08", "final database structure", function() {
    check_columns(final_database, c(db_key, db_measures, "ASR"), "final database")
    check_unique(final_database, db_key, "final database")
    check_finite_nonnegative(final_database, db_measures, "final database")
    expect_true(
      final_database[!is.na(ASR) & (!is.finite(ASR) | ASR < 0), .N] == 0L,
      "final database contains invalid ASR values"
    )
    check_allowed(final_database, "Grouping", 1:14, "final database")
    check_allowed(final_database, "Sex", 1:3, "final database")
    check_allowed(final_database, "Age", 0:26, "final database")
    expected_years <- seq.int(cfg$settings$start_year, cfg$settings$end_year)
    expect_true(
      identical(sort(unique(as.integer(final_database$DeathYear))), expected_years),
      "final database does not cover the configured year range"
    )
    "unique database key, valid dimensions, finite measures"
  })
  run_check("08", "ASR is constant across age rows", function() {
    check <- final_database[, .(
      n_asr = data.table::uniqueN(ASR, na.rm = FALSE)
    ), by = .(Grouping, Sex, nbdcode, DeathYear)]
    expect_true(check[n_asr > 1L, .N] == 0L, "ASR varies across Age rows within the same cause-year stratum")
    paste0(format_number(nrow(check)), " ASR strata checked")
  })
  run_check("08", "national grouping equals provinces", function() {
    expected <- final_database[Grouping %in% 1:9, lapply(.SD, sum), by = .(
      Sex, nbdcode, DeathYear, Age
    ), .SDcols = db_measures]
    observed <- final_database[
      Grouping == 10L,
      c("Sex", "nbdcode", "DeathYear", "Age", db_measures),
      with = FALSE
    ]
    compare_measure_tables(
      expected,
      observed,
      c("Sex", "nbdcode", "DeathYear", "Age"),
      db_measures,
      "final national grouping"
    )
  })
  run_check("08", "person deaths and YLLs equal male plus female", function() {
    measures <- c("Deaths", "YLL00", "YLL03", "YLL015")
    expected <- final_database[Sex %in% 1:2, lapply(.SD, sum), by = .(
      Grouping, nbdcode, DeathYear, Age
    ), .SDcols = measures]
    observed <- final_database[
      Sex == 3L,
      c("Grouping", "nbdcode", "DeathYear", "Age", measures),
      with = FALSE
    ]
    compare_measure_tables(
      expected,
      observed,
      c("Grouping", "nbdcode", "DeathYear", "Age"),
      measures,
      "person aggregation"
    )
  })
  run_check("08", "aggregate age rows", function() {
    base <- final_database[Age %in% 0:19]
    definitions <- list(
      `20` = list(deaths = 0:2, population = 1:2),
      `21` = list(deaths = 3:4, population = 3:4),
      `22` = list(deaths = 5:10, population = 5:10),
      `23` = list(deaths = 11:13, population = 11:13),
      `24` = list(deaths = 14:19, population = 14:19),
      `25` = list(deaths = 0:19, population = 1:19),
      `26` = list(deaths = 5:19, population = 5:19)
    )
    key <- c("Grouping", "Sex", "nbdcode", "DeathYear")
    measures <- c("Deaths", "YLL00", "YLL03", "YLL015")
    worst <- 0
    for (age_code in names(definitions)) {
      definition <- definitions[[age_code]]
      expected_measures <- base[
        Age %in% definition$deaths,
        lapply(.SD, sum),
        by = key,
        .SDcols = measures
      ]
      expected_population <- base[
        Age %in% definition$population,
        .(Pop = sum(Pop)),
        by = key
      ]
      expected <- merge(expected_measures, expected_population, by = key, all = TRUE, sort = FALSE)
      observed <- final_database[
        Age == as.integer(age_code),
        c(key, measures, "Pop"),
        with = FALSE
      ]
      compare_measure_tables(
        expected,
        observed,
        key,
        c(measures, "Pop"),
        paste0("aggregate age ", age_code)
      )
    }
    paste0(length(definitions), " aggregate age definitions reproduced")
  })
  run_check("08", "ZA 140 structural rows", function() {
    rows <- final_database[nbdcode == 140L]
    expect_true(nrow(rows) > 0L, "ZA 140 structural rows are absent")
    expect_true(
      rows[abs(Deaths) > 1e-12 | abs(YLL00) > 1e-12 | abs(YLL03) > 1e-12 | abs(YLL015) > 1e-12, .N] == 0L,
      "ZA 140 contains non-zero deaths/YLLs under the current mapping"
    )
    paste0(format_number(nrow(rows)), " structural-zero rows")
  })

  if (!is.null(deaths_yll_all)) {
    run_check("08", "province base-age transformation from Stage 07", function() {
      source <- deaths_yll_all[Death_Prov %in% 1:9]
      source[, Age := as.integer(age - 1L)]
      expected <- source[, .(
        Deaths = sum(Count),
        YLL00 = sum(YLL00),
        YLL03 = sum(YLL03),
        YLL015 = sum(YLL015)
      ), by = .(Grouping = Death_Prov, Sex, nbdcode, DeathYear, Age)]
      observed <- final_database[
        Grouping %in% 1:9 & Sex %in% 1:2 & Age %in% 0:19,
        .(Grouping, Sex, nbdcode, DeathYear, Age, Deaths, YLL00, YLL03, YLL015)
      ]
      compare_measure_tables(
        expected,
        observed,
        c("Grouping", "Sex", "nbdcode", "DeathYear", "Age"),
        c("Deaths", "YLL00", "YLL03", "YLL015"),
        "Stage 07 to province database"
      )
    })
    run_check("08", "population-group base-age transformation from Stage 07", function() {
      source <- deaths_yll_all[Death_Prov %in% 1:9]
      source[, `:=`(
        Age = as.integer(age - 1L),
        Grouping = as.integer(Popgroup + 10L)
      )]
      expected <- source[, .(
        Deaths = sum(Count),
        YLL00 = sum(YLL00),
        YLL03 = sum(YLL03),
        YLL015 = sum(YLL015)
      ), by = .(Grouping, Sex, nbdcode, DeathYear, Age)]
      observed <- final_database[
        Grouping %in% 11:14 & Sex %in% 1:2 & Age %in% 0:19,
        .(Grouping, Sex, nbdcode, DeathYear, Age, Deaths, YLL00, YLL03, YLL015)
      ]
      compare_measure_tables(
        expected,
        observed,
        c("Grouping", "Sex", "nbdcode", "DeathYear", "Age"),
        c("Deaths", "YLL00", "YLL03", "YLL015"),
        "Stage 07 to population-group database"
      )
    })
  }
}

# Final report -----------------------------------------------------------------
report_path <- table_file(cfg, "pipeline_validation_summary.csv")
data.table::fwrite(validation_results, report_path)

message("\nValidation summary")
print(validation_results[, .N, by = status][order(status)])
message("Detailed report: ", report_path)

failures <- validation_results[status == "FAIL"]
if (nrow(failures) && STOP_ON_FAILURE) {
  stop(
    "Validation completed with ", nrow(failures),
    " failed check(s). Review output/tables/pipeline_validation_summary.csv before proceeding.",
    call. = FALSE
  )
}

invisible(validation_results)
