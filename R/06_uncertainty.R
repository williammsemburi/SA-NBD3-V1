# ==============================================================================
# 06_uncertainty: Joint uncertainty propagation
#
# Draw order: African natural completeness -> one joint IMS/FAMHIS survey-
# design replicate for injury level, demographic profile and specified cause
# fractions -> NIMS count uncertainty -> HIV/AIDS coefficient covariance ->
# continuous weights on the expert-approved redistribution targets.
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Completeness, injury, HIV/AIDS and redistribution draws
# ------------------------------------------------------------------------------

# NBD3 joint uncertainty engine -----------------------------------------------
#
# This module starts from the validated Stage 03-06 checkpoints and propagates
# four evidence-based sources of uncertainty in one joint draw:
#
#   1. African natural-cause completeness, using province-specific temporal
#      variation in annual aggregate S2 after age and sex are collapsed;
#   2. injury completeness and cause composition: one stratified PSU bootstrap
#      replicate from each of IMS 2009 and FAMHIS 2017 jointly determines the
#      national injury total, province-sex-age profile and well-specified cause
#      counts. NIMS 2000 contributes count-based cause-fraction uncertainty;
#   3. HIV/AIDS reallocation, using the fitted Stage 05 coefficient
#      variance-covariance matrices; and
#   4. garbage/ill-defined redistribution, using continuous positive random
#      multipliers on the same expert-approved targets used by Stage 06.
#
# The injury component introduces no published-total substitution, free
# concentration multiplier or between-survey bridge variance. Conditional on
# the survey replicates and NIMS composition draw, the annual paths are fixed.
# Components without an empirical variance source (S1 and ANC prevalence)
# remain fixed and are listed explicitly in the run manifest.

NBD3_UNCERTAINTY_VERSION <- "1.8.2"

UC_KEY <- c(
  "Death_Prov", "Sex", "DeathYear", "Popgroup", "age5", "nbdcode"
)
UC_CELL_KEY <- setdiff(UC_KEY, "nbdcode")
UC_INJURY_CODES <- if (exists("INJURY_CODES", inherits = TRUE)) {
  sort(as.integer(get("INJURY_CODES", inherits = TRUE)))
} else {
  c(
    124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L,
    135L, 136L, 137L, 138L, 139L, 140L, 141L
  )
}
if (length(UC_INJURY_CODES) != 15L || anyDuplicated(UC_INJURY_CODES)) {
  stop(
    "The joint uncertainty model requires the final 15-cause injury set.",
    call. = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# General helpers --------------------------------------------------------------

uc_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required.", call. = FALSE)
  }
  invisible(TRUE)
}

uc_as_dt <- function(x) {
  uc_require("data.table")
  data.table::as.data.table(data.table::copy(x))
}

uc_assert_columns <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(
      label, " is missing column(s): ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

uc_assert_finite_nonnegative <- function(
    x,
    column = "Deaths",
    label = "data") {
  values <- as.numeric(x[[column]])
  bad <- !is.finite(values) | values < -1e-9
  if (any(bad)) {
    stop(
      label, " contains ", sum(bad),
      " non-finite or negative value(s) in ", column, ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

uc_assert_unique <- function(x, key, label) {
  duplicates <- x[, .N, by = eval(key)][N > 1L]
  if (nrow(duplicates)) {
    stop(
      label, " is not unique on: ", paste(key, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

uc_aggregate_counts <- function(x, value_column = "Deaths") {
  x <- uc_as_dt(x)
  uc_assert_columns(x, c(UC_KEY, value_column), "count table")
  if (value_column != "Deaths") data.table::setnames(x, value_column, "Deaths")
  x[, Deaths := as.numeric(Deaths)]
  x <- x[, .(Deaths = sum(Deaths, na.rm = TRUE)), by = eval(UC_KEY)]
  x[abs(Deaths) < 1e-12, Deaths := 0]
  uc_assert_finite_nonnegative(x, "Deaths", "count table")
  uc_assert_unique(x, UC_KEY, "count table")
  x[]
}

uc_add_envelope <- function(x) {
  x <- uc_aggregate_counts(x)
  x[, envelope__ := data.table::fifelse(
    nbdcode %in% UC_INJURY_CODES,
    "injury",
    "natural"
  )]
  x[]
}

uc_recursive_defaults <- function(defaults, supplied) {
  if (is.null(supplied)) return(defaults)
  utils::modifyList(defaults, supplied, keep.null = TRUE)
}

uc_derived_path <- function(root, cfg, filename) {
  if (exists("derived_file", mode = "function", inherits = TRUE)) {
    return(derived_file(cfg, filename))
  }
  derived <- cfg$paths$derived %||% file.path("data", "derived")
  if (!grepl("^([A-Za-z]:)?[/\\\\]", derived)) {
    derived <- file.path(root, derived)
  }
  file.path(derived, filename)
}

uc_read_parquet <- function(path, label) {
  uc_require("arrow")
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
  uc_as_dt(arrow::read_parquet(path, as_data_frame = TRUE))
}

uc_standardize_count_input <- function(x, label) {
  x <- uc_as_dt(x)
  candidate <- intersect(c("Deaths", "Count", "adjNBDcount", "deaths"), names(x))
  if (!length(candidate)) {
    stop(label, " has no recognised count column.", call. = FALSE)
  }
  if (candidate[[1L]] != "Deaths") {
    data.table::setnames(x, candidate[[1L]], "Deaths")
  }
  uc_aggregate_counts(x, "Deaths")
}

uc_standardize_injury_fraction_panel <- function(
    x,
    label = "injury fractions") {
  x <- uc_as_dt(x)
  if ("year" %in% names(x) && !"DeathYear" %in% names(x)) {
    data.table::setnames(x, "year", "DeathYear")
  } else if (all(c("year", "DeathYear") %in% names(x))) {
    mismatch <- x[
      as.integer(year) != as.integer(DeathYear) |
        xor(is.na(year), is.na(DeathYear)),
      .N
    ]
    if (mismatch) {
      stop(
        label, " contains inconsistent year and DeathYear columns.",
        call. = FALSE
      )
    }
    x[, year := NULL]
  }

  key <- c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age5", "nbdcode")
  uc_assert_columns(x, c(key, "cf_final"), label)
  x <- x[nbdcode %in% UC_INJURY_CODES]
  x[, `:=`(
    Death_Prov = as.integer(Death_Prov),
    Sex = as.integer(Sex),
    DeathYear = as.integer(DeathYear),
    Popgroup = as.integer(Popgroup),
    age5 = as.integer(age5),
    nbdcode = as.integer(nbdcode),
    cf_final = as.numeric(cf_final)
  )]
  uc_assert_unique(x, key, label)
  if (x[!is.finite(cf_final) | cf_final <= 0 | cf_final > 1, .N]) {
    stop(label, " contains non-finite or invalid fractions.", call. = FALSE)
  }
  cell <- c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age5")
  closure <- x[, .(
    n_causes = data.table::uniqueN(nbdcode),
    fraction_sum = sum(cf_final)
  ), by = eval(cell)]
  if (closure[
    n_causes != length(UC_INJURY_CODES) | abs(fraction_sum - 1) > 1e-10,
    .N
  ]) {
    stop(label, " is incomplete or does not close to one.", call. = FALSE)
  }
  data.table::setorderv(x, key)
  x[, c(key, "cf_final"), with = FALSE]
}

uc_injury_fraction_panel_as_counts <- function(
    x,
    label = "injury fractions") {
  out <- uc_standardize_injury_fraction_panel(x, label)
  out[, Deaths := cf_final]
  out[, c(UC_KEY, "Deaths"), with = FALSE]
}

uc_file_signature <- function(path) {
  info <- file.info(path)
  paste(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    info$size,
    format(info$mtime, tz = "UTC", usetz = TRUE),
    unname(tools::md5sum(path)),
    sep = "|"
  )
}

UC_COMPONENT_SEED_OFFSETS <- c(
  completeness = 101L,
  injury_envelope = 173L,
  injury = 211L,
  hiv_model = 307L,
  redistribution = 401L,
  draw = 509L
)

uc_component_seed <- function(base_seed, draw_id, component) {
  component <- as.character(component)
  if (length(component) != 1L ||
      !component %in% names(UC_COMPONENT_SEED_OFFSETS)) {
    stop("Unknown uncertainty seed component: ", component, ".", call. = FALSE)
  }
  seed <- (
    as.double(base_seed) + 10007 * as.double(draw_id) +
      as.double(UC_COMPONENT_SEED_OFFSETS[[component]])
  ) %% .Machine$integer.max
  as.integer(if (seed < 1) seed + 1 else seed)
}

uc_max_table_error <- function(
    observed,
    expected,
    tolerance = 0,
    absolute_tolerance = 0) {
  tolerance <- as.numeric(tolerance)
  absolute_tolerance <- as.numeric(absolute_tolerance)
  if (!is.finite(tolerance) || tolerance < 0 ||
      !is.finite(absolute_tolerance) || absolute_tolerance < 0) {
    stop(
      "Table-comparison tolerances must be finite and non-negative.",
      call. = FALSE
    )
  }

  a <- uc_aggregate_counts(observed)
  b <- uc_aggregate_counts(expected)
  z <- merge(
    a,
    b,
    by = UC_KEY,
    all = TRUE,
    suffixes = c("_observed", "_expected")
  )
  z[is.na(Deaths_observed), Deaths_observed := 0]
  z[is.na(Deaths_expected), Deaths_expected := 0]
  z[, `:=`(
    abs_error = abs(Deaths_observed - Deaths_expected),
    scale = pmax(1, abs(Deaths_expected))
  )]
  z[, allowed_error := pmax(absolute_tolerance, tolerance * scale)]

  list(
    max_abs = max(z$abs_error, na.rm = TRUE),
    max_rel = max(z$abs_error / z$scale, na.rm = TRUE),
    max_tolerance_ratio = if (all(z$allowed_error == 0)) {
      if (max(z$abs_error, na.rm = TRUE) == 0) 0 else Inf
    } else {
      max(
        z$abs_error / pmax(z$allowed_error, .Machine$double.eps),
        na.rm = TRUE
      )
    },
    n_over = z[abs_error > allowed_error, .N]
  )
}

uc_max_cell_error <- function(
    before,
    after,
    cell_key = UC_CELL_KEY,
    tolerance = 0) {
  a <- before[, .(before = sum(Deaths)), by = eval(cell_key)]
  b <- after[, .(after = sum(Deaths)), by = eval(cell_key)]
  z <- merge(a, b, by = cell_key, all = TRUE)
  z[is.na(before), before := 0]
  z[is.na(after), after := 0]
  z[, `:=`(
    abs_error = abs(after - before),
    scale = pmax(1, abs(before))
  )]
  list(
    max_abs = max(z$abs_error, na.rm = TRUE),
    max_rel = max(z$abs_error / z$scale, na.rm = TRUE),
    n_over = z[abs_error > tolerance * scale, .N]
  )
}

# Configuration ----------------------------------------------------------------

uc_default_config <- function() {
  list(
    run = list(
      profile = "analysis",
      output_name = "nbd3_v1_joint_survey_injury_weighted_redist",
      n_draws = 100L,
      base_seed = 20260810L,
      scenarios = "joint",
      overwrite = FALSE,
      stop_on_validation_failure = TRUE,
      conservation_tolerance = 1e-7,
      reconstruction_relative_tolerance = 2e-7,
      reconstruction_absolute_tolerance = 1e-6
    ),
    components = list(
      completeness = list(
        enabled = TRUE,
        source_scalar = "S2",
        target_population_group = 1L,
        target_envelope = "natural",
        distribution = "province_specific_time_log_sd",
        draw_level = "province"
      ),
      injury = list(
        enabled = TRUE,
        model_file = "03_injury_fraction_model.rds",
        survey_design_file = "03_injury_survey_design.rds",
        sampling_distribution = "stratified_psu_bootstrap_joint",
        inter_survey_path = "deterministic_linear_plus_smoother",
        envelope_model_file = "04_injury_envelope_model.rds",
        envelope_sampling_distribution =
          "stratified_psu_bootstrap_joint_level_profile",
        pre_2009_policy = "hold_ims_2009",
        between_surveys_policy = "log_linear",
        post_2017_policy = "hold_famhis_2017"
      ),
      hiv = list(
        enabled = TRUE,
        coefficient_covariance = TRUE,
        covariance_file = "05_hiv_model_covariance.rds",
        prevalence_uncertainty = "fixed"
      ),
      redistribution = list(
        enabled = TRUE,
        target_weight_distribution =
          "subset_allocation_variance_matched_gamma",
        target_weight_scope = "rule",
        point_multiplier = 1
      )
    ),
    reporting = list(
      top_n_causes = 214L,
      include_all_provinces = TRUE,
      include_population_groups = TRUE,
      # The compact headline draw table remains Person-only for compatibility.
      sexes = 3L,
      # A second wide base-age table powers exact intervals for every sex,
      # explorer age and supported measure in the interactive report.
      full_ui_enabled = TRUE,
      full_ui_sexes = 1:3,
      age_groups = list(
        under_1 = 1:2,
        under_5 = 1:3,
        age_5_plus = 4:20,
        all_ages = 1:20
      )
    )
  )
}

uc_validate_config <- function(config) {
  required_components <- c("completeness", "injury", "hiv", "redistribution")
  missing_components <- setdiff(required_components, names(config$components))
  if (length(missing_components)) {
    stop(
      "Missing uncertainty component configuration: ",
      paste(missing_components, collapse = ", "), ".",
      call. = FALSE
    )
  }

  config$run$profile <- tolower(trimws(as.character(config$run$profile)))
  if (!config$run$profile %in% c("analysis", "release")) {
    stop("run.profile must be 'analysis' or 'release'.", call. = FALSE)
  }
  config$run$output_name <- trimws(as.character(config$run$output_name))
  if (length(config$run$output_name) != 1L ||
      !nzchar(config$run$output_name) ||
      grepl("[/\\\\]", config$run$output_name)) {
    stop("run.output_name must be one folder name without slashes.", call. = FALSE)
  }
  config$run$n_draws <- as.integer(config$run$n_draws)
  config$run$base_seed <- as.integer(config$run$base_seed)
  config$run$scenarios <- unique(as.character(unlist(config$run$scenarios)))
  if (!identical(config$run$scenarios, "joint")) {
    stop(
      "This release runs one scenario only: run.scenarios must be 'joint'.",
      call. = FALSE
    )
  }
  if (length(config$run$n_draws) != 1L || is.na(config$run$n_draws) ||
      config$run$n_draws < 1L) {
    stop("run.n_draws must be at least 1.", call. = FALSE)
  }
  if (length(config$run$base_seed) != 1L || is.na(config$run$base_seed) ||
      config$run$base_seed < 1L) {
    stop("run.base_seed must be a positive integer.", call. = FALSE)
  }
  for (name in c(
    "conservation_tolerance",
    "reconstruction_relative_tolerance",
    "reconstruction_absolute_tolerance"
  )) {
    value <- suppressWarnings(as.numeric(config$run[[name]]))
    if (length(value) != 1L || !is.finite(value) || value < 0 ||
        (name != "reconstruction_absolute_tolerance" && value <= 0)) {
      stop("run.", name, " has an invalid value.", call. = FALSE)
    }
    config$run[[name]] <- value
  }
  config$run$overwrite <- isTRUE(config$run$overwrite)
  config$run$stop_on_validation_failure <- isTRUE(
    config$run$stop_on_validation_failure
  )

  for (component in required_components) {
    config$components[[component]]$enabled <- isTRUE(
      config$components[[component]]$enabled
    )
    if (!config$components[[component]]$enabled) {
      stop(
        "All four uncertainty components must be enabled for the joint run: ",
        component, ".",
        call. = FALSE
      )
    }
  }

  completeness <- config$components$completeness
  if (!identical(as.character(completeness$source_scalar), "S2")) {
    stop("Completeness uncertainty must use the observed S2 input.", call. = FALSE)
  }
  completeness$target_population_group <- as.integer(
    completeness$target_population_group
  )
  if (!identical(completeness$target_population_group, 1L)) {
    stop("Completeness uncertainty targets African deaths (Popgroup 1).",
         call. = FALSE)
  }
  if (!identical(as.character(completeness$target_envelope), "natural")) {
    stop("Completeness uncertainty targets the natural-cause envelope.",
         call. = FALSE)
  }
  if (!identical(
    as.character(completeness$distribution),
    "province_specific_time_log_sd"
  )) {
    stop(
      "Completeness uncertainty must use province-specific temporal log-S2 dispersion after age and sex are collapsed.",
      call. = FALSE
    )
  }
  if (!identical(as.character(completeness$draw_level), "province")) {
    stop(
      "Completeness uncertainty must draw one factor for each province.",
      call. = FALSE
    )
  }
  config$components$completeness <- completeness

  injury <- config$components$injury
  for (field in c("model_file", "survey_design_file", "envelope_model_file")) {
    injury[[field]] <- trimws(as.character(injury[[field]]))
    if (length(injury[[field]]) != 1L || !nzchar(injury[[field]]) ||
        grepl("[/\\\\]", injury[[field]])) {
      stop("components.injury.", field, " must be one filename.",
           call. = FALSE)
    }
  }
  injury$sampling_distribution <- tolower(trimws(as.character(
    injury$sampling_distribution
  )))
  if (!identical(
    injury$sampling_distribution,
    "stratified_psu_bootstrap_joint"
  )) {
    stop(
      paste(
        "Injury uncertainty must use one stratified PSU bootstrap replicate",
        "per survey for the injury level, demographic profile and specified",
        "cause counts."
      ),
      call. = FALSE
    )
  }
  injury$inter_survey_path <- tolower(trimws(as.character(
    injury$inter_survey_path
  )))
  if (!identical(
    injury$inter_survey_path,
    "deterministic_linear_plus_smoother"
  )) {
    stop(
      "The injury inter-survey path must rerun the deterministic ALR interpolation and smoother.",
      call. = FALSE
    )
  }
  injury$envelope_sampling_distribution <- tolower(trimws(as.character(
    injury$envelope_sampling_distribution
  )))
  if (!identical(
    injury$envelope_sampling_distribution,
    "stratified_psu_bootstrap_joint_level_profile"
  )) {
    stop(
      paste(
        "Injury-envelope uncertainty must use the same stratified PSU",
        "bootstrap replicate for the survey-derived national level and",
        "relative province-sex-age profile."
      ),
      call. = FALSE
    )
  }
  policy_expectations <- c(
    pre_2009_policy = "hold_ims_2009",
    between_surveys_policy = "log_linear",
    post_2017_policy = "hold_famhis_2017"
  )
  for (name in names(policy_expectations)) {
    value <- tolower(trimws(as.character(injury[[name]])))
    if (!identical(value, policy_expectations[[name]])) {
      stop(
        "components.injury.", name, " must be '",
        policy_expectations[[name]], "'.",
        call. = FALSE
      )
    }
    injury[[name]] <- value
  }
  config$components$injury <- injury

  hiv <- config$components$hiv
  if (!isTRUE(hiv$coefficient_covariance)) {
    stop("components.hiv.coefficient_covariance must be true.", call. = FALSE)
  }
  hiv$covariance_file <- trimws(as.character(hiv$covariance_file))
  if (length(hiv$covariance_file) != 1L || !nzchar(hiv$covariance_file) ||
      grepl("[/\\\\]", hiv$covariance_file)) {
    stop("components.hiv.covariance_file must be one filename.", call. = FALSE)
  }
  hiv$prevalence_uncertainty <- tolower(trimws(as.character(
    hiv$prevalence_uncertainty %||% "fixed"
  )))
  if (!identical(hiv$prevalence_uncertainty, "fixed")) {
    stop("ANC prevalence is fixed until a variance input is supplied.",
         call. = FALSE)
  }
  config$components$hiv <- hiv

  redistribution <- config$components$redistribution
  redistribution$target_weight_distribution <- tolower(trimws(as.character(
    redistribution$target_weight_distribution
  )))
  if (!identical(
    redistribution$target_weight_distribution,
    "subset_allocation_variance_matched_gamma"
  )) {
    stop(
      paste(
        "Redistribution uncertainty must use continuous positive Gamma",
        "multipliers on the approved target vector."
      ),
      call. = FALSE
    )
  }
  redistribution$target_weight_scope <- tolower(trimws(as.character(
    redistribution$target_weight_scope
  )))
  if (!identical(redistribution$target_weight_scope, "rule")) {
    stop("One redistribution target-weight vector is drawn per expert rule.",
         call. = FALSE)
  }
  redistribution$point_multiplier <- suppressWarnings(as.numeric(
    redistribution$point_multiplier
  ))
  if (length(redistribution$point_multiplier) != 1L ||
      !is.finite(redistribution$point_multiplier) ||
      redistribution$point_multiplier != 1) {
    stop("The deterministic redistribution target multiplier must equal one.",
         call. = FALSE)
  }
  config$components$redistribution <- redistribution

  config$reporting$top_n_causes <- as.integer(config$reporting$top_n_causes)
  if (is.na(config$reporting$top_n_causes) ||
      config$reporting$top_n_causes < 1L) {
    stop("reporting.top_n_causes must be a positive integer.", call. = FALSE)
  }
  config$reporting$include_all_provinces <- isTRUE(
    config$reporting$include_all_provinces
  )
  config$reporting$include_population_groups <- isTRUE(
    config$reporting$include_population_groups %||% FALSE
  )
  config$reporting$sexes <- sort(unique(as.integer(unlist(
    config$reporting$sexes
  ))))
  config$reporting$full_ui_enabled <- isTRUE(
    config$reporting$full_ui_enabled %||% TRUE
  )
  config$reporting$full_ui_sexes <- sort(unique(as.integer(unlist(
    config$reporting$full_ui_sexes %||% 1:3
  ))))
  if (!identical(config$reporting$full_ui_sexes, 1:3)) {
    stop(
      "reporting.full_ui_sexes must contain Male, Female and Person (1, 2, 3).",
      call. = FALSE
    )
  }
  for (name in names(config$reporting$age_groups)) {
    values <- sort(unique(as.integer(unlist(
      config$reporting$age_groups[[name]]
    ))))
    if (!length(values) || anyNA(values) || any(!values %in% 1:20)) {
      stop("Invalid reporting age group: ", name, ".", call. = FALSE)
    }
    config$reporting$age_groups[[name]] <- values
  }
  config
}

# Completeness uncertainty -----------------------------------------------------

uc_weak_compositions <- function(total, parts) {
  total <- as.integer(total)
  parts <- as.integer(parts)
  if (length(total) != 1L || length(parts) != 1L ||
      is.na(total) || is.na(parts) || total < 0L || parts < 1L) {
    stop("A non-negative total and a positive number of parts are required.",
         call. = FALSE)
  }
  if (parts == 1L) return(matrix(total, nrow = 1L, ncol = 1L))

  # Stars and bars: choose the separator locations, then convert adjacent
  # separator distances to the non-negative counts in each bootstrap sample.
  separators <- utils::combn(total + parts - 1L, parts - 1L)
  bounds <- rbind(0L, separators, total + parts)
  counts <- t(
    bounds[2:nrow(bounds), , drop = FALSE] -
      bounds[1:(nrow(bounds) - 1L), , drop = FALSE] - 1L
  )
  storage.mode(counts) <- "integer"
  counts
}

uc_weighted_quantile <- function(x, weights, probabilities) {
  x <- as.numeric(x)
  weights <- as.numeric(weights)
  probabilities <- as.numeric(probabilities)
  valid <- is.finite(x) & is.finite(weights) & weights > 0
  x <- x[valid]
  weights <- weights[valid]
  if (!length(x) || sum(weights) <= 0) {
    return(rep(NA_real_, length(probabilities)))
  }
  order_index <- order(x)
  x <- x[order_index]
  weights <- weights[order_index] / sum(weights)
  cumulative <- cumsum(weights)
  vapply(probabilities, function(probability) {
    probability <- min(1, max(0, probability))
    index <- which(cumulative >= probability)[1L]
    if (is.na(index)) tail(x, 1L) else x[[index]]
  }, numeric(1))
}

uc_estimate_completeness_distribution <- function(scalars, stage04, cfg) {
  s <- uc_as_dt(scalars)
  x <- uc_aggregate_counts(stage04)
  key <- c("Death_Prov", "Sex", "DeathYear", "age5")
  uc_assert_columns(s, c(key, "S2"), "Stage 04 completeness scalars")
  s[, `:=`(
    Death_Prov = as.integer(Death_Prov),
    Sex = as.integer(Sex),
    DeathYear = as.integer(DeathYear),
    age5 = as.integer(age5),
    S2 = as.numeric(S2)
  )]
  uc_assert_unique(s, key, "Stage 04 completeness scalars")

  freeze_year <- as.integer(cfg$settings$completeness_freeze_year)
  if (length(freeze_year) != 1L || is.na(freeze_year)) {
    stop("settings.completeness_freeze_year is required.", call. = FALSE)
  }

  # The stochastic draw is one multiplier per province. Its uncertainty scale
  # should therefore reflect variation in the province-wide reporting level,
  # rather than systematic differences between age groups. Collapse sex and age
  # within each province-year using implied pre-adjustment African natural deaths
  # as weights, then estimate the death-weighted SD of log S2 across years.
  african_natural <- x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    .(adjusted_african_natural = sum(Deaths)),
    by = eval(key)
  ]
  candidate <- merge(s, african_natural, by = key, all.x = TRUE, sort = FALSE)
  candidate <- candidate[DeathYear <= freeze_year & age5 != 1L]
  eligible <- candidate[
    is.finite(S2) & S2 > 0 &
      is.finite(adjusted_african_natural) & adjusted_african_natural > 0
  ]
  eligible[, pre_adjustment_african_natural := adjusted_african_natural / S2]
  eligible <- eligible[
    is.finite(pre_adjustment_african_natural) &
      pre_adjustment_african_natural > 0
  ]
  if (!nrow(eligible)) {
    stop(
      "No positive pre-freeze African natural deaths are available to estimate province-specific temporal S2 dispersion.",
      call. = FALSE
    )
  }
  if (!identical(sort(unique(eligible$Death_Prov)), 1:9)) {
    stop(
      "Province-specific completeness uncertainty requires all nine provinces.",
      call. = FALSE
    )
  }

  time_cells <- eligible[, .(
    contributing_sex_age_cells = .N,
    contributing_sexes = data.table::uniqueN(Sex),
    contributing_age_groups = data.table::uniqueN(age5),
    adjusted_african_natural = sum(adjusted_african_natural),
    pre_adjustment_african_natural = sum(pre_adjustment_african_natural)
  ), by = .(Death_Prov, DeathYear)]
  time_cells[, S2 := adjusted_african_natural /
    pre_adjustment_african_natural]
  time_cells <- time_cells[
    is.finite(S2) & S2 > 0 &
      is.finite(pre_adjustment_african_natural) &
      pre_adjustment_african_natural > 0
  ]
  time_cells[, log_s2 := log(S2)]

  province_summary <- time_cells[, {
    weights <- as.numeric(pre_adjustment_african_natural)
    values <- as.numeric(log_s2)
    weight_total <- sum(weights)
    weighted_mean <- sum(weights * values) / weight_total
    weighted_variance <- sum(
      weights * (values - weighted_mean)^2
    ) / weight_total
    log_sd <- sqrt(max(0, weighted_variance))
    factor_quantiles <- exp(
      -0.5 * log_sd^2 +
        stats::qnorm(c(0.025, 0.5, 0.975)) * log_sd
    )
    list(
      eligible_time_cells = .N,
      eligible_years = data.table::uniqueN(DeathYear),
      contributing_sex_age_cells = sum(contributing_sex_age_cells),
      pre_adjustment_deaths = sum(pre_adjustment_african_natural),
      adjusted_deaths = sum(adjusted_african_natural),
      aggregate_s2 = sum(adjusted_african_natural) /
        sum(pre_adjustment_african_natural),
      weighted_mean_log_s2 = weighted_mean,
      log_sd = log_sd,
      minimum_annual_s2 = min(S2),
      median_annual_s2 = stats::median(S2),
      maximum_annual_s2 = max(S2),
      factor_q025 = factor_quantiles[[1L]],
      factor_median = factor_quantiles[[2L]],
      factor_mean = 1,
      factor_q975 = factor_quantiles[[3L]]
    )
  }, by = Death_Prov]
  data.table::setorder(province_summary, Death_Prov)

  invalid_province <- province_summary[
    !is.finite(aggregate_s2) | aggregate_s2 <= 0 |
      !is.finite(log_sd) | log_sd < 0 |
      !is.finite(factor_q025) | factor_q025 <= 0 |
      !is.finite(factor_q975) | factor_q975 <= 0
  ]
  if (nrow(invalid_province)) {
    stop(
      "At least one province has invalid temporal completeness dispersion.",
      call. = FALSE
    )
  }
  if (!any(province_summary$log_sd > 0)) {
    stop(
      "The province-specific temporal completeness distributions contain no dispersion.",
      call. = FALSE
    )
  }

  time_cells <- merge(
    time_cells,
    province_summary[, .(Death_Prov, weighted_mean_log_s2, log_sd)],
    by = "Death_Prov",
    all.x = TRUE,
    sort = FALSE
  )
  time_cells[, log_s2_residual := log_s2 - weighted_mean_log_s2]

  province_weights <- province_summary$pre_adjustment_deaths
  diagnostic_log_sd <- sqrt(
    sum(province_weights * province_summary$log_sd^2) /
      sum(province_weights)
  )
  if (!is.finite(diagnostic_log_sd) || diagnostic_log_sd <= 0) {
    stop(
      "The death-weighted summary of province-specific temporal log-S2 dispersion is invalid.",
      call. = FALSE
    )
  }

  factor_support <- province_summary[, .(
    Death_Prov,
    log_sd,
    factor_q025,
    factor_median,
    factor_mean,
    factor_q975
  )]

  excluded <- data.table::data.table(
    reason = c(
      "post_freeze_repetition",
      "copied_neonatal_row",
      "nonpositive_or_nonfinite_s2",
      "zero_or_missing_african_natural_weight"
    ),
    rows = c(
      s[DeathYear > freeze_year, .N],
      s[DeathYear <= freeze_year & age5 == 1L, .N],
      candidate[!is.finite(S2) | S2 <= 0, .N],
      candidate[
        is.finite(S2) & S2 > 0 &
          (!is.finite(adjusted_african_natural) |
             adjusted_african_natural <= 0),
        .N
      ]
    )
  )

  list(
    method = paste(
      "death-weighted within-province log-S2 standard deviation",
      "across time after sex and age are collapsed"
    ),
    draw_level = "one independent mean-one factor per province",
    freeze_year = freeze_year,
    log_sd = diagnostic_log_sd,
    log_sd_min = min(province_summary$log_sd),
    log_sd_median = stats::median(province_summary$log_sd),
    log_sd_max = max(province_summary$log_sd),
    q025 = min(province_summary$factor_q025),
    median = stats::median(province_summary$factor_median),
    q975 = max(province_summary$factor_q975),
    eligible_rows = nrow(eligible),
    eligible_strata = nrow(time_cells),
    provinces = sort(unique(eligible$Death_Prov)),
    factor_support = factor_support,
    weighted_cells = time_cells[, .(
      Death_Prov,
      DeathYear,
      contributing_sex_age_cells,
      contributing_sexes,
      contributing_age_groups,
      S2,
      log_s2,
      pre_adjustment_african_natural,
      adjusted_african_natural,
      weighted_mean_log_s2,
      log_s2_residual,
      log_sd
    )],
    province_summary = province_summary,
    excluded = excluded
  )
}

uc_completeness_distribution_summary <- function(distribution) {
  province_rows <- data.table::rbindlist(lapply(
    seq_len(nrow(distribution$province_summary)),
    function(index) {
      row <- distribution$province_summary[index]
      data.table::data.table(
        section = paste0("province_", row$Death_Prov),
        metric = c(
          "aggregate_s2", "log_sd", "factor_q025",
          "factor_median", "factor_mean", "factor_q975",
          "eligible_time_cells", "eligible_years"
        ),
        value = as.character(c(
          signif(row$aggregate_s2, 12),
          signif(row$log_sd, 12),
          signif(row$factor_q025, 12),
          signif(row$factor_median, 12),
          1,
          signif(row$factor_q975, 12),
          row$eligible_time_cells,
          row$eligible_years
        ))
      )
    }
  ))

  data.table::rbindlist(list(
    data.table::data.table(
      section = "method",
      metric = c(
        "method", "draw_level", "freeze_year",
        "death_weighted_rms_log_sd", "minimum_province_log_sd",
        "median_province_log_sd", "maximum_province_log_sd",
        "eligible_rows", "eligible_time_strata"
      ),
      value = as.character(c(
        distribution$method,
        distribution$draw_level,
        distribution$freeze_year,
        signif(distribution$log_sd, 12),
        signif(distribution$log_sd_min, 12),
        signif(distribution$log_sd_median, 12),
        signif(distribution$log_sd_max, 12),
        distribution$eligible_rows,
        distribution$eligible_strata
      ))
    ),
    province_rows,
    distribution$excluded[, .(
      section = "excluded_rows",
      metric = reason,
      value = as.character(rows)
    )]
  ), use.names = TRUE, fill = TRUE)
}

uc_draw_completeness <- function(
    data,
    distribution,
    stochastic,
    seed) {
  x <- uc_aggregate_counts(data)
  provinces <- sort(unique(as.integer(x$Death_Prov)))
  if (!identical(provinces, 1:9)) {
    stop("Stage 04 data do not contain all nine provinces.", call. = FALSE)
  }

  province_parameters <- uc_as_dt(distribution$province_summary)
  uc_assert_columns(
    province_parameters,
    c("Death_Prov", "log_sd"),
    "province-specific completeness parameters"
  )
  province_parameters <- province_parameters[, .(
    Death_Prov = as.integer(Death_Prov),
    log_sd = as.numeric(log_sd)
  )]
  data.table::setorder(province_parameters, Death_Prov)
  if (!identical(province_parameters$Death_Prov, provinces) ||
      province_parameters[
        !is.finite(log_sd) | log_sd < 0,
        .N
      ] > 0L) {
    stop(
      "Province-specific completeness parameters are invalid.",
      call. = FALSE
    )
  }

  if (!isTRUE(stochastic)) {
    return(list(
      data = x,
      factors = province_parameters[, .(
        Death_Prov,
        log_sd,
        standard_normal = 0,
        factor = 1
      )],
      diagnostics = list(
        log_sd = distribution$log_sd,
        log_sd_min = distribution$log_sd_min,
        log_sd_median = distribution$log_sd_median,
        log_sd_max = distribution$log_sd_max,
        total_before = sum(x$Deaths),
        total_after = sum(x$Deaths),
        affected_before = x[
          Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
          sum(Deaths)
        ],
        affected_after = x[
          Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
          sum(Deaths)
        ],
        factor_min = 1,
        factor_max = 1,
        factor_mean = 1
      )
    ))
  }

  set.seed(as.integer(seed))
  factors <- data.table::copy(province_parameters)
  factors[, standard_normal := stats::rnorm(.N)]
  factors[, factor := exp(
    -0.5 * log_sd^2 + log_sd * standard_normal
  )]
  if (factors[!is.finite(factor) | factor <= 0, .N]) {
    stop("A province-specific completeness factor is invalid.", call. = FALSE)
  }

  affected_before_by_province <- x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    .(affected_before = sum(Deaths)),
    by = Death_Prov
  ]
  factors <- merge(
    factors,
    affected_before_by_province,
    by = "Death_Prov",
    all.x = TRUE,
    sort = FALSE
  )
  factors[is.na(affected_before), affected_before := 0]

  x[, completeness_factor__ := factors$factor[
    match(Death_Prov, factors$Death_Prov)
  ]]
  if (x[!is.finite(completeness_factor__) |
          completeness_factor__ <= 0, .N]) {
    stop("Completeness factors could not be matched to all provinces.",
         call. = FALSE)
  }

  before_total <- sum(x$Deaths)
  affected <- x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    sum(Deaths)
  ]
  x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    Deaths := Deaths * completeness_factor__
  ]
  x[, completeness_factor__ := NULL]
  affected_after <- x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    sum(Deaths)
  ]
  out <- x[, .(Deaths = sum(Deaths)), by = eval(UC_KEY)]
  uc_assert_finite_nonnegative(out, "Deaths", "completeness draw")

  weighted_factor_mean <- if (sum(factors$affected_before) > 0) {
    sum(factors$affected_before * factors$factor) /
      sum(factors$affected_before)
  } else {
    mean(factors$factor)
  }

  list(
    data = out[],
    factors = factors[, .(
      Death_Prov,
      log_sd,
      standard_normal,
      factor,
      affected_before
    )],
    diagnostics = list(
      log_sd = distribution$log_sd,
      log_sd_min = distribution$log_sd_min,
      log_sd_median = distribution$log_sd_median,
      log_sd_max = distribution$log_sd_max,
      total_before = before_total,
      total_after = sum(out$Deaths),
      affected_before = affected,
      affected_after = affected_after,
      factor_min = min(factors$factor),
      factor_max = max(factors$factor),
      factor_mean = weighted_factor_mean
    )
  )
}

# Injury composition -----------------------------------------------------------

uc_validate_injury_artifact <- function(artifact) {
  required_functions <- c(
    "injury_compose_fraction_panel",
    "injury_draw_fraction_panel"
  )
  missing_functions <- required_functions[!vapply(
    required_functions,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )]
  if (length(missing_functions)) {
    stop(
      "The uncertainty run requires Stage 03 function(s): ",
      paste(missing_functions, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.list(artifact) || is.null(artifact$parameters) ||
      is.null(artifact$source_fractions) || is.null(artifact$start_year) ||
      is.null(artifact$end_year)) {
    stop("The Stage 03 injury interpolation artifact is incomplete.", call. = FALSE)
  }
  required_parameters <- c(
    "method", "anchor_years", "years", "smoothing_window",
    "interpolation_matrix", "smoothing_matrix"
  )
  absent <- required_parameters[vapply(
    required_parameters,
    function(field) is.null(artifact$parameters[[field]]),
    logical(1)
  )]
  if (length(absent)) {
    stop(
      "The Stage 03 injury interpolation artifact is missing parameter(s): ",
      paste(absent, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!identical(
    as.character(artifact$parameters$method),
    "nims_ims_famhis_alr_linear_triangular_ma"
  )) {
    stop("The Stage 03 injury artifact uses an unsupported method.", call. = FALSE)
  }
  required_source <- c(
    "Death_Prov", "Sex", "Popgroup", "age5", "year", "nbdcode",
    "fraction", "survey_effective_n", "fraction_source", "donor_level"
  )
  uc_assert_columns(
    uc_as_dt(artifact$source_fractions),
    required_source,
    "Stage 03 injury interpolation artifact"
  )
  invisible(TRUE)
}


uc_draw_injury_envelope <- function(
    data,
    point_artifact,
    survey_draw = NULL,
    cfg,
    stochastic = TRUE) {
  validate_injury_envelope_artifact(point_artifact)

  fitted <- if (isTRUE(stochastic)) {
    if (is.null(survey_draw) || is.null(survey_draw$envelope_surveys)) {
      stop("A joint IMS/FAMHIS survey-design draw is required.", call. = FALSE)
    }
    fit_injury_envelope_model(
      baseline = data,
      envelope_surveys = survey_draw$envelope_surveys,
      cfg = cfg
    )
  } else {
    list(
      anchors = point_artifact$anchors,
      annual_factors = point_artifact$annual_point,
      artifact = point_artifact
    )
  }

  factors <- uc_as_dt(fitted$annual_factors)
  factors[, `:=`(
    draw_log_profile_factor = log_profile_factor,
    draw_profile_factor = profile_factor,
    draw_log_level_ratio = point_log_level_ratio,
    draw_level_ratio = point_level_ratio
  )]
  adjustment <- apply_injury_envelope_adjustment(
    data,
    factors,
    profile_column = "draw_profile_factor",
    level_column = "draw_level_ratio",
    profile_log_column = "draw_log_profile_factor",
    level_log_column = "draw_log_level_ratio"
  )
  out <- uc_aggregate_counts(adjustment$data)
  before <- uc_aggregate_counts(data)
  conservation <- uc_max_cell_error(before, out, UC_CELL_KEY)
  anchor_draws <- unique(uc_as_dt(fitted$anchors)[, .(
    survey,
    survey_year,
    survey_level_total,
    derived_survey_level_total,
    routine_level_total,
    point_level_ratio,
    outside_reference_interval
  )])

  list(
    data = out,
    factors = factors,
    level_anchor_draws = anchor_draws,
    audit = adjustment$audit,
    diagnostics = list(
      maximum_total_absolute_error = conservation$max_abs,
      maximum_total_relative_error = conservation$max_rel,
      minimum_profile_factor = min(factors$draw_profile_factor, na.rm = TRUE),
      maximum_profile_factor = max(factors$draw_profile_factor, na.rm = TRUE),
      minimum_level_ratio = min(factors$draw_level_ratio, na.rm = TRUE),
      maximum_level_ratio = max(factors$draw_level_ratio, na.rm = TRUE),
      injury_before = adjustment$diagnostics$injury_before,
      injury_after = adjustment$diagnostics$injury_after
    )
  )
}

uc_draw_injury_fraction_panel <- function(
    artifact,
    seed,
    stochastic = TRUE,
    survey_counts = NULL) {
  uc_validate_injury_artifact(artifact)
  panel <- if (isTRUE(stochastic)) {
    injury_draw_fraction_panel(
      artifact,
      seed = seed,
      survey_counts = survey_counts
    )
  } else {
    injury_compose_fraction_panel(artifact)
  }
  uc_standardize_injury_fraction_panel(
    panel,
    if (isTRUE(stochastic)) {
      "Stage 03 survey-design injury fraction draw"
    } else {
      "Stage 03 deterministic injury fraction panel"
    }
  )
}

uc_replace_injury_composition <- function(data, fractions) {
  x <- uc_aggregate_counts(data)
  injury_cell <- c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age5")
  envelope <- x[
    nbdcode %in% UC_INJURY_CODES,
    .(injury_envelope__ = sum(Deaths)),
    by = eval(injury_cell)
  ]
  noninjury <- x[!nbdcode %in% UC_INJURY_CODES]
  new_injury <- merge(
    envelope,
    fractions,
    by = injury_cell,
    all.x = TRUE,
    sort = FALSE
  )
  if (new_injury[is.na(cf_final), .N]) {
    stop(
      "Injury fraction draw does not cover all Stage 04 injury cells.",
      call. = FALSE
    )
  }
  new_injury[, Deaths := injury_envelope__ * cf_final]
  new_injury <- new_injury[, c(UC_KEY, "Deaths"), with = FALSE]
  out <- data.table::rbindlist(list(noninjury, new_injury), use.names = TRUE)
  out <- out[, .(Deaths = sum(Deaths)), by = eval(UC_KEY)]

  check <- out[
    nbdcode %in% UC_INJURY_CODES,
    .(draw_envelope__ = sum(Deaths)),
    by = eval(injury_cell)
  ]
  check <- merge(envelope, check, by = injury_cell, all = TRUE)
  check[is.na(injury_envelope__), injury_envelope__ := 0]
  check[is.na(draw_envelope__), draw_envelope__ := 0]
  check[, `:=`(
    abs_error__ = abs(injury_envelope__ - draw_envelope__),
    scale__ = pmax(1, abs(injury_envelope__))
  )]
  uc_assert_finite_nonnegative(out, "Deaths", "injury composition draw")

  list(
    data = out[],
    diagnostics = list(
      max_envelope_abs_error = max(check$abs_error__, na.rm = TRUE),
      max_envelope_relative_error = max(
        check$abs_error__ / check$scale__,
        na.rm = TRUE
      )
    )
  )
}

# Expert-target redistribution uncertainty -----------------------------------

uc_run_stage06 <- function(
    data = NULL,
    wide = NULL,
    target_weight_overrides = NULL) {
  required <- c(
    "long_to_wide_causes",
    "wide_to_long_causes",
    "redistribute_garbage_codes"
  )
  missing <- required[!vapply(
    required,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )]
  if (length(missing)) {
    stop(
      "The uncertainty run requires Stage 06 function(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  supplied <- c(long = !is.null(data), wide = !is.null(wide))
  if (sum(supplied) != 1L) {
    stop(
      "Supply exactly one Stage 06 input: `data` (long) or `wide`.",
      call. = FALSE
    )
  }

  if (isTRUE(supplied[["long"]])) {
    stage05_long <- uc_as_dt(data)
    uc_assert_columns(stage05_long, c(UC_KEY, "Deaths"), "Stage 06 long input")
    stage05_long[, Deaths := as.numeric(Deaths)]
    uc_assert_finite_nonnegative(stage05_long, "Deaths", "Stage 06 long input")
    uc_assert_unique(stage05_long, UC_KEY, "Stage 06 long input")
    wide <- long_to_wide_causes(stage05_long, value = "Deaths", codes = 1:214)
  } else {
    wide <- uc_as_dt(wide)
    uc_assert_columns(wide, UC_CELL_KEY, "Stage 06 wide input")
    uc_assert_unique(wide, UC_CELL_KEY, "Stage 06 wide input")
    wide <- ensure_cause_columns(wide, codes = 1:214, copy = FALSE)
  }

  redistributed <- redistribute_garbage_codes(
    wide,
    cfg = NULL,
    target_weight_overrides = target_weight_overrides
  )
  target_audit <- attr(redistributed, "redistribution_target_audit")
  long <- wide_to_long_causes(
    redistributed,
    value = "Deaths",
    codes = 1:214,
    drop_zero = FALSE
  )
  list(data = uc_aggregate_counts(long), target_audit = target_audit)
}

uc_redistribution_audit_summary <- function(audit) {
  if (is.null(audit) || !nrow(audit)) {
    return(list(
      rules = 0L,
      stochastic_rules = 0L,
      mean_multiplier_cv = 0,
      min_multiplier = 1,
      max_multiplier = 1,
      mean_multiplier = 1,
      target_count = 0L
    ))
  }
  x <- uc_as_dt(audit)
  list(
    rules = nrow(x),
    stochastic_rules = x[stochastic_weights == TRUE, .N],
    mean_multiplier_cv = mean(x$multiplier_cv, na.rm = TRUE),
    min_multiplier = min(x$multiplier_min, na.rm = TRUE),
    max_multiplier = max(x$multiplier_max, na.rm = TRUE),
    mean_multiplier = mean(x$multiplier_mean, na.rm = TRUE),
    target_count = sum(x$target_count, na.rm = TRUE)
  )
}

uc_optional_cause_labels <- function(root, codes) {
  path <- file.path(root, "data", "lookups", "analysis_codes.csv")
  out <- data.table::data.table(
    nbdcode = as.integer(codes),
    label = NA_character_
  )
  if (!file.exists(path)) return(out)
  lookup <- tryCatch(data.table::fread(path), error = function(error) NULL)
  if (is.null(lookup)) return(out)
  code_col <- intersect(
    c("nbdcode", "analysis_code", "code", "Code"),
    names(lookup)
  )
  label_col <- intersect(
    c("analysis_label", "label", "cause", "description", "nbd_label"),
    names(lookup)
  )
  if (!length(code_col) || !length(label_col)) return(out)
  small <- lookup[, .(
    nbdcode = as.integer(get(code_col[[1L]])),
    label = as.character(get(label_col[[1L]]))
  )]
  small <- small[!is.na(nbdcode)]
  small <- small[, .(
    label = {
      candidate <- label[!is.na(label) & nzchar(label)]
      if (length(candidate)) candidate[[1L]] else NA_character_
    }
  ), by = nbdcode]
  merge(out[, .(nbdcode)], small, by = "nbdcode", all.x = TRUE, sort = FALSE)
}

uc_build_cause_catalog <- function(root, point_stage06, top_n) {
  totals <- point_stage06[
    nbdcode %in% 1:214,
    .(Deaths = sum(Deaths)),
    by = nbdcode
  ]
  data.table::setorder(totals, -Deaths, nbdcode)
  top <- head(totals$nbdcode, as.integer(top_n))
  codes <- sort(unique(c(2L, UC_INJURY_CODES, top)))
  labels <- uc_optional_cause_labels(root, codes)
  labels[, cause_id := paste0("nbd_", nbdcode)]
  labels[, forced := nbdcode %in% c(2L, UC_INJURY_CODES)]
  labels[, point_total_deaths := totals$Deaths[match(nbdcode, totals$nbdcode)]]
  labels[]
}

uc_report_draw <- function(data, catalog, config, scenario, draw_id) {
  x <- uc_aggregate_counts(data)
  detail_codes <- catalog$nbdcode
  base <- x[
    nbdcode %in% 1:214 & Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Death_Prov, Sex, DeathYear, age5, nbdcode)
  ]

  national <- base[, .(Deaths = sum(Deaths)),
                   by = .(Sex, DeathYear, age5, nbdcode)]
  national[, Death_Prov := 10L]
  geo <- data.table::rbindlist(list(base, national), use.names = TRUE)

  persons <- geo[
    Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Death_Prov, DeathYear, age5, nbdcode)
  ]
  persons[, Sex := 3L]
  geo <- data.table::rbindlist(list(geo, persons), use.names = TRUE)

  if (!isTRUE(config$reporting$include_all_provinces)) {
    geo <- geo[Death_Prov == 10L]
  }
  geo <- geo[Sex %in% config$reporting$sexes]

  detail <- geo[nbdcode %in% detail_codes]
  detail[, cause_id := paste0("nbd_", nbdcode)]
  detail[, nbdcode := NULL]

  all_causes <- geo[, .(Deaths = sum(Deaths)),
                    by = .(Death_Prov, Sex, DeathYear, age5)]
  all_causes[, cause_id := "all_causes"]

  all_injuries <- geo[
    nbdcode %in% UC_INJURY_CODES,
    .(Deaths = sum(Deaths)),
    by = .(Death_Prov, Sex, DeathYear, age5)
  ]
  all_injuries[, cause_id := "all_injuries"]

  cause_age <- data.table::rbindlist(
    list(detail, all_causes, all_injuries),
    use.names = TRUE,
    fill = TRUE
  )

  age_outputs <- vector("list", length(config$reporting$age_groups))
  names(age_outputs) <- names(config$reporting$age_groups)
  for (name in names(age_outputs)) {
    ages <- config$reporting$age_groups[[name]]
    age_outputs[[name]] <- cause_age[
      age5 %in% ages,
      .(Deaths = sum(Deaths)),
      by = .(Death_Prov, Sex, DeathYear, cause_id)
    ][, age_group := name]
  }
  out <- data.table::rbindlist(age_outputs, use.names = TRUE)
  out[, `:=`(
    scenario = as.character(scenario),
    draw_id = as.integer(draw_id)
  )]
  data.table::setcolorder(
    out,
    c(
      "scenario", "draw_id", "Death_Prov", "Sex", "DeathYear",
      "age_group", "cause_id", "Deaths"
    )
  )
  out[]
}



# The provincial/national reporting table deliberately collapses population
# group. Population-group uncertainty is retained in a separate compact draw
# file so the final report can show exact national population-group intervals
# without enlarging the already substantial province draw files.
uc_report_population_draw <- function(
    data,
    catalog,
    config,
    scenario,
    draw_id) {
  x <- uc_aggregate_counts(data)
  detail_codes <- catalog$nbdcode

  base <- x[
    nbdcode %in% 1:214,
    .(Deaths = sum(Deaths)),
    by = .(Popgroup, Sex, DeathYear, age5, nbdcode)
  ]
  base <- base[Popgroup %in% 1:4]

  persons <- base[
    Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Popgroup, DeathYear, age5, nbdcode)
  ]
  persons[, Sex := 3L]
  geo <- data.table::rbindlist(list(base, persons), use.names = TRUE)
  geo <- geo[Sex %in% config$reporting$sexes]

  detail <- geo[nbdcode %in% detail_codes]
  detail[, cause_id := paste0("nbd_", nbdcode)]
  detail[, nbdcode := NULL]

  all_causes <- geo[, .(Deaths = sum(Deaths)),
                    by = .(Popgroup, Sex, DeathYear, age5)]
  all_causes[, cause_id := "all_causes"]

  all_injuries <- geo[
    nbdcode %in% UC_INJURY_CODES,
    .(Deaths = sum(Deaths)),
    by = .(Popgroup, Sex, DeathYear, age5)
  ]
  all_injuries[, cause_id := "all_injuries"]

  cause_age <- data.table::rbindlist(
    list(detail, all_causes, all_injuries),
    use.names = TRUE,
    fill = TRUE
  )

  age_outputs <- vector("list", length(config$reporting$age_groups))
  names(age_outputs) <- names(config$reporting$age_groups)
  for (name in names(age_outputs)) {
    ages <- config$reporting$age_groups[[name]]
    age_outputs[[name]] <- cause_age[
      age5 %in% ages,
      .(Deaths = sum(Deaths)),
      by = .(Popgroup, Sex, DeathYear, cause_id)
    ][, age_group := name]
  }
  out <- data.table::rbindlist(age_outputs, use.names = TRUE)
  out[, `:=`(
    scenario = as.character(scenario),
    draw_id = as.integer(draw_id)
  )]
  data.table::setcolorder(
    out,
    c(
      "scenario", "draw_id", "Popgroup", "Sex", "DeathYear",
      "age_group", "cause_id", "Deaths"
    )
  )
  out[]
}



# Full uncertainty reporting grid ---------------------------------------------
#
# The compact headline draw files above are retained for the model-comparison
# panels and convergence diagnostics. The functions below write a second,
# wide base-age representation for the general results explorers. One row
# contains all 20 base-age death counts for one geography, sex, year and
# analysis-cause series. Aggregate ages, crude rates and ASRs are derived only
# when a collaborator requests a selection in the Shiny report. This avoids a
# prohibitively large long table while preserving every joint draw.

uc_full_age_columns <- function() paste0("age_", 0:19)

uc_full_ui_sexes <- function(config) {
  values <- config$reporting$full_ui_sexes %||% 1:3
  values <- sort(unique(as.integer(unlist(values, use.names = FALSE))))
  values <- values[values %in% 1:3]
  if (!length(values)) values <- 1:3
  values
}

uc_complete_full_age_columns <- function(x) {
  out <- uc_as_dt(x)
  age_columns <- uc_full_age_columns()
  missing <- setdiff(age_columns, names(out))
  if (length(missing)) {
    for (column in missing) out[, (column) := 0]
  }
  out
}

uc_report_full_draw <- function(data, config, scenario, draw_id) {
  x <- uc_aggregate_counts(data)
  base <- x[
    nbdcode %in% 1:214 & Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Death_Prov, Sex, DeathYear, age5, nbdcode)
  ]

  national <- base[, .(Deaths = sum(Deaths)),
                   by = .(Sex, DeathYear, age5, nbdcode)]
  national[, Death_Prov := 10L]
  geo <- data.table::rbindlist(list(base, national), use.names = TRUE)

  persons <- geo[
    Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Death_Prov, DeathYear, age5, nbdcode)
  ]
  persons[, Sex := 3L]
  geo <- data.table::rbindlist(list(geo, persons), use.names = TRUE)

  if (!isTRUE(config$reporting$include_all_provinces)) {
    geo <- geo[Death_Prov == 10L]
  }
  geo <- geo[Sex %in% uc_full_ui_sexes(config)]

  detail <- geo[nbdcode %in% 1:214]
  detail[, cause_id := paste0("nbd_", nbdcode)]
  detail[, nbdcode := NULL]

  all_causes <- geo[, .(Deaths = sum(Deaths)),
                    by = .(Death_Prov, Sex, DeathYear, age5)]
  all_causes[, cause_id := "all_causes"]

  all_injuries <- geo[
    nbdcode %in% UC_INJURY_CODES,
    .(Deaths = sum(Deaths)),
    by = .(Death_Prov, Sex, DeathYear, age5)
  ]
  all_injuries[, cause_id := "all_injuries"]

  long <- data.table::rbindlist(
    list(detail, all_causes, all_injuries),
    use.names = TRUE,
    fill = TRUE
  )
  if (long[!age5 %in% 1:20, .N]) {
    stop("The full uncertainty report contains an age code outside 1:20.",
         call. = FALSE)
  }
  long[, age_column__ := paste0("age_", as.integer(age5) - 1L)]
  long[, age5 := NULL]

  wide <- data.table::dcast(
    long,
    Death_Prov + Sex + DeathYear + cause_id ~ age_column__,
    value.var = "Deaths",
    fun.aggregate = sum,
    fill = 0
  )
  wide <- uc_complete_full_age_columns(wide)
  age_columns <- uc_full_age_columns()
  wide[, (age_columns) := lapply(.SD, as.numeric), .SDcols = age_columns]
  if (wide[, any(!is.finite(unlist(.SD)) | unlist(.SD) < -1e-9),
           .SDcols = age_columns]) {
    stop("The full province uncertainty report contains invalid deaths.",
         call. = FALSE)
  }
  wide[, `:=`(
    scenario = as.character(scenario),
    draw_id = as.integer(draw_id)
  )]
  data.table::setcolorder(
    wide,
    c(
      "scenario", "draw_id", "Death_Prov", "Sex", "DeathYear",
      "cause_id", age_columns
    )
  )
  data.table::setorder(wide, cause_id, Death_Prov, Sex, DeathYear)
  wide[]
}

uc_report_full_population_draw <- function(
    data,
    config,
    scenario,
    draw_id) {
  x <- uc_aggregate_counts(data)
  base <- x[
    nbdcode %in% 1:214 & Popgroup %in% 1:4 & Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Popgroup, Sex, DeathYear, age5, nbdcode)
  ]

  persons <- base[
    Sex %in% 1:2,
    .(Deaths = sum(Deaths)),
    by = .(Popgroup, DeathYear, age5, nbdcode)
  ]
  persons[, Sex := 3L]
  geo <- data.table::rbindlist(list(base, persons), use.names = TRUE)
  geo <- geo[Sex %in% uc_full_ui_sexes(config)]

  detail <- geo[nbdcode %in% 1:214]
  detail[, cause_id := paste0("nbd_", nbdcode)]
  detail[, nbdcode := NULL]

  all_causes <- geo[, .(Deaths = sum(Deaths)),
                    by = .(Popgroup, Sex, DeathYear, age5)]
  all_causes[, cause_id := "all_causes"]

  all_injuries <- geo[
    nbdcode %in% UC_INJURY_CODES,
    .(Deaths = sum(Deaths)),
    by = .(Popgroup, Sex, DeathYear, age5)
  ]
  all_injuries[, cause_id := "all_injuries"]

  long <- data.table::rbindlist(
    list(detail, all_causes, all_injuries),
    use.names = TRUE,
    fill = TRUE
  )
  if (long[!age5 %in% 1:20, .N]) {
    stop("The full population-group uncertainty report contains an age code outside 1:20.",
         call. = FALSE)
  }
  long[, age_column__ := paste0("age_", as.integer(age5) - 1L)]
  long[, age5 := NULL]

  wide <- data.table::dcast(
    long,
    Popgroup + Sex + DeathYear + cause_id ~ age_column__,
    value.var = "Deaths",
    fun.aggregate = sum,
    fill = 0
  )
  wide <- uc_complete_full_age_columns(wide)
  age_columns <- uc_full_age_columns()
  wide[, (age_columns) := lapply(.SD, as.numeric), .SDcols = age_columns]
  if (wide[, any(!is.finite(unlist(.SD)) | unlist(.SD) < -1e-9),
           .SDcols = age_columns]) {
    stop("The full population-group uncertainty report contains invalid deaths.",
         call. = FALSE)
  }
  wide[, `:=`(
    scenario = as.character(scenario),
    draw_id = as.integer(draw_id)
  )]
  data.table::setcolorder(
    wide,
    c(
      "scenario", "draw_id", "Popgroup", "Sex", "DeathYear",
      "cause_id", age_columns
    )
  )
  data.table::setorder(wide, cause_id, Popgroup, Sex, DeathYear)
  wide[]
}

uc_write_parquet_atomic <- function(x, path) {
  uc_require("arrow")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  arrow::write_parquet(x, temporary, compression = "zstd")
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    stop("Could not finalise ", path, call. = FALSE)
  }
  invisible(path)
}

uc_write_csv_atomic <- function(x, path) {
  uc_require("data.table")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  data.table::fwrite(x, temporary)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    stop("Could not finalise ", path, call. = FALSE)
  }
  invisible(path)
}

# Scalable uncertainty finalisation -------------------------------------------
#
# The full uncertainty profile writes one compact reporting table and one wide
# base-age table per draw. At 1,000 draws, combining even the compact tables in
# memory or serialising them as one Parquet file can exceed Arrow/Thrift limits.
# The functions below retain the per-draw files as the canonical store and
# calculate exact compact summaries in bounded row blocks.

uc_draw_key_columns <- function() {
  c("scenario", "Death_Prov", "Sex", "DeathYear", "age_group", "cause_id")
}

uc_keys_identical <- function(current, reference, columns) {
  if (nrow(current) != nrow(reference)) return(FALSE)
  all(vapply(
    columns,
    function(column) identical(current[[column]], reference[[column]]),
    logical(1)
  ))
}

uc_safe_max <- function(values) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values)) max(values) else NA_real_
}

uc_read_csv_stack <- function(paths, add_draw_id = FALSE) {
  pieces <- vector("list", length(paths))
  for (index in seq_along(paths)) {
    pieces[[index]] <- data.table::fread(paths[[index]])
    if (isTRUE(add_draw_id)) {
      pieces[[index]][, draw_id := as.integer(index)]
    }
  }
  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

uc_hiv_covariance_state_init <- function(draw) {
  x <- draw[
    Death_Prov == 10L & Sex == 3L & age_group == "all_ages" &
      grepl("^nbd_[0-9]+$", cause_id)
  ]
  data.table::setorder(x, DeathYear, cause_id)
  if (!nrow(x) || !"nbd_2" %in% x$cause_id) return(NULL)

  hiv <- x[cause_id == "nbd_2", .(DeathYear, hiv_deaths = Deaths)]
  other <- x[cause_id != "nbd_2"]
  other <- merge(other, hiv, by = "DeathYear", all = FALSE, sort = FALSE)
  data.table::setorder(other, DeathYear, cause_id)
  list(
    keys = other[, .(scenario, DeathYear, cause_id)],
    n = 0L,
    sum_x = numeric(nrow(other)),
    sum_x2 = numeric(nrow(other)),
    sum_y = numeric(nrow(other)),
    sum_y2 = numeric(nrow(other)),
    sum_xy = numeric(nrow(other))
  )
}

uc_hiv_covariance_state_update <- function(state, draw) {
  if (is.null(state)) return(NULL)
  x <- draw[
    Death_Prov == 10L & Sex == 3L & age_group == "all_ages" &
      grepl("^nbd_[0-9]+$", cause_id)
  ]
  data.table::setorder(x, DeathYear, cause_id)
  hiv <- x[cause_id == "nbd_2", .(DeathYear, hiv_deaths = Deaths)]
  other <- x[cause_id != "nbd_2"]
  other <- merge(other, hiv, by = "DeathYear", all = FALSE, sort = FALSE)
  data.table::setorder(other, DeathYear, cause_id)
  current_keys <- other[, .(scenario, DeathYear, cause_id)]
  if (!uc_keys_identical(
    current_keys,
    state$keys,
    c("scenario", "DeathYear", "cause_id")
  )) {
    stop("HIV covariance keys differ between completed draws.", call. = FALSE)
  }
  xv <- as.numeric(other$Deaths)
  yv <- as.numeric(other$hiv_deaths)
  state$n <- state$n + 1L
  state$sum_x <- state$sum_x + xv
  state$sum_x2 <- state$sum_x2 + xv * xv
  state$sum_y <- state$sum_y + yv
  state$sum_y2 <- state$sum_y2 + yv * yv
  state$sum_xy <- state$sum_xy + xv * yv
  state
}

uc_hiv_covariance_state_finish <- function(state, catalog) {
  if (is.null(state) || state$n < 1L) return(data.table::data.table())
  n <- state$n
  mean_x <- state$sum_x / n
  mean_y <- state$sum_y / n
  if (n > 1L) {
    var_x <- pmax(0, (state$sum_x2 - state$sum_x^2 / n) / (n - 1L))
    var_y <- pmax(0, (state$sum_y2 - state$sum_y^2 / n) / (n - 1L))
    covariance <- (state$sum_xy - state$sum_x * state$sum_y / n) /
      (n - 1L)
    sd_x <- sqrt(var_x)
    sd_y <- sqrt(var_y)
  } else {
    var_y <- rep(NA_real_, length(mean_x))
    covariance <- rep(NA_real_, length(mean_x))
    sd_x <- rep(NA_real_, length(mean_x))
    sd_y <- rep(NA_real_, length(mean_x))
  }
  correlation <- data.table::fifelse(
    is.finite(sd_x) & sd_x > 0 & is.finite(sd_y) & sd_y > 0,
    covariance / (sd_x * sd_y),
    NA_real_
  )
  slope <- data.table::fifelse(
    is.finite(var_y) & var_y > 0,
    covariance / var_y,
    NA_real_
  )
  out <- cbind(
    data.table::copy(state$keys),
    data.table::data.table(
      n_draws = as.integer(n),
      mean_cause_deaths = mean_x,
      sd_cause_deaths = sd_x,
      mean_hiv_deaths = mean_y,
      sd_hiv_deaths = sd_y,
      covariance_with_hiv = covariance,
      correlation_with_hiv = correlation,
      cause_change_per_hiv_death = slope
    )
  )
  labels <- uc_as_dt(catalog)[, .(
    cause_id,
    nbdcode = as.integer(nbdcode),
    cause_label = as.character(label)
  )]
  out <- merge(out, labels, by = "cause_id", all.x = TRUE, sort = FALSE)
  hiv_linked_codes <- sort(unique(c(
    2L,
    as.integer(unlist(HIV_PSEUDO_DESTINATION, use.names = FALSE))
  )))
  out[, hiv_linked_destination := nbdcode %in% hiv_linked_codes]
  data.table::setorder(
    out,
    -hiv_linked_destination,
    DeathYear,
    correlation_with_hiv,
    nbdcode
  )
  out[]
}

uc_summarise_binary_draw_matrix <- function(
    matrix_path,
    keys,
    n_draws,
    point_report,
    block_size = 2000L) {
  uc_require("matrixStats")
  key_columns <- uc_draw_key_columns()
  n_cells <- nrow(keys)
  block_size <- max(1L, as.integer(block_size))
  starts <- seq.int(1L, n_cells, by = block_size)
  pieces <- vector("list", length(starts))
  con <- file(matrix_path, open = "rb")
  on.exit(close(con), add = TRUE)

  message(
    "Calculating exact type-8 compact summaries in ", length(starts),
    " bounded row block(s)..."
  )
  for (piece_index in seq_along(starts)) {
    first <- starts[[piece_index]]
    last <- min(n_cells, first + block_size - 1L)
    rows <- first:last
    n_block <- length(rows)
    values <- matrix(NA_real_, nrow = n_block, ncol = n_draws)

    for (draw_index in seq_len(n_draws)) {
      byte_offset <- as.double(
        ((draw_index - 1L) * n_cells + (first - 1L)) * 8
      )
      seek(con, where = byte_offset, origin = "start")
      current <- readBin(
        con,
        what = double(),
        n = n_block,
        size = 8L,
        endian = .Platform$endian
      )
      if (length(current) != n_block) {
        stop(
          "Could not read the expected values from the temporary compact ",
          "draw matrix for block ", piece_index, " and draw ", draw_index,
          ".",
          call. = FALSE
        )
      }
      values[, draw_index] <- current
    }
    if (any(!is.finite(values))) {
      stop(
        "The completed compact uncertainty draws contain a non-finite death ",
        "value in summary block ", piece_index, ".",
        call. = FALSE
      )
    }

    quantiles <- matrixStats::rowQuantiles(
      values,
      probs = c(0.025, 0.5, 0.975),
      type = 8L,
      useNames = FALSE,
      drop = FALSE
    )
    means <- matrixStats::rowMeans2(values)
    standard_deviations <- if (n_draws > 1L) {
      matrixStats::rowSds(values)
    } else {
      rep(NA_real_, n_block)
    }
    statistics <- data.table::data.table(
      n_draws = as.integer(n_draws),
      mean = as.numeric(means),
      median = as.numeric(quantiles[, 2L]),
      sd = as.numeric(standard_deviations),
      lower = as.numeric(quantiles[, 1L]),
      upper = as.numeric(quantiles[, 3L])
    )
    pieces[[piece_index]] <- cbind(
      data.table::copy(keys[rows, ..key_columns]),
      statistics
    )

    rm(values, quantiles, means, standard_deviations, statistics)
    if (piece_index == 1L || piece_index %% 10L == 0L ||
        piece_index == length(starts)) {
      message("  compact summary blocks: ", piece_index, "/", length(starts))
      invisible(gc(verbose = FALSE))
    }
  }

  summary <- data.table::rbindlist(pieces, use.names = TRUE, fill = FALSE)
  point <- point_report[, .(
    Death_Prov, Sex, DeathYear, age_group, cause_id,
    point = as.numeric(Deaths)
  )]
  summary <- merge(
    summary,
    point,
    by = c("Death_Prov", "Sex", "DeathYear", "age_group", "cause_id"),
    all.x = TRUE,
    sort = FALSE
  )
  if (any(!is.finite(summary$point))) {
    stop(
      "One or more compact uncertainty cells could not be matched to the ",
      "deterministic point report.",
      call. = FALSE
    )
  }
  summary[, `:=`(
    cv = data.table::fifelse(mean > 0, sd / mean, NA_real_),
    interval_width = upper - lower,
    relative_interval_width = data.table::fifelse(
      point > 0,
      (upper - lower) / point,
      NA_real_
    ),
    monte_carlo_se_mean = sd / sqrt(n_draws)
  )]
  data.table::setcolorder(
    summary,
    c(
      key_columns, "point", "mean", "median", "sd", "cv", "lower", "upper",
      "interval_width", "relative_interval_width", "monte_carlo_se_mean",
      "n_draws"
    )
  )
  summary[]
}

uc_summarise_draw_files <- function(
    draw_paths,
    point_report,
    catalog,
    scenario_path,
    block_size = 2000L) {
  uc_require("arrow")
  uc_require("data.table")
  key_columns <- uc_draw_key_columns()
  n_draws <- length(draw_paths)
  if (!n_draws) stop("No compact draw files were supplied.", call. = FALSE)

  matrix_path <- file.path(
    scenario_path,
    paste0(".compact_deaths_", Sys.getpid(), ".bin")
  )
  if (file.exists(matrix_path)) unlink(matrix_path)
  con <- file(matrix_path, open = "wb")
  connection_open <- TRUE
  on.exit({
    if (isTRUE(connection_open)) try(close(con), silent = TRUE)
    if (file.exists(matrix_path)) unlink(matrix_path)
  }, add = TRUE)

  key_reference <- NULL
  headline_pieces <- vector("list", n_draws)
  hiv_state <- NULL
  message(
    "Finalising ", n_draws,
    " compact draw files without creating a monolithic Parquet table..."
  )

  for (draw_index in seq_len(n_draws)) {
    draw <- data.table::as.data.table(
      arrow::read_parquet(draw_paths[[draw_index]], as_data_frame = TRUE)
    )
    required_columns <- c(key_columns, "draw_id", "Deaths")
    missing_columns <- setdiff(required_columns, names(draw))
    if (length(missing_columns)) {
      stop(
        "Compact draw file is missing column(s): ",
        paste(missing_columns, collapse = ", "), ". File: ",
        draw_paths[[draw_index]],
        call. = FALSE
      )
    }
    observed <- unique(as.integer(draw$draw_id))
    if (length(observed) != 1L || is.na(observed) || observed != draw_index) {
      stop(
        "Draw identifier mismatch in ", draw_paths[[draw_index]],
        ": expected ", draw_index, " but found ",
        paste(observed, collapse = ", "), ".",
        call. = FALSE
      )
    }
    data.table::setorderv(draw, key_columns)
    if (any(!is.finite(draw$Deaths))) {
      stop("Compact draw ", draw_index, " contains non-finite deaths.",
           call. = FALSE)
    }

    current_keys <- draw[, ..key_columns]
    if (is.null(key_reference)) {
      key_reference <- data.table::copy(current_keys)
      hiv_state <- uc_hiv_covariance_state_init(draw)
      temporary_gib <- nrow(key_reference) * n_draws * 8 / 1024^3
      message(
        "  compact reporting cells per draw: ",
        format(nrow(key_reference), big.mark = ",", scientific = FALSE),
        "; temporary binary matrix: approximately ",
        sprintf("%.2f", temporary_gib), " GiB"
      )
    } else if (!uc_keys_identical(current_keys, key_reference, key_columns)) {
      stop(
        "The compact reporting key differs between completed draws; first ",
        "detected in draw ", draw_index, ".",
        call. = FALSE
      )
    }

    writeBin(
      as.double(draw$Deaths),
      con,
      size = 8L,
      endian = .Platform$endian
    )
    headline_pieces[[draw_index]] <- draw[
      Death_Prov == 10L & Sex == 3L & age_group == "all_ages" &
        cause_id %in% c("all_causes", "all_injuries", "nbd_2"),
      .(
        scenario, draw_id, Death_Prov, Sex, DeathYear,
        age_group, cause_id, Deaths
      )
    ]
    hiv_state <- uc_hiv_covariance_state_update(hiv_state, draw)

    if (draw_index == 1L || draw_index %% 50L == 0L ||
        draw_index == n_draws) {
      message("  compact draw files finalised: ", draw_index, "/", n_draws)
      invisible(gc(verbose = FALSE))
    }
    rm(draw, current_keys)
  }
  close(con)
  connection_open <- FALSE

  summary <- uc_summarise_binary_draw_matrix(
    matrix_path = matrix_path,
    keys = key_reference,
    n_draws = n_draws,
    point_report = point_report,
    block_size = block_size
  )
  headline_draws <- data.table::rbindlist(
    headline_pieces,
    use.names = TRUE,
    fill = FALSE
  )
  convergence <- uc_convergence_headlines(headline_draws)
  hiv_cause_covariance <- uc_hiv_covariance_state_finish(hiv_state, catalog)
  if (file.exists(matrix_path)) unlink(matrix_path)

  list(
    summary = summary,
    convergence = convergence,
    hiv_cause_covariance = hiv_cause_covariance
  )
}

uc_write_draw_storage_manifest <- function(draw_sets, scenario_path) {
  uc_require("data.table")
  pieces <- lapply(names(draw_sets), function(storage_type) {
    paths <- draw_sets[[storage_type]]
    if (!length(paths)) return(NULL)
    draw_ids <- suppressWarnings(as.integer(sub(
      "^draw_([0-9]+)[.]parquet$", "\\1", basename(paths)
    )))
    if (anyNA(draw_ids)) {
      stop(
        "Could not parse one or more draw filenames for storage type ",
        storage_type, ".",
        call. = FALSE
      )
    }
    data.table::data.table(
      storage_type = storage_type,
      draw_id = draw_ids,
      file = normalizePath(paths, winslash = "/", mustWork = TRUE),
      size_bytes = as.numeric(file.info(paths)$size)
    )
  })
  manifest <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  data.table::setorder(manifest, storage_type, draw_id)
  uc_write_csv_atomic(
    manifest,
    file.path(scenario_path, "uncertainty_draw_storage.csv")
  )
  writeLines(
    c(
      "The canonical uncertainty data are the individual Parquet files under:",
      "  draws/                    compact province/national Person outputs",
      "  population_draws/         compact population-group Person outputs",
      "  full_draws/               base-age Male/Female/Person province outputs",
      "  population_full_draws/    base-age Male/Female/Person population-group outputs",
      "",
      "A monolithic uncertainty_draws.parquet is intentionally not created.",
      "The report reads the per-draw files and calculates selected intervals from",
      "the complete joint draws. Compact summaries are calculated in bounded",
      "blocks without loading all draw rows into memory."
    ),
    file.path(scenario_path, "UNCERTAINTY_DRAW_STORAGE.txt")
  )

  consolidated <- file.path(scenario_path, "uncertainty_draws.parquet")
  stale_temporary <- list.files(
    scenario_path,
    pattern = "^uncertainty_draws[.]parquet[.]tmp-",
    full.names = TRUE
  )
  if (file.exists(consolidated)) unlink(consolidated)
  if (length(stale_temporary)) unlink(stale_temporary)
  invisible(manifest)
}

uc_combine_draws <- function(draw_paths) {
  uc_require("arrow")
  uc_require("data.table")
  pieces <- lapply(draw_paths, function(path) {
    data.table::as.data.table(
      arrow::read_parquet(path, as_data_frame = TRUE)
    )
  })
  data.table::rbindlist(pieces, use.names = TRUE)
}

uc_summarise_draws <- function(draws, point_report) {
  keys <- c(
    "scenario", "Death_Prov", "Sex", "DeathYear", "age_group", "cause_id"
  )
  summary <- draws[, .(
    n_draws = data.table::uniqueN(draw_id),
    mean = mean(Deaths),
    median = stats::median(Deaths),
    sd = stats::sd(Deaths),
    lower = as.numeric(stats::quantile(
      Deaths, 0.025, names = FALSE, type = 8
    )),
    upper = as.numeric(stats::quantile(
      Deaths, 0.975, names = FALSE, type = 8
    ))
  ), by = eval(keys)]
  point <- point_report[, .(
    Death_Prov, Sex, DeathYear, age_group, cause_id,
    point = Deaths
  )]
  summary <- merge(
    summary,
    point,
    by = c("Death_Prov", "Sex", "DeathYear", "age_group", "cause_id"),
    all.x = TRUE,
    sort = FALSE
  )
  summary[, `:=`(
    cv = data.table::fifelse(mean > 0, sd / mean, NA_real_),
    interval_width = upper - lower,
    relative_interval_width = data.table::fifelse(
      point > 0,
      (upper - lower) / point,
      NA_real_
    ),
    monte_carlo_se_mean = sd / sqrt(n_draws)
  )]
  data.table::setcolorder(
    summary,
    c(
      keys, "point", "mean", "median", "sd", "cv", "lower", "upper",
      "interval_width", "relative_interval_width", "monte_carlo_se_mean",
      "n_draws"
    )
  )
  summary[]
}

uc_convergence_headlines <- function(draws) {
  n_total <- data.table::uniqueN(draws$draw_id)
  checkpoints <- sort(unique(pmax(1L, c(25L, 50L, n_total))))
  checkpoints <- checkpoints[checkpoints <= n_total]
  headline_causes <- c("all_causes", "all_injuries", "nbd_2")
  x <- draws[
    Death_Prov == 10L & Sex == 3L & age_group == "all_ages" &
      cause_id %in% headline_causes
  ]
  if (!nrow(x)) return(data.table::data.table())

  pieces <- lapply(checkpoints, function(n) {
    x[draw_id <= n, .(
      lower = as.numeric(stats::quantile(
        Deaths, 0.025, names = FALSE, type = 8
      )),
      median = stats::median(Deaths),
      upper = as.numeric(stats::quantile(
        Deaths, 0.975, names = FALSE, type = 8
      ))
    ), by = .(scenario, DeathYear, cause_id)][, n_draws := n]
  })
  out <- data.table::rbindlist(pieces, use.names = TRUE)
  final <- out[n_draws == max(n_draws), .(
    scenario, DeathYear, cause_id,
    final_lower = lower,
    final_median = median,
    final_upper = upper
  )]
  out <- merge(
    out,
    final,
    by = c("scenario", "DeathYear", "cause_id"),
    all.x = TRUE,
    sort = FALSE
  )
  out[, `:=`(
    lower_change_from_final = lower - final_lower,
    median_change_from_final = median - final_median,
    upper_change_from_final = upper - final_upper
  )]
  out[]
}


uc_hiv_cause_covariance_diagnostics <- function(draws, catalog) {
  x <- uc_as_dt(draws)
  required <- c(
    "scenario", "draw_id", "Death_Prov", "Sex", "DeathYear",
    "age_group", "cause_id", "Deaths"
  )
  uc_assert_columns(x, required, "joint uncertainty draw table")
  x <- x[
    Death_Prov == 10L & Sex == 3L & age_group == "all_ages" &
      grepl("^nbd_[0-9]+$", cause_id)
  ]
  if (!nrow(x) || !"nbd_2" %in% x$cause_id) {
    return(data.table::data.table())
  }

  hiv <- x[cause_id == "nbd_2", .(
    scenario, draw_id, DeathYear, hiv_deaths = Deaths
  )]
  other <- x[cause_id != "nbd_2"]
  joined <- merge(
    other,
    hiv,
    by = c("scenario", "draw_id", "DeathYear"),
    all = FALSE,
    sort = FALSE
  )
  if (!nrow(joined)) return(data.table::data.table())

  out <- joined[, {
    cause_sd <- stats::sd(Deaths)
    hiv_sd <- stats::sd(hiv_deaths)
    covariance <- stats::cov(Deaths, hiv_deaths)
    correlation <- if (is.finite(cause_sd) && cause_sd > 0 &&
      is.finite(hiv_sd) && hiv_sd > 0) {
      stats::cor(Deaths, hiv_deaths)
    } else {
      NA_real_
    }
    regression_slope <- if (is.finite(hiv_sd) && hiv_sd > 0) {
      covariance / stats::var(hiv_deaths)
    } else {
      NA_real_
    }
    .(
      n_draws = data.table::uniqueN(draw_id),
      mean_cause_deaths = mean(Deaths),
      sd_cause_deaths = cause_sd,
      mean_hiv_deaths = mean(hiv_deaths),
      sd_hiv_deaths = hiv_sd,
      covariance_with_hiv = covariance,
      correlation_with_hiv = correlation,
      cause_change_per_hiv_death = regression_slope
    )
  }, by = .(scenario, DeathYear, cause_id)]

  labels <- uc_as_dt(catalog)[, .(
    cause_id,
    nbdcode = as.integer(nbdcode),
    cause_label = as.character(label)
  )]
  out <- merge(out, labels, by = "cause_id", all.x = TRUE, sort = FALSE)
  hiv_linked_codes <- sort(unique(c(
    2L,
    as.integer(unlist(HIV_PSEUDO_DESTINATION, use.names = FALSE))
  )))
  out[, hiv_linked_destination := nbdcode %in% hiv_linked_codes]
  data.table::setorder(
    out,
    -hiv_linked_destination,
    DeathYear,
    correlation_with_hiv,
    nbdcode
  )
  out[]
}

uc_prepare_scenario_directory <- function(path, signature, overwrite) {
  signature_path <- file.path(path, "run_signature.txt")
  if (dir.exists(path) && isTRUE(overwrite)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  if (dir.exists(path) && !file.exists(signature_path) &&
      length(list.files(path, all.files = TRUE, no.. = TRUE))) {
    stop(
      "The scenario output directory already contains files but has no run signature. ",
      "Rerun with overwrite enabled or remove the stale directory.",
      call. = FALSE
    )
  }
  if (dir.exists(path) && file.exists(signature_path)) {
    existing_lines <- readLines(signature_path, warn = FALSE)
    existing <- if (length(existing_lines)) trimws(existing_lines[[1L]]) else ""
    if (!identical(existing, signature)) {
      stop(
        "Existing uncertainty draws were created from different code, settings, ",
        "or checkpoints. Rerun with overwrite enabled or use a new output_name.",
        call. = FALSE
      )
    }
  }
  dir.create(file.path(path, "draws"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "full_draws"), recursive = TRUE, showWarnings = FALSE)
  dir.create(
    file.path(path, "population_draws"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(path, "population_full_draws"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(path, "diagnostics"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(signature, signature_path)
  invisible(path)
}

uc_common_seed_map <- function(config) {
  draws <- seq_len(config$run$n_draws)
  out <- data.table::data.table(draw_id = draws)
  for (component in names(UC_COMPONENT_SEED_OFFSETS)) {
    out[, (paste0("seed_", component)) := vapply(
      draw_id,
      function(id) uc_component_seed(config$run$base_seed, id, component),
      integer(1)
    )]
  }
  out[]
}

# Input loading and deterministic reconstruction -------------------------------

uc_load_inputs <- function(root, cfg, uncertainty_config) {
  paths <- list(
    injury_point_fractions = uc_derived_path(
      root, cfg, "03_injury_fractions.parquet"
    ),
    injury_model_artifact = uc_derived_path(
      root,
      cfg,
      uncertainty_config$components$injury$model_file
    ),
    injury_survey_comparison = uc_derived_path(
      root, cfg, "03_injury_survey_model_comparison.parquet"
    ),
    injury_survey_design = uc_derived_path(
      root,
      cfg,
      uncertainty_config$components$injury$survey_design_file
    ),
    injury_envelope_model_artifact = uc_derived_path(
      root,
      cfg,
      uncertainty_config$components$injury$envelope_model_file
    ),
    completeness_scalars = uc_derived_path(
      root, cfg, "04_completeness_scalars.parquet"
    ),
    stage04_pre_injury_envelope = uc_derived_path(
      root,
      cfg,
      "04_investigation_subpopulation_pre_injury_envelope.parquet"
    ),
    stage04 = uc_derived_path(
      root, cfg, "04_investigation_subpopulation.parquet"
    ),
    prevalence = uc_derived_path(
      root, cfg, "05_prevalence_population.parquet"
    ),
    hiv_model_covariance = uc_derived_path(
      root,
      cfg,
      uncertainty_config$components$hiv$covariance_file
    ),
    stage05 = uc_derived_path(
      root, cfg, "05_hiv_reallocated_long.parquet"
    ),
    stage05_wide = uc_derived_path(
      root, cfg, "05_hiv_reallocated_wide.parquet"
    ),
    stage06 = uc_derived_path(
      root, cfg, "06_redistributed_analysis_long.parquet"
    )
  )
  missing <- unlist(paths)[!file.exists(unlist(paths))]
  if (length(missing)) {
    stop(
      "Required uncertainty input(s) are missing:\n- ",
      paste(missing, collapse = "\n- "),
      call. = FALSE
    )
  }

  injury_artifact <- readRDS(paths$injury_model_artifact)
  uc_validate_injury_artifact(injury_artifact)
  injury_envelope_artifact <- readRDS(paths$injury_envelope_model_artifact)
  validate_injury_envelope_artifact(injury_envelope_artifact)

  injury_survey_design <- readRDS(paths$injury_survey_design)
  if (!is.list(injury_survey_design) || is.null(injury_survey_design$ims) ||
      is.null(injury_survey_design$famhis)) {
    stop("The Stage 03 injury survey-design artifact is incomplete.",
         call. = FALSE)
  }

  # Point anchors must be reproducible from the supplied survey microdata.
  # Published estimates remain external audit fields and do not replace or
  # gate the empirical values.
  level_audit <- unique(uc_as_dt(injury_envelope_artifact$anchors)[, .(
    survey_year,
    survey_level_total,
    derived_survey_level_total,
    published_survey_level_total,
    published_survey_level_lower,
    published_survey_level_upper,
    outside_reference_interval
  )])
  uc_assert_unique(level_audit, "survey_year", "injury-level point anchors")
  design_totals <- data.table::data.table(
    survey_year = c(2009L, 2017L),
    design_total = c(
      injury_survey_design$ims$weighted_level_total,
      injury_survey_design$famhis$weighted_level_total
    )
  )
  level_audit <- merge(level_audit, design_totals, by = "survey_year", all.x = TRUE)
  if (level_audit[
    !is.finite(survey_level_total) | survey_level_total <= 0 |
      !is.finite(derived_survey_level_total) |
      !is.finite(design_total) |
      abs(survey_level_total - derived_survey_level_total) >
        1e-10 * pmax(1, abs(derived_survey_level_total)) |
      abs(survey_level_total - design_total) >
        1e-10 * pmax(1, abs(design_total)),
    .N
  ]) {
    stop(
      "The deterministic injury-level anchors do not reproduce the supplied survey microdata.",
      call. = FALSE
    )
  }

  injury_point_fractions <- uc_standardize_injury_fraction_panel(
    uc_read_parquet(
      paths$injury_point_fractions,
      "Stage 03 modelled injury fraction panel"
    ),
    "Stage 03 modelled injury fraction panel"
  )

  completeness_scalars <- uc_read_parquet(
    paths$completeness_scalars,
    "Stage 04 completeness scalars"
  )
  stage04_pre_injury_envelope <- uc_standardize_count_input(
    uc_read_parquet(
      paths$stage04_pre_injury_envelope,
      "Stage 04 pre-injury-envelope subpopulation output"
    ),
    "Stage 04 pre-injury-envelope output"
  )
  stage04 <- uc_standardize_count_input(
    uc_read_parquet(paths$stage04, "Stage 04 subpopulation output"),
    "Stage 04"
  )
  completeness_distribution <- uc_estimate_completeness_distribution(
    completeness_scalars,
    stage04_pre_injury_envelope,
    cfg
  )

  prevalence <- uc_read_parquet(
    paths$prevalence,
    "Stage 05 prevalence-population panel"
  )
  prevalence_key <- c("Death_Prov", "Sex", "DeathYear", "age5")
  uc_assert_columns(
    prevalence,
    c(prevalence_key, "ASSABase", "ANCPrev"),
    "Stage 05 prevalence-population panel"
  )
  prevalence <- prevalence[, .(
    ASSABase = as.numeric(ASSABase),
    ANCPrev = as.numeric(ANCPrev)
  ), by = eval(prevalence_key)]
  uc_assert_unique(
    prevalence,
    prevalence_key,
    "Stage 05 prevalence-population panel"
  )

  hiv_model_covariance <- readRDS(paths$hiv_model_covariance)
  if (!is.list(hiv_model_covariance) ||
      is.null(hiv_model_covariance$background) ||
      is.null(hiv_model_covariance$zero_prevalence)) {
    stop("The Stage 05 HIV covariance artifact is incomplete.", call. = FALSE)
  }

  list(
    paths = paths,
    cfg = cfg,
    injury_artifact = injury_artifact,
    injury_survey_design = injury_survey_design,
    injury_envelope_artifact = injury_envelope_artifact,
    injury_point_fractions = injury_point_fractions,
    completeness_scalars = completeness_scalars,
    stage04_pre_injury_envelope = stage04_pre_injury_envelope,
    completeness_distribution = completeness_distribution,
    prevalence = prevalence,
    hiv_model_covariance = hiv_model_covariance,
    stage04 = stage04,
    stage05 = uc_standardize_count_input(
      uc_read_parquet(paths$stage05, "Stage 05 HIV output"),
      "Stage 05"
    ),
    stage05_wide = uc_read_parquet(
      paths$stage05_wide,
      "Stage 05 HIV wide output"
    ),
    stage06 = uc_standardize_count_input(
      uc_read_parquet(paths$stage06, "Stage 06 redistribution output"),
      "Stage 06"
    )
  )
}

uc_point_reconstruction <- function(
    inputs,
    tolerance,
    absolute_tolerance = 0) {
  injury_fractions <- uc_draw_injury_fraction_panel(
    inputs$injury_artifact,
    seed = 1L,
    stochastic = FALSE
  )
  injury_model_error <- uc_max_table_error(
    uc_injury_fraction_panel_as_counts(
      injury_fractions,
      "reconstructed Stage 03 injury fractions"
    ),
    uc_injury_fraction_panel_as_counts(
      inputs$injury_point_fractions,
      "saved Stage 03 injury fractions"
    ),
    tolerance,
    absolute_tolerance
  )

  # Reconstruct the deterministic IMS/FAMHIS level-plus-profile calibration
  # from the exact Stage 04 boundary at which it was fitted. This verifies the
  # national survey-weighted injury totals, the relative province-sex-age
  # profiles and the time interpolation before stochastic factors are added.
  envelope_point <- apply_injury_envelope_adjustment(
    inputs$stage04_pre_injury_envelope,
    inputs$injury_envelope_artifact$annual_point,
    profile_column = "profile_factor",
    level_column = "point_level_ratio"
  )$data
  injury_envelope_error <- uc_max_table_error(
    envelope_point,
    inputs$stage04,
    tolerance,
    absolute_tolerance
  )

  injury_rebuilt <- uc_replace_injury_composition(
    inputs$stage04,
    injury_fractions
  )$data
  injury_point <- inputs$stage04[nbdcode %in% UC_INJURY_CODES]
  injury_rebuilt <- injury_rebuilt[nbdcode %in% UC_INJURY_CODES]
  injury_stage04_error <- uc_max_table_error(
    injury_rebuilt,
    injury_point,
    tolerance,
    absolute_tolerance
  )

  hiv_point <- run_hiv_reallocation_from_covariance(
    subpopulation = inputs$stage04,
    prevalence = inputs$prevalence,
    cfg = inputs$cfg,
    covariance_artifact = inputs$hiv_model_covariance,
    seed = 1L,
    stochastic = FALSE
  )$data
  hiv_error <- uc_max_table_error(
    hiv_point,
    inputs$stage05,
    tolerance,
    absolute_tolerance
  )

  # Validate Stage 06 at its exact deterministic input boundary. Stage 05 is
  # already validated immediately above against the reconstructed HIV output.
  # The deterministic pipeline reads the saved wide Stage 05 checkpoint; using
  # that same checkpoint here avoids allowing sub-machine floating-point
  # differences in a second reconstruction to select a different biological
  # zero-denominator fallback branch.
  garbage_point <- uc_run_stage06(
    wide = inputs$stage05_wide,
    target_weight_overrides = NULL
  )
  garbage_error <- uc_max_table_error(
    garbage_point$data,
    inputs$stage06,
    tolerance,
    absolute_tolerance
  )

  list(
    injury_model_max_abs_error = injury_model_error$max_abs,
    injury_model_max_relative_error = injury_model_error$max_rel,
    injury_model_cells_over_tolerance = injury_model_error$n_over,
    injury_envelope_point_max_abs_error = injury_envelope_error$max_abs,
    injury_envelope_point_max_relative_error = injury_envelope_error$max_rel,
    injury_envelope_point_cells_over_tolerance = injury_envelope_error$n_over,
    injury_stage04_max_abs_error = injury_stage04_error$max_abs,
    injury_stage04_max_relative_error = injury_stage04_error$max_rel,
    injury_stage04_cells_over_tolerance = injury_stage04_error$n_over,
    hiv_max_abs_error = hiv_error$max_abs,
    hiv_max_relative_error = hiv_error$max_rel,
    hiv_cells_over_tolerance = hiv_error$n_over,
    redistribution_max_abs_error = garbage_error$max_abs,
    redistribution_max_relative_error = garbage_error$max_rel,
    redistribution_cells_over_tolerance = garbage_error$n_over,
    valid = injury_model_error$n_over == 0L &&
      injury_envelope_error$n_over == 0L &&
      injury_stage04_error$n_over == 0L &&
      hiv_error$n_over == 0L &&
      garbage_error$n_over == 0L
  )
}

# Joint draws ------------------------------------------------------------------

uc_run_one_draw <- function(inputs, config, scenario, draw_id, catalog) {
  if (!identical(as.character(scenario), "joint")) {
    stop("Only the joint uncertainty scenario is available.", call. = FALSE)
  }
  base_seed <- config$run$base_seed
  seeds <- lapply(names(UC_COMPONENT_SEED_OFFSETS), function(component) {
    uc_component_seed(base_seed, draw_id, component)
  })
  names(seeds) <- names(UC_COMPONENT_SEED_OFFSETS)
  started <- proc.time()[["elapsed"]]

  # Start at the exact Stage 04 boundary before deterministic injury
  # calibration. Completeness is perturbed first, then one joint survey-design
  # replicate supplies the IMS/FAMHIS injury level, demographic profile and
  # specified cause counts. This avoids applying the point calibration twice.
  completeness <- uc_draw_completeness(
    inputs$stage04_pre_injury_envelope,
    inputs$completeness_distribution,
    stochastic = TRUE,
    seed = seeds$completeness
  )
  current <- completeness$data
  total_after_completeness <- sum(current$Deaths)

  survey_draw <- injury_survey_design_draw(
    inputs$injury_survey_design,
    seed = seeds$injury_envelope,
    stochastic = TRUE
  )
  injury_envelope <- uc_draw_injury_envelope(
    data = current,
    point_artifact = inputs$injury_envelope_artifact,
    survey_draw = survey_draw,
    cfg = inputs$cfg,
    stochastic = TRUE
  )
  current <- injury_envelope$data

  fractions <- uc_draw_injury_fraction_panel(
    inputs$injury_artifact,
    seed = seeds$injury,
    stochastic = TRUE,
    survey_counts = survey_draw$cause_surveys
  )
  injury <- uc_replace_injury_composition(current, fractions)
  current <- injury$data

  before_hiv <- current
  hiv_draw <- run_hiv_reallocation_from_covariance(
    subpopulation = before_hiv,
    prevalence = inputs$prevalence,
    cfg = inputs$cfg,
    covariance_artifact = inputs$hiv_model_covariance,
    seed = seeds$hiv_model,
    stochastic = TRUE
  )
  current <- uc_as_dt(hiv_draw$data)
  uc_assert_columns(current, c(UC_KEY, "Deaths"), "drawn Stage 05 output")
  uc_assert_finite_nonnegative(current, "Deaths", "drawn Stage 05 output")
  uc_assert_unique(current, UC_KEY, "drawn Stage 05 output")
  hiv_conservation <- uc_max_cell_error(before_hiv, current, UC_CELL_KEY)

  target_weight_overrides <- stage06_draw_target_weight_overrides(
    seeds$redistribution
  )
  before_redistribution <- uc_add_envelope(current)
  redistribution <- uc_run_stage06(
    current,
    target_weight_overrides = target_weight_overrides
  )
  final <- redistribution$data
  after_redistribution <- uc_add_envelope(final)
  redistribution_conservation <- uc_max_cell_error(
    before_redistribution,
    after_redistribution,
    c(UC_CELL_KEY, "envelope__")
  )
  redistribution_summary <- uc_redistribution_audit_summary(
    redistribution$target_audit
  )

  survey_diag <- uc_as_dt(survey_draw$diagnostics)
  ims_level <- survey_diag[survey_year == 2009L, level_total]
  famhis_level <- survey_diag[survey_year == 2017L, level_total]
  ims_selected <- survey_diag[survey_year == 2009L, distinct_selected_psus]
  famhis_selected <- survey_diag[survey_year == 2017L, distinct_selected_psus]

  total_final <- sum(final$Deaths)
  total_propagation_error <- abs(total_final - total_after_completeness)
  total_propagation_relative_error <- total_propagation_error /
    max(1, abs(total_after_completeness))
  reportable_total <- final[nbdcode %in% 1:214, sum(Deaths)]
  nonreportable_total <- final[!nbdcode %in% 1:214, sum(Deaths)]
  nonreportable_relative <- abs(nonreportable_total) / max(1, abs(total_final))
  nonnegative <- final[!is.finite(Deaths) | Deaths < -1e-9, .N] == 0L
  elapsed <- proc.time()[["elapsed"]] - started

  diagnostics <- data.table::data.table(
    version = NBD3_UNCERTAINTY_VERSION,
    profile = config$run$profile,
    scenario = scenario,
    draw_id = as.integer(draw_id),
    seed = seeds$draw,
    seed_completeness = seeds$completeness,
    seed_injury_survey_design = seeds$injury_envelope,
    seed_nims_composition = seeds$injury,
    seed_hiv_model = seeds$hiv_model,
    seed_redistribution = seeds$redistribution,
    seconds = elapsed,
    stochastic_completeness = TRUE,
    stochastic_injury_survey_design = TRUE,
    stochastic_nims_composition = TRUE,
    stochastic_hiv = TRUE,
    stochastic_redistribution = TRUE,
    completeness_empirical_log_sd = completeness$diagnostics$log_sd,
    completeness_province_log_sd_min = completeness$diagnostics$log_sd_min,
    completeness_province_log_sd_median = completeness$diagnostics$log_sd_median,
    completeness_province_log_sd_max = completeness$diagnostics$log_sd_max,
    completeness_factor_min = completeness$diagnostics$factor_min,
    completeness_factor_max = completeness$diagnostics$factor_max,
    completeness_factor_mean = completeness$diagnostics$factor_mean,
    completeness_affected_before = completeness$diagnostics$affected_before,
    completeness_affected_after = completeness$diagnostics$affected_after,
    survey_ims_2009_level_total = ims_level,
    survey_famhis_2017_level_total = famhis_level,
    survey_ims_distinct_selected_psus = ims_selected,
    survey_famhis_distinct_selected_psus = famhis_selected,
    total_stage04_point = sum(inputs$stage04$Deaths),
    total_after_completeness = total_after_completeness,
    total_final = total_final,
    reportable_total = reportable_total,
    nonreportable_total = nonreportable_total,
    nonreportable_relative = nonreportable_relative,
    total_propagation_error = total_propagation_error,
    total_propagation_relative_error = total_propagation_relative_error,
    injury_level_total_max_abs_error = injury_envelope$diagnostics$maximum_total_absolute_error,
    injury_level_total_max_relative_error = injury_envelope$diagnostics$maximum_total_relative_error,
    injury_level_profile_factor_min = injury_envelope$diagnostics$minimum_profile_factor,
    injury_level_profile_factor_max = injury_envelope$diagnostics$maximum_profile_factor,
    injury_level_ratio_min = injury_envelope$diagnostics$minimum_level_ratio,
    injury_level_ratio_max = injury_envelope$diagnostics$maximum_level_ratio,
    injury_level_deaths_before = injury_envelope$diagnostics$injury_before,
    injury_level_deaths_after = injury_envelope$diagnostics$injury_after,
    injury_envelope_max_abs_error = injury$diagnostics$max_envelope_abs_error,
    injury_envelope_max_relative_error = injury$diagnostics$max_envelope_relative_error,
    hiv_conservation_max_abs_error = hiv_conservation$max_abs,
    hiv_conservation_max_relative_error = hiv_conservation$max_rel,
    redistribution_conservation_max_abs_error = redistribution_conservation$max_abs,
    redistribution_conservation_max_relative_error = redistribution_conservation$max_rel,
    hiv_draw_modelled_deaths = hiv_draw$modelled_hiv_deaths,
    redistribution_rules = redistribution_summary$rules,
    redistribution_stochastic_rules = redistribution_summary$stochastic_rules,
    redistribution_mean_multiplier_cv = redistribution_summary$mean_multiplier_cv,
    redistribution_min_multiplier = redistribution_summary$min_multiplier,
    redistribution_max_multiplier = redistribution_summary$max_multiplier,
    redistribution_mean_multiplier = redistribution_summary$mean_multiplier,
    redistribution_target_count = redistribution_summary$target_count,
    final_nonnegative = nonnegative
  )

  tolerance <- config$run$conservation_tolerance
  valid <- nonnegative &&
    injury_envelope$diagnostics$maximum_total_relative_error <= tolerance &&
    injury$diagnostics$max_envelope_relative_error <= tolerance &&
    hiv_conservation$max_rel <= tolerance &&
    redistribution_conservation$max_rel <= tolerance &&
    total_propagation_relative_error <= tolerance &&
    nonreportable_relative <= tolerance
  diagnostics[, valid := valid]
  if (!valid && isTRUE(config$run$stop_on_validation_failure)) {
    stop(
      "Uncertainty draw ", draw_id,
      " failed conservation or non-negativity checks.",
      call. = FALSE
    )
  }

  list(
    report = uc_report_draw(final, catalog, config, scenario, draw_id),
    population_report = if (isTRUE(config$reporting$include_population_groups)) {
      uc_report_population_draw(final, catalog, config, scenario, draw_id)
    } else {
      data.table::data.table()
    },
    full_report = if (isTRUE(config$reporting$full_ui_enabled)) {
      uc_report_full_draw(final, config, scenario, draw_id)
    } else {
      data.table::data.table()
    },
    full_population_report = if (
      isTRUE(config$reporting$full_ui_enabled) &&
      isTRUE(config$reporting$include_population_groups)
    ) {
      uc_report_full_population_draw(final, config, scenario, draw_id)
    } else {
      data.table::data.table()
    },
    diagnostics = diagnostics,
    completeness_factors = completeness$factors,
    injury_envelope_factors = injury_envelope$factors,
    redistribution_audit = redistribution$target_audit
  )
}

uc_preflight_table <- function(inputs, reconstruction, config) {
  multi_target_rules <- stage06_redistribution_rules()
  multi_target_rules <- sum(vapply(
    multi_target_rules,
    function(rule) length(rule$targets) > 1L,
    logical(1)
  ))
  envelope_anchors <- uc_as_dt(inputs$injury_envelope_artifact$anchors)
  envelope_annual <- uc_as_dt(inputs$injury_envelope_artifact$annual_point)
  info <- data.table::data.table(
    section = "checkpoint",
    metric = c(
      "stage03_injury_fraction_rows",
      "stage03_injury_method",
      "stage03_injury_trajectories",
      "injury_envelope_method",
      "injury_envelope_anchor_rows",
      "injury_envelope_pre_2009_policy",
      "injury_envelope_between_policy",
      "injury_envelope_post_2017_policy",
      "injury_envelope_point_profile_factor_min",
      "injury_envelope_point_profile_factor_max",
      "injury_envelope_point_level_ratio_min",
      "injury_envelope_point_level_ratio_max",
      "stage04_pre_injury_envelope_rows",
      "completeness_death_weighted_rms_log_sd",
      "completeness_minimum_province_log_sd",
      "completeness_median_province_log_sd",
      "completeness_maximum_province_log_sd",
      "completeness_factor_q025_min",
      "completeness_factor_q975_max",
      "completeness_eligible_rows",
      "completeness_eligible_strata",
      "stage04_rows",
      "stage05_prevalence_rows",
      "stage05_hiv_covariance_models",
      "stage05_rows",
      "stage06_rows",
      "redistribution_multi_target_rules"
    ),
    value = as.character(c(
      nrow(inputs$injury_point_fractions),
      inputs$injury_artifact$parameters$method,
      inputs$injury_artifact$trajectory_count,
      inputs$injury_envelope_artifact$method,
      nrow(envelope_anchors),
      inputs$injury_envelope_artifact$parameters$pre_2009_policy,
      inputs$injury_envelope_artifact$parameters$between_policy,
      inputs$injury_envelope_artifact$parameters$post_2017_policy,
      signif(min(envelope_annual$profile_factor), 12),
      signif(max(envelope_annual$profile_factor), 12),
      signif(min(envelope_annual$point_level_ratio), 12),
      signif(max(envelope_annual$point_level_ratio), 12),
      nrow(inputs$stage04_pre_injury_envelope),
      signif(inputs$completeness_distribution$log_sd, 12),
      signif(inputs$completeness_distribution$log_sd_min, 12),
      signif(inputs$completeness_distribution$log_sd_median, 12),
      signif(inputs$completeness_distribution$log_sd_max, 12),
      signif(min(inputs$completeness_distribution$province_summary$factor_q025), 12),
      signif(max(inputs$completeness_distribution$province_summary$factor_q975), 12),
      inputs$completeness_distribution$eligible_rows,
      inputs$completeness_distribution$eligible_strata,
      nrow(inputs$stage04),
      nrow(inputs$prevalence),
      length(inputs$hiv_model_covariance$background$models),
      nrow(inputs$stage05),
      nrow(inputs$stage06),
      multi_target_rules
    )),
    tolerance = NA_character_,
    status = "INFO"
  )
  reconstruction_rows <- data.table::data.table(
    section = "point_reconstruction",
    metric = names(reconstruction),
    value = vapply(reconstruction, as.character, character(1)),
    tolerance = as.character(max(
      config$run$conservation_tolerance,
      config$run$reconstruction_relative_tolerance,
      config$run$reconstruction_absolute_tolerance
    )),
    status = data.table::fifelse(
      names(reconstruction) == "valid",
      data.table::fifelse(isTRUE(reconstruction$valid), "PASS", "FAIL"),
      "INFO"
    )
  )
  data.table::rbindlist(
    list(info, reconstruction_rows),
    use.names = TRUE,
    fill = TRUE
  )
}

uc_run_signature <- function(root, inputs, config) {
  signature_config <- config
  signature_config$run$n_draws <- NULL
  signature_config$run$overwrite <- NULL
  signature_config$run$stop_on_validation_failure <- NULL
  code_paths <- c(
    sort(list.files(
      file.path(root, "R"),
      pattern = "^0[0-6]_.*\\.R$",
      full.names = TRUE
    )),
    file.path(root, "config", "config.yml"),
    file.path(root, "config", "uncertainty_joint.yml")
  )
  lines <- c(
    paste0("version=", NBD3_UNCERTAINTY_VERSION),
    paste0("effective_config=", capture.output(dput(signature_config))),
    paste0(
      normalizePath(code_paths, winslash = "/"),
      "=",
      unname(tools::md5sum(code_paths))
    ),
    vapply(inputs$paths, uc_file_signature, character(1)),
    paste0(
      "completeness_province_summary=",
      paste(
        signif(c(
          inputs$completeness_distribution$province_summary$aggregate_s2,
          inputs$completeness_distribution$province_summary$log_sd,
          inputs$completeness_distribution$province_summary$pre_adjustment_deaths,
          inputs$completeness_distribution$province_summary$adjusted_deaths
        ), 17),
        collapse = ","
      )
    )
  )
  temporary <- tempfile(fileext = ".txt")
  writeLines(lines, temporary)
  on.exit(unlink(temporary), add = TRUE)
  unname(tools::md5sum(temporary))
}

uc_write_manifest <- function(
    path,
    signature,
    config_path,
    inputs,
    reconstruction,
    config) {
  drivers <- c(
    paste0(
      "under_reporting=one independent mean-one factor per province; each province-specific log standard deviation is the death-weighted dispersion of annual aggregate log S2 across time after sex and age are collapsed; province log-SD range ",
      signif(inputs$completeness_distribution$log_sd_min, 8),
      "-",
      signif(inputs$completeness_distribution$log_sd_max, 8)
    ),
    "injury_level_and_profile=published-compatible IMS 2009 and later-cleaned FAMHIS 2017 eligibility rules and supplied weights determine empirical national and province-sex-age anchors; one stratified PSU bootstrap replicate per survey jointly propagates level and profile uncertainty; the IMS correction is held through 2009, log components are interpolated to 2017, and FAMHIS is held thereafter",
    "injury_composition=well-specified IMS 2009 and FAMHIS 2017 cause counts come from the same stratified PSU replicate used for injury level/profile; the national NIMS 2000 sex-age composition is sampled from count information before IMS-based spatial expansion; causes 132, 136 and 138 are then harmonised before hierarchical ALR interpolation and the triangular moving average; no additional path-variance parameter is introduced",
    "hiv=fitted Stage 05 coefficient draws from model variance-covariance matrices after completeness and injury-envelope draws; ANC prevalence fixed because no variance input is supplied; preserves deaths within demographic cells",
    "redistribution=one continuous positive Gamma multiplier vector per multi-target expert rule; multipliers are centred on the deterministic all-one expert vector and marginal variance is moment-matched to the former non-empty-subset support; the weighted proportional algorithm preserves natural and injury envelopes",
    "reporting=compact Person draw files are retained for model-comparison compatibility, while wide base-age files retain Male, Female and Person deaths for provinces, South Africa and national population groups; the report derives deaths, crude rates and all-age ASRs within each draw",
    "fixed_without_variance_source=S1 completeness, independent NPR-specific variance, ANC prevalence, and additional injury path variance"
  )
  rows <- data.table::rbindlist(list(
    data.table::data.table(
      item = "module_version",
      value = NBD3_UNCERTAINTY_VERSION
    ),
    data.table::data.table(item = "profile", value = config$run$profile),
    data.table::data.table(
      item = "output_name",
      value = config$run$output_name
    ),
    data.table::data.table(item = "run_signature", value = signature),
    data.table::data.table(
      item = "configuration",
      value = normalizePath(config_path, winslash = "/")
    ),
    data.table::data.table(
      item = paste0("input_", names(inputs$paths)),
      value = vapply(inputs$paths, uc_file_signature, character(1))
    ),
    data.table::data.table(
      item = names(reconstruction),
      value = vapply(reconstruction, as.character, character(1))
    ),
    data.table::data.table(
      item = "configured_draws",
      value = as.character(config$run$n_draws)
    ),
    data.table::data.table(
      item = "base_seed",
      value = as.character(config$run$base_seed)
    ),
    data.table::data.table(
      item = paste0("uncertainty_driver_", seq_along(drivers)),
      value = drivers
    )
  ), use.names = TRUE, fill = TRUE)
  uc_write_csv_atomic(rows, file.path(path, "run_manifest.csv"))
}

# Public runner ----------------------------------------------------------------

run_nbd3_uncertainty <- function(
    root,
    cfg,
    uncertainty_cfg,
    uncertainty_config_path = file.path(
      root, "config", "uncertainty_joint.yml"
    )) {
  uc_require("data.table")
  uc_require("arrow")
  supplied <- if (is.null(uncertainty_cfg)) list() else uncertainty_cfg
  config <- uc_validate_config(
    uc_recursive_defaults(uc_default_config(), supplied)
  )
  if (!is.null(cfg$settings$n_threads)) {
    data.table::setDTthreads(as.integer(cfg$settings$n_threads))
  }

  message(
    "NBD3 joint uncertainty v", NBD3_UNCERTAINTY_VERSION,
    " [", config$run$profile, "]"
  )
  message("Loading validated Stage 03-06 checkpoints...")
  inputs <- uc_load_inputs(root, cfg, config)
  message(
    "Completeness: drawing one mean-one factor per province. Each province-specific log SD is estimated from death-weighted annual aggregate S2 variation across time after age and sex are collapsed (log-SD range ",
    signif(inputs$completeness_distribution$log_sd_min, 6),
    "-",
    signif(inputs$completeness_distribution$log_sd_max, 6),
    ")."
  )
  message(
    "Injuries: using published-compatible IMS 2009 and later-cleaned ",
    "FAMHIS 2017 eligibility rules and supplied weights. One stratified PSU ",
    "bootstrap replicate per survey jointly determines the national injury ",
    "level, province-sex-age profile and well-specified cause counts; NIMS ",
    "contributes count-based cause-fraction uncertainty."
  )
  message(
    "HIV/AIDS: drawing fitted Stage 05 coefficients from their variance-covariance matrices after completeness and injury level/profile calibration."
  )
  message(
    "Redistribution: drawing continuous positive multiplier vectors on the ",
    "same expert-approved target lists and applying weighted proportional ",
    "redistribution."
  )
  if (isTRUE(config$reporting$include_population_groups)) {
    message(
      paste(
        "Reporting: retaining compact Person outputs plus full base-age",
        "Male, Female and Person deaths for provinces, South Africa and",
        "national population groups in every draw."
      )
    )
  }

  reconstruction <- uc_point_reconstruction(
    inputs,
    config$run$reconstruction_relative_tolerance,
    config$run$reconstruction_absolute_tolerance
  )

  output_root <- file.path(
    root,
    "output",
    "uncertainty",
    config$run$output_name
  )
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  uc_write_csv_atomic(
    uc_completeness_distribution_summary(
      inputs$completeness_distribution
    ),
    file.path(output_root, "completeness_empirical_distribution.csv")
  )
  uc_write_parquet_atomic(
    inputs$completeness_distribution$weighted_cells,
    file.path(output_root, "completeness_weighted_cells.parquet")
  )
  uc_write_csv_atomic(
    inputs$completeness_distribution$factor_support,
    file.path(output_root, "completeness_factor_support.csv")
  )
  uc_write_csv_atomic(
    inputs$completeness_distribution$province_summary,
    file.path(output_root, "completeness_by_province.csv")
  )
  uc_write_csv_atomic(
    inputs$injury_envelope_artifact$anchors,
    file.path(output_root, "injury_envelope_anchors.csv")
  )
  uc_write_parquet_atomic(
    inputs$injury_envelope_artifact$annual_point,
    file.path(output_root, "injury_envelope_point_factors.parquet")
  )
  if (exists("read_asr_factors", mode = "function", inherits = TRUE)) {
    uc_write_csv_atomic(
      read_asr_factors(inputs$cfg),
      file.path(output_root, "asr_factors.csv")
    )
  }

  preflight <- uc_preflight_table(inputs, reconstruction, config)
  uc_write_csv_atomic(
    preflight,
    file.path(output_root, "preflight_validation.csv")
  )
  if (!isTRUE(reconstruction$valid)) {
    stop(
      "Point-reconstruction validation failed. Review ",
      file.path(output_root, "preflight_validation.csv"),
      "; stochastic draws were not run.",
      call. = FALSE
    )
  }
  message(
    "Point reconstruction passed: deterministic injury interpolation max relative error ",
    signif(reconstruction$injury_model_max_relative_error, 4),
    "; injury-envelope calibration max relative error ",
    signif(reconstruction$injury_envelope_point_max_relative_error, 4),
    "; Stage 04 injury max relative error ",
    signif(reconstruction$injury_stage04_max_relative_error, 4),
    "; HIV max relative error ",
    signif(reconstruction$hiv_max_relative_error, 4),
    "; redistribution max relative error ",
    signif(reconstruction$redistribution_max_relative_error, 4), "."
  )

  catalog <- uc_build_cause_catalog(
    root,
    inputs$stage06,
    config$reporting$top_n_causes
  )
  point_report <- uc_report_draw(
    inputs$stage06,
    catalog,
    config,
    scenario = "point",
    draw_id = 0L
  )
  point_population_report <- if (isTRUE(
    config$reporting$include_population_groups
  )) {
    uc_report_population_draw(
      inputs$stage06,
      catalog,
      config,
      scenario = "point",
      draw_id = 0L
    )
  } else {
    data.table::data.table()
  }
  point_full_report <- if (isTRUE(config$reporting$full_ui_enabled)) {
    uc_report_full_draw(
      inputs$stage06, config, scenario = "point", draw_id = 0L
    )
  } else {
    data.table::data.table()
  }
  point_full_population_report <- if (
    isTRUE(config$reporting$full_ui_enabled) &&
    isTRUE(config$reporting$include_population_groups)
  ) {
    uc_report_full_population_draw(
      inputs$stage06, config, scenario = "point", draw_id = 0L
    )
  } else {
    data.table::data.table()
  }
  signature <- uc_run_signature(root, inputs, config)
  uc_write_csv_atomic(catalog, file.path(output_root, "cause_catalog.csv"))
  uc_write_parquet_atomic(
    point_report,
    file.path(output_root, "point_report.parquet")
  )
  if (nrow(point_population_report)) {
    uc_write_parquet_atomic(
      point_population_report,
      file.path(output_root, "population_point_report.parquet")
    )
  }
  if (nrow(point_full_report)) {
    uc_write_parquet_atomic(
      point_full_report,
      file.path(output_root, "full_point_report.parquet")
    )
  }
  if (nrow(point_full_population_report)) {
    uc_write_parquet_atomic(
      point_full_population_report,
      file.path(output_root, "population_full_point_report.parquet")
    )
  }
  uc_write_csv_atomic(
    uc_common_seed_map(config),
    file.path(output_root, "common_component_seeds.csv")
  )

  scenario_results <- vector("list", length(config$run$scenarios))
  names(scenario_results) <- config$run$scenarios
  for (scenario in config$run$scenarios) {
    scenario_path <- file.path(output_root, scenario)
    uc_prepare_scenario_directory(
      scenario_path,
      signature,
      config$run$overwrite
    )
    uc_write_manifest(
      scenario_path,
      signature,
      uncertainty_config_path,
      inputs,
      reconstruction,
      config
    )

    n_draws <- config$run$n_draws
    message("Scenario '", scenario, "': ", n_draws, " joint draw(s).")
    width <- max(3L, nchar(as.character(n_draws)))
    draw_paths <- character(n_draws)
    population_draw_paths <- character(n_draws)
    full_draw_paths <- character(n_draws)
    population_full_draw_paths <- character(n_draws)
    diagnostic_paths <- character(n_draws)
    completeness_factor_paths <- character(n_draws)
    injury_envelope_factor_paths <- character(n_draws)
    redistribution_audit_paths <- character(n_draws)

    for (draw_id in seq_len(n_draws)) {
      draw_name <- sprintf(paste0("draw_%0", width, "d"), draw_id)
      draw_path <- file.path(
        scenario_path,
        "draws",
        paste0(draw_name, ".parquet")
      )
      population_draw_path <- file.path(
        scenario_path,
        "population_draws",
        paste0(draw_name, ".parquet")
      )
      full_draw_path <- file.path(
        scenario_path,
        "full_draws",
        paste0(draw_name, ".parquet")
      )
      population_full_draw_path <- file.path(
        scenario_path,
        "population_full_draws",
        paste0(draw_name, ".parquet")
      )
      diagnostic_path <- file.path(
        scenario_path,
        "diagnostics",
        paste0(draw_name, ".csv")
      )
      completeness_factor_path <- file.path(
        scenario_path,
        "diagnostics",
        paste0(draw_name, "_completeness_factors.csv")
      )
      injury_envelope_factor_path <- file.path(
        scenario_path,
        "diagnostics",
        paste0(draw_name, "_injury_envelope_factors.csv")
      )
      redistribution_audit_path <- file.path(
        scenario_path,
        "diagnostics",
        paste0(draw_name, "_redistribution_target_weights.csv")
      )
      draw_paths[[draw_id]] <- draw_path
      population_draw_paths[[draw_id]] <- population_draw_path
      full_draw_paths[[draw_id]] <- full_draw_path
      population_full_draw_paths[[draw_id]] <- population_full_draw_path
      diagnostic_paths[[draw_id]] <- diagnostic_path
      completeness_factor_paths[[draw_id]] <- completeness_factor_path
      injury_envelope_factor_paths[[draw_id]] <- injury_envelope_factor_path
      redistribution_audit_paths[[draw_id]] <- redistribution_audit_path

      complete_files <- c(
        draw_path,
        if (isTRUE(config$reporting$include_population_groups)) {
          population_draw_path
        },
        if (isTRUE(config$reporting$full_ui_enabled)) {
          full_draw_path
        },
        if (
          isTRUE(config$reporting$full_ui_enabled) &&
          isTRUE(config$reporting$include_population_groups)
        ) {
          population_full_draw_path
        },
        diagnostic_path,
        completeness_factor_path,
        injury_envelope_factor_path,
        redistribution_audit_path
      )
      if (all(file.exists(complete_files)) && !config$run$overwrite) {
        message("  draw ", draw_id, "/", n_draws, " already complete")
        next
      }

      result <- uc_run_one_draw(
        inputs,
        config,
        scenario,
        draw_id,
        catalog
      )
      uc_write_parquet_atomic(result$report, draw_path)
      if (isTRUE(config$reporting$include_population_groups)) {
        uc_write_parquet_atomic(
          result$population_report,
          population_draw_path
        )
      }
      if (isTRUE(config$reporting$full_ui_enabled)) {
        uc_write_parquet_atomic(result$full_report, full_draw_path)
      }
      if (
        isTRUE(config$reporting$full_ui_enabled) &&
        isTRUE(config$reporting$include_population_groups)
      ) {
        uc_write_parquet_atomic(
          result$full_population_report,
          population_full_draw_path
        )
      }
      uc_write_csv_atomic(result$diagnostics, diagnostic_path)
      uc_write_csv_atomic(
        result$completeness_factors,
        completeness_factor_path
      )
      uc_write_csv_atomic(
        result$injury_envelope_factors,
        injury_envelope_factor_path
      )
      uc_write_csv_atomic(
        result$redistribution_audit,
        redistribution_audit_path
      )
      message(
        "  draw ", draw_id, "/", n_draws, " complete (",
        round(result$diagnostics$seconds, 1), " s)"
      )
      rm(result)
      if (draw_id %% 5L == 0L) invisible(gc(verbose = FALSE))
    }

    required_outputs <- c(
      draw_paths,
      if (isTRUE(config$reporting$include_population_groups)) {
        population_draw_paths
      },
      if (isTRUE(config$reporting$full_ui_enabled)) {
        full_draw_paths
      },
      if (
        isTRUE(config$reporting$full_ui_enabled) &&
        isTRUE(config$reporting$include_population_groups)
      ) {
        population_full_draw_paths
      },
      diagnostic_paths,
      completeness_factor_paths,
      injury_envelope_factor_paths,
      redistribution_audit_paths
    )
    if (any(!file.exists(required_outputs))) {
      stop("One or more uncertainty draw outputs are missing.", call. = FALSE)
    }

    compact_final <- uc_summarise_draw_files(
      draw_paths = draw_paths,
      point_report = point_report,
      catalog = catalog,
      scenario_path = scenario_path,
      block_size = 2000L
    )
    diagnostics <- uc_read_csv_stack(diagnostic_paths)
    if (diagnostics[valid != TRUE, .N]) {
      stop("One or more completed draws failed validation.", call. = FALSE)
    }
    summary <- compact_final$summary
    convergence <- compact_final$convergence
    hiv_cause_covariance <- compact_final$hiv_cause_covariance
    completeness_factors <- uc_read_csv_stack(
      completeness_factor_paths,
      add_draw_id = TRUE
    )
    injury_envelope_factors <- uc_read_csv_stack(
      injury_envelope_factor_paths,
      add_draw_id = TRUE
    )
    redistribution_audit <- uc_read_csv_stack(
      redistribution_audit_paths,
      add_draw_id = TRUE
    )

    uc_write_draw_storage_manifest(
      list(
        compact_province = draw_paths,
        compact_population_group = if (
          isTRUE(config$reporting$include_population_groups)
        ) population_draw_paths else character(),
        full_province = if (
          isTRUE(config$reporting$full_ui_enabled)
        ) full_draw_paths else character(),
        full_population_group = if (
          isTRUE(config$reporting$full_ui_enabled) &&
          isTRUE(config$reporting$include_population_groups)
        ) population_full_draw_paths else character()
      ),
      scenario_path
    )
    uc_write_parquet_atomic(
      summary,
      file.path(scenario_path, "uncertainty_summary.parquet")
    )
    uc_write_csv_atomic(
      summary,
      file.path(scenario_path, "uncertainty_summary.csv")
    )
    uc_write_csv_atomic(
      diagnostics,
      file.path(scenario_path, "draw_diagnostics.csv")
    )
    uc_write_csv_atomic(
      convergence,
      file.path(scenario_path, "convergence_headlines.csv")
    )
    uc_write_csv_atomic(
      completeness_factors,
      file.path(scenario_path, "completeness_factors.csv")
    )
    uc_write_parquet_atomic(
      injury_envelope_factors,
      file.path(scenario_path, "injury_envelope_factors.parquet")
    )
    uc_write_csv_atomic(
      redistribution_audit,
      file.path(scenario_path, "redistribution_target_weights.csv")
    )
    uc_write_csv_atomic(
      hiv_cause_covariance,
      file.path(scenario_path, "hiv_cause_covariance.csv")
    )

    scenario_results[[scenario]] <- list(
      profile = config$run$profile,
      scenario = scenario,
      output_path = scenario_path,
      n_draws = n_draws,
      mean_seconds = mean(diagnostics$seconds),
      max_validation_abs_error = uc_safe_max(unlist(
        diagnostics[, intersect(
          c(
            "injury_level_total_max_abs_error",
            "injury_envelope_max_abs_error",
            "hiv_conservation_max_abs_error",
            "redistribution_conservation_max_abs_error",
            "total_propagation_error"
          ),
          names(diagnostics)
        ), with = FALSE],
        use.names = FALSE
      )),
      max_validation_relative_error = uc_safe_max(unlist(
        diagnostics[, intersect(
          c(
            "injury_level_total_max_relative_error",
            "injury_envelope_max_relative_error",
            "hiv_conservation_max_relative_error",
            "redistribution_conservation_max_relative_error",
            "total_propagation_relative_error"
          ),
          names(diagnostics)
        ), with = FALSE],
        use.names = FALSE
      ))
    )
    message(
      "Scenario '", scenario, "' complete. Mean draw runtime: ",
      round(scenario_results[[scenario]]$mean_seconds, 1), " s."
    )
    rm(
      compact_final,
      diagnostics,
      summary,
      convergence,
      completeness_factors,
      injury_envelope_factors,
      redistribution_audit,
      hiv_cause_covariance
    )
    invisible(gc(verbose = FALSE))
  }

  overview <- data.table::rbindlist(
    lapply(scenario_results, data.table::as.data.table),
    use.names = TRUE,
    fill = TRUE
  )
  uc_write_csv_atomic(
    overview,
    file.path(output_root, "run_overview.csv")
  )

  invisible(list(
    profile = config$run$profile,
    output_root = output_root,
    scenarios = scenario_results
  ))
}
