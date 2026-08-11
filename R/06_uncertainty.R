# ==============================================================================
# 06_uncertainty: Joint uncertainty propagation
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
#   1. African natural-cause completeness, using the observed cross-province
#      variation in the deterministic S2 completeness inputs;
#   2. injury cause composition, using survey-composition draws based on the
#      available count/Kish-equivalent information and rerunning the same fast
#      Stage 03 interpolation and moving-average smoother;
#   3. HIV/AIDS reallocation, using the fitted Stage 05 coefficient
#      variance-covariance matrices; and
#   4. garbage/ill-defined redistribution, using uniformly sampled non-empty
#      subsets of the same expert-approved target lists used by Stage 06.
#
# The injury component introduces no free concentration multiplier and no
# between-survey bridge variance. Conditional on each survey draw, the annual
# path is the deterministic linear-ALR interpolation plus triangular smoother.
# Components without an empirical variance source (S1, the NPR envelope, ANC
# prevalence) remain fixed and are listed explicitly in the run manifest.

NBD3_UNCERTAINTY_VERSION <- "1.0.0"

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
      output_name = "nbd3_v1_joint",
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
        distribution = "cluster_bootstrap_national_mean",
        draw_level = "national_shared"
      ),
      injury = list(
        enabled = TRUE,
        model_file = "03_injury_fraction_model.rds",
        sampling_distribution = "dirichlet_effective_n",
        inter_survey_path = "deterministic_linear_plus_smoother"
      ),
      hiv = list(
        enabled = TRUE,
        coefficient_covariance = TRUE,
        covariance_file = "05_hiv_model_covariance.rds",
        prevalence_uncertainty = "fixed"
      ),
      redistribution = list(
        enabled = TRUE,
        target_subset_distribution = "uniform_nonempty",
        target_scope = "rule"
      )
    ),
    reporting = list(
      top_n_causes = 214L,
      include_all_provinces = TRUE,
      include_population_groups = TRUE,
      sexes = 3L,
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
    "cluster_bootstrap_national_mean"
  )) {
    stop(
      "Completeness uncertainty must use the cluster bootstrap of the national S2 mean.",
      call. = FALSE
    )
  }
  if (!identical(as.character(completeness$draw_level), "national_shared")) {
    stop("Completeness uncertainty must use one shared national factor per draw.", call. = FALSE)
  }
  config$components$completeness <- completeness

  injury <- config$components$injury
  injury$model_file <- trimws(as.character(injury$model_file))
  if (length(injury$model_file) != 1L || !nzchar(injury$model_file) ||
      grepl("[/\\\\]", injury$model_file)) {
    stop("components.injury.model_file must be one filename.", call. = FALSE)
  }
  injury$sampling_distribution <- tolower(trimws(as.character(
    injury$sampling_distribution
  )))
  if (!identical(injury$sampling_distribution, "dirichlet_effective_n")) {
    stop(
      "Injury uncertainty must use the survey count/Kish effective-N Dirichlet draw.",
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
      "The injury inter-survey path must rerun the deterministic linear interpolation and smoother.",
      call. = FALSE
    )
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
  if (!identical(
    as.character(redistribution$target_subset_distribution),
    "uniform_nonempty"
  )) {
    stop(
      "Redistribution uncertainty must sample uniformly from non-empty target subsets.",
      call. = FALSE
    )
  }
  if (!identical(as.character(redistribution$target_scope), "rule")) {
    stop("One redistribution target subset is drawn per expert rule.",
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

  # Estimate one aggregate S2 multiplier for each province using the
  # pre-adjustment African natural deaths as weights. The nine province values
  # are evidence about the uncertainty of the national completeness level;
  # they are not nine interchangeable errors to multiply back onto the nine
  # already-adjusted province estimates independently.
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
      "No positive pre-freeze African natural deaths are available to weight S2.",
      call. = FALSE
    )
  }
  if (!identical(sort(unique(eligible$Death_Prov)), 1:9)) {
    stop(
      "The completeness bootstrap requires all nine provinces.",
      call. = FALSE
    )
  }

  province_summary <- eligible[, .(
    eligible_cells = .N,
    pre_adjustment_deaths = sum(pre_adjustment_african_natural),
    adjusted_deaths = sum(adjusted_african_natural),
    aggregate_s2 = sum(adjusted_african_natural) /
      sum(pre_adjustment_african_natural),
    minimum_s2 = min(S2),
    median_s2 = stats::median(S2),
    maximum_s2 = max(S2)
  ), by = Death_Prov]
  data.table::setorder(province_summary, Death_Prov)
  if (province_summary[
    !is.finite(aggregate_s2) | aggregate_s2 <= 0 |
      !is.finite(pre_adjustment_deaths) | pre_adjustment_deaths <= 0,
    .N
  ]) {
    stop(
      "A province-level aggregate S2 multiplier or weight is invalid.",
      call. = FALSE
    )
  }

  national_aggregate_s2 <- sum(province_summary$adjusted_deaths) /
    sum(province_summary$pre_adjustment_deaths)
  province_summary[, relative_to_national := aggregate_s2 / national_aggregate_s2]

  # Exact ordinary nonparametric bootstrap support. Resampling nine provinces
  # with replacement has C(17, 8) = 24,310 unique count patterns. Enumerating
  # those patterns avoids a user-selected bootstrap simulation size and gives
  # each pattern its exact multinomial probability.
  province_count <- nrow(province_summary)
  bootstrap_counts <- uc_weak_compositions(province_count, province_count)
  adjusted_bootstrap <- as.numeric(
    bootstrap_counts %*% province_summary$adjusted_deaths
  )
  pre_adjustment_bootstrap <- as.numeric(
    bootstrap_counts %*% province_summary$pre_adjustment_deaths
  )
  bootstrap_s2 <- adjusted_bootstrap / pre_adjustment_bootstrap

  log_probability <- lgamma(province_count + 1) -
    rowSums(lgamma(bootstrap_counts + 1)) -
    province_count * log(province_count)
  probability <- exp(log_probability)
  probability <- probability / sum(probability)

  raw_factor <- bootstrap_s2 / national_aggregate_s2
  probability_mean <- sum(probability * raw_factor)
  factor <- raw_factor / probability_mean
  if (any(!is.finite(factor) | factor <= 0) ||
      abs(sum(probability * factor) - 1) > 1e-12) {
    stop(
      "The national completeness bootstrap did not produce valid mean-one factors.",
      call. = FALSE
    )
  }

  factor_support <- data.table::data.table(
    bootstrap_id = seq_along(factor),
    aggregate_s2 = bootstrap_s2,
    factor = factor,
    probability = probability
  )
  count_table <- data.table::as.data.table(bootstrap_counts)
  data.table::setnames(
    count_table,
    paste0("province_", province_summary$Death_Prov, "_count")
  )
  factor_support <- cbind(factor_support, count_table)

  factor_quantiles <- uc_weighted_quantile(
    factor_support$factor,
    factor_support$probability,
    c(0.025, 0.5, 0.975)
  )
  log_factor <- log(factor_support$factor)
  weighted_log_mean <- sum(factor_support$probability * log_factor)
  diagnostic_log_sd <- sqrt(sum(
    factor_support$probability * (log_factor - weighted_log_mean)^2
  ))
  if (!is.finite(diagnostic_log_sd) || diagnostic_log_sd <= 0) {
    stop(
      "The national completeness bootstrap has no finite dispersion.",
      call. = FALSE
    )
  }

  eligible_strata <- nrow(unique(eligible[, .(Sex, DeathYear, age5)]))
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
      "exact cluster bootstrap of the death-weighted national",
      "aggregate S2 multiplier"
    ),
    draw_level = "one shared national factor",
    freeze_year = freeze_year,
    national_aggregate_s2 = national_aggregate_s2,
    log_sd = diagnostic_log_sd,
    q025 = factor_quantiles[[1L]],
    median = factor_quantiles[[2L]],
    q975 = factor_quantiles[[3L]],
    eligible_rows = nrow(eligible),
    eligible_strata = eligible_strata,
    provinces = sort(unique(eligible$Death_Prov)),
    factor_support = factor_support,
    weighted_cells = eligible[, .(
      Death_Prov, Sex, DeathYear, age5, S2,
      adjusted_african_natural, pre_adjustment_african_natural
    )],
    province_summary = province_summary,
    excluded = excluded
  )
}

uc_completeness_distribution_summary <- function(distribution) {
  support <- distribution$factor_support
  quantiles <- uc_weighted_quantile(
    support$factor,
    support$probability,
    c(0.025, 0.5, 0.975)
  )
  data.table::rbindlist(list(
    data.table::data.table(
      section = "method",
      metric = c(
        "method", "draw_level", "freeze_year", "support_patterns",
        "national_aggregate_s2", "diagnostic_log_sd",
        "eligible_rows", "eligible_strata"
      ),
      value = as.character(c(
        distribution$method,
        distribution$draw_level,
        distribution$freeze_year,
        nrow(support),
        signif(distribution$national_aggregate_s2, 12),
        signif(distribution$log_sd, 12),
        distribution$eligible_rows,
        distribution$eligible_strata
      ))
    ),
    data.table::data.table(
      section = "national_factor_distribution",
      metric = c("minimum", "q025", "median", "mean", "q975", "maximum"),
      value = as.character(signif(c(
        min(support$factor),
        quantiles[[1L]],
        quantiles[[2L]],
        sum(support$probability * support$factor),
        quantiles[[3L]],
        max(support$factor)
      ), 12))
    ),
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

  if (!isTRUE(stochastic)) {
    return(list(
      data = x,
      factors = data.table::data.table(
        Death_Prov = provinces,
        bootstrap_id = NA_integer_,
        factor = 1
      ),
      diagnostics = list(
        log_sd = distribution$log_sd,
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

  support <- uc_as_dt(distribution$factor_support)
  uc_assert_columns(
    support,
    c("bootstrap_id", "factor", "probability"),
    "national completeness bootstrap support"
  )
  if (!nrow(support) ||
      support[
        !is.finite(factor) | factor <= 0 |
          !is.finite(probability) | probability <= 0,
        .N
      ] > 0L ||
      abs(sum(support$probability) - 1) > 1e-10) {
    stop("National completeness bootstrap support is invalid.", call. = FALSE)
  }

  set.seed(as.integer(seed))
  selected_index <- sample.int(
    nrow(support),
    size = 1L,
    prob = support$probability
  )
  selected <- support[selected_index]
  shared_factor <- as.numeric(selected$factor[[1L]])
  factors <- data.table::data.table(
    Death_Prov = provinces,
    bootstrap_id = as.integer(selected$bootstrap_id[[1L]]),
    factor = shared_factor
  )

  before_total <- sum(x$Deaths)
  affected <- x[Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES, sum(Deaths)]
  x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    Deaths := Deaths * shared_factor
  ]
  affected_after <- x[
    Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
    sum(Deaths)
  ]
  out <- x[, .(Deaths = sum(Deaths)), by = eval(UC_KEY)]
  uc_assert_finite_nonnegative(out, "Deaths", "completeness draw")

  list(
    data = out[],
    factors = factors[],
    diagnostics = list(
      log_sd = distribution$log_sd,
      total_before = before_total,
      total_after = sum(out$Deaths),
      affected_before = affected,
      affected_after = affected_after,
      factor_min = shared_factor,
      factor_max = shared_factor,
      factor_mean = shared_factor
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

uc_draw_injury_fraction_panel <- function(artifact, seed, stochastic = TRUE) {
  uc_validate_injury_artifact(artifact)
  panel <- if (isTRUE(stochastic)) {
    injury_draw_fraction_panel(artifact, seed = seed)
  } else {
    injury_compose_fraction_panel(artifact)
  }
  uc_standardize_injury_fraction_panel(
    panel,
    if (isTRUE(stochastic)) {
      "Stage 03 survey-propagated injury fraction draw"
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
    target_overrides = NULL) {
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
    uc_assert_columns(
      stage05_long,
      c(UC_KEY, "Deaths"),
      "Stage 06 long input"
    )
    stage05_long[, Deaths := as.numeric(Deaths)]
    uc_assert_finite_nonnegative(
      stage05_long,
      "Deaths",
      "Stage 06 long input"
    )
    uc_assert_unique(stage05_long, UC_KEY, "Stage 06 long input")
    wide <- long_to_wide_causes(
      stage05_long,
      value = "Deaths",
      codes = 1:214
    )
  } else {
    wide <- uc_as_dt(wide)
    uc_assert_columns(wide, UC_CELL_KEY, "Stage 06 wide input")
    uc_assert_unique(wide, UC_CELL_KEY, "Stage 06 wide input")
    wide <- ensure_cause_columns(wide, codes = 1:214, copy = FALSE)
  }

  redistributed <- redistribute_garbage_codes(
    wide,
    cfg = NULL,
    target_overrides = target_overrides
  )
  target_audit <- attr(redistributed, "redistribution_target_audit")
  long <- wide_to_long_causes(
    redistributed,
    value = "Deaths",
    codes = 1:214,
    drop_zero = FALSE
  )
  list(
    data = uc_aggregate_counts(long),
    target_audit = target_audit
  )
}

uc_redistribution_audit_summary <- function(audit) {
  if (is.null(audit) || !nrow(audit)) {
    return(list(
      rules = 0L,
      stochastic_rules = 0L,
      mean_selected_fraction = 1,
      min_selected_fraction = 1,
      max_selected_fraction = 1,
      selected_targets = 0L,
      full_targets = 0L
    ))
  }
  x <- uc_as_dt(audit)
  x[, selected_fraction := selected_target_count / full_target_count]
  list(
    rules = nrow(x),
    stochastic_rules = x[stochastic_subset == TRUE, .N],
    mean_selected_fraction = mean(x$selected_fraction),
    min_selected_fraction = min(x$selected_fraction),
    max_selected_fraction = max(x$selected_fraction),
    selected_targets = sum(x$selected_target_count),
    full_targets = sum(x$full_target_count)
  )
}

# Reporting tables -------------------------------------------------------------

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
    nbdcode %in% 1:214,
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
    completeness_scalars = uc_derived_path(
      root, cfg, "04_completeness_scalars.parquet"
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
  stage04 <- uc_standardize_count_input(
    uc_read_parquet(paths$stage04, "Stage 04 subpopulation output"),
    "Stage 04"
  )
  completeness_distribution <- uc_estimate_completeness_distribution(
    completeness_scalars,
    stage04,
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
    injury_point_fractions = injury_point_fractions,
    completeness_scalars = completeness_scalars,
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
    target_overrides = NULL
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

  completeness <- uc_draw_completeness(
    inputs$stage04,
    inputs$completeness_distribution,
    stochastic = TRUE,
    seed = seeds$completeness
  )
  current <- completeness$data
  total_after_completeness <- sum(current$Deaths)

  fractions <- uc_draw_injury_fraction_panel(
    inputs$injury_artifact,
    seed = seeds$injury,
    stochastic = TRUE
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
  # Preserve the exact Stage 05 long output through the Stage 06 boundary.
  # The HIV constructor already returns a unique, finite, non-negative table;
  # an additional thresholding aggregation here can turn numerical traces into
  # structural zeros (or vice versa) before biological fallback allocation.
  current <- uc_as_dt(hiv_draw$data)
  uc_assert_columns(current, c(UC_KEY, "Deaths"), "drawn Stage 05 output")
  uc_assert_finite_nonnegative(current, "Deaths", "drawn Stage 05 output")
  uc_assert_unique(current, UC_KEY, "drawn Stage 05 output")
  hiv_conservation <- uc_max_cell_error(before_hiv, current, UC_CELL_KEY)

  target_overrides <- stage06_draw_target_overrides(seeds$redistribution)
  before_redistribution <- uc_add_envelope(current)
  redistribution <- uc_run_stage06(
    current,
    target_overrides = target_overrides
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
    seed_injury = seeds$injury,
    seed_hiv_model = seeds$hiv_model,
    seed_redistribution = seeds$redistribution,
    seconds = elapsed,
    stochastic_completeness = TRUE,
    stochastic_injury = TRUE,
    stochastic_hiv = TRUE,
    stochastic_redistribution = TRUE,
    completeness_empirical_log_sd = completeness$diagnostics$log_sd,
    completeness_factor_min = completeness$diagnostics$factor_min,
    completeness_factor_max = completeness$diagnostics$factor_max,
    completeness_factor_mean = completeness$diagnostics$factor_mean,
    completeness_affected_before = completeness$diagnostics$affected_before,
    completeness_affected_after = completeness$diagnostics$affected_after,
    total_stage04_point = sum(inputs$stage04$Deaths),
    total_after_completeness = total_after_completeness,
    total_final = total_final,
    reportable_total = reportable_total,
    nonreportable_total = nonreportable_total,
    nonreportable_relative = nonreportable_relative,
    total_propagation_error = total_propagation_error,
    total_propagation_relative_error = total_propagation_relative_error,
    injury_envelope_max_abs_error = injury$diagnostics$max_envelope_abs_error,
    injury_envelope_max_relative_error = injury$diagnostics$max_envelope_relative_error,
    hiv_conservation_max_abs_error = hiv_conservation$max_abs,
    hiv_conservation_max_relative_error = hiv_conservation$max_rel,
    redistribution_conservation_max_abs_error = redistribution_conservation$max_abs,
    redistribution_conservation_max_relative_error = redistribution_conservation$max_rel,
    hiv_draw_modelled_deaths = hiv_draw$modelled_hiv_deaths,
    redistribution_rules = redistribution_summary$rules,
    redistribution_stochastic_rules = redistribution_summary$stochastic_rules,
    redistribution_mean_selected_fraction = redistribution_summary$mean_selected_fraction,
    redistribution_min_selected_fraction = redistribution_summary$min_selected_fraction,
    redistribution_max_selected_fraction = redistribution_summary$max_selected_fraction,
    redistribution_selected_targets = redistribution_summary$selected_targets,
    redistribution_full_targets = redistribution_summary$full_targets,
    final_nonnegative = nonnegative
  )

  tolerance <- config$run$conservation_tolerance
  valid <- nonnegative &&
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
    diagnostics = diagnostics,
    completeness_factors = completeness$factors,
    redistribution_audit = redistribution$target_audit
  )
}

# Run metadata -----------------------------------------------------------------

uc_preflight_table <- function(inputs, reconstruction, config) {
  multi_target_rules <- stage06_redistribution_rules()
  multi_target_rules <- sum(vapply(
    multi_target_rules,
    function(rule) length(rule$targets) > 1L,
    logical(1)
  ))
  info <- data.table::data.table(
    section = "checkpoint",
    metric = c(
      "stage03_injury_fraction_rows",
      "stage03_injury_method",
      "stage03_injury_trajectories",
      "completeness_diagnostic_log_sd",
      "completeness_factor_support_min",
      "completeness_factor_support_max",
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
      signif(inputs$completeness_distribution$log_sd, 12),
      signif(min(inputs$completeness_distribution$factor_support$factor), 12),
      signif(max(inputs$completeness_distribution$factor_support$factor), 12),
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
      "under_reporting=one shared national factor sampled from the exact nine-province cluster-bootstrap distribution of the death-weighted aggregate S2 mean; the factor is centred to probability-weighted mean one and applied to all African natural-cause cells; central 95% range ",
      signif(inputs$completeness_distribution$q025, 8),
      "-",
      signif(inputs$completeness_distribution$q975, 8)
    ),
    "injury=Dirichlet draws of the NIMS 2000, IMS 2009 and FAMHIS 2017 compositions using their available count/Kish effective sample sizes; each draw reruns the same hierarchical ALR linear interpolation, flat boundary tails and fixed triangular moving average; no additional path-variance parameter is introduced; preserves the injury envelope",
    "hiv=fitted Stage 05 coefficient draws from model variance-covariance matrices after the completeness draw; ANC prevalence fixed because no variance input is supplied; preserves deaths within demographic cells",
    "redistribution=one uniformly sampled non-empty subset of each multi-target expert rule per draw; selected targets use the unchanged deterministic proportional redistribution algorithm; preserves natural and injury envelopes",
    "reporting=province, national and national population-group Person estimates are retained in separate per-draw files so final cause intervals preserve the full within-draw covariance",
    "fixed_without_variance_source=S1 completeness, NPR envelope, ANC prevalence and the total injury envelope"
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
    "Completeness: drawing one shared national factor from the exact nine-province cluster bootstrap of the death-weighted S2 mean (central 95% range ",
    signif(inputs$completeness_distribution$q025, 6),
    "-",
    signif(inputs$completeness_distribution$q975, 6),
    "); the relative provincial point pattern is retained."
  )
  message(
    "Injuries: drawing the three survey compositions from their count/Kish ",
    "effective sample sizes and rerunning the fixed interpolation and smoother; ",
    "the total injury envelope remains fixed."
  )
  message(
    "HIV/AIDS: drawing fitted Stage 05 coefficients from their variance-covariance matrices after completeness scaling."
  )
  message(
    "Redistribution: drawing non-empty subsets of the same expert-approved target lists and applying the unchanged proportional algorithm."
  )
  if (isTRUE(config$reporting$include_population_groups)) {
    message(
      "Reporting: retaining province, South Africa, and national population-group Person estimates in every draw."
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
  signature <- uc_run_signature(root, inputs, config)
  uc_write_csv_atomic(catalog, file.path(output_root, "cause_catalog.csv"))
  if (nrow(point_population_report)) {
    uc_write_parquet_atomic(
      point_population_report,
      file.path(output_root, "population_point_report.parquet")
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
    diagnostic_paths <- character(n_draws)
    completeness_factor_paths <- character(n_draws)
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
      redistribution_audit_path <- file.path(
        scenario_path,
        "diagnostics",
        paste0(draw_name, "_redistribution_targets.csv")
      )
      draw_paths[[draw_id]] <- draw_path
      population_draw_paths[[draw_id]] <- population_draw_path
      diagnostic_paths[[draw_id]] <- diagnostic_path
      completeness_factor_paths[[draw_id]] <- completeness_factor_path
      redistribution_audit_paths[[draw_id]] <- redistribution_audit_path

      complete_files <- c(
        draw_path,
        if (isTRUE(config$reporting$include_population_groups)) {
          population_draw_path
        },
        diagnostic_path,
        completeness_factor_path,
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
      uc_write_csv_atomic(result$diagnostics, diagnostic_path)
      uc_write_csv_atomic(
        result$completeness_factors,
        completeness_factor_path
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
      diagnostic_paths,
      completeness_factor_paths,
      redistribution_audit_paths
    )
    if (any(!file.exists(required_outputs))) {
      stop("One or more uncertainty draw outputs are missing.", call. = FALSE)
    }

    draws <- uc_combine_draws(draw_paths)
    diagnostics <- data.table::rbindlist(
      lapply(diagnostic_paths, data.table::fread),
      use.names = TRUE,
      fill = TRUE
    )
    if (diagnostics[valid != TRUE, .N]) {
      stop("One or more completed draws failed validation.", call. = FALSE)
    }
    summary <- uc_summarise_draws(draws, point_report)
    convergence <- uc_convergence_headlines(draws)
    completeness_factors <- data.table::rbindlist(
      Map(function(path, draw_id) {
        x <- data.table::fread(path)
        x[, draw_id := as.integer(draw_id)]
        x
      }, completeness_factor_paths, seq_along(completeness_factor_paths)),
      use.names = TRUE,
      fill = TRUE
    )
    redistribution_audit <- data.table::rbindlist(
      Map(function(path, draw_id) {
        x <- data.table::fread(path)
        x[, draw_id := as.integer(draw_id)]
        x
      }, redistribution_audit_paths, seq_along(redistribution_audit_paths)),
      use.names = TRUE,
      fill = TRUE
    )
    hiv_cause_covariance <- uc_hiv_cause_covariance_diagnostics(
      draws,
      catalog
    )

    uc_write_parquet_atomic(
      draws,
      file.path(scenario_path, "uncertainty_draws.parquet")
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
    uc_write_csv_atomic(
      redistribution_audit,
      file.path(scenario_path, "redistribution_target_subsets.csv")
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
      max_validation_abs_error = max(
        diagnostics$injury_envelope_max_abs_error,
        diagnostics$hiv_conservation_max_abs_error,
        diagnostics$redistribution_conservation_max_abs_error,
        diagnostics$total_propagation_error,
        na.rm = TRUE
      ),
      max_validation_relative_error = max(
        diagnostics$injury_envelope_max_relative_error,
        diagnostics$hiv_conservation_max_relative_error,
        diagnostics$redistribution_conservation_max_relative_error,
        diagnostics$total_propagation_relative_error,
        na.rm = TRUE
      )
    )
    message(
      "Scenario '", scenario, "' complete. Mean draw runtime: ",
      round(scenario_results[[scenario]]$mean_seconds, 1), " s."
    )
    rm(
      draws,
      diagnostics,
      summary,
      convergence,
      completeness_factors,
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
