#!/usr/bin/env Rscript

# ==============================================================================
# Compare alternative empirical SD assumptions for S2 under-reporting
# ==============================================================================
#
# This is a diagnostic script only. It does not change the NBD3 pipeline,
# overwrite uncertainty draws, or modify any point estimate.
#
# Run from anywhere with:
#   Rscript dev/compare_completeness_sd_assumptions.R
#
# The script compares four ways of estimating the log-scale SD used for the
# African natural-cause completeness factor:
#
#   1. pooled_all_cells
#      One death-weighted SD of log(S2) across all province x sex x age x year
#      cells.
#
#   2. within_province_age_time
#      One SD per province across year x age cells after collapsing sex.
#      This is the current province-specific method.
#
#   3. within_province_age
#      One SD per province across age cells after collapsing sex and year.
#      This retains only between-age variation within each province.
#
#   4. within_province_time
#      One SD per province across year cells after collapsing sex and age.
#      This retains only temporal variation within each province.
#
# All calculations:
#   * use positive finite S2 values up to the configured freeze year;
#   * exclude the copied neonatal S2 row (age5 == 1);
#   * weight cells by implied pre-adjustment African natural deaths;
#   * use the Stage 04 file before the injury-envelope calibration when present.
#
# Outputs are written to output/tables and output/figures.
# ============================================================================== 

options(stringsAsFactors = FALSE)

required_packages <- c("arrow", "data.table", "ggplot2", "yaml")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing package(s): ", paste(missing_packages, collapse = ", "),
    ". Run Rscript install_packages.R first.",
    call. = FALSE
  )
}

find_root_for_diagnostic <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "config", "config.yml")) &&
        file.exists(file.path(current, "R", "00_core.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not find the SA-NBD3-V1 project root. Run this script inside ",
        "the repository or place it under dev/.",
        call. = FALSE
      )
    }
    current <- parent
  }
}

root <- find_root_for_diagnostic()
source(file.path(root, "R", "00_core.R"), local = .GlobalEnv)
cfg <- read_project_config(root)

dir.create(cfg$paths$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$paths$figures, recursive = TRUE, showWarnings = FALSE)

read_parquet_dt <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " not found: ", path, call. = FALSE)
  }
  data.table::as.data.table(
    arrow::read_parquet(path, as_data_frame = TRUE)
  )
}

scalars_path <- file.path(
  cfg$paths$derived,
  "04_completeness_scalars.parquet"
)

stage04_candidates <- file.path(
  cfg$paths$derived,
  c(
    "04_investigation_subpopulation_pre_injury_envelope.parquet",
    "04_investigation_subpopulation.parquet"
  )
)
stage04_path <- stage04_candidates[file.exists(stage04_candidates)][1L]
if (is.na(stage04_path)) {
  stop(
    "Neither Stage 04 subpopulation file was found. Expected one of:\n- ",
    paste(stage04_candidates, collapse = "\n- "),
    call. = FALSE
  )
}

message("Project root: ", root)
message("Completeness scalars: ", scalars_path)
message("Stage 04 weights: ", stage04_path)

scalars <- read_parquet_dt(scalars_path, "Stage 04 completeness scalars")
stage04 <- read_parquet_dt(stage04_path, "Stage 04 subpopulation output")

scalar_key <- c("Death_Prov", "Sex", "DeathYear", "age5")
required_scalar_columns <- c(scalar_key, "S2")
missing_scalar_columns <- setdiff(required_scalar_columns, names(scalars))
if (length(missing_scalar_columns)) {
  stop(
    "Completeness scalar file is missing: ",
    paste(missing_scalar_columns, collapse = ", "),
    call. = FALSE
  )
}

required_stage04_columns <- c(
  "Death_Prov", "Sex", "DeathYear", "age5",
  "Popgroup", "nbdcode"
)
missing_stage04_columns <- setdiff(required_stage04_columns, names(stage04))
if (length(missing_stage04_columns)) {
  stop(
    "Stage 04 file is missing: ",
    paste(missing_stage04_columns, collapse = ", "),
    call. = FALSE
  )
}

value_candidates <- c("Deaths", "Count", "num")
value_column <- value_candidates[value_candidates %in% names(stage04)][1L]
if (is.na(value_column)) {
  stop(
    "Stage 04 file does not contain a recognised death-count column ",
    "(Deaths, Count, or num).",
    call. = FALSE
  )
}

scalars[, `:=`(
  Death_Prov = as.integer(Death_Prov),
  Sex = as.integer(Sex),
  DeathYear = as.integer(DeathYear),
  age5 = as.integer(age5),
  S2 = as.numeric(S2)
)]

if (scalars[, .N, by = eval(scalar_key)][N > 1L, .N]) {
  stop("Completeness scalars are not unique on the expected key.", call. = FALSE)
}

stage04[, `:=`(
  Death_Prov = as.integer(Death_Prov),
  Sex = as.integer(Sex),
  DeathYear = as.integer(DeathYear),
  age5 = as.integer(age5),
  Popgroup = as.integer(Popgroup),
  nbdcode = as.integer(nbdcode),
  value__ = as.numeric(get(value_column))
)]

# African natural-cause deaths after deterministic S2 adjustment. These are the
# weights used by the uncertainty engine. The implied pre-adjustment deaths are
# reconstructed as adjusted deaths divided by S2.
african_natural <- stage04[
  Popgroup == 1L & !nbdcode %in% INJURY_CODES,
  .(adjusted_african_natural = sum(value__, na.rm = TRUE)),
  by = eval(scalar_key)
]

candidate <- merge(
  scalars[, c(scalar_key, "S2"), with = FALSE],
  african_natural,
  by = scalar_key,
  all.x = TRUE,
  sort = FALSE
)

freeze_year <- as.integer(cfg$settings$completeness_freeze_year)
if (length(freeze_year) != 1L || is.na(freeze_year)) {
  stop("settings.completeness_freeze_year is invalid.", call. = FALSE)
}

eligible <- candidate[
  DeathYear <= freeze_year &
    age5 != 1L &
    is.finite(S2) & S2 > 0 &
    is.finite(adjusted_african_natural) &
    adjusted_african_natural > 0
]
eligible[, pre_adjustment_african_natural :=
  adjusted_african_natural / S2]
eligible <- eligible[
  is.finite(pre_adjustment_african_natural) &
    pre_adjustment_african_natural > 0
]
eligible[, log_s2 := log(S2)]

if (!nrow(eligible)) {
  stop("No eligible completeness cells remain after filtering.", call. = FALSE)
}
if (!identical(sort(unique(eligible$Death_Prov)), 1:9)) {
  stop("The diagnostic requires all nine provinces.", call. = FALSE)
}

weighted_log_sd <- function(values, weights) {
  values <- as.numeric(values)
  weights <- as.numeric(weights)
  valid <- is.finite(values) & is.finite(weights) & weights > 0
  values <- values[valid]
  weights <- weights[valid]
  if (!length(values) || sum(weights) <= 0) {
    return(list(
      n_cells = 0L,
      weight_total = NA_real_,
      weighted_mean_log_s2 = NA_real_,
      log_sd = NA_real_,
      minimum_s2 = NA_real_,
      median_s2 = NA_real_,
      maximum_s2 = NA_real_
    ))
  }
  weight_total <- sum(weights)
  weighted_mean <- sum(weights * values) / weight_total
  weighted_variance <- sum(
    weights * (values - weighted_mean)^2
  ) / weight_total
  list(
    n_cells = length(values),
    weight_total = weight_total,
    weighted_mean_log_s2 = weighted_mean,
    log_sd = sqrt(max(0, weighted_variance)),
    minimum_s2 = min(exp(values)),
    median_s2 = stats::median(exp(values)),
    maximum_s2 = max(exp(values))
  )
}

summarise_sd <- function(data, group_columns = character(), assumption) {
  if (!length(group_columns)) {
    result <- data.table::as.data.table(weighted_log_sd(
      data$log_s2,
      data$pre_adjustment_african_natural
    ))
  } else {
    result <- data[, weighted_log_sd(
      log_s2,
      pre_adjustment_african_natural
    ), by = eval(group_columns)]
  }
  result[, assumption := assumption]
  result[]
}

collapse_ratio <- function(data, group_columns) {
  result <- data[, .(
    input_cells = .N,
    adjusted_african_natural = sum(adjusted_african_natural),
    pre_adjustment_african_natural = sum(pre_adjustment_african_natural)
  ), by = eval(group_columns)]
  result[, S2 := adjusted_african_natural /
    pre_adjustment_african_natural]
  result <- result[
    is.finite(S2) & S2 > 0 &
      is.finite(pre_adjustment_african_natural) &
      pre_adjustment_african_natural > 0
  ]
  result[, log_s2 := log(S2)]
  result[]
}

# 1. One pooled SD from all sex-specific province x age x year cells.
pooled_all_cells <- summarise_sd(
  eligible,
  group_columns = character(),
  assumption = "pooled_all_province_age_sex_time"
)
pooled_all_cells[, Death_Prov := NA_integer_]

# 2. Current method: collapse sex, then estimate within province across age x time.
province_age_time_cells <- collapse_ratio(
  eligible,
  c("Death_Prov", "DeathYear", "age5")
)
within_province_age_time <- summarise_sd(
  province_age_time_cells,
  group_columns = "Death_Prov",
  assumption = "within_province_age_time"
)

# 3. Collapse sex and year, then estimate within province across age.
province_age_cells <- collapse_ratio(
  eligible,
  c("Death_Prov", "age5")
)
within_province_age <- summarise_sd(
  province_age_cells,
  group_columns = "Death_Prov",
  assumption = "within_province_age"
)

# 4. Collapse sex and age, then estimate within province across time.
province_time_cells <- collapse_ratio(
  eligible,
  c("Death_Prov", "DeathYear")
)
within_province_time <- summarise_sd(
  province_time_cells,
  group_columns = "Death_Prov",
  assumption = "within_province_time"
)

province_ids <- 1:9
province_names <- unname(PROVINCE_LABELS[as.character(province_ids)])
province_lookup <- data.table::data.table(
  Death_Prov = province_ids,
  province = province_names
)

# Replicate the pooled SD across provinces so all four assumptions can be viewed
# on one province-by-assumption table. This does not choose whether the factors
# would be shared or independent; it only compares the SD assigned to each
# province.
pooled_replicated <- data.table::CJ(
  Death_Prov = province_ids,
  sorted = TRUE
)
for (column in setdiff(names(pooled_all_cells), "Death_Prov")) {
  pooled_replicated[, (column) := pooled_all_cells[[column]][1L]]
}

comparison <- data.table::rbindlist(
  list(
    pooled_replicated,
    within_province_age_time,
    within_province_age,
    within_province_time
  ),
  use.names = TRUE,
  fill = TRUE
)
comparison <- merge(
  comparison,
  province_lookup,
  by = "Death_Prov",
  all.x = TRUE,
  sort = FALSE
)

mean_one_factor_quantiles <- function(log_sd) {
  log_sd <- as.numeric(log_sd)
  q <- stats::qnorm(c(0.025, 0.5, 0.975))
  data.table::data.table(
    factor_q025 = exp(-0.5 * log_sd^2 + q[[1L]] * log_sd),
    factor_median = exp(-0.5 * log_sd^2 + q[[2L]] * log_sd),
    factor_mean = 1,
    factor_q975 = exp(-0.5 * log_sd^2 + q[[3L]] * log_sd)
  )
}

quantile_table <- mean_one_factor_quantiles(comparison$log_sd)
comparison <- cbind(comparison, quantile_table)
comparison[, `:=`(
  factor_lower_percent = 100 * (factor_q025 - 1),
  factor_upper_percent = 100 * (factor_q975 - 1),
  interval_ratio = factor_q975 / factor_q025
)]

assumption_labels <- c(
  pooled_all_province_age_sex_time =
    "Pooled across province, age, sex and time",
  within_province_age_time =
    "Within province: age and time",
  within_province_age =
    "Within province: age only",
  within_province_time =
    "Within province: time only"
)
comparison[, assumption_label := unname(
  assumption_labels[assumption]
)]

# Province weights used to show the implied national effect if all four options
# use the same current draw architecture: one independent factor per province.
province_weights <- eligible[, .(
  pre_adjustment_deaths = sum(pre_adjustment_african_natural)
), by = Death_Prov]
province_weights[, national_weight := pre_adjustment_deaths /
  sum(pre_adjustment_deaths)]

set.seed(20260812L)
N_SIM <- 100000L
simulate_national_factor <- function(parameter_rows, assumption_name) {
  parameter_rows <- merge(
    province_lookup,
    parameter_rows[, .(Death_Prov, log_sd)],
    by = "Death_Prov",
    all.x = TRUE,
    sort = TRUE
  )
  parameter_rows <- merge(
    parameter_rows,
    province_weights[, .(Death_Prov, national_weight)],
    by = "Death_Prov",
    all.x = TRUE,
    sort = TRUE
  )
  if (parameter_rows[
    !is.finite(log_sd) | log_sd < 0 |
      !is.finite(national_weight) | national_weight <= 0,
    .N
  ]) {
    stop(
      "Invalid province parameters for assumption: ", assumption_name,
      call. = FALSE
    )
  }
  z <- matrix(
    stats::rnorm(N_SIM * nrow(parameter_rows)),
    nrow = N_SIM,
    ncol = nrow(parameter_rows)
  )
  sigma <- parameter_rows$log_sd
  factors <- exp(
    sweep(z, 2L, sigma, `*`) -
      matrix(
        0.5 * sigma^2,
        nrow = N_SIM,
        ncol = length(sigma),
        byrow = TRUE
      )
  )
  national_factor <- as.numeric(
    factors %*% parameter_rows$national_weight
  )
  quantiles <- stats::quantile(
    national_factor,
    probs = c(0.025, 0.5, 0.975),
    names = FALSE,
    type = 8
  )
  data.table::data.table(
    assumption = assumption_name,
    simulation_draws = N_SIM,
    national_factor_mean = mean(national_factor),
    national_factor_sd = stats::sd(national_factor),
    national_factor_q025 = quantiles[[1L]],
    national_factor_median = quantiles[[2L]],
    national_factor_q975 = quantiles[[3L]],
    national_interval_ratio = quantiles[[3L]] / quantiles[[1L]]
  )
}

national_summary <- data.table::rbindlist(lapply(
  names(assumption_labels),
  function(assumption_name) {
    simulate_national_factor(
      comparison[assumption == assumption_name],
      assumption_name
    )
  }
))
national_summary[, assumption_label := unname(
  assumption_labels[assumption]
)]

assumption_summary <- comparison[, .(
  provinces = data.table::uniqueN(Death_Prov),
  log_sd_min = min(log_sd),
  log_sd_median = stats::median(log_sd),
  log_sd_mean = mean(log_sd),
  log_sd_max = max(log_sd),
  factor_q025_min = min(factor_q025),
  factor_q025_median = stats::median(factor_q025),
  factor_q975_median = stats::median(factor_q975),
  factor_q975_max = max(factor_q975),
  median_interval_ratio = stats::median(interval_ratio),
  max_interval_ratio = max(interval_ratio)
), by = .(assumption, assumption_label)]
assumption_summary <- merge(
  assumption_summary,
  national_summary,
  by = c("assumption", "assumption_label"),
  all.x = TRUE,
  sort = FALSE
)

assumption_definitions <- data.table::data.table(
  assumption = names(assumption_labels),
  label = unname(assumption_labels),
  sex_handling = c(
    "Retained as separate cells",
    "Collapsed within province x year x age",
    "Collapsed with year within province x age",
    "Collapsed with age within province x year"
  ),
  variation_used_for_sd = c(
    "All eligible province x sex x age x year cells",
    "Age x time variation within each province",
    "Age variation within each province after pooling years",
    "Time variation within each province after pooling ages"
  ),
  draw_interpretation = c(
    "Same pooled SD would be assigned to each province",
    "One province-specific factor informed by age and time",
    "One province-specific factor informed by age pattern",
    "One province-specific factor informed by temporal pattern"
  )
)

# Write outputs -----------------------------------------------------------------
province_output <- file.path(
  cfg$paths$tables,
  "completeness_sd_assumptions_by_province.csv"
)
summary_output <- file.path(
  cfg$paths$tables,
  "completeness_sd_assumptions_summary.csv"
)
national_output <- file.path(
  cfg$paths$tables,
  "completeness_sd_assumptions_national_simulation.csv"
)
definitions_output <- file.path(
  cfg$paths$tables,
  "completeness_sd_assumption_definitions.csv"
)
input_output <- file.path(
  cfg$paths$tables,
  "completeness_sd_eligible_input_cells.csv"
)

data.table::fwrite(comparison, province_output)
data.table::fwrite(assumption_summary, summary_output)
data.table::fwrite(national_summary, national_output)
data.table::fwrite(assumption_definitions, definitions_output)
data.table::fwrite(
  eligible[, .(
    Death_Prov,
    province = unname(PROVINCE_LABELS[as.character(Death_Prov)]),
    Sex,
    DeathYear,
    age5,
    age_label = unname(AGE5_LABELS[as.character(age5)]),
    S2,
    log_s2,
    adjusted_african_natural,
    pre_adjustment_african_natural
  )],
  input_output
)

# Plots -------------------------------------------------------------------------
comparison[, province := factor(
  province,
  levels = rev(unname(PROVINCE_LABELS[as.character(1:9)]))
)]
comparison[, assumption_label := factor(
  assumption_label,
  levels = unname(assumption_labels)
)]

p_sd <- ggplot2::ggplot(
  comparison,
  ggplot2::aes(
    x = province,
    y = log_sd,
    shape = assumption_label,
    group = assumption_label
  )
) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.55),
    size = 2.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Alternative empirical SD assumptions for S2 under-reporting",
    subtitle = paste0(
      "Death-weighted log-S2 dispersion; eligible years through ",
      freeze_year
    ),
    x = NULL,
    y = "Log-scale standard deviation",
    shape = "Assumption"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  filename = file.path(
    cfg$paths$figures,
    "completeness_log_sd_by_assumption.png"
  ),
  plot = p_sd,
  width = 11,
  height = 7,
  dpi = 180
)

p_range <- ggplot2::ggplot(
  comparison,
  ggplot2::aes(
    x = province,
    y = factor_median,
    ymin = factor_q025,
    ymax = factor_q975,
    shape = assumption_label,
    group = assumption_label
  )
) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::geom_linerange(
    position = ggplot2::position_dodge(width = 0.65),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.65),
    size = 2.2
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "Implied mean-one completeness factor ranges",
    subtitle = "Central 95% ranges from the log-S2 SD; logarithmic factor scale",
    x = NULL,
    y = "Multiplicative factor (log scale)",
    shape = "Assumption"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  filename = file.path(
    cfg$paths$figures,
    "completeness_factor_ranges_by_assumption.png"
  ),
  plot = p_range,
  width = 11,
  height = 7,
  dpi = 180
)

national_plot_data <- data.table::copy(national_summary)
national_plot_data[, assumption_label := factor(
  assumption_label,
  levels = rev(unname(assumption_labels))
)]

p_national <- ggplot2::ggplot(
  national_plot_data,
  ggplot2::aes(
    x = assumption_label,
    y = national_factor_median,
    ymin = national_factor_q025,
    ymax = national_factor_q975
  )
) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::geom_linerange(linewidth = 1) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Implied national factor under independent province draws",
    subtitle = paste0(
      "Empirical quantiles from ", format(N_SIM, big.mark = ","),
      " simulations using province death weights"
    ),
    x = NULL,
    y = "National multiplicative factor"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  filename = file.path(
    cfg$paths$figures,
    "completeness_national_ranges_by_assumption.png"
  ),
  plot = p_national,
  width = 9,
  height = 5.5,
  dpi = 180
)

message("\nCompleteness SD diagnostic complete.\n")
print(
  assumption_summary[, .(
    assumption = assumption_label,
    log_sd_min = round(log_sd_min, 4),
    log_sd_median = round(log_sd_median, 4),
    log_sd_max = round(log_sd_max, 4),
    national_q025 = round(national_factor_q025, 4),
    national_median = round(national_factor_median, 4),
    national_q975 = round(national_factor_q975, 4)
  )]
)

message("\nWritten tables:")
message("- ", province_output)
message("- ", summary_output)
message("- ", national_output)
message("- ", definitions_output)
message("- ", input_output)
message("\nWritten figures:")
message("- ", file.path(
  cfg$paths$figures,
  "completeness_log_sd_by_assumption.png"
))
message("- ", file.path(
  cfg$paths$figures,
  "completeness_factor_ranges_by_assumption.png"
))
message("- ", file.path(
  cfg$paths$figures,
  "completeness_national_ranges_by_assumption.png"
))
