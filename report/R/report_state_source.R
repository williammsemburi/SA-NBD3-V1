# Prepared report objects evaluated offline by report_state_builder.R.
# `root` and all report helper functions are supplied by the builder.

config <- read_viz_config(root)
runtime_path <- derived_path(config, "viz_input_new.rds")
if (!file.exists(runtime_path)) {
  stop(
    "Prepared report runtime is missing: ", runtime_path,
    ". Run the report-input build before building report state.",
    call. = FALSE
  )
}
runtime <- readRDS(runtime_path)
runtime$config <- config

ui_cache_manifest_path <- file.path(
  root, "output", "report-data", "ui_uncertainty_cache", "manifest.yml"
)
ui_cache_manifest <- if (file.exists(ui_cache_manifest_path)) {
  yaml::read_yaml(ui_cache_manifest_path)
} else {
  list()
}
uncertainty_draw_n <- suppressWarnings(as.integer(
  ui_cache_manifest$n_draws %||% ui_cache_manifest$draws %||% NA_integer_
))
ui_cache_version_key <- paste(
  ui_cache_manifest$cache_version %||% "cache",
  ui_cache_manifest$built_at %||% "unknown",
  if (is.finite(uncertainty_draw_n)) uncertainty_draw_n else "unknown",
  sep = "|"
)
comparison_data <- data.table::as.data.table(data.table::copy(runtime$comparisons))

# Only a compact catalogue is loaded into the hosted worker state. The annual
# model-comparison values are written as a small, domain-partitioned Parquet
# store by report_state_builder.R and read lazily when a comparison panel is
# first used.
comparison_catalog <- unique(comparison_data[, .(
  domain = as.character(domain),
  geography_type = as.character(geography_type),
  geography_code = as.integer(geography_code),
  geography = as.character(geography),
  sex_code = as.integer(sex_code),
  sex = as.character(sex),
  age_id = as.character(age_id),
  age_label = as.character(age_label),
  age_sort_order = as.integer(age_sort_order),
  series_id = as.character(series_id),
  series_label = as.character(series_label),
  series_sort_order = as.integer(series_sort_order),
  model = as.character(model),
  measure = as.character(measure)
)])
data.table::setorder(
  comparison_catalog,
  domain, geography_type, geography_code, sex_code,
  age_sort_order, series_sort_order, model
)

cause_uncertainty <- data.table::data.table()
full_catalog <- runtime$catalogs
full_availability <- data.table::as.data.table(data.table::copy(
  full_catalog$availability
))

# Keep only compact catalogues in the serialized worker state. The complete
# cross-product availability table can contain more than a million rows and is
# already represented by the query-optimised Parquet cache. Serialising it into
# report_state.rds duplicates hundreds of megabytes and materially delays hosted
# worker startup.
result_rows <- full_availability[
  model == "NBD3-R" &
    geography_type %in% c("national", "province", "population_group")
]
result_geography_catalog <- unique(result_rows[, .(
  geography_type, geography_code, geography
)])[order(match(geography_type, c("national", "province", "population_group")),
          geography_code)]
result_sex_catalog <- unique(result_rows[, .(
  sex_code, sex
)])[order(sex_code)]
result_age_catalog <- unique(result_rows[, .(
  age_id, age_label, age_sort_order
)])[order(age_sort_order)]
result_cause_catalog <- unique(result_rows[, .(
  series_id, series_label, hierarchy, domain, cause_type, series_sort_order
)])[order(series_sort_order)]

catalog <- list(
  models = full_catalog$models,
  geographies = data.table::as.data.table(data.table::copy(
    full_catalog$geographies
  )),
  sexes = data.table::as.data.table(data.table::copy(full_catalog$sexes)),
  ages = data.table::as.data.table(data.table::copy(full_catalog$ages)),
  causes = data.table::as.data.table(data.table::copy(full_catalog$causes)),
  availability = data.table::data.table(),
  comparison_coverage = data.table::as.data.table(data.table::copy(
    full_catalog$comparison_coverage
  ))
)
injury_inputs <- load_injury_report_inputs(config)
injury_broad <- summarise_injury_broad_fractions(injury_inputs)
injury_broad_comparison <- data.table::as.data.table(data.table::copy(
  injury_broad$comparison
))
injury_detailed_comparison <- data.table::as.data.table(data.table::copy(
  injury_broad$detailed_comparison
))
injury_level_audit_path <- report_method_asset(
  config,
  "03_injury_survey_reference_audit.csv",
  fallback = file.path(
    root, "output", "tables", "03_injury_survey_reference_audit.csv"
  )
)
injury_level_audit <- if (file.exists(injury_level_audit_path)) {
  data.table::fread(injury_level_audit_path)
} else {
  data.table::data.table()
}

uncertainty_draw_label <- if (length(uncertainty_draw_n) == 1L && !is.na(uncertainty_draw_n) && is.finite(uncertainty_draw_n)) {
  paste0(format(uncertainty_draw_n, big.mark = ",", scientific = FALSE), " joint draws")
} else {
  "joint draws"
}
model_names_available <- ordered_model_names(unique(comparison_catalog$model), config)

measure_choices <- c(
  "Deaths" = "deaths",
  "Crude mortality rate per 100,000" = "crude_rate",
  "Age-standardised rate per 100,000" = "asr"
)














# Section 4.3: broad cause group × age profile -------------------------------
#
# The panel below uses the same optimized point/uncertainty cache as the two
# general result explorers, but it requests point-estimate deaths only. Four
# fixed years are displayed in a 2 × 2 layout, with the age-specific deaths
# stacked into the four broad cause groups used in the published figure.

broad_age_profile_years <- c(1997L, 2005L, 2012L, 2019L)

broad_age_profile_ages <- data.table::data.table(
  age_id = c(
    "age_under1", "age_1_4", "age_5_9", "age_10_14", "age_15_19",
    "age_20_24", "age_25_29", "age_30_34", "age_35_39", "age_40_44",
    "age_45_49", "age_50_54", "age_55_59", "age_60_64", "age_65_69",
    "age_70_74", "age_75_79", "age_80_84", "age_85_plus"
  ),
  age_label = c(
    "0", "1–4", "5–9", "10–14", "15–19",
    "20–24", "25–29", "30–34", "35–39", "40–44",
    "45–49", "50–54", "55–59", "60–64", "65–69",
    "70–74", "75–79", "80–84", "85+"
  ),
  age_order = seq_len(19L)
)

# The colours are sampled from the reference figure supplied for this report.
# Stack order is bottom to top; legend order is the reverse, matching the
# reference figure.
broad_age_profile_causes <- data.table::data.table(
  series_id = c("za_168", "za_169", "za_170", "za_171"),
  series_label = c(
    paste(
      "Communicable diseases, perinatal conditions, maternal causes,",
      "and nutritional deficiencies"
    ),
    "HIV/AIDS and tuberculosis",
    "Non-communicable diseases",
    "Injuries"
  ),
  legend_label = c(
    paste(
      "Communicable diseases, perinatal conditions, maternal causes,",
      "and nutritional deficiencies"
    ),
    "HIV/AIDS and tuberculosis",
    "Non-communicable diseases",
    "Injuries"
  ),
  colour = c("#926DB2", "#AC0639", "#3B9FC2", "#72B43C"),
  stack_order = 1:4,
  legend_order = c(4L, 3L, 2L, 1L)
)







# Injury source table used in the methods section.
injury_source_table <- data.table::data.table()
source_fractions <- data.table::as.data.table(data.table::copy(
  injury_inputs$source_fractions
))
if (nrow(source_fractions) && all(c(
  "survey", "year", "nbdcode", "observed_count"
) %in% names(source_fractions))) {
  labels <- data.table::fread(file.path(root, "data", "lookups", "analysis_codes.csv"))
  injury_source_table <- source_fractions[, .(
    observed_deaths = sum(observed_count, na.rm = TRUE)
  ), by = .(survey, year, nbdcode)]
  injury_source_table[, percent := safe_ratio(
    observed_deaths, sum(observed_deaths), 100
  ), by = .(survey, year)]
  injury_source_table <- merge(
    injury_source_table,
    labels[, .(nbdcode = as.integer(analysis_code), Cause = analysis_label)],
    by = "nbdcode",
    all.x = TRUE,
    sort = FALSE
  )
  injury_source_table[is.na(Cause), Cause := paste("Cause", nbdcode)]
  injury_source_table[, display := paste0(
    "<span class=\"injury-table-count\">",
    format(round(observed_deaths), big.mark = ",", scientific = FALSE),
    "</span><span class=\"injury-table-share\">",
    sprintf("%.2f%%", percent),
    "</span>"
  )]
  injury_source_table <- data.table::dcast(
    injury_source_table,
    nbdcode + Cause ~ survey,
    value.var = "display",
    fill = "—"
  )
  data.table::setorder(injury_source_table, nbdcode)
  data.table::setnames(
    injury_source_table,
    c("nbdcode", "Cause"),
    c("Code", "Cause of injury")
  )
  survey_columns <- intersect(
    c("NIMS 2000", "IMS 2009", "FAMHIS 2017"),
    names(injury_source_table)
  )
  data.table::setcolorder(
    injury_source_table,
    c("Code", "Cause of injury", survey_columns)
  )
}

injury_cause_labels <- data.table::fread(
  file.path(root, "data", "lookups", "analysis_codes.csv")
)[, .(
  nbdcode = as.integer(analysis_code),
  Cause = as.character(analysis_label)
)]

injury_broad_fit_table <- data.table::data.table()
if (nrow(injury_broad_comparison)) {
  injury_broad_fit_table <- injury_broad_comparison[, .(
    Survey = survey,
    Year = year,
    `Broad injury group` = broad_group,
    `Survey input (%)` = 100 * survey_fraction,
    `Final smoothed estimate (%)` = 100 * model_fraction,
    `Difference (percentage points)` = difference_percentage_points
  )]
  data.table::setorderv(
    injury_broad_fit_table,
    c("Year", "Broad injury group")
  )
}

injury_flagged_fit_table <- data.table::data.table()
if (nrow(injury_detailed_comparison)) {
  injury_flagged_fit_table <- merge(
    injury_detailed_comparison[nbdcode %in% c(132L, 136L, 138L)],
    injury_cause_labels,
    by = "nbdcode",
    all.x = TRUE,
    sort = FALSE
  )[, .(
    Survey = survey,
    Year = year,
    Code = nbdcode,
    Cause,
    `Survey input (%)` = 100 * survey_fraction,
    `Final smoothed estimate (%)` = 100 * model_fraction,
    `Difference (percentage points)` = difference_percentage_points
  )]
  data.table::setorder(injury_flagged_fit_table, Year, Code)
}

# -----------------------------------------------------------------------------
# Research-report additions: completeness diagnostics and two general result
# explorers. These are report-layer helpers only; they do not alter estimates.
# -----------------------------------------------------------------------------
report_fallback_colours <- c(
  "#0071BC", "#1B998B", "#D48A16", "#B23A48", "#7A5195",
  "#5B8E7D", "#2F4858", "#E76F51", "#6C757D", "#8A5A44",
  "#3A86FF", "#8338EC"
)

analysis_config <- yaml::read_yaml(file.path(root, "config", "config.yml"))
uncertainty_config <- yaml::read_yaml(
  file.path(root, "config", "uncertainty_joint.yml")
)


uncertainty_report_root <- find_uncertainty_report_root()
report_injury_codes <- c(
  124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L,
  135L, 136L, 137L, 138L, 139L, 140L, 141L
)
report_province_labels <- c(
  `1` = "Western Cape", `2` = "Eastern Cape", `3` = "Northern Cape",
  `4` = "Free State", `5` = "KwaZulu-Natal", `6` = "North West",
  `7` = "Gauteng", `8` = "Mpumalanga", `9` = "Limpopo"
)

# Prefer the exact annual cells written by the uncertainty engine. If they are
# not yet available, reconstruct the same death-weighted province-year S2 view
# from the deterministic Stage 04 checkpoints.
completeness_time_data <- read_optional_report_table(report_method_asset(
  config,
  "completeness_weighted_cells.parquet",
  fallback = if (!is.na(uncertainty_report_root)) file.path(
    uncertainty_report_root, "completeness_weighted_cells.parquet"
  ) else NA_character_
))

if (!nrow(completeness_time_data)) {
  scalar_path <- file.path(
    root, "data", "derived", "04_completeness_scalars.parquet"
  )
  stage04_candidates <- c(
    file.path(
      root, "data", "derived",
      "04_investigation_subpopulation_pre_injury_envelope.parquet"
    ),
    file.path(
      root, "data", "derived", "04_investigation_subpopulation.parquet"
    )
  )
  stage04_path <- stage04_candidates[file.exists(stage04_candidates)][1L]
  completeness_scalars <- read_optional_report_table(scalar_path)
  completeness_stage04 <- read_optional_report_table(stage04_path)
  required_scalar <- c("Death_Prov", "Sex", "DeathYear", "age5", "S2")
  required_stage04 <- c(
    "Death_Prov", "Sex", "DeathYear", "age5", "Popgroup",
    "nbdcode", "Deaths"
  )
  if (all(required_scalar %in% names(completeness_scalars)) &&
      all(required_stage04 %in% names(completeness_stage04))) {
    freeze_year <- as.integer(
      analysis_config$settings$completeness_freeze_year %||% 2010L
    )
    african_natural <- completeness_stage04[
      Popgroup == 1L & !nbdcode %in% report_injury_codes,
      .(adjusted_african_natural = sum(Deaths, na.rm = TRUE)),
      by = .(Death_Prov, Sex, DeathYear, age5)
    ]
    candidate <- merge(
      completeness_scalars[, .(
        Death_Prov = as.integer(Death_Prov),
        Sex = as.integer(Sex),
        DeathYear = as.integer(DeathYear),
        age5 = as.integer(age5),
        S2 = as.numeric(S2)
      )],
      african_natural,
      by = c("Death_Prov", "Sex", "DeathYear", "age5"),
      all.x = TRUE,
      sort = FALSE
    )
    candidate <- candidate[
      DeathYear <= freeze_year & age5 != 1L &
        is.finite(S2) & S2 > 0 &
        is.finite(adjusted_african_natural) &
        adjusted_african_natural > 0
    ]
    candidate[, pre_adjustment_african_natural :=
                adjusted_african_natural / S2]
    completeness_time_data <- candidate[
      is.finite(pre_adjustment_african_natural) &
        pre_adjustment_african_natural > 0,
      .(
        contributing_sex_age_cells = .N,
        contributing_sexes = data.table::uniqueN(Sex),
        contributing_age_groups = data.table::uniqueN(age5),
        adjusted_african_natural = sum(adjusted_african_natural),
        pre_adjustment_african_natural = sum(
          pre_adjustment_african_natural
        )
      ),
      by = .(Death_Prov, DeathYear)
    ]
    completeness_time_data[, S2 :=
      adjusted_african_natural / pre_adjustment_african_natural]
    completeness_time_data[, log_s2 := log(S2)]
  }
}

if (nrow(completeness_time_data)) {
  completeness_time_data[, `:=`(
    Death_Prov = as.integer(Death_Prov),
    DeathYear = as.integer(DeathYear),
    S2 = as.numeric(S2)
  )]
  completeness_time_data[, Province := unname(
    report_province_labels[as.character(Death_Prov)]
  )]
  completeness_time_data[is.na(Province), Province := paste(
    "Province", Death_Prov
  )]
  completeness_time_data[, implied_completeness := 100 / S2]
  data.table::setorder(completeness_time_data, Death_Prov, DeathYear)
}

completeness_province_data <- read_optional_report_table(report_method_asset(
  config,
  "completeness_by_province.csv",
  fallback = if (!is.na(uncertainty_report_root)) file.path(
    uncertainty_report_root, "completeness_by_province.csv"
  ) else NA_character_
))

if (!nrow(completeness_province_data) && nrow(completeness_time_data)) {
  completeness_province_data <- completeness_time_data[, {
    weights <- as.numeric(pre_adjustment_african_natural)
    values <- as.numeric(log(S2))
    weighted_mean_log <- sum(weights * values) / sum(weights)
    list(
      eligible_years = data.table::uniqueN(DeathYear),
      pre_adjustment_deaths = sum(pre_adjustment_african_natural),
      adjusted_deaths = sum(adjusted_african_natural),
      aggregate_s2 = sum(adjusted_african_natural) /
        sum(pre_adjustment_african_natural),
      log_sd = sqrt(sum(weights * (values - weighted_mean_log)^2) /
        sum(weights)),
      minimum_annual_s2 = min(S2),
      median_annual_s2 = stats::median(S2),
      maximum_annual_s2 = max(S2)
    )
  }, by = Death_Prov]
}

if (nrow(completeness_province_data)) {
  completeness_province_data[, Death_Prov := as.integer(Death_Prov)]
  completeness_province_data[, Province := unname(
    report_province_labels[as.character(Death_Prov)]
  )]
  completeness_province_data[is.na(Province), Province := paste(
    "Province", Death_Prov
  )]
  for (column in c(
    "aggregate_s2", "minimum_annual_s2", "median_annual_s2",
    "maximum_annual_s2", "log_sd"
  )) {
    if (!column %in% names(completeness_province_data)) {
      completeness_province_data[, (column) := NA_real_]
    }
  }
  completeness_province_data[, `:=`(
    implied_completeness = 100 / aggregate_s2,
    minimum_annual_completeness = 100 / maximum_annual_s2,
    median_annual_completeness = 100 / median_annual_s2,
    maximum_annual_completeness = 100 / minimum_annual_s2
  )]
  data.table::setorder(completeness_province_data, Death_Prov)
}

injury_anchor_data <- read_optional_report_table(report_method_asset(
  config,
  "04_injury_envelope_anchors.parquet",
  fallback = file.path(
    root, "data", "derived", "04_injury_envelope_anchors.parquet"
  )
))
if (nrow(injury_anchor_data)) {
  anchor_columns <- intersect(c(
    "survey_year", "survey", "survey_level_total", "routine_level_total",
    "published_survey_level_total", "published_survey_level_lower",
    "published_survey_level_upper", "derived_survey_level_source"
  ), names(injury_anchor_data))
  injury_anchor_data <- unique(injury_anchor_data[, ..anchor_columns])
  data.table::setorder(injury_anchor_data, survey_year)
}

# The live report uses the compact catalogues created above. An empty legacy
# availability object is retained only for backward compatibility with older
# report-state setup code; interactive selectors no longer read it.
result_availability <- data.table::data.table()

report_hierarchy_labels <- c(
  broad = "All causes and broad causes",
  reporting = "Reporting aggregates",
  group = "Disease categories",
  detailed = "Detailed causes",
  custom = "Custom aggregates"
)
result_hierarchies <- intersect(
  names(report_hierarchy_labels), unique(result_cause_catalog$hierarchy)
)
result_hierarchy_choices <- c(
  "All hierarchy levels" = "all",
  stats::setNames(result_hierarchies, report_hierarchy_labels[result_hierarchies])
)

# Release large build-only objects before serialisation.
rm(result_rows, full_availability, full_catalog, runtime)
invisible(gc())
