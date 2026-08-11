# ==============================================================================
# 03_completeness_hiv: Completeness adjustment and HIV/AIDS reallocation
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Under-registration and NPR adjustment
# ------------------------------------------------------------------------------

# Registration completeness and NPR adjustment --------------------------------

read_child_completeness <- function(cfg) {
  require_package("data.table")

  x <- read_tabular(raw_file(cfg, "completeness_child", must_exist = TRUE))
  x <- rename_first_match(x, "age5", c("Age5", "age"), TRUE)
  x <- rename_first_match(x, "Death_Prov", c("province", "DeathProv"), TRUE)

  year_columns <- grep("^[yY]?[0-9]{4}$", names(x), value = TRUE)
  if (!length(year_columns)) {
    stop("Child completeness file has no columns named yYYYY or YYYY.", call. = FALSE)
  }

  long <- data.table::melt(
    x,
    id.vars = c("age5", "Death_Prov"),
    measure.vars = year_columns,
    variable.name = "year_field",
    value.name = "completeness",
    variable.factor = FALSE
  )

  long[, `:=`(
    age5 = suppressWarnings(as.integer(age5)),
    Death_Prov = suppressWarnings(as.integer(Death_Prov)),
    DeathYear = suppressWarnings(as.integer(gsub("[^0-9]", "", year_field))),
    completeness = suppressWarnings(as.numeric(completeness))
  )]
  long[, year_field := NULL]

  years <- seq.int(
    as.integer(cfg$settings$start_year),
    as.integer(cfg$settings$completeness_freeze_year)
  )

  # The source supplies factors for post-neonatal infants (age5 2) and
  # children aged 1-4 years (age5 3). Older ages receive factor one. A
  # separate neonatal factor is not estimated in the legacy method, so age5 1
  # inherits the age5 2 value.
  source <- long[
    Death_Prov %in% 1:9 & DeathYear %in% years & age5 %in% 2:3
  ]
  assert_unique_key(
    source,
    c("Death_Prov", "DeathYear", "age5"),
    "child completeness source"
  )

  grid <- data.table::CJ(
    Death_Prov = 1:9,
    DeathYear = years,
    age5 = 2:20,
    sorted = FALSE
  )
  out <- merge(
    grid,
    source,
    by = c("Death_Prov", "DeathYear", "age5"),
    all.x = TRUE,
    sort = FALSE
  )

  out[age5 >= 4L, completeness := 1]

  missing_child <- out[
    age5 %in% 2:3 & !is.finite(completeness),
    .N
  ]
  if (missing_child) {
    warn_or_stop(
      paste0(
        "Child completeness input is missing ", missing_child,
        " required province-year-age values for age5 2 or 3."
      ),
      strict = isTRUE(cfg$settings$strict_checks)
    )
    out[age5 %in% 2:3 & !is.finite(completeness), completeness := 1]
  }

  invalid <- out[!is.finite(completeness) | completeness < 0, .N]
  if (invalid) {
    warn_or_stop(
      paste0(
        "Child completeness input has ", invalid,
        " non-finite or negative value(s)."
      ),
      strict = isTRUE(cfg$settings$strict_checks)
    )
    out[!is.finite(completeness) | completeness < 0, completeness := 1]
  }

  neonatal <- data.table::copy(out[age5 == 2L])
  neonatal[, age5 := 1L]
  out <- data.table::rbindlist(list(neonatal, out), use.names = TRUE)

  assert_unique_key(
    out,
    c("Death_Prov", "DeathYear", "age5"),
    "child completeness panel"
  )
  data.table::setorder(out, DeathYear, Death_Prov, age5)
  out[]
}


read_adjusted_deaths_by_province <- function(cfg) {
  require_package("readxl")
  require_package("data.table")
  path <- raw_file(cfg, "completeness_province", must_exist = TRUE)
  sheet_codes <- c("WC", "EC", "NC", "FS", "KZ", "NW", "GT", "MP", "LM")
  available <- readxl::excel_sheets(path)
  missing <- setdiff(sheet_codes, available)
  if (length(missing)) {
    stop("Completeness workbook is missing sheet(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  pieces <- lapply(seq_along(sheet_codes), function(i) {
    x <- data.table::as.data.table(readxl::read_excel(path, sheet = sheet_codes[[i]], .name_repair = "unique_quiet"))
    x <- rename_first_match(x, "Sex", c("sex"), TRUE)
    x <- rename_first_match(x, "age5", c("age", "Age5"), TRUE)
    text_columns <- names(x)[normalise_token(names(x)) == "agetext"]
    if (length(text_columns)) x[, (text_columns) := NULL]
    x[, Death_Prov := as.integer(i)]
    x
  })
  x <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  year_columns <- grep("^[Yy]?[0-9]{4}$", names(x), value = TRUE)
  if (!length(year_columns)) stop("Adjusted-deaths workbook has no YYYYY/year columns.", call. = FALSE)

  out <- data.table::melt(
    x,
    id.vars = c("Sex", "age5", "Death_Prov"),
    measure.vars = year_columns,
    variable.name = "year_field",
    value.name = "Adj",
    variable.factor = FALSE
  )
  out[, `:=`(
    Sex = as.integer(Sex),
    age5 = as.integer(age5),
    Death_Prov = as.integer(Death_Prov),
    DeathYear = as.integer(gsub("[^0-9]", "", year_field)),
    Adj = suppressWarnings(as.numeric(Adj))
  )]
  out[, year_field := NULL]
  out <- out[!is.na(Sex) & !is.na(age5) & !is.na(DeathYear)]
  out[DeathYear == 1998L & age5 >= 6L, Adj := Adj / 1.042]

  child <- read_child_completeness(cfg)
  out <- merge(out, child, by = c("Death_Prov", "DeathYear", "age5"), all.x = TRUE, sort = FALSE)
  out[is.na(completeness), completeness := 1]
  out[, Adj := Adj * completeness]
  out[, completeness := NULL]
  out[, .(Adj = sum(Adj, na.rm = TRUE)), by = .(Death_Prov, Sex, DeathYear, age5)]
}

derive_completeness_scalars <- function(natural, injury, adjusted, cfg) {
  require_package("data.table")
  natural <- data.table::as.data.table(data.table::copy(natural))
  injury <- data.table::as.data.table(data.table::copy(injury))
  natural[, Inj := 2L]
  if (!("Inj" %in% names(injury))) injury[, Inj := 1L]
  combined <- data.table::rbindlist(list(natural, injury), use.names = TRUE, fill = TRUE)
  combined[, Pop_class := data.table::fifelse(Popgroup == 1L, 1L, 2L)]
  combined[age5 == 1L, age5 := 2L]
  summary <- combined[, .(D = sum(Deaths, na.rm = TRUE)), by = .(
    Inj, Pop_class, Sex, DeathYear, Death_Prov, age5
  )]
  summary[, component := paste0("D", Pop_class, Inj)]
  wide <- data.table::dcast(
    summary,
    Sex + DeathYear + Death_Prov + age5 ~ component,
    value.var = "D",
    fill = 0
  )
  for (column in c("D11", "D12", "D21", "D22")) {
    if (!column %in% names(wide)) wide[, (column) := 0]
  }
  wide <- merge(wide, adjusted, by = c("Death_Prov", "Sex", "DeathYear", "age5"), all.x = TRUE, sort = FALSE)
  freeze <- as.integer(cfg$settings$completeness_freeze_year)

  # The adjusted-deaths workbook ends at the freeze year in the legacy
  # pipeline. Post-freeze rows are placeholders because their S1/S2 values are
  # replaced below with the freeze-year scalars. Missing values at or before
  # the freeze year remain a validation error under strict checks.
  missing_adj <- wide[DeathYear <= freeze & is.na(Adj), .N]
  if (missing_adj) {
    warn_or_stop(
      paste0(
        "Adjusted death totals are missing for ", missing_adj,
        " completeness strata at or before ", freeze, "."
      ),
      strict = isTRUE(cfg$settings$strict_checks)
    )
  }
  wide[is.na(Adj), Adj := D11 + D12 + D21 + D22]

  wide[, observed_non_african_natural := D11 + D21 + D22]
  wide[, S1 := data.table::fifelse(
    observed_non_african_natural > 0,
    Adj / observed_non_african_natural,
    1
  )]
  for (column in c("D11", "D21", "D22")) {
    wide[S1 < 1, (column) := get(column) * S1]
  }
  wide[, other_total := D11 + D21 + D22]
  wide[, S2 := data.table::fifelse(D12 > 0, (Adj - other_total) / D12, 1)]

  # Proportional scaling can leave machine-precision negative residues when
  # other_total should equal Adj exactly. Values within tolerance are zero;
  # materially negative or non-finite scalars are a model/data error.
  scalar_tolerance <- 1e-10
  wide[S1 < 0 & S1 >= -scalar_tolerance, S1 := 0]
  wide[S2 < 0 & S2 >= -scalar_tolerance, S2 := 0]
  invalid_scalars <- wide[
    !is.finite(S1) | !is.finite(S2) |
      S1 < -scalar_tolerance | S2 < -scalar_tolerance
  ]
  if (nrow(invalid_scalars)) {
    first <- invalid_scalars[1]
    stop(
      nrow(invalid_scalars),
      " materially invalid completeness scalar(s). First stratum: province=",
      first$Death_Prov, ", sex=", first$Sex, ", year=", first$DeathYear,
      ", age5=", first$age5, ", S1=", signif(first$S1, 12),
      ", S2=", signif(first$S2, 12), ".",
      call. = FALSE
    )
  }
  scales <- wide[, .(Death_Prov, Sex, DeathYear, age5, S1, S2)]

  # Copy post-neonatal completeness to neonates.
  neonatal <- data.table::copy(scales[age5 == 2L])
  neonatal[, age5 := 1L]
  scales <- data.table::rbindlist(list(scales[age5 != 1L], neonatal), use.names = TRUE)

  reference <- scales[DeathYear == freeze, .(
    Death_Prov, Sex, age5,
    S1_reference = S1,
    S2_reference = S2
  )]
  scales <- merge(scales, reference, by = c("Death_Prov", "Sex", "age5"), all.x = TRUE, sort = FALSE)
  missing_reference <- scales[DeathYear > freeze & (is.na(S1_reference) | is.na(S2_reference)), .N]
  if (missing_reference) {
    warn_or_stop(
      paste0(
        "Freeze-year completeness scalars are missing for ", missing_reference,
        " post-", freeze, " rows."
      ),
      strict = isTRUE(cfg$settings$strict_checks)
    )
  }
  scales[DeathYear > freeze & !is.na(S1_reference), S1 := S1_reference]
  scales[DeathYear > freeze & !is.na(S2_reference), S2 := S2_reference]
  scales[, c("S1_reference", "S2_reference") := NULL]
  assert_unique_key(scales, c("Death_Prov", "Sex", "DeathYear", "age5"), "completeness scalars")
  scales[]
}

apply_completeness_scalars <- function(natural, injury, scales, cfg) {
  require_package("data.table")
  natural <- data.table::as.data.table(data.table::copy(natural))
  injury <- data.table::as.data.table(data.table::copy(injury))
  natural[, Inj := 2L]
  if (!("Inj" %in% names(injury))) injury[, Inj := 1L]
  x <- data.table::rbindlist(list(natural, injury), use.names = TRUE, fill = TRUE)
  x[, Pop_class := data.table::fifelse(Popgroup == 1L, 1L, 2L)]
  x <- merge(x, scales, by = c("Death_Prov", "Sex", "DeathYear", "age5"), all.x = TRUE, sort = FALSE)
  missing <- x[is.na(S1) | is.na(S2), .N]
  if (missing) {
    warn_or_stop(
      paste0("Completeness scalar missing for ", missing, " cause-specific rows; using 1."),
      strict = isTRUE(cfg$settings$strict_checks)
    )
    x[is.na(S1), S1 := 1]
    x[is.na(S2), S2 := 1]
  }

  x[, Scale := 1]
  x[Pop_class == 1L & Inj == 2L, Scale := S2]
  x[((Pop_class == 1L & Inj == 1L) | Pop_class == 2L) & S1 < 1, Scale := S1]
  x[, Deaths := Deaths * Scale]
  x[!is.finite(Deaths) | Deaths < 0, Deaths := 0]
  x[, c("Pop_class", "S1", "S2", "Scale") := NULL]
  x[, .(Deaths = sum(Deaths)), by = .(
    Death_Prov, Sex, DeathYear, Popgroup, Inj, nbdcode, age5
  )]
}

prepare_npr <- function(cfg) {
  require_package("data.table")
  x <- read_tabular(raw_file(cfg, "npr", must_exist = TRUE))
  x <- rename_first_match(x, "Sex", c("sex"), TRUE)
  x <- rename_first_match(x, "DeathYear", c("yd", "year"), TRUE)
  x <- rename_first_match(x, "Province", c("province"), TRUE)
  x <- rename_first_match(x, "npr", c("deaths", "count"), TRUE)
  x <- rename_first_match(x, "age", c("agegrp95", "age_group"), TRUE)
  x <- rename_first_match(x, "type", c("Type"), TRUE)
  for (column in c("Sex", "DeathYear", "npr", "age", "type")) x[, (column) := suppressWarnings(as.numeric(get(column)))]
  x[, Province := toupper(trimws(as.character(Province)))]
  province_map <- c(WC = 1L, EC = 2L, NC = 3L, FS = 4L, KZN = 5L, KZ = 5L,
                    NW = 6L, GT = 7L, GP = 7L, MP = 8L, LM = 9L, LP = 9L)
  x[, Death_Prov := as.integer(province_map[Province])]
  x <- x[!is.na(Death_Prov)]
  x[, Inj := as.integer(2 - type)]
  x[, age5 := as.integer(3 + age / 5)]
  x[age5 < 4, age5 := 3L]
  x[age == 0, age5 := 2L]
  x[age5 > 20, age5 := 20L]
  x <- x[
    DeathYear <= as.integer(cfg$settings$end_year),
    .(npr = sum(npr, na.rm = TRUE)),
    by = .(Death_Prov, age5, Sex, DeathYear, Inj)
  ]
  x[]
}

apply_npr_adjustment <- function(data, npr, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  start <- as.integer(cfg$settings$npr_start_year)

  # Calibrate NPR to vital-registration deaths using the combined 2015-2016
  # relationship, separately by province, age, sex and natural/injury status.
  vr_cal <- data.table::copy(x[DeathYear %in% c(2015L, 2016L)])
  vr_cal[age5 < 2L, age5 := 2L]
  vr_cal <- vr_cal[, .(Deaths = sum(Deaths)), by = .(Death_Prov, age5, Sex, DeathYear, Inj)]
  calibration <- merge(
    vr_cal,
    npr[DeathYear %in% c(2015L, 2016L)],
    by = c("Death_Prov", "age5", "Sex", "DeathYear", "Inj"),
    all = TRUE,
    sort = FALSE
  )
  calibration <- calibration[, .(
    Deaths = sum(Deaths, na.rm = TRUE),
    npr = sum(npr, na.rm = TRUE)
  ), by = .(Death_Prov, age5, Sex, Inj)]
  # A missing/zero NPR denominator produced a missing rescale in Stata and
  # therefore no downstream adjustment. Keep it missing rather than replacing
  # it with one here, which would incorrectly apply raw NPR counts.
  calibration[, npr_rescale := data.table::fifelse(
    npr > 0,
    Deaths / npr,
    NA_real_
  )]
  calibration <- calibration[, .(Death_Prov, age5, Sex, Inj, npr_rescale)]

  npr_future <- merge(
    npr[DeathYear >= start],
    calibration,
    by = c("Death_Prov", "age5", "Sex", "Inj"),
    all.x = TRUE,
    sort = FALSE
  )
  npr_future[, npr_scaled := npr * npr_rescale]

  vr_future <- data.table::copy(x[DeathYear >= start])
  vr_future[age5 < 2L, age5 := 2L]
  vr_future <- vr_future[, .(Deaths = sum(Deaths)), by = .(Death_Prov, age5, Sex, DeathYear, Inj)]
  scalar <- merge(
    vr_future,
    npr_future[, .(Death_Prov, age5, Sex, DeathYear, Inj, npr_scaled)],
    by = c("Death_Prov", "age5", "Sex", "DeathYear", "Inj"),
    all.x = TRUE,
    sort = FALSE
  )
  scalar[, npr_scalar := data.table::fifelse(
    Deaths > 0 & !is.na(npr_scaled),
    npr_scaled / Deaths,
    1
  )]
  scalar <- scalar[, .(Death_Prov, age5, Sex, DeathYear, Inj, npr_scalar)]
  neonatal <- data.table::copy(scalar[age5 == 2L])
  neonatal[, age5 := 1L]
  scalar <- data.table::rbindlist(list(scalar, neonatal), use.names = TRUE)

  future <- merge(
    x[DeathYear >= start], scalar,
    by = c("Death_Prov", "age5", "Sex", "DeathYear", "Inj"),
    all.x = TRUE,
    sort = FALSE
  )
  future[is.na(npr_scalar), npr_scalar := 1]
  future[, Deaths := Deaths * npr_scalar]
  future[, npr_scalar := NULL]
  out <- data.table::rbindlist(list(x[DeathYear < start], future), use.names = TRUE, fill = TRUE)
  out[nbdcode == 142L, nbdcode := 141L]
  out <- out[, .(Deaths = sum(Deaths, na.rm = TRUE)), by = .(
    Death_Prov, DeathYear, Popgroup, age5, Sex, nbdcode
  )]
  assert_nonnegative(out, "Deaths")
  out[]
}

build_national_investigation <- function(subpopulation, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(subpopulation)
  national <- x[, .(adjNBDcount = sum(Deaths, na.rm = TRUE)), by = .(
    Sex, DeathYear, nbdcode, age5
  )]
  parameter_path <- raw_file(cfg, "investigation_parameters")
  if (file.exists(parameter_path)) {
    parameters <- read_tabular(parameter_path)
    parameters <- rename_first_match(parameters, "Sex", c("sex"), TRUE)
    parameters <- rename_first_match(parameters, "DeathYear", c("year"), TRUE)
    parameters <- rename_first_match(parameters, "age5", c("age"), TRUE)
    assert_unique_key(parameters, c("Sex", "DeathYear", "age5"), "investigation parameters")
    national <- merge(national, parameters, by = c("Sex", "DeathYear", "age5"), all.x = TRUE, sort = FALSE)
  }
  national[]
}

# ------------------------------------------------------------------------------
# Antenatal HIV prevalence preparation
# ------------------------------------------------------------------------------

# Province ANC prevalence preparation ------------------------------------------
#
# This module translates `Prevalence estimates.do`. It converts the province
# prevalence series in `ProvPrevs.csv` to the mortality-pipeline age/year panel,
# merges it with the population denominators prepared in Stage 01, and returns
# the prevalence-population input used by the HIV misclassification model.

standardise_anc_prevalence_source <- function(data) {
  require_package("data.table")

  x <- data.table::as.data.table(data.table::copy(data))
  normalised <- normalise_token(names(x))

  # Accept either the original wide P1-P9 layout or an already-long extract.
  long_required <- c("year", "deathprov", "ancprev")
  if (all(long_required %in% normalised)) {
    x <- rename_first_match(x, "Year", c("year"), TRUE)
    x <- rename_first_match(x, "Death_Prov", c("province", "deathprov"), TRUE)
    x <- rename_first_match(x, "ANCPrev", c("prevalence", "prev"), TRUE)
    out <- x[, .(Year, Death_Prov, ANCPrev)]
  } else {
    x <- rename_first_match(x, "Year", c("year"), TRUE)
    province_columns <- names(x)[grepl("^P[1-9]$", names(x), ignore.case = TRUE)]
    if (length(province_columns) != 9L) {
      stop(
        "Province prevalence input must contain Year and P1-P9, or long fields ",
        "Year/Death_Prov/ANCPrev.",
        call. = FALSE
      )
    }
    out <- data.table::melt(
      x,
      id.vars = "Year",
      measure.vars = province_columns,
      variable.name = "province_column",
      value.name = "ANCPrev",
      variable.factor = FALSE
    )
    out[, Death_Prov := as.integer(sub("^[Pp]", "", province_column))]
    out[, province_column := NULL]
  }

  for (column in c("Year", "Death_Prov", "ANCPrev")) {
    out[, (column) := suppressWarnings(as.numeric(get(column)))]
  }
  out <- out[Death_Prov %in% 1:9 & is.finite(Year)]
  out <- out[, .(
    ANCPrev = if (any(is.finite(ANCPrev))) mean(ANCPrev[is.finite(ANCPrev)]) else NA_real_
  ), by = .(Year = as.integer(Year), Death_Prov = as.integer(Death_Prov))]
  assert_unique_key(out, c("Year", "Death_Prov"), "province ANC prevalence source")
  out[]
}

build_calendar_anc_prevalence <- function(source, cfg) {
  require_package("data.table")

  x <- standardise_anc_prevalence_source(source)
  start_year <- as.integer(cfg$settings$start_year)
  anchor_end <- min(2009L, as.integer(cfg$settings$end_year))
  if (anchor_end < start_year) {
    stop("The configured period ends before the ANC prevalence construction period.", call. = FALSE)
  }

  # The legacy program creates 19 age rows, then adds one to the age code. The
  # resulting pipeline ages are 2-20. Calendar-year prevalence is lagged by one
  # year for age 2, three years for age 3, and five years for ages 4-20.
  panel <- data.table::CJ(
    Death_Prov = 1:9,
    DeathYear = seq.int(start_year, anchor_end),
    age5 = 2:20,
    sorted = FALSE
  )
  panel[, prevalence_lag := data.table::fcase(
    age5 == 2L, 1L,
    age5 == 3L, 3L,
    default = 5L
  )]
  panel[, source_year := DeathYear - prevalence_lag]

  # Prevalence estimates.do creates Y2008 as an exact copy of Y2007 before
  # applying the age-specific calendar shifts. Map every request for source
  # year 2008 to 2007 directly so the R implementation does not depend on
  # whether a supplied source file happens to contain a 2008 column.
  panel[source_year == 2008L, source_year := 2007L]

  lookup <- x[, .(Death_Prov, source_year = Year, ANCPrev)]
  panel <- merge(
    panel,
    lookup,
    by = c("Death_Prov", "source_year"),
    all.x = TRUE,
    sort = FALSE
  )

  # After constructing the lagged calendar series, the Stata source replaces a
  # still-missing 2009 value with the corresponding 2008 calendar estimate.
  # This is distinct from the source-year 2008-to-2007 substitution above and
  # matters when an earlier source value is itself missing.
  fallback_calendar_2008 <- panel[
    DeathYear == 2008L,
    .(Death_Prov, age5, ANCPrevCalendar2008 = ANCPrev)
  ]
  panel <- merge(
    panel,
    fallback_calendar_2008,
    by = c("Death_Prov", "age5"),
    all.x = TRUE,
    sort = FALSE
  )
  panel[
    DeathYear == 2009L & !is.finite(ANCPrev),
    ANCPrev := ANCPrevCalendar2008
  ]
  panel[, c("ANCPrevCalendar2008", "prevalence_lag", "source_year") := NULL]

  assert_unique_key(
    panel,
    c("Death_Prov", "DeathYear", "age5"),
    "calendar-year ANC prevalence"
  )
  panel[]
}

prepare_prevalence_population <- function(source, population, cfg) {
  require_package("data.table")

  prevalence <- build_calendar_anc_prevalence(source, cfg)
  pop <- data.table::as.data.table(data.table::copy(population))
  assert_has_columns(
    pop,
    c("Death_Prov", "Sex", "DeathYear", "age5", "Pop"),
    "prepared population"
  )
  pop <- pop[
    Death_Prov %in% 1:9 & Sex %in% 1:2,
    .(ASSABase = sum(Pop, na.rm = TRUE)),
    by = .(Death_Prov, Sex, DeathYear, age5)
  ]

  # Stata's merge retains population-only rows. This is important for 2010-2019:
  # ANC prevalence is initially missing there and is subsequently frozen at the
  # 2009 value by standardise_prevalence_population().
  panel <- merge(
    prevalence,
    pop,
    by = c("Death_Prov", "DeathYear", "age5"),
    all = TRUE,
    sort = FALSE
  )
  panel <- standardise_prevalence_population(panel, cfg)
  data.table::setorder(panel, DeathYear, Death_Prov, Sex, age5)
  panel[]
}

# ------------------------------------------------------------------------------
# HIV/AIDS statistical model and reallocation
# ------------------------------------------------------------------------------

# HIV misclassification model --------------------------------------------------
#
# This module translates 3 HIV estimation meglm.do into explicit stages:
# 1. combine selected registered causes into HIV pseudo-cause groups;
# 2. estimate background trends from ages 75-84;
# 3. estimate zero-prevalence all-cause intercepts from 1997-2003;
# 4. derive non-HIV baselines and misclassified HIV deaths;
# 5. allocate those deaths back to population groups and consolidate residual
#    non-HIV deaths into the destination analysis causes.

standardise_prevalence_population <- function(data, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  x <- rename_first_match(x, "Death_Prov", c("province", "DeathProv"), TRUE)
  x <- rename_first_match(x, "Sex", c("sex"), TRUE)
  x <- rename_first_match(x, "DeathYear", c("year", "deathyear"), TRUE)
  x <- rename_first_match(x, "age5", c("age", "age_group"), TRUE)
  x <- rename_first_match(x, "ASSABase", c("Pop", "population", "N"), TRUE)
  x <- rename_first_match(x, "ANCPrev", c("prevalence", "anc_prev", "prev"), TRUE)
  for (column in c("Death_Prov", "Sex", "DeathYear", "age5", "ASSABase", "ANCPrev")) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
  }
  x <- x[
    DeathYear >= as.integer(cfg$settings$start_year) &
      DeathYear <= as.integer(cfg$settings$end_year)
  ]

  # Reconstruct national and both-sex rows from the nine province x two sex
  # base cells. Population-weighted prevalence is used where the helper Stata
  # program would otherwise have supplied an aggregate row.
  base <- x[Death_Prov %in% 1:9 & Sex %in% 1:2]
  weighted_group <- function(dt, by) {
    dt[, {
      valid_population <- is.finite(ASSABase) & ASSABase > 0
      population_total <- sum(ASSABase[valid_population], na.rm = TRUE)
      valid_prevalence <- valid_population & is.finite(ANCPrev)
      prevalence_denominator <- sum(ASSABase[valid_prevalence], na.rm = TRUE)
      prevalence <- if (prevalence_denominator > 0) {
        sum(ANCPrev[valid_prevalence] * ASSABase[valid_prevalence]) / prevalence_denominator
      } else {
        candidate <- mean(ANCPrev[is.finite(ANCPrev)], na.rm = TRUE)
        if (is.finite(candidate)) candidate else NA_real_
      }
      .(ASSABase = population_total, ANCPrev = prevalence)
    }, by = by]
  }
  # Collapse duplicate source rows before constructing aggregate geography/sex
  # cells. This prevents `unique()` from silently retaining an arbitrary row.
  base <- weighted_group(base, c("Death_Prov", "Sex", "DeathYear", "age5"))
  both_sex <- weighted_group(base, c("Death_Prov", "DeathYear", "age5"))
  both_sex[, Sex := 3L]
  national_sex <- weighted_group(base, c("Sex", "DeathYear", "age5"))
  national_sex[, Death_Prov := 10L]
  national_both <- weighted_group(base, c("DeathYear", "age5"))
  national_both[, `:=`(Death_Prov = 10L, Sex = 3L)]
  x <- data.table::rbindlist(
    list(base, both_sex, national_sex, national_both),
    use.names = TRUE,
    fill = TRUE
  )
  key <- c("Death_Prov", "Sex", "DeathYear", "age5")
  x <- unique(x, by = key)
  assert_no_missing(x, key, "prevalence/population panel")
  assert_unique_key(x, key, "prevalence/population panel")

  # ANC prevalence is held at its 2009 value after 2009, while population
  # denominators continue to vary by year.
  prev_2009 <- x[DeathYear == 2009L, .(
    Death_Prov, Sex, age5, ANCPrev2009 = ANCPrev
  )]
  assert_unique_key(prev_2009, c("Death_Prov", "Sex", "age5"), "2009 prevalence values")
  x <- merge(x, prev_2009, by = c("Death_Prov", "Sex", "age5"), all.x = TRUE, sort = FALSE)
  # The Stata reshape overwrites every post-2009 value, including with missing
  # when a 2009 prevalence value is unavailable.
  x[DeathYear >= 2010L, ANCPrev := ANCPrev2009]
  x[, ANCPrev2009 := NULL]
  x[!is.finite(ASSABase) | ASSABase < 0, ASSABase := 0]
  x[]
}

read_prevalence_population <- function(path, cfg) {
  standardise_prevalence_population(read_tabular(path), cfg)
}

expand_counts_to_national_both_sex <- function(data) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  base <- x[Death_Prov %in% 1:9 & Sex %in% 1:2]
  value_columns <- setdiff(names(base), c("Death_Prov", "Sex", "DeathYear", "age5", "nbdcode"))
  aggregate_counts <- function(dt, by) dt[, lapply(.SD, sum, na.rm = TRUE), by = by, .SDcols = value_columns]
  both <- aggregate_counts(base, c("Death_Prov", "DeathYear", "age5", "nbdcode"))
  both[, Sex := 3L]
  national <- aggregate_counts(base, c("Sex", "DeathYear", "age5", "nbdcode"))
  national[, Death_Prov := 10L]
  national_both <- aggregate_counts(base, c("DeathYear", "age5", "nbdcode"))
  national_both[, `:=`(Death_Prov = 10L, Sex = 3L)]
  data.table::rbindlist(list(base, both, national, national_both), use.names = TRUE, fill = TRUE)
}

build_pseudo_cause_counts <- function(subpopulation, include_aggregates = TRUE) {
  require_package("data.table")
  x <- data.table::as.data.table(subpopulation)[,
    .(NBD = sum(Deaths, na.rm = TRUE)),
    by = .(Death_Prov, Sex, DeathYear, age5, nbdcode)
  ]
  pieces <- lapply(names(HIV_PSEUDO_SOURCE_MAP), function(code) {
    sources <- HIV_PSEUDO_SOURCE_MAP[[code]]
    out <- x[nbdcode %in% sources, .(NBD = sum(NBD)), by = .(
      Death_Prov, Sex, DeathYear, age5
    )]
    out[, nbdcode := as.integer(code)]
    out
  })
  direct <- x[nbdcode %in% c(0L, 2L)]
  out <- data.table::rbindlist(c(pieces, list(direct)), use.names = TRUE, fill = TRUE)
  if (isTRUE(include_aggregates)) out <- expand_counts_to_national_both_sex(out)
  out[]
}

build_pseudo_population_counts <- function(subpopulation) {
  require_package("data.table")
  x <- data.table::as.data.table(subpopulation)
  pieces <- lapply(names(HIV_PSEUDO_SOURCE_MAP), function(code) {
    out <- x[nbdcode %in% HIV_PSEUDO_SOURCE_MAP[[code]],
      .(NBD = sum(Deaths, na.rm = TRUE)),
      by = .(Death_Prov, Sex, DeathYear, age5, Popgroup)
    ]
    out[, nbdcode := as.integer(code)]
    out
  })
  direct <- x[nbdcode %in% c(0L, 2L), .(
    NBD = sum(Deaths, na.rm = TRUE)
  ), by = .(Death_Prov, Sex, DeathYear, age5, Popgroup, nbdcode)]
  data.table::rbindlist(c(pieces, list(direct)), use.names = TRUE, fill = TRUE)
}

estimate_hiv_base_year <- function(prevalence) {
  require_package("data.table")
  rows <- prevalence[age5 == 4L & Sex == 1L & DeathYear < 2003L & Death_Prov %in% 1:10]
  estimates <- rows[, {
    valid <- is.finite(ANCPrev) & is.finite(DeathYear)
    intercept <- 1997
    if (sum(valid) >= 2L && length(unique(ANCPrev[valid])) >= 2L) {
      fit <- stats::lm(DeathYear[valid] ~ ANCPrev[valid])
      intercept <- unname(stats::coef(fit)[[1L]])
    }
    .(base_year_candidate = intercept)
  }, by = Death_Prov]
  if (!nrow(estimates)) return(1997L)
  estimates[, base_year_candidate := data.table::fifelse(
    is.finite(base_year_candidate) & base_year_candidate < 1997,
    base_year_candidate,
    1997
  )]
  candidate <- min(estimates$base_year_candidate, na.rm = TRUE)
  if (!is.finite(candidate)) candidate <- 1997
  # Stata retains the fractional zero-prevalence intercept. Do not round or
  # floor it: doing so changes every subsequent log-time value.
  as.numeric(candidate)
}

hiv_background_model_codes <- function(cfg) {
  codes <- HIV_PSEUDO_CODES
  include_234 <- isTRUE(cfg$settings$hiv_include_pseudo_234_in_background_model)
  if (!include_234) codes <- setdiff(codes, 234L)
  codes
}

hiv_finite_covariance_block <- function(fit) {
  beta <- stats::coef(fit)
  covariance <- tryCatch(stats::vcov(fit), error = function(error) NULL)
  if (is.null(covariance) || !length(beta)) return(NULL)

  coefficient_names <- intersect(names(beta), rownames(covariance))
  coefficient_names <- coefficient_names[
    is.finite(beta[coefficient_names]) &
      is.finite(diag(covariance[coefficient_names, coefficient_names, drop = FALSE]))
  ]
  if (!length(coefficient_names)) return(NULL)

  covariance <- covariance[coefficient_names, coefficient_names, drop = FALSE]
  keep <- apply(is.finite(covariance), 1L, all) &
    apply(is.finite(covariance), 2L, all)
  coefficient_names <- coefficient_names[keep]
  if (!length(coefficient_names)) return(NULL)

  list(
    coefficient = as.numeric(beta[coefficient_names]),
    coefficient_names = coefficient_names,
    covariance = unname(covariance[coefficient_names, coefficient_names, drop = FALSE])
  )
}

hiv_prediction_contrast <- function(fit, new0, new1, coefficient_names) {
  terms_object <- stats::delete.response(stats::terms(fit))
  x0 <- stats::model.matrix(
    terms_object,
    data = new0,
    contrasts.arg = fit$contrasts
  )
  x1 <- stats::model.matrix(
    terms_object,
    data = new1,
    contrasts.arg = fit$contrasts
  )
  missing <- setdiff(coefficient_names, colnames(x0))
  if (length(missing)) {
    stop(
      "HIV covariance export could not reconstruct design column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  unname(x1[, coefficient_names, drop = FALSE] -
    x0[, coefficient_names, drop = FALSE])
}

fit_background_trends <- function(panel, base_year, cfg, return_artifact = FALSE) {
  require_package("data.table")
  all_causes <- HIV_PSEUDO_CODES
  model_causes <- hiv_background_model_codes(cfg)

  model_data <- panel[
    age5 %in% c(18L, 19L) & Sex %in% 1:2 & Death_Prov %in% 1:10 &
      nbdcode %in% model_causes & ASSABase > 0,
    .(
      NBD = sum(NBD, na.rm = TRUE),
      ASSABase = sum(ASSABase, na.rm = TRUE)
    ),
    by = .(Sex, nbdcode, ANCPrev, DeathYear, Death_Prov)
  ]
  if (isTRUE(cfg$settings$hiv_round_background_counts %||% TRUE)) {
    model_data[, NBD := round(NBD)]
  }
  model_data[, `:=`(
    Year = pmax(0, DeathYear - base_year),
    log1pt = log1p(pmax(0, DeathYear - base_year)),
    logN = log(ASSABase),
    NBD = pmax(NBD, 0),
    Sex_factor = factor(Sex, levels = 1:2),
    Province_factor = factor(Death_Prov, levels = 1:10)
  )]

  combinations <- data.table::CJ(
    nbdcode = all_causes,
    Death_Prov = 1:10,
    Sex = 1:2,
    sorted = FALSE
  )
  combinations[, `:=`(
    g1 = NA_real_,
    constant = NA_real_,
    model_status = data.table::fifelse(
      nbdcode %in% model_causes,
      "no_model",
      "excluded_legacy_compatibility"
    )
  )]

  results <- vector("list", length(all_causes))
  artifacts <- vector("list", length(all_causes))
  names(artifacts) <- as.character(all_causes)

  for (index in seq_along(all_causes)) {
    code <- all_causes[[index]]
    skeleton <- data.table::copy(combinations[nbdcode == code])
    artifact <- list(
      nbdcode = as.integer(code),
      cells = skeleton[, .(Death_Prov, Sex)],
      full = NULL,
      fallback = NULL
    )
    if (!code %in% model_causes) {
      results[[index]] <- skeleton
      artifact$model_status <- skeleton$model_status
      artifact$cells <- skeleton[, .(
        Death_Prov,
        Sex,
        g1_point = g1,
        constant_point = constant
      )]
      artifacts[[index]] <- artifact
      next
    }

    d <- model_data[nbdcode == code]
    if (nrow(d) >= 10L && sum(d$NBD, na.rm = TRUE) > 0) {
      fit <- tryCatch(
        suppressWarnings(stats::glm(
          NBD ~ log1pt * Sex_factor * Province_factor,
          family = stats::poisson(link = "log"),
          offset = logN,
          data = d
        )),
        error = function(error) NULL
      )
      if (!is.null(fit) && isTRUE(fit$converged)) {
        new0 <- skeleton[, .(
          Sex_factor = factor(Sex, levels = 1:2),
          Province_factor = factor(Death_Prov, levels = 1:10),
          log1pt = 0,
          logN = 0
        )]
        new1 <- data.table::copy(new0)
        new1[, log1pt := 1]
        eta0 <- tryCatch(
          stats::predict(fit, newdata = new0, type = "link"),
          error = function(error) rep(NA_real_, nrow(new0))
        )
        eta1 <- tryCatch(
          stats::predict(fit, newdata = new1, type = "link"),
          error = function(error) rep(NA_real_, nrow(new1))
        )
        valid_prediction <- is.finite(eta0) & is.finite(eta1)
        skeleton[valid_prediction, `:=`(
          constant = as.numeric(eta0[valid_prediction]),
          g1 = as.numeric(eta1[valid_prediction] - eta0[valid_prediction]),
          model_status = "full_interaction"
        )]

        covariance_block <- hiv_finite_covariance_block(fit)
        if (!is.null(covariance_block)) {
          contrast <- hiv_prediction_contrast(
            fit, new0, new1, covariance_block$coefficient_names
          )
          artifact$full <- c(
            covariance_block,
            list(
              contrast = contrast,
              valid_rows = which(valid_prediction)
            )
          )
        }
      }
    }

    if (any(skeleton$model_status == "no_model") &&
        nrow(d) >= 3L && sum(d$NBD, na.rm = TRUE) > 0) {
      smaller <- tryCatch(
        suppressWarnings(stats::glm(
          NBD ~ log1pt + Sex_factor + Province_factor,
          family = stats::poisson(link = "log"),
          offset = logN,
          data = d
        )),
        error = function(error) NULL
      )
      if (!is.null(smaller) && isTRUE(smaller$converged)) {
        slope <- unname(stats::coef(smaller)["log1pt"])
        if (is.finite(slope)) {
          fallback_rows <- which(skeleton$model_status == "no_model")
          skeleton[fallback_rows, `:=`(
            g1 = slope,
            model_status = "main_effect_fallback"
          )]
          covariance_block <- hiv_finite_covariance_block(smaller)
          if (!is.null(covariance_block) &&
              "log1pt" %in% covariance_block$coefficient_names) {
            artifact$fallback <- c(
              covariance_block,
              list(
                slope_index = match(
                  "log1pt", covariance_block$coefficient_names
                ),
                valid_rows = fallback_rows
              )
            )
          }
        }
      }
    }
    artifact$model_status <- skeleton$model_status
    artifact$cells <- skeleton[, .(
      Death_Prov,
      Sex,
      g1_point = g1,
      constant_point = constant
    )]
    results[[index]] <- skeleton
    artifacts[[index]] <- artifact
  }

  out <- data.table::rbindlist(results)
  assert_unique_key(out, c("nbdcode", "Death_Prov", "Sex"), "HIV background trends")
  if (!isTRUE(return_artifact)) return(out[])

  list(
    data = out[],
    artifact = list(
      version = "hiv-background-covariance-v1",
      base_year = as.numeric(base_year),
      models = artifacts
    )
  )
}

estimate_zero_prevalence_intercepts <- function(panel, return_artifact = FALSE) {
  require_package("data.table")
  total <- panel[nbdcode %in% HIV_PSEUDO_CODES, {
    population <- if (any(is.finite(ASSABase))) max(ASSABase, na.rm = TRUE) else 0
    prevalence <- if (any(is.finite(ANCPrev))) max(ANCPrev, na.rm = TRUE) else NA_real_
    .(
      D = sum(NBD, na.rm = TRUE),
      N = population,
      Pt = prevalence
    )
  }, by = .(Death_Prov, Sex, DeathYear, age5)]
  total[, DR := data.table::fifelse(N > 0, 1e5 * D / N, NA_real_)]
  early <- total[DeathYear %in% 1997:2003 & age5 %in% 2:17 & Death_Prov %in% 1:10]

  standard_keys <- data.table::CJ(
    Death_Prov = 1:10,
    Sex = 1:3,
    age5 = 4:17,
    sorted = FALSE
  )
  standard_rows <- vector("list", nrow(standard_keys))
  standard_models <- vector("list", nrow(standard_keys))
  for (index in seq_len(nrow(standard_keys))) {
    key <- standard_keys[index]
    d <- early[
      Death_Prov == key$Death_Prov & Sex == key$Sex & age5 == key$age5
    ]
    valid <- is.finite(d$DR) & is.finite(d$Pt)
    fit <- NULL
    intercept <- NA_real_
    if (sum(valid) >= 2L && length(unique(d$Pt[valid])) >= 2L) {
      fit <- tryCatch(
        stats::lm(DR ~ Pt, data = d[valid]),
        error = function(error) NULL
      )
      if (!is.null(fit)) intercept <- unname(stats::coef(fit)[[1L]])
    }
    status <- "linear_model"
    if (!is.finite(intercept)) {
      intercept <- if (any(valid)) stats::median(d$DR[valid]) else 0
      status <- "fixed_fallback"
      fit <- NULL
    }
    intercept <- pmax(0, intercept)
    standard_rows[[index]] <- data.table::data.table(
      Death_Prov = key$Death_Prov,
      Sex = key$Sex,
      age5 = key$age5,
      all_cause_intercept = intercept
    )
    covariance_block <- if (!is.null(fit)) hiv_finite_covariance_block(fit) else NULL
    standard_models[[index]] <- list(
      Death_Prov = as.integer(key$Death_Prov),
      Sex = as.integer(key$Sex),
      age5 = as.integer(key$age5),
      transform = "identity",
      fixed_value = as.numeric(intercept),
      status = status,
      covariance = covariance_block,
      design = if (!is.null(covariance_block)) {
        as.numeric(c("(Intercept)" = 1, "Pt" = 0)[
          covariance_block$coefficient_names
        ])
      } else NULL
    )
  }
  standard <- data.table::rbindlist(standard_rows)

  young_keys <- data.table::CJ(
    Death_Prov = 1:10,
    age5 = 2:3,
    sorted = FALSE
  )
  young_rows <- vector("list", nrow(young_keys))
  young_models <- vector("list", nrow(young_keys))
  for (index in seq_len(nrow(young_keys))) {
    key <- young_keys[index]
    d <- early[
      Death_Prov == key$Death_Prov & Sex == 3L & age5 == key$age5
    ]
    valid <- is.finite(d$DR) & d$DR > 0 & is.finite(d$Pt)
    fit <- NULL
    intercept <- NA_real_
    if (sum(valid) >= 2L && length(unique(d$Pt[valid])) >= 2L) {
      fit <- tryCatch(
        stats::lm(log(DR) ~ Pt, data = d[valid]),
        error = function(error) NULL
      )
      if (!is.null(fit)) intercept <- exp(unname(stats::coef(fit)[[1L]]))
    }
    status <- "log_linear_model"
    if (!is.finite(intercept)) {
      intercept <- if (any(valid)) stats::median(d$DR[valid]) else 0
      status <- "fixed_fallback"
      fit <- NULL
    }
    intercept <- pmax(0, intercept)
    young_rows[[index]] <- data.table::data.table(
      Death_Prov = key$Death_Prov,
      age5 = key$age5,
      all_cause_intercept = intercept
    )
    covariance_block <- if (!is.null(fit)) hiv_finite_covariance_block(fit) else NULL
    young_models[[index]] <- list(
      Death_Prov = as.integer(key$Death_Prov),
      Sex = NA_integer_,
      age5 = as.integer(key$age5),
      transform = "exp",
      fixed_value = as.numeric(intercept),
      status = status,
      covariance = covariance_block,
      design = if (!is.null(covariance_block)) {
        as.numeric(c("(Intercept)" = 1, "Pt" = 0)[
          covariance_block$coefficient_names
        ])
      } else NULL
    )
  }
  young <- data.table::rbindlist(young_rows)
  young <- cross_join_dt(young, data.table::data.table(Sex = 1:3))

  out <- data.table::rbindlist(list(standard, young), use.names = TRUE, fill = TRUE)
  assert_unique_key(out, c("Death_Prov", "Sex", "age5"), "HIV zero-prevalence intercepts")
  if (!isTRUE(return_artifact)) return(out[])

  list(
    data = out[],
    artifact = list(
      version = "hiv-zero-prevalence-covariance-v1",
      standard = standard_models,
      young = young_models
    )
  )
}

allocate_intercept_to_causes <- function(panel, intercepts) {
  require_package("data.table")
  source <- panel[
    DeathYear == 1997L & Sex %in% 1:2 & Death_Prov %in% 1:10 &
      age5 > 1L & nbdcode %in% HIV_PSEUDO_CODES
  ]
  complete <- cross_join_dt(
    unique(source[, .(Death_Prov, Sex, age5)]),
    data.table::data.table(nbdcode = HIV_PSEUDO_CODES)
  )
  source <- merge(
    complete,
    source[, .(NBD = sum(NBD)), by = .(Death_Prov, Sex, age5, nbdcode)],
    by = c("Death_Prov", "Sex", "age5", "nbdcode"),
    all.x = TRUE,
    sort = FALSE
  )
  source[is.na(NBD), NBD := 0]
  source[, total := sum(NBD), by = .(Death_Prov, Sex, age5)]
  # The legacy model leaves the cause share missing when the 1997 pseudo-cause
  # total is zero. Downstream this means no HIV reclassification for that cell,
  # which is preferable to inventing an equal cause composition.
  source[, Prop := data.table::fifelse(total > 0, NBD / total, NA_real_)]
  source <- merge(source, intercepts, by = c("Death_Prov", "Sex", "age5"), all.x = TRUE, sort = FALSE)
  source[is.na(all_cause_intercept), all_cause_intercept := 0]
  source[, cause_intercept := Prop * all_cause_intercept]
  out <- source[, .(Death_Prov, Sex, age5, nbdcode, cause_intercept)]
  assert_unique_key(out, c("Death_Prov", "Sex", "age5", "nbdcode"), "HIV cause intercepts")
  out[]
}

estimate_misclassified_hiv <- function(panel, trends, cause_intercepts, base_year) {
  require_package("data.table")
  x <- panel[
    Sex %in% 1:2 & Death_Prov %in% 1:10 & nbdcode %in% HIV_PSEUDO_CODES
  ]
  x <- merge(
    x,
    trends[, .(nbdcode, Death_Prov, Sex, g1, model_status)],
    by = c("nbdcode", "Death_Prov", "Sex"),
    all.x = TRUE,
    sort = FALSE
  )
  x <- merge(x, cause_intercepts, by = c("nbdcode", "Death_Prov", "Sex", "age5"), all.x = TRUE, sort = FALSE)
  x[, age_weight := as.numeric(HIV_BACKGROUND_AGE_WEIGHT[as.character(age5)])]
  x[is.na(age_weight), age_weight := 0]
  x[, Year := pmax(0, DeathYear - base_year)]
  x[, background_rate := cause_intercept * (1 + Year)^(age_weight * g1)]
  x[, NonHIV := background_rate * ASSABase / 1e5]

  # A cause with no estimable/selected background model receives no HIV
  # reclassification. This reproduces the effective legacy behaviour for code
  # 234 when it is excluded by the `inrange(215, 231)` filter.
  no_background_model <- is.na(x$model_status) | x$model_status %in% c(
    "no_model", "excluded_legacy_compatibility"
  )
  x[!is.finite(NonHIV) | NonHIV < 0, NonHIV := 0]
  x[no_background_model | NonHIV > NBD | age5 >= 18L | !is.finite(background_rate), NonHIV := NBD]
  x[nbdcode == 219L & age5 < 6L, NonHIV := NBD]
  x[, Misclassified := pmax(0, NBD - NonHIV)]
  x[, .(Death_Prov, Sex, DeathYear, age5, nbdcode, NBD, NonHIV, Misclassified)]
}

allocate_hiv_to_population_groups <- function(
    pop_pseudo,
    misclassified,
    tolerance = 1e-8) {
  require_package("data.table")

  counts_source <- data.table::as.data.table(data.table::copy(pop_pseudo))
  modelled <- data.table::as.data.table(data.table::copy(misclassified))

  keys <- c("Death_Prov", "Sex", "DeathYear", "age5", "nbdcode")
  full_key <- c(keys, "Popgroup")

  assert_has_columns(
    counts_source,
    c(full_key, "NBD"),
    "population-group pseudo-cause counts"
  )
  assert_has_columns(
    modelled,
    c(keys, "Misclassified"),
    "modelled misclassified HIV deaths"
  )
  assert_unique_key(
    counts_source,
    full_key,
    "population-group pseudo-cause counts"
  )

  if (counts_source[!is.finite(NBD) | NBD < -tolerance, .N]) {
    stop(
      "Population-group pseudo-cause counts contain non-finite or negative values.",
      call. = FALSE
    )
  }
  counts_source[NBD < 0 & NBD >= -tolerance, NBD := 0]

  # Existing HIV (code 2) and unclassified cause 0 transfer directly to HIV.
  # Modelled pseudo-causes receive the estimated excess-HIV total.
  direct <- counts_source[nbdcode %in% c(0L, 2L), .(
    D = sum(NBD, na.rm = TRUE)
  ), by = keys]

  pseudo <- modelled[Death_Prov %in% 1:9, .(
    D = sum(Misclassified, na.rm = TRUE)
  ), by = keys]

  totals <- data.table::rbindlist(
    list(pseudo, direct),
    use.names = TRUE,
    fill = TRUE
  )
  totals <- totals[, .(D = sum(D, na.rm = TRUE)), by = keys]
  assert_unique_key(
    totals,
    keys,
    "HIV totals for population-group allocation"
  )

  # Build the allocation grid from every observed pseudo/direct cause key.
  # This retains neonatal and other observed cells outside the fitted HIV model
  # panel. Where no modelled total is available, D is zero, so the complete
  # observed count remains NonHIV.
  allocation_keys <- unique(data.table::rbindlist(
    list(
      counts_source[, ..keys],
      totals[, ..keys]
    ),
    use.names = TRUE
  ))

  grid <- cross_join_dt(
    allocation_keys,
    data.table::data.table(Popgroup = 1:4)
  )

  counts <- merge(
    grid,
    counts_source,
    by = full_key,
    all.x = TRUE,
    sort = FALSE
  )
  counts[is.na(NBD), NBD := 0]

  counts <- merge(
    counts,
    totals,
    by = keys,
    all.x = TRUE,
    sort = FALSE
  )
  counts[is.na(D), D := 0]

  counts[, NBDtot := sum(NBD), by = keys]
  counts[, share := data.table::fifelse(
    NBDtot > 0,
    NBD / NBDtot,
    1 / 4
  )]

  counts[, AIDSDeaths := pmin(NBD, pmax(0, D * share))]
  counts[, NonHIV := NBD - AIDSDeaths]

  counts[, accounting_error := abs(NBD - AIDSDeaths - NonHIV)]
  counts[, accounting_scale := pmax(1, abs(NBD))]
  bad_rows <- counts[
    !is.finite(AIDSDeaths) |
      !is.finite(NonHIV) |
      AIDSDeaths < -tolerance |
      NonHIV < -tolerance |
      accounting_error > tolerance * accounting_scale
  ]
  if (nrow(bad_rows)) {
    stop(
      "HIV population-group allocation failed row-level accounting in ",
      nrow(bad_rows), " row(s).",
      call. = FALSE
    )
  }

  before <- counts_source[, .(
    before = sum(NBD, na.rm = TRUE)
  ), by = keys]
  after <- counts[, .(
    after = sum(AIDSDeaths + NonHIV, na.rm = TRUE)
  ), by = keys]
  check <- merge(before, after, by = keys, all = TRUE, sort = FALSE)
  check[is.na(before), before := 0]
  check[is.na(after), after := 0]
  check[, difference := abs(before - after)]
  check[, scale := pmax(1, abs(before), abs(after))]
  bad_keys <- check[difference > tolerance * scale]
  if (nrow(bad_keys)) {
    first <- bad_keys[1]
    stop(
      "HIV population-group allocation did not preserve observed deaths in ",
      nrow(bad_keys), " key(s). First key: province=", first$Death_Prov,
      ", sex=", first$Sex,
      ", year=", first$DeathYear,
      ", age5=", first$age5,
      ", nbdcode=", first$nbdcode,
      ", before=", signif(first$before, 12),
      ", after=", signif(first$after, 12), ".",
      call. = FALSE
    )
  }

  out <- counts[, .(
    Death_Prov,
    Sex,
    DeathYear,
    age5,
    Popgroup,
    nbdcode,
    NBD,
    AIDSDeaths,
    NonHIV
  )]

  assert_unique_key(out, full_key, "population-group HIV allocation")
  data.table::setorder(out, DeathYear, Death_Prov, Sex, age5, Popgroup, nbdcode)
  out[]
}


construct_hiv_reallocated_long <- function(subpopulation, allocation) {
  require_package("data.table")
  original <- data.table::as.data.table(data.table::copy(subpopulation))
  before <- sum(original$Deaths, na.rm = TRUE)
  source_codes <- unique(c(unlist(HIV_PSEUDO_SOURCE_MAP, use.names = FALSE), 0L, 2L))

  unaffected <- original[!nbdcode %in% source_codes, .(
    Deaths = sum(Deaths, na.rm = TRUE)
  ), by = .(Death_Prov, Sex, DeathYear, age5, Popgroup, nbdcode)]

  residual <- allocation[nbdcode %in% HIV_PSEUDO_CODES]
  residual[, destination := as.integer(HIV_PSEUDO_DESTINATION[as.character(nbdcode)])]
  residual <- residual[, .(
    Deaths = sum(NonHIV, na.rm = TRUE)
  ), by = .(Death_Prov, Sex, DeathYear, age5, Popgroup, nbdcode = destination)]

  hiv <- allocation[, .(
    Deaths = sum(AIDSDeaths, na.rm = TRUE)
  ), by = .(Death_Prov, Sex, DeathYear, age5, Popgroup)]
  hiv[, nbdcode := 2L]

  out <- data.table::rbindlist(list(unaffected, residual, hiv), use.names = TRUE, fill = TRUE)
  out <- out[, .(Deaths = sum(Deaths, na.rm = TRUE)), by = .(
    Death_Prov, Sex, DeathYear, age5, Popgroup, nbdcode
  )]
  out[Deaths < 0 | !is.finite(Deaths), Deaths := 0]
  after <- sum(out$Deaths, na.rm = TRUE)
  assert_total_preserved(before, after, tolerance = 1e-6, label = "HIV reallocation total")
  out[]
}

hiv_draw_mvn <- function(coefficient, covariance) {
  coefficient <- as.numeric(coefficient)
  covariance <- as.matrix(covariance)
  if (!length(coefficient)) return(coefficient)
  if (!identical(dim(covariance), c(length(coefficient), length(coefficient)))) {
    stop("HIV covariance matrix dimension does not match its coefficient vector.", call. = FALSE)
  }
  covariance <- 0.5 * (covariance + t(covariance))
  decomposition <- eigen(covariance, symmetric = TRUE)
  tolerance <- max(1, max(abs(decomposition$values))) * 1e-10
  if (any(decomposition$values < -tolerance)) {
    warning(
      "A fitted HIV covariance matrix was not positive semidefinite; ",
      "negative numerical eigenvalues were truncated to zero.",
      call. = FALSE
    )
  }
  values <- pmax(0, decomposition$values)
  as.numeric(
    coefficient +
      decomposition$vectors %*%
        (sqrt(values) * stats::rnorm(length(coefficient)))
  )
}

draw_hiv_background_trends <- function(artifact, seed, stochastic = TRUE) {
  require_package("data.table")
  if (is.null(artifact$models)) {
    stop("The HIV background covariance artifact has no fitted models.", call. = FALSE)
  }
  if (isTRUE(stochastic)) set.seed(as.integer(seed))
  pieces <- lapply(artifact$models, function(model) {
    cells <- data.table::as.data.table(data.table::copy(model$cells))
    cells[, `:=`(
      nbdcode = as.integer(model$nbdcode),
      g1 = as.numeric(g1_point),
      constant = as.numeric(constant_point),
      model_status = as.character(model$model_status)
    )]
    if (isTRUE(stochastic) && !is.null(model$full) && length(model$full$valid_rows)) {
      beta <- hiv_draw_mvn(model$full$coefficient, model$full$covariance)
      slopes <- as.numeric(model$full$contrast %*% beta)
      rows <- as.integer(model$full$valid_rows)
      cells[rows, g1 := slopes[rows]]
    }
    if (isTRUE(stochastic) && !is.null(model$fallback) && length(model$fallback$valid_rows)) {
      beta <- hiv_draw_mvn(
        model$fallback$coefficient,
        model$fallback$covariance
      )
      slope <- beta[[as.integer(model$fallback$slope_index)]]
      cells[as.integer(model$fallback$valid_rows), g1 := slope]
    }
    cells[]
  })
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  out[, c("g1_point", "constant_point") := NULL]
  assert_unique_key(out, c("nbdcode", "Death_Prov", "Sex"), "drawn HIV background trends")
  out[]
}

draw_hiv_zero_prevalence_intercepts <- function(artifact, seed, stochastic = TRUE) {
  require_package("data.table")
  if (isTRUE(stochastic)) set.seed(as.integer(seed))

  draw_item <- function(item) {
    value <- as.numeric(item$fixed_value)
    block <- item$covariance
    if (isTRUE(stochastic) && !is.null(block) && !is.null(item$design)) {
      beta <- hiv_draw_mvn(block$coefficient, block$covariance)
      linear <- sum(as.numeric(item$design) * beta)
      value <- if (identical(item$transform, "exp")) exp(linear) else linear
    }
    pmax(0, as.numeric(value))
  }

  standard <- data.table::rbindlist(lapply(artifact$standard, function(item) {
    data.table::data.table(
      Death_Prov = as.integer(item$Death_Prov),
      Sex = as.integer(item$Sex),
      age5 = as.integer(item$age5),
      all_cause_intercept = draw_item(item)
    )
  }))
  young_base <- data.table::rbindlist(lapply(artifact$young, function(item) {
    data.table::data.table(
      Death_Prov = as.integer(item$Death_Prov),
      age5 = as.integer(item$age5),
      all_cause_intercept = draw_item(item)
    )
  }))
  young <- cross_join_dt(young_base, data.table::data.table(Sex = 1:3))
  out <- data.table::rbindlist(list(standard, young), use.names = TRUE, fill = TRUE)
  assert_unique_key(out, c("Death_Prov", "Sex", "age5"), "drawn HIV zero-prevalence intercepts")
  out[]
}

run_hiv_reallocation_from_covariance <- function(
    subpopulation,
    prevalence,
    cfg,
    covariance_artifact,
    seed,
    stochastic = TRUE) {
  require_package("data.table")
  if (is.null(covariance_artifact$background) ||
      is.null(covariance_artifact$zero_prevalence)) {
    stop("The HIV model covariance artifact is incomplete.", call. = FALSE)
  }

  pseudo <- build_pseudo_cause_counts(subpopulation, include_aggregates = TRUE)
  pseudo <- pseudo[nbdcode %in% HIV_PSEUDO_CODES]
  panel_grid <- cross_join_dt(
    prevalence,
    data.table::data.table(nbdcode = HIV_PSEUDO_CODES)
  )
  panel_key <- c("Death_Prov", "Sex", "DeathYear", "age5", "nbdcode")
  assert_unique_key(pseudo, panel_key, "HIV pseudo-cause counts")
  panel <- merge(
    panel_grid,
    pseudo,
    by = panel_key,
    all.x = TRUE,
    sort = FALSE
  )
  panel[is.na(NBD), NBD := 0]
  panel <- panel[is.finite(ASSABase) & ASSABase > 0]

  seed <- as.integer(seed)
  trends <- draw_hiv_background_trends(
    covariance_artifact$background,
    seed = seed,
    stochastic = stochastic
  )
  intercepts <- draw_hiv_zero_prevalence_intercepts(
    covariance_artifact$zero_prevalence,
    seed = if (seed >= .Machine$integer.max - 1009L) seed - 1009L else seed + 1009L,
    stochastic = stochastic
  )
  cause_intercepts <- allocate_intercept_to_causes(panel, intercepts)
  misclassified <- estimate_misclassified_hiv(
    panel,
    trends,
    cause_intercepts,
    covariance_artifact$base_year
  )
  population_counts <- build_pseudo_population_counts(subpopulation)
  allocation <- allocate_hiv_to_population_groups(population_counts, misclassified)
  reallocated <- construct_hiv_reallocated_long(subpopulation, allocation)

  list(
    data = reallocated,
    trends = trends,
    intercepts = intercepts,
    misclassified = misclassified,
    modelled_hiv_deaths = sum(allocation$AIDSDeaths, na.rm = TRUE)
  )
}

run_hiv_reallocation <- function(subpopulation, prevalence, cfg) {
  require_package("data.table")
  pseudo <- build_pseudo_cause_counts(subpopulation, include_aggregates = TRUE)
  pseudo <- pseudo[nbdcode %in% HIV_PSEUDO_CODES]
  panel_grid <- cross_join_dt(
    prevalence,
    data.table::data.table(nbdcode = HIV_PSEUDO_CODES)
  )
  panel_key <- c("Death_Prov", "Sex", "DeathYear", "age5", "nbdcode")
  assert_unique_key(pseudo, panel_key, "HIV pseudo-cause counts")
  panel <- merge(
    panel_grid,
    pseudo,
    by = panel_key,
    all.x = TRUE,
    sort = FALSE
  )
  assert_unique_key(panel, panel_key, "HIV model panel")
  panel[is.na(NBD), NBD := 0]
  missing_population <- panel[is.na(ASSABase) | ASSABase <= 0, .N]
  if (missing_population) {
    warn_or_stop(
      paste0("Prevalence/population input is missing or zero for ", missing_population, " HIV model rows."),
      strict = isTRUE(cfg$settings$strict_checks)
    )
  }
  panel <- panel[is.finite(ASSABase) & ASSABase > 0]

  base_year <- estimate_hiv_base_year(prevalence)
  background_fit <- fit_background_trends(
    panel, base_year, cfg, return_artifact = TRUE
  )
  intercept_fit <- estimate_zero_prevalence_intercepts(
    panel, return_artifact = TRUE
  )
  trends <- background_fit$data
  intercepts <- intercept_fit$data
  cause_intercepts <- allocate_intercept_to_causes(panel, intercepts)
  misclassified <- estimate_misclassified_hiv(panel, trends, cause_intercepts, base_year)
  population_counts <- build_pseudo_population_counts(subpopulation)
  allocation <- allocate_hiv_to_population_groups(population_counts, misclassified)
  reallocated <- construct_hiv_reallocated_long(subpopulation, allocation)

  covariance_artifact <- list(
    version = "nbd3-hiv-covariance-v1",
    base_year = as.numeric(base_year),
    background = background_fit$artifact,
    zero_prevalence = intercept_fit$artifact,
    prevalence_uncertainty = "not supplied; prevalence held fixed",
    population_group_allocation = "deterministic conditional on each draw"
  )

  list(
    data = reallocated,
    wide = long_to_wide_causes(reallocated, value = "Deaths", codes = 1:214),
    trends = trends,
    intercepts = intercepts,
    misclassified = misclassified,
    base_year = base_year,
    covariance_artifact = covariance_artifact
  )
}
