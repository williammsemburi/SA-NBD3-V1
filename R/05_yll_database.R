# ==============================================================================
# 05_yll_database: YLL calculation and final database construction
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# YLL schedules, aggregate ages, rates and final database
# ------------------------------------------------------------------------------

# YLL calculation and final database ------------------------------------------

read_yll_schedule <- function(cfg) {
  require_package("data.table")
  x <- read_tabular(raw_file(cfg, "yll", must_exist = TRUE), sheet = "Sheet1")
  x <- rename_first_match(x, "age", c("age5", "Age"), TRUE)

  if (all(c("Sex", "L") %in% names(x))) {
    long <- x[, .(age, Sex, L)]
  } else {
    measure <- grep("^L[0-9]+$", names(x), value = TRUE, ignore.case = TRUE)
    if (!length(measure)) stop("YLL workbook requires L1/L2 columns or long fields Sex and L.", call. = FALSE)
    long <- data.table::melt(
      x,
      id.vars = "age",
      measure.vars = measure,
      variable.name = "sex_field",
      value.name = "L",
      variable.factor = FALSE
    )
    long[, Sex := as.integer(gsub("[^0-9]", "", sex_field))]
    long[, sex_field := NULL]
  }
  long[, `:=`(
    age = as.integer(age),
    Sex = as.integer(Sex),
    L = suppressWarnings(as.numeric(L))
  )]
  long[, `:=`(
    L00 = L,
    L03 = data.table::fifelse(L >= 0, (1 - exp(-0.03 * L)) / 0.03, NA_real_),
    L015 = data.table::fifelse(L >= 0, (1 - exp(-0.015 * L)) / 0.015, NA_real_)
  )]
  long[, L := NULL]
  assert_unique_key(long, c("Sex", "age"), "YLL schedule")
  long[]
}

calculate_yll_outputs <- function(
    redistributed_long,
    za_lookup,
    yll_schedule,
    strict = TRUE) {
  require_package("data.table")
  x <- apply_analysis_to_za(
    redistributed_long,
    za_lookup,
    value = "Count",
    strict = strict
  )
  data.table::setnames(x, "age5", "age")
  x <- merge(x, yll_schedule, by = c("Sex", "age"), all.x = TRUE, sort = FALSE)
  missing <- x[is.na(L00) | is.na(L03) | is.na(L015), .N]
  if (missing) stop("YLL schedule is missing for ", missing, " redistributed death rows.", call. = FALSE)
  x[, `:=`(
    YLL00 = L00 * Count,
    YLL03 = L03 * Count,
    YLL015 = L015 * Count
  )]
  x[, c("L00", "L03", "L015") := NULL]

  provincial <- x[Death_Prov %in% 1:9]
  national <- provincial[, .(
    Count = sum(Count, na.rm = TRUE),
    YLL00 = sum(YLL00, na.rm = TRUE),
    YLL03 = sum(YLL03, na.rm = TRUE),
    YLL015 = sum(YLL015, na.rm = TRUE)
  ), by = .(DeathYear, age, Sex, Popgroup, nbdcode)]
  national[, Death_Prov := 10L]
  all <- data.table::rbindlist(list(provincial, national), use.names = TRUE, fill = TRUE)
  data.table::setorder(all, DeathYear, Death_Prov, Popgroup, Sex, age, nbdcode)

  national_summary <- all[Death_Prov == 10L, .(
    Count = sum(Count),
    YLL00 = sum(YLL00),
    YLL03 = sum(YLL03),
    YLL015 = sum(YLL015)
  ), by = .(DeathYear, Sex, age, nbdcode)]
  list(all = all, national = national_summary)
}

prepare_database_population <- function(population) {
  require_package("data.table")

  x <- data.table::as.data.table(data.table::copy(population))
  assert_has_columns(
    x,
    c("Popgroup", "Sex", "DeathYear", "age5", "Death_Prov", "Pop"),
    "prepared population"
  )

  # Source denominators cover age5 2-20. After conversion to the final
  # database scale these become ages 1-19. The published database structure
  # also includes age 0, for which no separate neonatal denominator is
  # available; it is represented explicitly as a structural zero.
  x <- x[age5 %in% 2:20]
  x[, age := as.integer(age5 - 1L)]
  x <- x[, .(
    Pop = sum(Pop, na.rm = TRUE)
  ), by = .(Popgroup, Sex, DeathYear, Death_Prov, age)]

  neonatal <- unique(x[, .(Popgroup, Sex, DeathYear, Death_Prov)])
  neonatal[, `:=`(age = 0L, Pop = 0)]
  x <- data.table::rbindlist(list(x, neonatal), use.names = TRUE)

  assert_unique_key(
    x,
    c("Popgroup", "Sex", "DeathYear", "Death_Prov", "age"),
    "database population base cells"
  )

  province <- x[, .(Pop = sum(Pop, na.rm = TRUE)), by = .(
    Death_Prov, Sex, DeathYear, age
  )]
  popgroup <- x[, .(Pop = sum(Pop, na.rm = TRUE)), by = .(
    Popgroup, Sex, DeathYear, age
  )]

  popgroup[, Grouping := as.integer(Popgroup + 10L)]
  popgroup[, Popgroup := NULL]
  data.table::setnames(province, "Death_Prov", "Grouping")

  out <- data.table::rbindlist(
    list(province, popgroup),
    use.names = TRUE,
    fill = TRUE
  )
  assert_unique_key(
    out,
    c("Grouping", "Sex", "DeathYear", "age"),
    "database population denominators"
  )
  out[]
}


append_aggregate_ages <- function(data) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  measure <- c("Deaths", "YLL00", "YLL03", "YLL015", "Pop")
  id <- setdiff(names(x), c("age", measure))

  make_age <- function(code, death_ages, population_ages = death_ages) {
    deaths <- x[age %in% death_ages, lapply(.SD, sum, na.rm = TRUE), by = id, .SDcols = c("Deaths", "YLL00", "YLL03", "YLL015")]
    population <- x[age %in% population_ages, .(Pop = sum(Pop, na.rm = TRUE)), by = id]
    out <- merge(deaths, population, by = id, all = TRUE, sort = FALSE)
    for (column in measure) out[is.na(get(column)), (column) := 0]
    out[, age := as.integer(code)]
    out
  }

  aggregates <- list(
    make_age(20L, 0:2, 1:2),
    make_age(21L, 3:4),
    make_age(22L, 5:10),
    make_age(23L, 11:13),
    make_age(24L, 14:19),
    make_age(25L, 0:19, 1:19),
    make_age(26L, 5:19)
  )
  data.table::rbindlist(c(list(x), aggregates), use.names = TRUE, fill = TRUE)
}

read_asr_factors <- function(cfg) {
  require_package("data.table")
  x <- read_tabular(raw_file(cfg, "asr_factors", must_exist = TRUE))
  x <- rename_first_match(x, "age", c("age2", "age5"), TRUE)
  x <- rename_first_match(x, "F", c("factor", "weight"), TRUE)
  x[, `:=`(age = as.integer(age), F = suppressWarnings(as.numeric(F)) / 100)]
  out <- x[!is.na(age) & is.finite(F), .(age, F)]
  assert_unique_key(out, "age", "ASR factors")
  out[]
}

calculate_asr <- function(data, factors) {
  require_package("data.table")
  x <- data.table::as.data.table(data)
  base <- x[age %in% 0:19]
  id <- c("Grouping", "Sex", "nbdcode", "DeathYear")

  under5 <- base[age %in% 0:2, .(
    deaths = sum(Deaths, na.rm = TRUE),
    population = sum(Pop[age %in% 1:2], na.rm = TRUE)
  ), by = id]
  under5[, standard_age := 1L]

  older <- base[age %in% 3:19, .(
    deaths = sum(Deaths, na.rm = TRUE),
    population = sum(Pop, na.rm = TRUE)
  ), by = c(id, "age")]
  older[, standard_age := age - 1L]
  older[, age := NULL]

  rates <- data.table::rbindlist(list(under5, older), use.names = TRUE, fill = TRUE)
  rates[, rate := data.table::fifelse(population > 0, deaths / population, NA_real_)]
  weights <- factors[age %in% 1:18, .(standard_age = age, F)]
  rates <- merge(rates, weights, by = "standard_age", all.x = TRUE, sort = FALSE)
  rates <- rates[is.finite(rate) & is.finite(F)]
  rates[, .(
    ASR = {
      denominator <- sum(F, na.rm = TRUE)
      if (denominator > 0) 1e5 * sum(rate * F, na.rm = TRUE) / denominator else NA_real_
    }
  ), by = id]
}

build_final_database <- function(deaths_yll, population, asr_factors) {
  require_package("data.table")
  y <- data.table::as.data.table(data.table::copy(deaths_yll))[Death_Prov %in% 1:9]
  data.table::setnames(y, "Count", "Deaths")

  # Cause-of-death and YLL files retain the legacy 1-20 age codes. The
  # dashboard/database uses 0-19, so shift once at this boundary before deaths
  # are merged with population denominators prepared on the same 0-19 scale.
  invalid_age <- y[!age %in% 1:20, .N]
  if (invalid_age) {
    stop("Deaths/YLL data contain ", invalid_age, " row(s) outside age codes 1-20.", call. = FALSE)
  }
  y[, age := as.integer(age - 1L)]

  province <- y[, lapply(.SD, sum, na.rm = TRUE), by = .(
    DeathYear, age, Sex, Death_Prov, nbdcode
  ), .SDcols = c("Deaths", "YLL00", "YLL03", "YLL015")]
  data.table::setnames(province, "Death_Prov", "Grouping")

  popgroup <- y[, lapply(.SD, sum, na.rm = TRUE), by = .(
    DeathYear, age, Sex, Popgroup, nbdcode
  ), .SDcols = c("Deaths", "YLL00", "YLL03", "YLL015")]
  popgroup[, Grouping := as.integer(Popgroup + 10L)]
  popgroup[, Popgroup := NULL]
  deaths <- data.table::rbindlist(list(province, popgroup), use.names = TRUE, fill = TRUE)

  # The final Stata database explicitly creates structural-zero ZA 140 rows.
  # Analysis to ZA codes.do defines neither ZA 140 nor ZA 141, so these rows
  # preserve the published dashboard schema without inventing a mapping.
  zero140 <- unique(deaths[, .(DeathYear, age, Sex, Grouping)])
  zero140[, `:=`(nbdcode = 140L, Deaths = 0, YLL00 = 0, YLL03 = 0, YLL015 = 0)]
  deaths <- data.table::rbindlist(list(deaths, zero140), use.names = TRUE, fill = TRUE)
  deaths <- deaths[, lapply(.SD, sum, na.rm = TRUE), by = .(
    DeathYear, age, Sex, Grouping, nbdcode
  ), .SDcols = c("Deaths", "YLL00", "YLL03", "YLL015")]

  pop <- prepare_database_population(population)
  x <- merge(deaths, pop, by = c("DeathYear", "age", "Sex", "Grouping"), all.x = TRUE, sort = FALSE)
  missing_population <- x[is.na(Pop) & Deaths > 0, .N]
  if (missing_population) {
    stop(
      "Population denominators are missing for ", missing_population,
      " non-zero death rows in the final database.",
      call. = FALSE
    )
  }
  x[is.na(Pop), Pop := 0]
  x <- append_aggregate_ages(x)

  # National grouping from provinces, preserving population-group groupings as
  # separate outputs 11-14.
  national <- x[Grouping %in% 1:9, lapply(.SD, sum, na.rm = TRUE), by = .(
    DeathYear, age, Sex, nbdcode
  ), .SDcols = c("Deaths", "YLL00", "YLL03", "YLL015", "Pop")]
  national[, Grouping := 10L]
  x <- data.table::rbindlist(list(x, national), use.names = TRUE, fill = TRUE)

  # Add person estimates. For sex-specific causes, use the relevant sex
  # denominator rather than the sum of male and female populations.
  person <- x[Sex %in% 1:2, lapply(.SD, sum, na.rm = TRUE), by = .(
    DeathYear, age, Grouping, nbdcode
  ), .SDcols = c("Deaths", "YLL00", "YLL03", "YLL015", "Pop")]
  person_key <- c("DeathYear", "age", "Grouping", "nbdcode")
  female_pop <- x[Sex == 2L, .(DeathYear, age, Grouping, nbdcode, female_pop = Pop)]
  male_pop <- x[Sex == 1L, .(DeathYear, age, Grouping, nbdcode, male_pop = Pop)]
  assert_unique_key(female_pop, person_key, "female population denominators")
  assert_unique_key(male_pop, person_key, "male population denominators")
  person <- merge(person, female_pop, by = person_key, all.x = TRUE, sort = FALSE)
  person <- merge(person, male_pop, by = person_key, all.x = TRUE, sort = FALSE)
  person[nbdcode %in% c(42L, 43L), Pop := female_pop]
  person[nbdcode == 46L, Pop := male_pop]
  person[, c("female_pop", "male_pop") := NULL]
  person[, Sex := 3L]
  x <- data.table::rbindlist(list(x, person), use.names = TRUE, fill = TRUE)

  # Retain the final Stata compatibility rule that consolidates any ZA 141
  # rows into ZA 140. The supplied Analysis-to-ZA source does not currently
  # create ZA 141, but applying the rule here makes a future authoritative mapping
  # safe. Collapse deaths and YLLs before calculating ASRs so denominators are
  # not duplicated.
  x[nbdcode == 141L, nbdcode := 140L]
  x <- x[, .(
    Deaths = sum(Deaths, na.rm = TRUE),
    YLL00 = sum(YLL00, na.rm = TRUE),
    YLL03 = sum(YLL03, na.rm = TRUE),
    YLL015 = sum(YLL015, na.rm = TRUE),
    Pop = if (any(is.finite(Pop))) max(Pop[is.finite(Pop)]) else 0
  ), by = .(Grouping, Sex, nbdcode, DeathYear, age)]

  asr <- calculate_asr(x, asr_factors)
  x <- merge(x, asr, by = c("Grouping", "Sex", "nbdcode", "DeathYear"), all.x = TRUE, sort = FALSE)
  x[, Age := as.integer(age)]
  x[, age := NULL]
  x[!is.finite(ASR), ASR := NA_real_]
  data.table::setorder(x, Grouping, Sex, DeathYear, nbdcode, Age)
  x[]
}
