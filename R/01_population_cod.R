# ==============================================================================
# 01_population_cod: Population and registered cause-of-death data
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Population denominators
# ------------------------------------------------------------------------------

# Population denominators ------------------------------------------------------
#
# The historical workbook stores each population group in a separate block on
# each annual sheet. This reader replaces 136 temporary Stata files with one
# in-memory pass and returns the canonical long table used by later stages.

read_population_block <- function(path, year, range, popgroup, sex) {
  require_package("readxl")
  x <- readxl::read_excel(
    path,
    sheet = as.character(year),
    range = range,
    col_names = TRUE,
    .name_repair = "unique_quiet"
  )
  require_package("data.table")
  x <- data.table::as.data.table(x)
  if (ncol(x) < 10L) {
    stop("Population block ", range, " in sheet ", year, " has fewer than 10 columns.", call. = FALSE)
  }
  x <- x[, 1:10]
  data.table::setnames(x, c("age", paste0("Pop", 1:9)))
  x[, `:=`(
    age = suppressWarnings(as.numeric(age)),
    Popgroup = as.integer(popgroup),
    Sex = as.integer(sex),
    DeathYear = as.integer(year)
  )]
  for (column in paste0("Pop", 1:9)) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
  }
  x[!is.na(age)]
}

build_population_from_workbook <- function(cfg) {
  path <- raw_file(cfg, "population_workbook", must_exist = TRUE)
  years <- seq.int(
    as.integer(cfg$settings$start_year),
    as.integer(cfg$settings$population_workbook_end_year)
  )

  # block, population group in the NBD coding order
  blocks <- data.table::data.table(
    range_male = c("A3:J22", "M3:V22", "Y3:AH22", "AK3:AT22"),
    range_female = c("A25:J44", "M25:V44", "Y25:AH44", "AK25:AT44"),
    Popgroup = c(1L, 4L, 3L, 2L)
  )

  pieces <- vector("list", length(years) * nrow(blocks) * 2L)
  position <- 1L
  for (year in years) {
    for (row in seq_len(nrow(blocks))) {
      pieces[[position]] <- read_population_block(
        path, year, blocks$range_male[[row]], blocks$Popgroup[[row]], 1L
      )
      position <- position + 1L
      pieces[[position]] <- read_population_block(
        path, year, blocks$range_female[[row]], blocks$Popgroup[[row]], 2L
      )
      position <- position + 1L
    }
  }

  population <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  population[, age5 := data.table::fcase(
    age == 0, 2L,
    age == 1, 3L,
    age >= 5, as.integer(age / 5 + 3),
    default = NA_integer_
  )]

  population <- data.table::melt(
    population,
    id.vars = c("Popgroup", "Sex", "DeathYear", "age5"),
    measure.vars = paste0("Pop", 1:9),
    variable.name = "Death_Prov",
    value.name = "Pop",
    variable.factor = FALSE
  )
  population[, Death_Prov := as.integer(sub("^Pop", "", Death_Prov))]
  population[is.na(Pop) | Pop < 0, Pop := 0]
  population <- population[
    !is.na(age5),
    .(Pop = sum(Pop, na.rm = TRUE)),
    by = .(Popgroup, Sex, DeathYear, age5, Death_Prov)
  ]
  data.table::setorder(population, DeathYear, Death_Prov, Popgroup, Sex, age5)
  population[]
}

standardise_population <- function(data) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  x <- rename_first_match(x, "Popgroup", c("PopGroup", "population_group"), TRUE)
  x <- rename_first_match(x, "Sex", c("sex"), TRUE)
  x <- rename_first_match(x, "DeathYear", c("year", "deathyear"), TRUE)
  x <- rename_first_match(x, "age5", c("age", "age_group"), TRUE)
  x <- rename_first_match(x, "Death_Prov", c("province", "DeathProv"), TRUE)
  x <- rename_first_match(x, "Pop", c("population", "N"), TRUE)

  for (column in c("Popgroup", "Sex", "DeathYear", "age5", "Death_Prov", "Pop")) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
  }
  x <- x[
    !is.na(Popgroup) & !is.na(Sex) & !is.na(DeathYear) & !is.na(age5) &
      !is.na(Death_Prov),
    .(Pop = sum(Pop, na.rm = TRUE)),
    by = .(Popgroup, Sex, DeathYear, age5, Death_Prov)
  ]
  x[Pop < 0, Pop := 0]
  data.table::setorder(x, DeathYear, Death_Prov, Popgroup, Sex, age5)
  x[]
}

prepare_population <- function(cfg) {
  current_path <- raw_file(cfg, "population_current")
  if (file.exists(current_path)) {
    population <- standardise_population(read_tabular(current_path))
    source <- current_path
  } else {
    population <- build_population_from_workbook(cfg)
    source <- raw_file(cfg, "population_workbook")
  }

  start_year <- as.integer(cfg$settings$start_year)
  end_year <- as.integer(cfg$settings$end_year)
  expected_years <- seq.int(start_year, end_year)

  observed_years <- sort(unique(as.integer(population$DeathYear)))
  missing_years <- setdiff(expected_years, observed_years)
  if (length(missing_years)) {
    warn_or_stop(
      paste0(
        "Population input '", basename(source),
        "' does not cover pipeline year(s): ",
        paste(missing_years, collapse = ", "),
        ". Add ", cfg$files$population_current,
        " to data/raw or lower settings.end_year for a historical test run."
      ),
      strict = isTRUE(cfg$settings$strict_checks)
    )
  }

  # Current denominator files may extend beyond the mortality analysis period.
  # The canonical Stage 01 output is restricted to the configured years.
  population <- population[
    DeathYear >= start_year & DeathYear <= end_year
  ]

  unexpected_ages <- sort(unique(population[!age5 %in% 2:20, age5]))
  if (length(unexpected_ages)) {
    warn_or_stop(
      paste0(
        "Population input contains age5 value(s) outside the denominator ",
        "codes 2-20: ", paste(unexpected_ages, collapse = ", "), "."
      ),
      strict = isTRUE(cfg$settings$strict_checks)
    )
    population <- population[age5 %in% 2:20]
  }

  assert_unique_key(
    population,
    c("Popgroup", "Sex", "DeathYear", "age5", "Death_Prov"),
    "population"
  )
  assert_nonnegative(population, "Pop")
  data.table::setorder(population, DeathYear, Death_Prov, Popgroup, Sex, age5)
  population[]
}

# ------------------------------------------------------------------------------
# ICD and cause-list mappings
# ------------------------------------------------------------------------------

# Cause classification and ZA hierarchy ---------------------------------------
#
# The source Stata pipeline contains two different cause transformations:
#
# 1. NBD215grProgram.do assigns three-character ICD-10 categories to analysis
#    causes. Most assignments are static; a small number depend on age.
# 2. Analysis to ZA codes.do creates 139 detailed ZA causes and several
#    overlapping aggregate cause hierarchies.
#
# tools/build_lookups_from_stata.py converts the static source statements to
# version-controlled CSV files and records all 241 ordered assignments in a
# rule manifest. The functions below implement the genuinely dynamic rules and
# apply the resulting mappings without repeated wide/long reshaping.

normalise_icd_category <- function(x) {
  raw <- as.character(x)
  blank <- is.na(raw) | !nzchar(trimws(raw))
  code <- gsub("[^A-Z0-9]", "", toupper(raw))
  code <- substr(code, 1L, 3L)
  code[blank | !nzchar(code)] <- NA_character_
  code
}

read_icd_to_nbd_lookup <- function(cfg) {
  require_package("data.table")
  path <- lookup_file(cfg, "icd_to_nbd_lookup", must_exist = TRUE)
  lookup <- read_tabular(path)
  lookup <- rename_first_match(
    lookup,
    "icd10",
    c("icd", "cause", "underlying_cause"),
    required = TRUE
  )
  lookup <- rename_first_match(
    lookup,
    "nbdcode",
    c("nbd", "nbd2016", "analysis_code"),
    required = TRUE
  )
  assert_has_columns(
    lookup,
    c("nbd_label", "rule_type", "source_file", "source_line"),
    "generated ICD-to-NBD lookup"
  )

  lookup[, `:=`(
    icd10 = normalise_icd_category(icd10),
    nbdcode = suppressWarnings(as.integer(nbdcode)),
    nbd_label = trimws(as.character(nbd_label)),
    rule_type = trimws(as.character(rule_type)),
    source_file = trimws(as.character(source_file)),
    source_line = suppressWarnings(as.integer(source_line))
  )]

  malformed <- lookup[
    is.na(icd10) |
      !grepl("^([A-Z][0-9]{2}|000|888|999)$", icd10) |
      is.na(nbdcode) | !data.table::between(nbdcode, 0L, 215L) |
      is.na(nbd_label) | !nzchar(nbd_label) |
      is.na(rule_type) | rule_type != "static" |
      is.na(source_file) | source_file != "NBD215grProgram.do" |
      is.na(source_line) | source_line < 1L
  ]
  if (nrow(malformed)) {
    stop(
      "The generated ICD-to-NBD lookup contains ", nrow(malformed),
      " malformed or incomplete row(s). Regenerate it with ",
      "tools/build_lookups_from_stata.py.",
      call. = FALSE
    )
  }
  assert_unique_key(lookup, "icd10", "ICD-to-NBD lookup")

  # These categories cannot be represented by a single static mapping because
  # the Stata assignments depend on age. Their rules are applied below.
  dynamic_codes <- c("A34", "A35", "P23", "G80")
  unexpected <- intersect(dynamic_codes, lookup$icd10)
  if (length(unexpected)) {
    stop(
      "Age-dependent ICD categories must not appear in the static lookup: ",
      paste(unexpected, collapse = ", "),
      ". Regenerate the lookup with tools/build_lookups_from_stata.py.",
      call. = FALSE
    )
  }

  sentinel <- lookup[icd10 %in% c("000", "888", "999")]
  if (nrow(sentinel) != 3L || any(sentinel$nbdcode != 143L)) {
    stop("The static lookup must map 000, 888 and 999 to NBD 143.", call. = FALSE)
  }
  data.table::setorder(lookup, icd10)
  lookup[]
}

map_icd_to_nbd <- function(
    data,
    cause_column,
    lookup,
    output = "nbdcode",
    age_column = "age_",
    age_u5_column = "age_u5",
    copy = TRUE) {
  require_package("data.table")
  if (!is.logical(copy) || length(copy) != 1L || is.na(copy)) {
    stop("`copy` must be TRUE or FALSE.", call. = FALSE)
  }
  if (inherits(data, "data.table")) {
    x <- if (copy) data.table::copy(data) else data
  } else {
    if (!copy) stop("`copy = FALSE` requires data.table input.", call. = FALSE)
    x <- data.table::as.data.table(data.table::copy(data))
  }
  assert_has_columns(x, cause_column)
  assert_has_columns(lookup, c("icd10", "nbdcode"))

  key <- normalise_icd_category(x[[cause_column]])
  dynamic_rows <- key %in% c("A33", "A34", "A35", "A50", "A54", "P23", "G80")
  if (any(dynamic_rows)) {
    missing_age_fields <- setdiff(c(age_column, age_u5_column), names(x))
    if (length(missing_age_fields)) {
      stop(
        "Age-dependent ICD classification requires column(s): ",
        paste(missing_age_fields, collapse = ", "),
        call. = FALSE
      )
    }
  }

  index <- match(key, lookup$icd10)
  mapped <- as.integer(lookup$nbdcode[index])

  # The Stata source treats blank/unknown strings as ill-defined natural causes.
  mapped[is.na(key)] <- 143L

  age <- if (age_column %in% names(x)) {
    suppressWarnings(as.numeric(x[[age_column]]))
  } else {
    rep.int(NA_real_, nrow(x))
  }
  age_u5 <- if (age_u5_column %in% names(x)) {
    suppressWarnings(as.numeric(x[[age_u5_column]]))
  } else {
    rep.int(NA_real_, nrow(x))
  }

  # Stata numeric missing values compare above every finite number. Replacing
  # missing ages by +Inf reproduces that behaviour for the source inequalities
  # while still making equality tests against finite age codes false.
  age_stata <- age
  age_u5_stata <- age_u5
  age_stata[is.na(age_stata)] <- Inf
  age_u5_stata[is.na(age_u5_stata)] <- Inf

  set_rule <- function(value, condition) {
    hit <- which(condition %in% TRUE)
    if (length(hit)) mapped[hit] <<- as.integer(value)
    invisible(NULL)
  }

  # Preserve the statement order in NBD215grProgram.do. Later assignments
  # intentionally override earlier assignments.
  set_rule(5L, key %in% c("A33", "A34", "A35") & age_u5_stata == 4) # line 12
  set_rule(5L, key %in% c("A33", "A34", "A35") & age_stata >= 1)     # line 13

  set_rule(11L, key == "P23" & age_u5_stata >= 3 & age_u5_stata <= 4) # line 33
  set_rule(11L, key == "P23" & age_stata >= 1)                         # line 34
  set_rule(11L, key == "P23" & age_u5_stata == 5)                      # line 35
  set_rule(11L, key == "P23" & age_u5_stata == 3)                      # line 36
  set_rule(20L, key == "P23" & age_u5_stata == 2)                      # line 52

  set_rule(
    22L,
    key %in% c("A33", "A34", "A35", "A50", "A54") &
      age_u5_stata >= 2 & age_u5_stata <= 3
  )                                                               # line 55

  set_rule(149L, key == "G80" & age_stata < 20)                  # line 199
  set_rule(194L, key == "G80" & age_stata >= 20)                 # line 242

  x[, (output) := mapped]
  x[]
}

read_analysis_to_za_lookup <- function(cfg) {
  require_package("data.table")
  path <- lookup_file(cfg, "analysis_to_za_lookup", must_exist = TRUE)
  lookup <- read_tabular(path)
  assert_has_columns(
    lookup,
    c(
      "analysis_code", "za_code", "weight", "hierarchy",
      "analysis_label", "za_label", "source_file", "source_line"
    ),
    "generated analysis-to-ZA lookup"
  )

  lookup[, `:=`(
    analysis_code = suppressWarnings(as.integer(analysis_code)),
    za_code = suppressWarnings(as.integer(za_code)),
    weight = suppressWarnings(as.numeric(weight)),
    hierarchy = trimws(as.character(hierarchy)),
    analysis_label = trimws(as.character(analysis_label)),
    za_label = trimws(as.character(za_label)),
    source_file = trimws(as.character(source_file)),
    source_line = suppressWarnings(as.integer(source_line))
  )]

  malformed <- lookup[
    is.na(analysis_code) | !data.table::between(analysis_code, 0L, 215L) |
      is.na(za_code) | za_code < 1L |
      !is.finite(weight) | weight <= 0 |
      is.na(hierarchy) | !nzchar(hierarchy) |
      is.na(analysis_label) | !nzchar(analysis_label) |
      is.na(za_label) | !nzchar(za_label) |
      is.na(source_file) | source_file != "Analysis to ZA codes.do" |
      is.na(source_line) | source_line < 1L
  ]
  if (nrow(malformed)) {
    stop(
      "The generated analysis-to-ZA lookup contains ", nrow(malformed),
      " malformed or incomplete row(s). Regenerate it with ",
      "tools/build_lookups_from_stata.py.",
      call. = FALSE
    )
  }
  assert_unique_key(
    lookup,
    c("analysis_code", "za_code"),
    "analysis-to-ZA mapping"
  )

  expected_targets <- c(1:139, 145:181)
  actual_targets <- sort(unique(lookup$za_code))
  if (!identical(actual_targets, expected_targets)) {
    stop(
      "The analysis-to-ZA table must contain exactly ZA 1-139 and 145-181. ",
      "Missing: ", paste(setdiff(expected_targets, actual_targets), collapse = ", "),
      "; unexpected: ", paste(setdiff(actual_targets, expected_targets), collapse = ", "),
      call. = FALSE
    )
  }

  detailed <- lookup[hierarchy == "detailed"]
  if (!identical(sort(unique(detailed$za_code)), 1:139)) {
    stop("The detailed ZA layer must contain ZA 1-139.", call. = FALSE)
  }
  duplicate_sources <- detailed[, .N, by = analysis_code][N > 1L]
  if (nrow(duplicate_sources)) {
    stop(
      "An analysis cause contributes to more than one detailed ZA cause: ",
      paste(duplicate_sources$analysis_code, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_layers <- c("detailed", "cause_group", "broad_group", "report_group")
  unexpected_layers <- setdiff(unique(lookup$hierarchy), allowed_layers)
  if (length(unexpected_layers)) {
    stop(
      "Unexpected ZA hierarchy value(s): ",
      paste(unexpected_layers, collapse = ", "),
      call. = FALSE
    )
  }

  expected_by_layer <- list(
    detailed = 1:139,
    cause_group = 145:167,
    broad_group = 168:172,
    report_group = 173:181
  )
  for (layer in names(expected_by_layer)) {
    actual <- sort(unique(lookup[hierarchy == layer, za_code]))
    expected <- expected_by_layer[[layer]]
    if (!identical(actual, expected)) {
      stop(
        "ZA target coverage is invalid for hierarchy '", layer, "'. ",
        "Missing: ", paste(setdiff(expected, actual), collapse = ", "),
        "; unexpected: ", paste(setdiff(actual, expected), collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (any(abs(lookup$weight - 1) > .Machine$double.eps^0.5)) {
    stop(
      "The supplied Stata ZA hierarchy contains only unit contributions; ",
      "non-unit generated weights require source review.",
      call. = FALSE
    )
  }

  data.table::setorder(lookup, za_code, analysis_code)
  lookup[]
}

apply_analysis_to_za <- function(
    data,
    lookup,
    value = "Count",
    strict = TRUE,
    tolerance = 1e-8,
    complete = TRUE) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c("nbdcode", value))
  assert_has_columns(
    lookup,
    c("analysis_code", "za_code", "weight", "hierarchy")
  )

  x[, nbdcode := suppressWarnings(as.integer(nbdcode))]
  x[, (value) := suppressWarnings(as.numeric(get(value)))]
  x[is.na(get(value)), (value) := 0]

  id <- setdiff(names(x), c("nbdcode", value))
  x <- x[, .(value_internal = sum(get(value), na.rm = TRUE)),
    by = c(id, "nbdcode")
  ]

  mapped_sources <- unique(lookup$analysis_code)
  nonzero_unmapped <- x[
    !nbdcode %in% mapped_sources & abs(value_internal) > tolerance,
    .(amount = sum(value_internal, na.rm = TRUE)),
    by = nbdcode
  ]
  if (nrow(nonzero_unmapped)) {
    details <- paste0(
      nonzero_unmapped$nbdcode,
      " (",
      format(nonzero_unmapped$amount, scientific = FALSE, trim = TRUE),
      ")"
    )
    warn_or_stop(
      paste0(
        "Analysis to ZA codes.do omits non-zero analysis cause(s): ",
        paste(details, collapse = ", "),
        ". These causes should be zero after upstream redistribution; review ",
        "the source mapping before allowing them to be dropped."
      ),
      strict = strict
    )
  }

  map <- lookup[, .(
    nbdcode = analysis_code,
    za_code,
    weight,
    hierarchy
  )]
  mapped <- merge(
    x[nbdcode %in% mapped_sources],
    map,
    by = "nbdcode",
    all = FALSE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  mapped[, value_internal := value_internal * weight]
  mapped <- mapped[, .(
    value_internal = sum(value_internal, na.rm = TRUE)
  ), by = c(id, "za_code")]

  if (isTRUE(complete)) {
    za_codes <- data.table::data.table(za_code = sort(unique(lookup$za_code)))
    if (length(id)) {
      # Build an explicit cross join. A temporary key is clearer and more
      # robust across data.table versions than relying on merge(by = NULL).
      groups <- unique(x[, ..id])
      groups[, join_internal := 1L]
      za_codes[, join_internal := 1L]
      grid <- merge(
        groups,
        za_codes,
        by = "join_internal",
        allow.cartesian = TRUE,
        sort = FALSE
      )
      grid[, join_internal := NULL]
    } else {
      grid <- za_codes
    }
    out <- merge(
      grid,
      mapped,
      by = c(id, "za_code"),
      all.x = TRUE,
      sort = FALSE
    )
    out[is.na(value_internal), value_internal := 0]
  } else {
    out <- mapped
  }

  data.table::setnames(out, c("za_code", "value_internal"), c("nbdcode", value))

  # Detailed ZA causes form a partition of all analysis codes retained by the
  # source mapping. Aggregate layers intentionally repeat these deaths.
  detailed_sources <- unique(lookup[hierarchy == "detailed", analysis_code])
  detailed_targets <- unique(lookup[hierarchy == "detailed", za_code])
  if (length(id)) {
    before <- x[nbdcode %in% detailed_sources, .(
      before = sum(value_internal, na.rm = TRUE)
    ), by = id]
    after <- out[nbdcode %in% detailed_targets, .(
      after = sum(get(value), na.rm = TRUE)
    ), by = id]
    check <- merge(before, after, by = id, all = TRUE, sort = FALSE)
    check[is.na(before), before := 0]
    check[is.na(after), after := 0]
    check[, bad := abs(before - after) > tolerance * pmax(1, abs(before), abs(after))]
    if (check[bad == TRUE, .N]) {
      stop("Detailed analysis-to-ZA totals were not preserved by stratum.", call. = FALSE)
    }
  } else {
    before <- x[nbdcode %in% detailed_sources, sum(value_internal, na.rm = TRUE)]
    after <- out[nbdcode %in% detailed_targets, sum(get(value), na.rm = TRUE)]
    assert_total_preserved(before, after, tolerance, "detailed analysis-to-ZA mapping")
  }

  data.table::setorderv(out, c(id, "nbdcode"))
  out[]
}

read_cause_labels <- function(cfg, type = c("analysis", "za")) {
  require_package("data.table")
  type <- match.arg(type)
  key <- if (type == "analysis") "analysis_codes_lookup" else "za_codes_lookup"
  path <- lookup_file(cfg, key, must_exist = TRUE)
  labels <- read_tabular(path)

  if (type == "analysis") {
    assert_has_columns(
      labels,
      c("analysis_code", "analysis_label", "source_file", "source_line")
    )
    labels[, `:=`(
      code = suppressWarnings(as.integer(analysis_code)),
      label = trimws(as.character(analysis_label)),
      source_file = trimws(as.character(source_file)),
      source_line = suppressWarnings(as.integer(source_line))
    )]
    expected_codes <- 0:215
    expected_source <- "Analysis codes.do"
  } else {
    assert_has_columns(
      labels,
      c(
        "za_code", "za_label", "source_file", "source_line",
        "generated_by_source"
      )
    )
    labels[, generated_text := tolower(trimws(as.character(generated_by_source)))]
    labels[, `:=`(
      code = suppressWarnings(as.integer(za_code)),
      label = trimws(as.character(za_label)),
      source_file = trimws(as.character(source_file)),
      source_line = suppressWarnings(as.integer(source_line)),
      generated_by_source = data.table::fcase(
        generated_text %in% c("true", "t", "1"), TRUE,
        generated_text %in% c("false", "f", "0"), FALSE,
        default = NA
      )
    )]
    labels[, generated_text := NULL]
    expected_codes <- c(1:139, 141L, 145:181, 300L)
    expected_source <- "Analysis to ZA codes.do"
  }

  malformed <- labels[
    is.na(code) | is.na(label) | !nzchar(label) |
      is.na(source_file) | source_file != expected_source |
      is.na(source_line) | source_line < 1L
  ]
  if (nrow(malformed)) {
    stop(type, " cause labels contain malformed source-derived rows.", call. = FALSE)
  }
  actual_codes <- sort(unique(labels$code))
  if (!identical(actual_codes, as.integer(expected_codes))) {
    stop(
      type, " cause-label coverage is incomplete. Missing: ",
      paste(setdiff(expected_codes, actual_codes), collapse = ", "),
      "; unexpected: ", paste(setdiff(actual_codes, expected_codes), collapse = ", "),
      call. = FALSE
    )
  }
  if (type == "za") {
    expected_generated <- labels$code %in% c(1:139, 145:181)
    if (any(is.na(labels$generated_by_source)) ||
        !identical(labels$generated_by_source, expected_generated)) {
      stop(
        "ZA label metadata must mark only ZA141 and ZA300 as undefined by source.",
        call. = FALSE
      )
    }
  }
  labels <- labels[, .(code, label)]
  assert_unique_key(labels, "code", paste(type, "cause labels"))
  data.table::setorder(labels, code)
  labels[]
}

# ------------------------------------------------------------------------------
# Record-level ICD-10 verification
# ------------------------------------------------------------------------------

# Record-level ICD-10 verification and recoding --------------------------------
#
# This is a direct, testable translation of 1 ICD10_verify.do. Ambiguous Stata
# expressions have been parenthesised according to the disease-rule comments;
# all departures are listed in docs/MIGRATION_NOTES.md.


# Read only the COD columns used by record-level verification. The Stata source
# contains many additional fields that are never referenced downstream; avoiding
# them reduces import time and, more importantly, the memory footprint of the
# two record-level ICD mapping passes.
read_cod_microdata <- function(path, select_columns = TRUE) {
  require_package("haven")
  require_package("data.table")

  if (!isTRUE(select_columns)) {
    return(data.table::as.data.table(haven::read_dta(path)))
  }
  require_package("tidyselect")

  header <- haven::read_dta(path, n_max = 0, .name_repair = "minimal")
  available <- names(header)
  wanted <- normalise_token(cod_source_column_candidates())
  selected <- available[normalise_token(available) %in% wanted]

  if (!length(selected)) {
    stop("No recognised COD columns were found in: ", path, call. = FALSE)
  }

  out <- haven::read_dta(
    path,
    col_select = tidyselect::all_of(selected),
    .name_repair = "unique"
  )
  data.table::as.data.table(out)
}

icd_between <- function(x, lower, upper) {
  !is.na(x) & x >= lower & x <= upper
}

make_source_date <- function(year, month, day) {
  text <- sprintf(
    "%04d-%02d-%02d",
    suppressWarnings(as.integer(year)),
    suppressWarnings(as.integer(month)),
    suppressWarnings(as.integer(day))
  )
  out <- suppressWarnings(as.Date(text))
  out[is.na(year) | is.na(month) | is.na(day)] <- as.Date(NA)
  out
}

recode_perinatal_codes <- function(x, mask) {
  rules <- list(
    E46 = "P05",
    J18 = c("P22", "P23", "P28"),
    J98 = c("P24", "P25"),
    Q24 = "P29",
    B24 = "P35",
    A41 = "P36",
    A19 = "P37",
    I64 = "P52",
    D64 = "P61",
    E15 = "P70",
    K52 = "P77",
    A09 = "P78"
  )
  for (destination in names(rules)) {
    x[mask & cause %in% rules[[destination]], uc_recode := destination]
  }

  residual <-
    icd_between(x$cause, "P00", "P04") |
    icd_between(x$cause, "P06", "P15") |
    x$cause %in% c(
      "P20", "P21", "P26", "P27", "P38", "P39", "P50", "P51",
      "P71", "P72", "P74", "P76", "P80", "P81", "P83"
    ) |
    icd_between(x$cause, "P53", "P60") |
    icd_between(x$cause, "P90", "P96")
  x[mask & residual, uc_recode := "R99"]
  x
}

apply_icd10_verification <- function(
    data,
    lookup,
    copy = TRUE,
    progress = FALSE) {
  require_package("data.table")
  report <- function(text) {
    if (isTRUE(progress)) {
      message("    - ", text)
      flush.console()
    }
    invisible(NULL)
  }

  report("standardising certificate fields")
  x <- standardise_cod_names(data, copy = copy)
  assert_has_columns(
    x,
    c(
      "DeathType", "DeathYear", "AgeYear", "Sex", "Popgroup",
      "Death_Prov", "UnderlyingCause"
    )
  )

  # Optional multiple-cause columns are created as missing strings so the same
  # code can process older extracts that did not retain all four fields.
  for (column in c("CauseA", "CauseB", "CauseC", "CauseD")) {
    if (!column %in% names(x)) x[, (column) := NA_character_]
  }
  for (column in c("Pregnancy", "DeathMonth", "DeathDay", "BirthYear", "BirthMonth", "BirthDay")) {
    if (!column %in% names(x)) x[, (column) := NA_real_]
  }

  # `Sex_clean` is the R equivalent of the Stata working variable `Sex_`.
  # The raw certificate sex is retained only long enough to initialise it.
  x[, Sex_clean := as.integer(Sex)]
  x[Pregnancy == 1 & Sex >= 8 & DeathType == 1, Sex_clean := 2L]
  x[DeathType == 1 & UnderlyingCause == "P95", DeathType := 2]

  x[, `:=`(
    uc_recode = UnderlyingCause,
    cause = UnderlyingCause,
    CauseChar = substr(UnderlyingCause, 1L, 1L),
    age_clean = as.numeric(AgeYear)
  )]
  valid_icd <- function(z) icd_between(z, "A00", "Y99")
  x[, nrcauses := as.integer(valid_icd(CauseA)) + as.integer(valid_icd(CauseB)) +
      as.integer(valid_icd(CauseC)) + as.integer(valid_icd(CauseD))]

  death_date <- make_source_date(x$DeathYear, x$DeathMonth, x$DeathDay)
  birth_date <- make_source_date(x$BirthYear, x$BirthMonth, x$BirthDay)
  x[, agecalc := abs(as.numeric(death_date - birth_date))]
  x[AgeYear == 188, agecalc := NA_real_]

  x[, age_u5 := 9L]
  x[DeathType == 2, age_u5 := 1L]
  x[DeathType == 1 & AgeYear == 0 & data.table::between(agecalc, 0, 6), age_u5 := 2L]
  x[DeathType == 1 & AgeYear == 999 & age_u5 == 9 & CauseChar %in% c("P", "Q"), age_u5 := 2L]
  x[DeathType == 1 & AgeYear == 0 & data.table::between(agecalc, 7, 28), age_u5 := 3L]
  x[DeathType == 1 & AgeYear == 0 & data.table::between(agecalc, 29, 364), age_u5 := 4L]
  x[DeathType == 1 & data.table::between(AgeYear, 1, 4), age_u5 := 5L]

  # Initial classification is needed because several plausibility rules refer
  # to the pre-recode NBD group.
  report("running the initial ICD-to-NBD classification")
  x <- map_icd_to_nbd(
    x,
    cause_column = "UnderlyingCause",
    lookup = lookup,
    output = "nbd_initial",
    age_column = "age_clean",
    age_u5_column = "age_u5",
    copy = FALSE
  )

  x[DeathType == 1 & ((age_clean >= 0 & age_clean < 10) | age_clean >= 55) &
      data.table::between(nbd_initial, 14, 19), uc_recode := "R99"]
  x[DeathType == 1 & data.table::between(nbd_initial, 14, 19), Sex_clean := 2L]

  x[DeathType == 1 & AgeYear == 999 & data.table::between(nbd_initial, 20, 23), `:=`(
    age_clean = 0,
    age_u5 = 2L
  )]
  x[DeathType == 1 & age_clean > 0 & age_clean < 999 &
      data.table::between(nbd_initial, 20, 23), uc_recode := "R99"]

  report("applying age, sex, perinatal, cancer, and multiple-cause rules")
  x <- recode_perinatal_codes(x, x$DeathType == 1 & x$age_u5 == 4)
  x <- recode_perinatal_codes(x, x$DeathType == 1 & x$age_clean >= 1)
  x[age_clean == 0 & age_u5 == 9, age_u5 := 2L]

  # Cancer age/sex plausibility checks.
  set_unknown_age <- function(mask) x[mask, age_clean := 999]
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 24) & x$nbd_initial == 30 & x$uc_recode == "C15")
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 24) & x$nbd_initial == 31 & x$uc_recode == "C16")
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 24) & x$nbd_initial == 32 & icd_between(x$uc_recode, "C18", "C21"))
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 9) & x$nbd_initial == 33 & x$uc_recode == "C22")
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 24) & x$nbd_initial == 35 & x$uc_recode == "C25")
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 20 & x$nbd_initial == 37 & x$uc_recode %in% c("C33", "C34"))
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 20 & x$nbd_initial == 38 & x$uc_recode == "C43")
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 20 & x$nbd_initial == 39 & x$uc_recode == "C44")
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 15 & x$nbd_initial == 40 & x$uc_recode == "C50")

  x[DeathType == 1 & nbd_initial == 41 & uc_recode == "C53", Sex_clean := 2L]
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 20 & x$nbd_initial == 41 & x$uc_recode == "C53")
  x[DeathType == 1 & nbd_initial == 42 & uc_recode %in% c("C54", "C55"), Sex_clean := 2L]
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 20 & x$nbd_initial == 42 & x$uc_recode %in% c("C54", "C55"))
  x[DeathType == 1 & nbd_initial == 43 & uc_recode == "C56", Sex_clean := 2L]
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 20 & x$nbd_initial == 43 & x$uc_recode == "C56")

  x[DeathType == 1 & nbd_initial == 44 & uc_recode == "C61", Sex_clean := 1L]
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 25) & x$nbd_initial == 44 & x$uc_recode == "C61")
  x[DeathType == 1 & nbd_initial == 45 & uc_recode == "C62", Sex_clean := 1L]
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 10 & x$nbd_initial == 45 & x$uc_recode == "C62")
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 10 & x$nbd_initial == 46 & x$uc_recode == "C67")
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 20) & x$nbd_initial == 28 & icd_between(x$uc_recode, "C00", "C08"))
  set_unknown_age(x$DeathType == 1 & data.table::between(x$age_clean, 0, 20) & x$nbd_initial == 52 & icd_between(x$uc_recode, "C88", "C90"))
  set_unknown_age(x$DeathType == 1 & x$age_clean >= 0 & x$age_clean < 10 &
      x$nbd_initial %in% c(50L, 51L) & (icd_between(x$uc_recode, "C81", "C85") | x$uc_recode == "C96"))
  set_unknown_age(x$DeathType == 1 & x$age_clean == 0 & x$nbd_initial == 53 & icd_between(x$uc_recode, "C91", "C95"))
  x[AgeYear < 5 & age_clean == 999, age_u5 := 9L]

  x[DeathType == 1 & age_u5 %in% 2:3 & nbd_initial %in% 61:62, uc_recode := "P04"]
  x[DeathType == 1 & age_u5 == 4 & nbd_initial %in% 61:62, uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 1 & age_clean < 10 & nbd_initial %in% 61:62, uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 20 & nbd_initial == 69 & uc_recode == "G30", uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 15 & nbd_initial == 70 & uc_recode %in% c("G20", "G21"), uc_recode := "R99"]
  x[DeathType == 1 & age_u5 %in% 2:3 & nbd_initial == 86, uc_recode := "P52"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 10 & nbd_initial == 91 & icd_between(uc_recode, "J40", "J44"), uc_recode := "J84"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 20 & nbd_initial == 100 & uc_recode == "K70", uc_recode := "R99"]
  x[DeathType == 1 & nbd_initial == 105 & uc_recode == "N40", Sex_clean := 1L]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 30 & nbd_initial == 105 & uc_recode == "N40", uc_recode := "R99"]

  x[DeathType == 1 & AgeYear == 999 & data.table::between(nbd_initial, 114, 120), `:=`(
    age_clean = 0,
    age_u5 = 2L
  )]
  x[DeathType == 1 & age_clean >= 60 & age_clean < 999 & uc_recode %in% c("Q03", "Q05"), uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 30 & age_clean < 999 & icd_between(uc_recode, "Q35", "Q45"), uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 30 & age_clean < 999 & uc_recode == "Q86", uc_recode := "R99"]
  x[DeathType == 1 & data.table::between(age_clean, 0, 5) & nbd_initial == 172, uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 5 & nbd_initial == 108 & uc_recode %in% c("M05", "M06"), uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 15 & nbd_initial == 111 & uc_recode %in% c("M30", "M32"), uc_recode := "R99"]
  x[DeathType == 1 & age_clean >= 0 & age_clean < 5 & data.table::between(nbd_initial, 58, 68) & icd_between(uc_recode, "F01", "F99"), uc_recode := "Q86"]
  x[DeathType == 1 & age_u5 %in% 2:3 & icd_between(uc_recode, "I00", "I99"), uc_recode := "R99"]

  neonatal_invalid <- c(13L, 55L, 72L, 73L, 82L, 92L, 94L, 97L, 100L, 109L, 140L, 148L, 175L)
  x[DeathType == 1 & age_u5 %in% 2:3 & nbd_initial %in% neonatal_invalid, uc_recode := "P96"]
  x[DeathType == 1 & age_u5 >= 4 & nbd_initial %in% c(73L, 74L, 76L, 77L, 78L), uc_recode := "R99"]
  x[DeathType == 1 & (age_u5 == 4 | (age_clean >= 1 & age_clean < 15)) & nbd_initial == 92, uc_recode := "R99"]
  x[DeathType == 1 & (age_u5 == 4 | (age_clean >= 1 & age_clean < 5)) & nbd_initial == 148, uc_recode := "R99"]

  for (column in c("CauseA", "CauseB", "CauseC", "CauseD")) {
    x[, paste0(column, "3") := substr(get(column), 1L, 3L)]
  }

  # Diabetes certificates that also report a more specific cardiovascular
  # cause are reassigned following the exact Stata statement order. Priority is
  # therefore I20-I25, then I60-I69, then I11, then I12; within each group the
  # last matching multiple-cause field (D after C after B after A) wins.
  diabetes <- x$DeathType == 1 & icd_between(x$cause, "E10", "E14")
  cardiovascular_rules <- list(
    function(z) z == "I12",
    function(z) z == "I11",
    function(z) icd_between(z, "I60", "I69"),
    function(z) icd_between(z, "I20", "I25")
  )
  multiple_cause_fields <- paste0(c("CauseA", "CauseB", "CauseC", "CauseD"), "3")
  for (rule in cardiovascular_rules) {
    for (column in multiple_cause_fields) {
      x[diabetes & rule(get(column)), uc_recode := get(column)]
    }
  }

  # The legacy file sets I99/R99 and then immediately overwrites the nrcauses<=1
  # branch to I10. The effective final rule is therefore I10 for a single cause
  # and R99 where additional causes are present.
  x[DeathType == 1 & cause == "I10" & nrcauses <= 1, uc_recode := "I10"]
  x[DeathType == 1 & cause == "I10" & nrcauses > 1, uc_recode := "R99"]

  x[DeathType == 1 & DeathYear <= 2005 & age_u5 == 4 & cause == "P37", uc_recode := "A19"]
  x[DeathType == 1 & DeathYear <= 2005 & age_u5 == 4 & cause == "P61", uc_recode := "D64"]
  x[DeathType == 1 & DeathYear <= 2005 & age_u5 == 4 & cause == "P70", uc_recode := "E15"]
  x[DeathType == 1 & DeathYear <= 2005 & age_u5 == 4 & cause %in% c("P24", "P25"), uc_recode := "J98"]

  # Re-run the lookup after all ICD recodes, as SetUpDeathFormats.do does.
  report("running the final ICD-to-NBD classification")
  x <- map_icd_to_nbd(
    x,
    cause_column = "uc_recode",
    lookup = lookup,
    output = "nbdcode",
    age_column = "age_clean",
    age_u5_column = "age_u5",
    copy = FALSE
  )
  x[, `:=`(
    Sex = as.integer(Sex_clean),
    age_ = as.numeric(age_clean)
  )]
  x[, c("Sex_clean", "age_clean", "nbd_initial") := NULL]
  report("record-level ICD verification complete")
  x[]
}

# ------------------------------------------------------------------------------
# Cause-of-death cleaning and demographic redistribution
# ------------------------------------------------------------------------------

# Cause-of-death cleaning and demographic redistribution -----------------------
#
# Stage 02 has one unavoidable record-level component: ICD verification uses
# dates, multiple-cause fields, age and sex plausibility rules. Immediately
# after the final NBD assignment, the microdata are collapsed to the exact
# maximum key retained by the legacy workflow. Every subsequent operation uses
# narrow grouped tables and compact wide matrices.

COD_VERIFIED_GROUP_KEY <- c(
  "DeathType", "Death_Prov", "Sex", "DeathYear", "DeathMonth",
  "nbdcode", "age_", "age_u5", "Popgroup"
)

COD_MONTHLY_GROUP_KEY <- c(
  "Death_Prov", "Sex", "DeathYear", "DeathMonth",
  "nbdcode", "age5", "Popgroup"
)

COD_FINAL_KEY <- c(
  "Death_Prov", "Sex", "DeathYear", "nbdcode", "age5", "Popgroup"
)

cod_drop_zero_cells <- function(cfg) {
  isTRUE(cfg$settings$cod_drop_zero_cells %||% TRUE)
}

cod_working_table <- function(data, copy = TRUE) {
  require_package("data.table")
  if (!is.logical(copy) || length(copy) != 1L || is.na(copy)) {
    stop("`copy` must be TRUE or FALSE.", call. = FALSE)
  }
  if (inherits(data, "data.table")) {
    return(if (copy) data.table::copy(data) else data)
  }
  if (!copy) {
    stop("`copy = FALSE` requires data.table input.", call. = FALSE)
  }
  data.table::as.data.table(data.table::copy(data))
}

check_unmapped_live_births <- function(data, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(data)
  unmapped <- x[DeathType == 1 & !is.na(uc_recode) & is.na(nbdcode), .N]
  if (unmapped <= 0L) return(invisible(TRUE))

  examples <- x[
    DeathType == 1 & !is.na(uc_recode) & is.na(nbdcode),
    head(sort(unique(uc_recode)), 20L)
  ]
  warn_or_stop(
    paste0(
      "ICD-to-NBD lookup left ", unmapped,
      " live-birth records unmapped. Examples: ",
      paste(examples, collapse = ", ")
    ),
    strict = isTRUE(cfg$settings$strict_checks)
  )
}

# Collapse the verified record-level file using the maximum set of categories
# retained by 2 COD cleaning and aggregating.do. This is intentionally performed
# before exclusions and broad age grouping, matching the supplied fast-collapse
# prototype and creating a restartable boundary immediately after verification.
collapse_verified_cod <- function(verified_cod, cfg, copy = TRUE) {
  require_package("data.table")
  x <- cod_working_table(verified_cod, copy = copy)
  assert_has_columns(x, COD_VERIFIED_GROUP_KEY, "verified COD records")

  # Release every certificate-level working column before grouping. In the
  # production call `copy = FALSE`, so this reduces peak memory without making
  # another full copy of the verified microdata. The exact nine-field key is the
  # complete post-verification information required by the legacy COD workflow.
  discard <- setdiff(names(x), COD_VERIFIED_GROUP_KEY)
  if (length(discard)) x[, (discard) := NULL]
  data.table::setcolorder(x, COD_VERIFIED_GROUP_KEY)
  x[, num := 1.0]

  out <- fast_group_sum(
    x,
    by = COD_VERIFIED_GROUP_KEY,
    value = "num",
    backend = cod_aggregation_backend(cfg),
    sort = FALSE
  )
  out[]
}

# Apply exclusions and derive the final 20-category age code on the already
# grouped table. Filtering first creates a smaller working table; no full copy of
# the pre-filter checkpoint is made.
prepare_cod_analysis_groups <- function(grouped_verified_cod) {
  require_package("data.table")
  source <- data.table::as.data.table(grouped_verified_cod)
  assert_has_columns(
    source,
    c(COD_VERIFIED_GROUP_KEY, "num"),
    "grouped verified COD"
  )

  # Match the Stata exclusions: retain live births, exclude population-group 5,
  # and retain South African provinces only. Stata numeric missing provinces
  # compare above 97, so missing provinces are excluded explicitly here.
  x <- source[
    !is.na(DeathType) & DeathType == 1 &
      (is.na(Popgroup) | Popgroup != 5) &
      !is.na(Death_Prov) & Death_Prov < 97
  ]

  x[is.na(Popgroup) | Popgroup == 9, Popgroup := 8L]
  x[is.na(Sex) | Sex > 2, Sex := 8L]

  x[, age5 := age5_from_completed_years(age_)]
  x[!is.na(age_u5) & age_u5 <= 3, age5 := 1L]
  x[!is.na(age_u5) & age_u5 == 4, age5 := 2L]
  # Match the Stata statement order exactly. Numeric missing age can still be
  # classified from age_u5; the explicit 999 sentinel is reset to unknown.
  x[!is.na(age_) & age_ == 999, age5 := 999L]
  x[nbdcode == 215L, nbdcode := 0L]
  x <- x[!is.na(nbdcode)]

  x[, `:=`(
    Death_Prov = as.integer(Death_Prov),
    Sex = as.integer(Sex),
    DeathYear = as.integer(DeathYear),
    DeathMonth = as.integer(DeathMonth),
    nbdcode = as.integer(nbdcode),
    age5 = as.integer(age5),
    Popgroup = as.integer(Popgroup),
    num = as.numeric(num)
  )]
  x[]
}

# Compute only the cells affected by the legacy interpolation. For every
# province-sex-population-month stratum, 2004 ill-defined deaths at age 1-4 are
# replaced by two-thirds of 2003 plus one-third of 2006. Missing anchor cells are
# structural zeros, matching the Stata reshape-wide/fill-zero sequence.
cod_2004_ill_defined_correction <- function(prepared_cod, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(prepared_cod)
  assert_has_columns(x, c(COD_MONTHLY_GROUP_KEY, "num"), "prepared COD groups")

  anchors <- x[
    nbdcode == 143L & age5 == 3L & DeathYear %in% c(2003L, 2006L)
  ]
  if (!nrow(anchors)) {
    return(data.table::data.table(
      Death_Prov = integer(), Sex = integer(), DeathYear = integer(),
      nbdcode = integer(), age5 = integer(), Popgroup = integer(),
      num = numeric()
    ))
  }

  anchors <- fast_group_sum(
    anchors,
    by = c("Death_Prov", "Sex", "DeathMonth", "Popgroup", "DeathYear"),
    value = "num",
    backend = cod_aggregation_backend(cfg),
    sort = FALSE
  )
  wide <- data.table::dcast(
    anchors,
    Death_Prov + Sex + DeathMonth + Popgroup ~ DeathYear,
    value.var = "num",
    fun.aggregate = sum,
    fill = 0
  )
  for (year_name in c("2003", "2006")) {
    if (!year_name %in% names(wide)) {
      data.table::set(wide, j = year_name, value = 0.0)
    }
  }

  monthly <- wide[, .(
    Death_Prov,
    Sex,
    DeathYear = 2004L,
    nbdcode = 143L,
    age5 = 3L,
    Popgroup,
    num = as.numeric((2 / 3) * get("2003") + (1 / 3) * get("2006"))
  )]
  correction <- fast_group_sum(
    monthly,
    by = COD_FINAL_KEY,
    value = "num",
    backend = cod_aggregation_backend(cfg),
    sort = FALSE
  )
  correction[abs(num) > 1e-12][]
}

# Collapse the prepared exact-age/month groups directly to annual analysis
# cells. The previous translation first aggregated the complete table by month
# and then aggregated it again after interpolation. Only one full grouped pass
# is needed: the tiny 2003/2006 anchor subset is handled separately and replaces
# the affected 2004 annual cells afterwards.
aggregate_cod_annual_groups <- function(prepared_cod, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(prepared_cod)
  assert_has_columns(x, c(COD_MONTHLY_GROUP_KEY, "num"), "prepared COD groups")

  correction <- cod_2004_ill_defined_correction(x, cfg)
  annual <- fast_group_sum(
    x,
    by = COD_FINAL_KEY,
    value = "num",
    backend = cod_aggregation_backend(cfg),
    sort = FALSE
  )

  affected <- annual[
    DeathYear == 2004L & nbdcode == 143L & age5 == 3L,
    sum(num, na.rm = TRUE)
  ]
  annual <- annual[!(DeathYear == 2004L & nbdcode == 143L & age5 == 3L)]
  if (nrow(correction)) {
    annual <- data.table::rbindlist(
      list(annual, correction),
      use.names = TRUE,
      fill = FALSE
    )
  }
  diagnostics <- list(
    observed_2004 = as.numeric(affected),
    corrected_2004 = sum(correction$num, na.rm = TRUE),
    corrected_cells = nrow(correction)
  )
  annual <- annual[]
  attr(annual, "interpolation_diagnostics") <- diagnostics
  annual
}

cod_cast_category_wide <- function(
    data,
    id,
    category,
    value,
    levels,
    prefix) {
  require_package("data.table")
  x <- data.table::as.data.table(data)
  assert_has_columns(x, c(id, category, value), "COD redistribution input")

  actual <- sort(unique(x[[category]][!is.na(x[[category]])]))
  unexpected <- setdiff(actual, levels)
  if (length(unexpected) || anyNA(x[[category]])) {
    stop(
      "Unexpected or missing ", category,
      " code(s) before redistribution: ",
      paste(c(unexpected, if (anyNA(x[[category]])) "NA"), collapse = ", "),
      call. = FALSE
    )
  }

  formula <- stats::as.formula(
    paste(paste(id, collapse = " + "), "~", category)
  )
  wide <- data.table::dcast(
    x,
    formula,
    value.var = value,
    fun.aggregate = sum,
    fill = 0
  )

  level_names <- as.character(levels)
  missing <- setdiff(level_names, names(wide))
  for (column in missing) data.table::set(wide, j = column, value = 0.0)
  data.table::setcolorder(wide, c(id, level_names))
  renamed <- paste0(prefix, level_names)
  data.table::setnames(wide, level_names, renamed)
  for (column in renamed) {
    data.table::set(wide, j = column, value = as.numeric(wide[[column]]))
  }
  wide[]
}

cod_melt_category <- function(
    wide,
    id,
    category,
    value,
    levels,
    prefix,
    drop_zero = TRUE,
    tolerance = 1e-12) {
  require_package("data.table")
  measure <- paste0(prefix, as.character(levels))
  assert_has_columns(wide, c(id, measure), "wide COD redistribution output")

  out <- data.table::melt(
    wide,
    id.vars = id,
    measure.vars = measure,
    variable.name = "category_internal",
    value.name = value,
    variable.factor = FALSE
  )
  out[, (category) := as.integer(sub(paste0("^", prefix), "", category_internal))]
  out[, category_internal := NULL]
  data.table::setcolorder(out, c(id, category, value))
  if (isTRUE(drop_zero)) out <- out[abs(get(value)) > tolerance]
  out[]
}

redistribute_cod_sex <- function(data, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(data)
  id <- c("Death_Prov", "Popgroup", "DeathYear", "nbdcode", "age5")
  before <- sum(x$num, na.rm = TRUE)

  wide <- cod_cast_category_wide(
    x, id = id, category = "Sex", value = "num",
    levels = c(1L, 2L, 8L), prefix = "sex_"
  )
  wide <- redistribute_columns(
    wide,
    source_vars = "sex_8",
    target_vars = c("sex_1", "sex_2"),
    source_action = "drop",
    copy = FALSE,
    missing_counts = "zero",
    tolerance = 1e-10,
    quiet = TRUE,
    context = "unknown-sex redistribution"
  )
  out <- cod_melt_category(
    wide, id = id, category = "Sex", value = "num",
    levels = 1:2, prefix = "sex_",
    drop_zero = cod_drop_zero_cells(cfg)
  )
  assert_total_preserved(before, sum(out$num), 1e-9, "unknown-sex redistribution")
  out[]
}

redistribute_cod_age <- function(data, cfg) {
  require_package("data.table")
  x <- data.table::as.data.table(data)
  id <- c("Death_Prov", "Sex", "DeathYear", "nbdcode", "Popgroup")
  before <- sum(x$num, na.rm = TRUE)

  wide <- cod_cast_category_wide(
    x, id = id, category = "age5", value = "num",
    levels = c(1:20, 999L), prefix = "age_"
  )
  wide <- redistribute_columns(
    wide,
    source_vars = "age_999",
    target_vars = paste0("age_", 1:20),
    source_action = "drop",
    copy = FALSE,
    missing_counts = "zero",
    tolerance = 1e-10,
    quiet = TRUE,
    context = "unknown-age redistribution"
  )
  out <- cod_melt_category(
    wide, id = id, category = "age5", value = "num",
    levels = 1:20, prefix = "age_",
    drop_zero = cod_drop_zero_cells(cfg)
  )
  assert_total_preserved(before, sum(out$num), 1e-9, "unknown-age redistribution")
  out[]
}

# Population-group redistribution differs for 1997-1998. Those two years use
# the combined 1999-2000 known-population distribution; later years use their
# own observed distribution. When the 1999-2000 known total is zero or absent,
# the unknown count is split equally across the four targets, matching the
# explicit Stata fallback. The calculation uses four target columns plus one
# unknown column and never constructs a long-form Cartesian category grid.
redistribute_cod_population_group <- function(
    data,
    cfg,
    reference_years = 1999:2000,
    early_year_max = 1998L) {
  require_package("data.table")
  x <- data.table::as.data.table(data)
  by <- c("Death_Prov", "Sex", "nbdcode", "age5")
  year <- "DeathYear"
  id <- c(by, year)
  targets <- 1:4
  target_cols <- paste0("pop_", targets)
  source_col <- "pop_8"
  before <- sum(x$num, na.rm = TRUE)

  wide <- cod_cast_category_wide(
    x, id = id, category = "Popgroup", value = "num",
    levels = c(targets, 8L), prefix = "pop_"
  )
  wide[, .cod_row_id := .I]

  reference <- wide[
    get(year) %in% reference_years,
    lapply(.SD, sum, na.rm = TRUE),
    by = by,
    .SDcols = target_cols
  ]

  reference_share_cols <- paste0("reference_share_", targets)
  if (nrow(reference)) {
    reference_matrix <- as.matrix(reference[, ..target_cols])
    reference_total <- rowSums(reference_matrix)
    reference_shares <- matrix(
      1 / length(targets),
      nrow = nrow(reference_matrix),
      ncol = length(targets)
    )
    positive <- reference_total > 0
    if (any(positive)) {
      reference_shares[positive, ] <- sweep(
        reference_matrix[positive, , drop = FALSE],
        1L,
        reference_total[positive],
        "/"
      )
    }
    for (j in seq_along(reference_share_cols)) {
      reference[, (reference_share_cols[[j]]) := reference_shares[, j]]
    }
    reference <- reference[, c(by, reference_share_cols), with = FALSE]
    wide <- merge(wide, reference, by = by, all.x = TRUE, sort = FALSE)
    data.table::setorder(wide, .cod_row_id)
  } else {
    for (column in reference_share_cols) {
      data.table::set(wide, j = column, value = 1 / length(targets))
    }
  }

  # A by-stratum with no 1999-2000 row has missing joined shares. Treat its
  # reference known total as zero and apply the Stata equal-split fallback.
  for (column in reference_share_cols) {
    wide[is.na(get(column)), (column) := 1 / length(targets)]
  }

  target_matrix <- as.matrix(wide[, ..target_cols])
  source_total <- as.numeric(wide[[source_col]])
  current_total <- rowSums(target_matrix)
  current_shares <- matrix(
    1 / length(targets),
    nrow = nrow(target_matrix),
    ncol = length(targets)
  )
  positive <- current_total > 0
  if (any(positive)) {
    current_shares[positive, ] <- sweep(
      target_matrix[positive, , drop = FALSE],
      1L,
      current_total[positive],
      "/"
    )
  }

  reference_shares <- as.matrix(wide[, ..reference_share_cols])
  selected_shares <- current_shares
  early <- wide[[year]] <= early_year_max
  early[is.na(early)] <- FALSE
  if (any(early)) {
    selected_shares[early, ] <- reference_shares[early, , drop = FALSE]
  }

  new_target_matrix <- target_matrix + sweep(
    selected_shares, 1L, source_total, "*"
  )
  original_total <- rowSums(target_matrix) + source_total
  allocated_total <- rowSums(new_target_matrix)
  scale <- pmax(1, abs(original_total), abs(allocated_total))
  if (any(abs(original_total - allocated_total) > 1e-10 * scale)) {
    stop("Population-group redistribution failed row-total conservation.", call. = FALSE)
  }

  for (j in seq_along(target_cols)) {
    data.table::set(wide, j = target_cols[[j]], value = new_target_matrix[, j])
  }
  drop_columns <- c(source_col, reference_share_cols, ".cod_row_id")
  wide[, (drop_columns) := NULL]

  out <- cod_melt_category(
    wide, id = id, category = "Popgroup", value = "num",
    levels = targets, prefix = "pop_",
    drop_zero = cod_drop_zero_cells(cfg)
  )
  assert_total_preserved(
    before,
    sum(out$num),
    tolerance = 1e-9,
    label = "unknown-population-group redistribution"
  )
  out[]
}

finalize_clean_cod <- function(data, copy = TRUE) {
  require_package("data.table")
  x <- cod_working_table(data, copy = copy)
  assert_has_columns(x, c(COD_FINAL_KEY, "num"), "redistributed COD")
  data.table::setnames(x, "num", "Deaths")
  x[, `:=`(
    Death_Prov = as.integer(Death_Prov),
    Sex = as.integer(Sex),
    DeathYear = as.integer(DeathYear),
    nbdcode = as.integer(nbdcode),
    age5 = as.integer(age5),
    Popgroup = as.integer(Popgroup),
    Deaths = as.numeric(Deaths)
  )]
  assert_no_missing(x, COD_FINAL_KEY, "clean COD data")
  assert_unique_key(x, COD_FINAL_KEY, "clean COD data")
  assert_nonnegative(x, "Deaths")
  data.table::setorder(x, DeathYear, Death_Prov, Popgroup, Sex, age5, nbdcode)
  x[]
}

# In-memory convenience path used by tests and small development examples. The
# production workflow exposes each boundary as a separate {targets} file target.
clean_cod_data <- function(raw_cod, icd_lookup, cfg) {
  verified <- apply_icd10_verification(raw_cod, icd_lookup, copy = TRUE)
  check_unmapped_live_births(verified, cfg)
  grouped <- collapse_verified_cod(verified, cfg)
  prepared <- prepare_cod_analysis_groups(grouped)
  annual <- aggregate_cod_annual_groups(prepared, cfg)
  sex <- redistribute_cod_sex(annual, cfg)
  age <- redistribute_cod_age(sex, cfg)
  population <- redistribute_cod_population_group(age, cfg)
  finalize_clean_cod(population, copy = FALSE)
}

split_cod_by_injury <- function(clean_cod) {
  require_package("data.table")
  x <- data.table::as.data.table(clean_cod)
  list(
    natural = x[!nbdcode %in% RAW_INJURY_CODES],
    injury = x[nbdcode %in% RAW_INJURY_CODES]
  )
}
