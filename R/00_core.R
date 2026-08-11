# ==============================================================================
# 00_core: Project setup and shared helpers
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Configuration and project paths
# ------------------------------------------------------------------------------

# Project configuration ---------------------------------------------------------
#
# All file paths and year ranges are defined in config/config.yml. No function
# changes the working directory and no absolute path is embedded in the code.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    marker <- file.path(current, "config", "config.yml")
    if (file.exists(marker)) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not find config/config.yml. Start R inside the project folder ",
        "or pass an explicit project root.",
        call. = FALSE
      )
    }
    current <- parent
  }
}

read_project_config <- function(root = find_project_root()) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Run source('install_packages.R').", call. = FALSE)
  }

  config_path <- file.path(root, "config", "config.yml")
  cfg <- yaml::read_yaml(config_path)
  cfg$root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  path_keys <- names(cfg$paths)
  for (key in path_keys) {
    cfg$paths[[key]] <- file.path(cfg$root, cfg$paths[[key]])
  }

  dir_paths <- unname(unlist(cfg$paths, use.names = FALSE))
  invisible(lapply(dir_paths, dir.create, recursive = TRUE, showWarnings = FALSE))

  threads <- as.integer(cfg$settings$n_threads %||% 0L)
  if (is.na(threads) || threads <= 0L) {
    cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
    if (!is.finite(cores) || cores < 1L) cores <- 1L
    threads <- max(1L, as.integer(cores) - 1L)
  }
  cfg$settings$n_threads_resolved <- threads

  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(threads)
    options(datatable.verbose = FALSE)
  }
  if (requireNamespace("collapse", quietly = TRUE)) {
    collapse::set_collapse(nthreads = threads, sort = FALSE)
  }

  set.seed(as.integer(cfg$settings$seed %||% 20251125L))
  cfg
}

config_file <- function(cfg, section, key, must_exist = FALSE) {
  base_dir <- switch(
    section,
    raw = cfg$paths$raw,
    lookups = cfg$paths$lookups,
    derived = cfg$paths$derived,
    figures = cfg$paths$figures,
    tables = cfg$paths$tables,
    database = cfg$paths$database,
    stop("Unknown configuration section: ", section, call. = FALSE)
  )

  value <- cfg$files[[key]] %||% key
  path <- file.path(base_dir, value)

  if (must_exist && !file.exists(path)) {
    stop("Required file does not exist: ", path, call. = FALSE)
  }
  path
}

raw_file <- function(cfg, key, must_exist = FALSE) {
  config_file(cfg, "raw", key, must_exist = must_exist)
}

lookup_file <- function(cfg, key, must_exist = FALSE) {
  config_file(cfg, "lookups", key, must_exist = must_exist)
}

derived_file <- function(cfg, filename) {
  file.path(cfg$paths$derived, filename)
}

figure_file <- function(cfg, filename) {
  file.path(cfg$paths$figures, filename)
}

table_file <- function(cfg, filename) {
  file.path(cfg$paths$tables, filename)
}

database_file <- function(cfg, filename) {
  file.path(cfg$paths$database, filename)
}

# ------------------------------------------------------------------------------
# Input/output helpers
# ------------------------------------------------------------------------------

# Input/output helpers ----------------------------------------------------------
#
# Intermediate data are stored as Parquet: compact, fast, cross-platform and
# language-agnostic. Stata exports are optional compatibility products rather
# than the internal working format.

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package, "' is required. Run source('install_packages.R').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_tabular <- function(path, ...) {
  if (!file.exists(path)) {
    stop("Input file does not exist: ", path, call. = FALSE)
  }

  extension <- tolower(tools::file_ext(path))
  out <- switch(
    extension,
    dta = {
      require_package("haven")
      haven::read_dta(path, ...)
    },
    csv = {
      require_package("data.table")
      data.table::fread(path, ...)
    },
    txt = {
      require_package("data.table")
      data.table::fread(path, ...)
    },
    parquet = {
      require_package("arrow")
      arrow::read_parquet(path, as_data_frame = TRUE, ...)
    },
    rds = readRDS(path),
    xls = {
      require_package("readxl")
      readxl::read_excel(path, ...)
    },
    xlsx = {
      require_package("readxl")
      readxl::read_excel(path, ...)
    },
    stop("Unsupported input extension: .", extension, call. = FALSE)
  )

  require_package("data.table")
  data.table::as.data.table(out)
}

write_tabular <- function(data, path, parquet_compression = "zstd", ...) {
  require_package("data.table")
  data <- data.table::as.data.table(data)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  extension <- tolower(tools::file_ext(path))
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = dirname(path),
    fileext = paste0(".", extension)
  )
  on.exit(unlink(temporary), add = TRUE)

  switch(
    extension,
    dta = {
      require_package("haven")
      haven::write_dta(as.data.frame(data), temporary, version = 14, ...)
    },
    csv = data.table::fwrite(data, temporary, ...),
    parquet = {
      require_package("arrow")
      # data.table inherits from data.frame, so Arrow can consume it directly.
      # Avoid an explicit as.data.frame() conversion at multi-million-row
      # checkpoint boundaries.
      arrow::write_parquet(
        data,
        sink = temporary,
        compression = parquet_compression,
        ...
      )
    },
    rds = saveRDS(data, temporary, compress = "xz", ...),
    stop("Unsupported output extension: .", extension, call. = FALSE)
  )

  # Keep the previous completed file until the new file has been written and
  # closed successfully. On platforms that cannot rename over an existing file,
  # move the old file aside and restore it if the final rename fails.
  backup <- NULL
  if (file.exists(path)) {
    backup <- tempfile(
      pattern = paste0(".", basename(path), "-backup-"),
      tmpdir = dirname(path)
    )
    if (!file.rename(path, backup)) {
      stop("Could not create a rollback copy for: ", path, call. = FALSE)
    }
  }

  moved <- file.rename(temporary, path)
  if (!moved) {
    if (!is.null(backup) && file.exists(backup)) {
      file.rename(backup, path)
    }
    stop("Could not move temporary output into place: ", path, call. = FALSE)
  }
  if (!is.null(backup) && file.exists(backup)) unlink(backup)

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_stage_data <- function(data, cfg, stem, stata_export = FALSE) {
  parquet_path <- derived_file(cfg, paste0(stem, ".parquet"))
  compression <- as.character(cfg$settings$parquet_compression %||% "snappy")
  if (length(compression) != 1L || is.na(compression) || !nzchar(compression)) {
    stop("settings.parquet_compression must be one non-empty string.", call. = FALSE)
  }
  write_tabular(data, parquet_path, parquet_compression = compression)

  if (isTRUE(stata_export) && isTRUE(cfg$settings$write_stata_exports)) {
    dta_path <- derived_file(cfg, paste0(stem, ".dta"))
    write_tabular(data, dta_path)
  }

  parquet_path
}

read_stage_data <- function(cfg, stem) {
  read_tabular(derived_file(cfg, paste0(stem, ".parquet")))
}

write_csv_table <- function(data, cfg, filename) {
  write_tabular(data, table_file(cfg, filename))
}

select_stage_file <- function(paths, filename) {
  candidates <- as.character(paths)
  matches <- candidates[basename(candidates) == filename]
  if (length(matches) != 1L) {
    stop(
      "Expected exactly one stage output named '", filename,
      "' but found ", length(matches), ".",
      call. = FALSE
    )
  }
  normalizePath(matches, winslash = "/", mustWork = TRUE)
}

# ------------------------------------------------------------------------------
# Reusable assertions
# ------------------------------------------------------------------------------

# Validation helpers ------------------------------------------------------------

assert_has_columns <- function(data, columns, data_name = deparse(substitute(data))) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      data_name, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_unique_key <- function(data, key, data_name = deparse(substitute(data))) {
  assert_has_columns(data, key, data_name)
  require_package("data.table")
  x <- data.table::as.data.table(data)
  duplicate_index <- anyDuplicated(x, by = key)
  if (duplicate_index > 0L) {
    duplicate_mask <- duplicated(x, by = key) |
      duplicated(x, by = key, fromLast = TRUE)
    duplicate_rows <- unique(x[duplicate_mask, ..key])
    preview <- utils::capture.output(print(utils::head(duplicate_rows, 10L)))
    stop(
      data_name, " is not unique by: ", paste(key, collapse = ", "), "\n",
      paste(preview, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_nonnegative <- function(data, columns, tolerance = 1e-10) {
  assert_has_columns(data, columns)
  require_package("data.table")
  data <- data.table::as.data.table(data)

  for (column in columns) {
    bad <- data[!is.na(get(column)) & get(column) < -tolerance, .N]
    if (bad > 0L) {
      stop(column, " contains ", bad, " negative value(s).", call. = FALSE)
    }
  }
  invisible(TRUE)
}

assert_no_missing <- function(data, columns, data_name = deparse(substitute(data))) {
  assert_has_columns(data, columns, data_name)
  require_package("data.table")
  data <- data.table::as.data.table(data)

  counts <- vapply(columns, function(column) sum(is.na(data[[column]])), numeric(1))
  counts <- counts[counts > 0]
  if (length(counts)) {
    stop(
      data_name, " has missing values in required fields: ",
      paste(paste0(names(counts), "=", counts), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_total_preserved <- function(before, after, tolerance = 1e-7, label = "total") {
  if (!is.finite(before) || !is.finite(after)) {
    stop("Cannot validate ", label, ": total is not finite.", call. = FALSE)
  }

  scale <- max(1, abs(before), abs(after))
  if (abs(before - after) > tolerance * scale) {
    stop(
      label, " was not preserved. Before=", signif(before, 12),
      ", after=", signif(after, 12),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

warn_or_stop <- function(message, strict = TRUE) {
  if (isTRUE(strict)) {
    stop(message, call. = FALSE)
  }
  warning(message, call. = FALSE)
  invisible(FALSE)
}

# ------------------------------------------------------------------------------
# Shared code lists and labels
# ------------------------------------------------------------------------------

# Shared code lists and labels --------------------------------------------------

PROVINCE_LABELS <- c(
  `1` = "Western Cape",
  `2` = "Eastern Cape",
  `3` = "Northern Cape",
  `4` = "Free State",
  `5` = "KwaZulu-Natal",
  `6` = "North West",
  `7` = "Gauteng",
  `8` = "Mpumalanga",
  `9` = "Limpopo",
  `10` = "South Africa"
)

PROVINCE_ABBREVIATIONS <- c(
  `1` = "WC", `2` = "EC", `3` = "NC", `4` = "FS", `5` = "KZN",
  `6` = "NW", `7` = "GT", `8` = "MP", `9` = "LM", `10` = "SA"
)

PROVINCE_FROM_IMS <- c(
  `1` = 2L,  # Eastern Cape
  `2` = 4L,  # Free State
  `3` = 7L,  # Gauteng
  `4` = 6L,  # North West
  `5` = 5L,  # KwaZulu-Natal
  `6` = 9L,  # Limpopo
  `7` = 8L,  # Mpumalanga
  `8` = 3L,  # Northern Cape
  `9` = 1L   # Western Cape
)

POP_GROUP_LABELS <- c(
  `1` = "African",
  `2` = "White",
  `3` = "Asian/Indian",
  `4` = "Coloured"
)

# IMS coding is African, Coloured, Asian, White. NBD coding is African, White,
# Asian/Indian, Coloured.
POP_GROUP_FROM_IMS <- c(`1` = 1L, `2` = 4L, `3` = 3L, `4` = 2L)

AGE5_LABELS <- c(
  `1` = "<1 month",
  `2` = "1-11 months",
  `3` = "1-4 years",
  `4` = "5-9 years",
  `5` = "10-14 years",
  `6` = "15-19 years",
  `7` = "20-24 years",
  `8` = "25-29 years",
  `9` = "30-34 years",
  `10` = "35-39 years",
  `11` = "40-44 years",
  `12` = "45-49 years",
  `13` = "50-54 years",
  `14` = "55-59 years",
  `15` = "60-64 years",
  `16` = "65-69 years",
  `17` = "70-74 years",
  `18` = "75-79 years",
  `19` = "80-84 years",
  `20` = "85+ years"
)

DATABASE_AGE_LABELS <- c(
  `0` = "<1 month",
  `1` = "1-11 months",
  `2` = "1-4 years",
  `3` = "5-9 years",
  `4` = "10-14 years",
  `5` = "15-19 years",
  `6` = "20-24 years",
  `7` = "25-29 years",
  `8` = "30-34 years",
  `9` = "35-39 years",
  `10` = "40-44 years",
  `11` = "45-49 years",
  `12` = "50-54 years",
  `13` = "55-59 years",
  `14` = "60-64 years",
  `15` = "65-69 years",
  `16` = "70-74 years",
  `17` = "75-79 years",
  `18` = "80-84 years",
  `19` = "85+ years",
  `20` = "Under 5 years",
  `21` = "5-14 years",
  `22` = "15-44 years",
  `23` = "45-59 years",
  `24` = "60+ years",
  `25` = "All ages",
  `26` = "15+ years"
)

INJURY_CODES <- c(
  124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L,
  135L, 136L, 137L, 138L, 139L, 140L, 141L
)

# Broad groups used by the hierarchical NIMS-IMS-FAMHIS ALR interpolation.
# The four sets form an exhaustive, non-overlapping partition of the 15 final
# injury causes.
INJURY_TRANSPORT_CODES <- c(124L, 125L)
INJURY_OTHER_UNINTENTIONAL_CODES <- c(
  127L, 128L, 129L, 130L, 131L, 132L,
  135L, 136L, 137L, 138L, 139L
)
INJURY_SELF_HARM_CODES <- 140L
INJURY_VIOLENCE_CODES <- 141L

stopifnot(
  identical(
    sort(c(
      INJURY_TRANSPORT_CODES,
      INJURY_OTHER_UNINTENTIONAL_CODES,
      INJURY_SELF_HARM_CODES,
      INJURY_VIOLENCE_CODES
    )),
    sort(INJURY_CODES)
  )
)

INJURY_LABELS <- c(
  `124` = "Road injuries",
  `125` = "Other transport accidents",
  `127` = "Poisonings (including herbal)",
  `128` = "Falls",
  `129` = "Fires, heat and hot substances",
  `130` = "Drowning",
  `131` = "Other threats to breathing",
  `132` = "Mechanical forces",
  `135` = "Exposure to natural forces",
  `136` = "Adverse effects of medical and surgical treatment",
  `137` = "Mining accidents",
  `138` = "Animal contact",
  `139` = "Other unintentional injuries",
  `140` = "Self-inflicted injuries",
  `141` = "Interpersonal violence"
)

INJURY_NBD_GROUP_MAP <- c(
  `1` = 141L,
  `2` = 141L,
  `3` = 140L,
  `4` = 124L,
  `5` = 125L,
  `6` = 127L,
  `7` = 128L,
  `8` = 129L,
  `9` = 130L,
  `10` = 137L,
  `11` = 131L,
  `12` = 132L,
  `13` = 135L,
  `14` = 136L,
  `15` = 138L,
  `16` = 139L
)

FAMHIS_MECHANISM_MAP <- c(
  ZA126 = 124L,
  ZA127 = 125L,
  ZA128 = 127L,
  ZA129 = 128L,
  ZA130 = 129L,
  ZA131 = 130L,
  ZA1321 = 131L,
  ZA1322 = 137L,
  ZA133 = 132L,
  ZA134 = 135L,
  ZA135 = 136L,
  ZA136 = 138L,
  ZA137 = 139L,
  ZA138 = 140L,
  ZA1381 = 140L,
  ZA1382 = 140L,
  ZA1391 = 141L,
  ZA1392 = 141L
)

# Codes in the raw NBD file that are treated as injuries before the survey-based
# cause fraction model is applied.
RAW_INJURY_CODES <- c(
  124:142, 151L, 158:160, 165L, 196:201, 214L
)

HIV_PSEUDO_SOURCE_MAP <- list(
  `215` = c(1L, 176L, 178L, 210L, 211L),
  `216` = c(3L, 152L),
  `217` = 4L,
  `218` = 5L,
  `219` = c(8L, 193L, 209L, 212L),
  `220` = 161L,
  `221` = c(10L, 191L),
  `222` = c(11L, 175L),
  `223` = 172L,
  `224` = 26L,
  `225` = 27L,
  `226` = c(57L, 162L),
  `227` = 94L,
  `228` = 95L,
  `229` = c(103L, 170L, 181L),
  `230` = 75L,
  `231` = c(143L, 144L, 163L, 164L, 166L, 171L, 177L, 183L,
            185L, 189L, 190L, 202L, 203L),
  `234` = 24L
)

# After HIV deaths have been removed from each pseudo-cause group, the residual
# non-HIV deaths are consolidated into the destination used by the Stata code.
HIV_PSEUDO_DESTINATION <- c(
  `215` = 1L,
  `216` = 3L,
  `217` = 4L,
  `218` = 5L,
  `219` = 8L,
  `220` = 161L,
  `221` = 10L,
  `222` = 11L,
  `223` = 172L,
  `224` = 26L,
  `225` = 27L,
  `226` = 57L,
  `227` = 94L,
  `228` = 95L,
  `229` = 103L,
  `230` = 75L,
  `231` = 143L,
  `234` = 24L
)

HIV_PSEUDO_LABELS <- c(
  `215` = "Tuberculosis",
  `216` = "Sexually transmitted diseases excluding HIV",
  `217` = "Intestinal infectious diseases",
  `218` = "Selected vaccine-preventable diseases",
  `219` = "Meningitis and encephalitis",
  `220` = "Septicaemia",
  `221` = "Other infectious diseases",
  `222` = "Lower respiratory infections",
  `223` = "Adult respiratory distress",
  `224` = "Iron deficiency anaemia",
  `225` = "Other nutritional deficiencies",
  `226` = "Endocrine, nutritional, blood and immune disorders",
  `227` = "Other interstitial lung disease",
  `228` = "Other respiratory diseases",
  `229` = "Other digestive diseases",
  `230` = "Other neurological conditions",
  `231` = "Ill-defined causes",
  `234` = "Protein-energy malnutrition"
)

HIV_PSEUDO_CODES <- as.integer(names(HIV_PSEUDO_SOURCE_MAP))

# Age-specific attenuation applied to the background trend coefficient in the
# HIV reallocation model. These values reproduce the Stata implementation.
HIV_BACKGROUND_AGE_WEIGHT <- c(
  `2` = 0,
  `3` = 0.25,
  `4` = 0.50,
  `5` = 0.75,
  `6` = 1,
  `7` = 1,
  `8` = 1,
  `9` = 1,
  `10` = 1,
  `11` = 1,
  `12` = 0.75,
  `13` = 0.50,
  `14` = 0.25,
  `15` = 0,
  `16` = 0,
  `17` = 0,
  `18` = 0
)

# Natural causes used as redistribution targets for ill-defined natural deaths.
NATURAL_TARGETS_NEONATAL <- c(1:123, 161L, 213L)
NATURAL_TARGETS_POSTNEONATAL <- c(1:19, 24:123, 161L, 213L)

# ------------------------------------------------------------------------------
# data.table helpers
# ------------------------------------------------------------------------------

# data.table compatibility helpers --------------------------------------------
#
# These helpers centralise operations whose syntax has changed across
# data.table releases. In particular, recent data.table versions require a
# non-empty `by` argument in merge(), so Cartesian products are constructed
# with an explicit temporary join column rather than merge(..., by = NULL).

#' Cartesian join two data tables
#'
#' Returns every row of `left` paired with every row of `right`, preserving the
#' original column order. Inputs are copied and never modified by reference.
#'
#' @param left,right Objects coercible to data.table.
#' @param sort Passed to data.table::merge.data.table().
#' @return A data.table with nrow(left) * nrow(right) rows.
cross_join_dt <- function(left, right, sort = FALSE) {
  require_package("data.table")

  left_dt <- data.table::as.data.table(data.table::copy(left))
  right_dt <- data.table::as.data.table(data.table::copy(right))
  # `names.data.table()` may share the table's internal names vector. Make
  # independent character copies before adding the temporary join column;
  # otherwise the saved order can acquire that temporary name by reference.
  left_names <- paste0(names(left_dt))
  right_names <- paste0(names(right_dt))

  overlap <- intersect(left_names, right_names)
  if (length(overlap)) {
    stop(
      "Cross-join inputs must have disjoint column names. Overlap: ",
      paste(overlap, collapse = ", "),
      call. = FALSE
    )
  }

  if (!nrow(left_dt) || !nrow(right_dt)) {
    empty <- c(
      lapply(left_dt, function(column) column[0L]),
      lapply(right_dt, function(column) column[0L])
    )
    names(empty) <- c(left_names, right_names)
    return(data.table::as.data.table(empty))
  }

  join_column <- ".nbd3_cross_join"
  while (join_column %in% c(left_names, right_names)) {
    join_column <- paste0(join_column, "_")
  }

  data.table::set(left_dt, j = join_column, value = 1L)
  data.table::set(right_dt, j = join_column, value = 1L)
  out <- merge(
    left_dt,
    right_dt,
    by = join_column,
    allow.cartesian = TRUE,
    sort = isTRUE(sort)
  )
  data.table::set(out, j = join_column, value = NULL)
  data.table::setcolorder(out, c(left_names, right_names))
  out[]
}

# ------------------------------------------------------------------------------
# Column and value standardisation
# ------------------------------------------------------------------------------

# Canonical variable names and age groups --------------------------------------
#
# The source files were assembled over many years and use a mixture of Stata,
# lower-case and publication-facing names. These helpers translate aliases once
# at the boundary of the pipeline. Internal functions use the canonical names
# shown in the README data dictionary.

normalise_token <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "", x)
  tolower(x)
}

rename_first_match <- function(data, target, candidates, required = FALSE) {
  require_package("data.table")
  data <- data.table::as.data.table(data)

  if (target %in% names(data)) {
    return(data)
  }

  candidates <- unique(c(target, candidates))
  current_norm <- normalise_token(names(data))
  candidate_norm <- normalise_token(candidates)
  hit <- match(candidate_norm, current_norm, nomatch = 0L)
  hit <- hit[hit > 0L]

  if (length(hit)) {
    data.table::setnames(data, names(data)[hit[[1L]]], target)
  } else if (isTRUE(required)) {
    stop(
      "Could not find a source column for canonical field '", target,
      "'. Tried: ", paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  data
}

cod_name_aliases <- function() {
  list(
    DeathType = c("deathtype"),
    DeathYear = c("deathyear", "year_of_death"),
    DeathMonth = c("deathmonth"),
    DeathDay = c("deathday"),
    BirthYear = c("birthyear"),
    BirthMonth = c("birthmonth"),
    BirthDay = c("birthday"),
    AgeYear = c("ageyear", "age_years"),
    Sex = c("sex", "gender", "Sex_", "sex_"),
    Pregnancy = c("pregnancy"),
    Popgroup = c("PopGroup", "popgroup", "population_group"),
    Death_Prov = c("DeathProv", "deathprov_2016", "death_prov", "province"),
    UnderlyingCause = c("Underlyingcause", "underlyingcause", "underlying_cause"),
    CauseA = c("causea"),
    CauseB = c("causeb"),
    CauseC = c("causec"),
    CauseD = c("caused"),
    nbdcode = c("nbd2016", "nbdgr_", "nbd_code")
  )
}

cod_required_names <- function() {
  c(
    "DeathType", "DeathYear", "AgeYear", "Sex", "Popgroup",
    "Death_Prov", "UnderlyingCause"
  )
}

cod_source_column_candidates <- function() {
  aliases <- cod_name_aliases()
  unique(c(names(aliases), unlist(aliases, use.names = FALSE)))
}

standardise_cod_names <- function(data, copy = TRUE) {
  require_package("data.table")
  if (!is.data.frame(data)) {
    stop("COD input must inherit from data.frame.", call. = FALSE)
  }
  if (!is.logical(copy) || length(copy) != 1L || is.na(copy)) {
    stop("`copy` must be TRUE or FALSE.", call. = FALSE)
  }

  if (inherits(data, "data.table")) {
    x <- if (copy) data.table::copy(data) else data
  } else {
    if (!copy) {
      stop("`copy = FALSE` requires data.table input.", call. = FALSE)
    }
    x <- data.table::as.data.table(data.table::copy(data))
  }

  aliases <- cod_name_aliases()
  required <- cod_required_names()

  for (target in names(aliases)) {
    x <- rename_first_match(
      x,
      target = target,
      candidates = aliases[[target]],
      required = target %in% required
    )
  }

  for (column in intersect(
    c(
      "DeathType", "DeathYear", "DeathMonth", "DeathDay", "BirthYear",
      "BirthMonth", "BirthDay", "AgeYear", "Sex", "Pregnancy", "Popgroup",
      "Death_Prov", "nbdcode"
    ),
    names(x)
  )) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
  }

  for (column in intersect(
    c("UnderlyingCause", "CauseA", "CauseB", "CauseC", "CauseD"),
    names(x)
  )) {
    x[, (column) := toupper(trimws(as.character(get(column))))]
    x[get(column) %in% c("", ".", "NA"), (column) := NA_character_]
  }

  x
}

# Convert age in completed years to the 20-category age variable used through
# the estimation pipeline. Infants are split later using age_u5.
age5_from_completed_years <- function(age_years) {
  age_years <- suppressWarnings(as.numeric(age_years))
  out <- rep.int(999L, length(age_years))
  out[!is.na(age_years) & age_years == 0] <- 2L

  breaks <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
  for (i in seq_along(breaks)) {
    lower <- breaks[[i]]
    upper <- if (i < length(breaks)) breaks[[i + 1L]] - 1 else 150
    out[!is.na(age_years) & age_years >= lower & age_years <= upper] <- i + 2L
  }
  out[is.na(age_years) | age_years == 999] <- 999L
  out
}

# FAMHIS supplies a numeric age and a unit. This is kept separate because the
# original code distinguishes neonates (unit 0), other infants and completed
# years before mapping to the common 20-category age variable.
age5_from_famhis <- function(age, age_unit) {
  age <- suppressWarnings(as.numeric(age))
  age_unit <- suppressWarnings(as.numeric(age_unit))
  years <- ifelse(age_unit == 2, age, 0)
  out <- age5_from_completed_years(years)
  out[age_unit == 0] <- 1L
  out[is.na(age_unit)] <- 21L
  out
}

# Return a complete Cartesian grid for specified dimensions, with value columns
# filled by zero. The function is deliberately explicit: completing cells at a
# known boundary is safer than relying on repeated wide/long reshapes.
complete_cells <- function(data, dimensions, values, levels = list(), fill = 0) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c(dimensions, values))

  dimension_levels <- lapply(dimensions, function(column) {
    supplied <- levels[[column]]
    if (!is.null(supplied)) supplied else sort(unique(x[[column]]))
  })
  names(dimension_levels) <- dimensions

  grid <- do.call(data.table::CJ, c(dimension_levels, sorted = FALSE, unique = TRUE))
  x <- x[, lapply(.SD, sum, na.rm = TRUE), by = dimensions, .SDcols = values]
  out <- merge(grid, x, by = dimensions, all.x = TRUE, sort = FALSE)

  for (column in values) {
    data.table::set(out, which(is.na(out[[column]])), column, fill)
  }
  data.table::setorderv(out, dimensions)
  out
}

safe_ratio <- function(numerator, denominator, zero = 0) {
  out <- rep_len(zero, length(numerator))
  valid <- is.finite(numerator) & is.finite(denominator) & denominator != 0
  out[valid] <- numerator[valid] / denominator[valid]
  out
}

# ------------------------------------------------------------------------------
# Fast grouped aggregation and progress reporting
# ------------------------------------------------------------------------------

# Fast grouped aggregation and progress reporting ------------------------------
#
# COD is the only pipeline input that begins as several million unit records.
# Once record-level ICD verification is complete, the data are immediately
# reduced to the smallest lossless key used by the legacy workflow. The default
# grouped-sum backend is collapse::collapv(), with a data.table implementation
# retained as a transparent validation/fallback option.

cod_aggregation_backend <- function(cfg) {
  backend <- tolower(trimws(as.character(
    cfg$settings$cod_aggregation_backend %||% "collapse"
  )))
  allowed <- c("collapse", "data.table")
  if (length(backend) != 1L || is.na(backend) || !backend %in% allowed) {
    stop(
      "settings.cod_aggregation_backend must be one of: ",
      paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }
  backend
}

format_count <- function(x) {
  format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_elapsed <- function(seconds) {
  seconds <- max(0, as.numeric(seconds))
  if (seconds < 60) return(sprintf("%.1f s", seconds))
  minutes <- floor(seconds / 60)
  remaining <- seconds - 60 * minutes
  if (minutes < 60) return(sprintf("%d min %.1f s", minutes, remaining))
  hours <- floor(minutes / 60)
  minutes <- minutes - 60 * hours
  sprintf("%d h %d min %.1f s", hours, minutes, remaining)
}

run_timed_step <- function(label, code) {
  started <- proc.time()[["elapsed"]]
  message(label, " ... [", format(Sys.time(), "%H:%M:%S"), "]")
  flush.console()
  value <- tryCatch(
    force(code),
    error = function(error) {
      elapsed <- proc.time()[["elapsed"]] - started
      message(label, " failed after ", format_elapsed(elapsed), ".")
      flush.console()
      stop(error)
    }
  )
  elapsed <- proc.time()[["elapsed"]] - started
  message(label, " completed in ", format_elapsed(elapsed), ".")
  flush.console()
  value
}

cod_progress <- function(step, total, text) {
  message(sprintf("[COD %d/%d] %s", as.integer(step), as.integer(total), text))
  flush.console()
  invisible(NULL)
}

#' Fast grouped sum with a selectable backend
#'
#' @param data A data.frame or data.table.
#' @param by Character vector defining the grouping key.
#' @param value Name of the single numeric column to sum.
#' @param backend Either `"collapse"` (default for COD) or `"data.table"`.
#' @param sort Sort the grouped output. FALSE is faster and sufficient because
#'   canonical ordering is applied only at stable output boundaries.
#' @param tolerance Relative tolerance for the total-preservation check.
#'
#' @return A data.table unique by `by`, containing `by` and `value`.
fast_group_sum <- function(
    data,
    by,
    value = "num",
    backend = c("collapse", "data.table"),
    sort = FALSE,
    tolerance = 1e-10) {
  require_package("data.table")
  backend <- match.arg(backend)
  x <- data.table::as.data.table(data)
  assert_has_columns(x, c(by, value), "aggregation input")

  if (!length(by)) {
    out <- data.table::data.table(value_internal = sum(
      as.numeric(x[[value]]), na.rm = TRUE
    ))
    data.table::setnames(out, "value_internal", value)
    return(out[])
  }
  if (anyDuplicated(by)) {
    stop("Aggregation key contains duplicated column names.", call. = FALSE)
  }
  if (!nrow(x)) {
    empty <- data.table::copy(x)[0L, c(by, value), with = FALSE]
    data.table::setcolorder(empty, c(by, value))
    if (!is.double(empty[[value]])) {
      data.table::set(empty, j = value, value = numeric())
    }
    return(empty[])
  }
  if (!is.numeric(x[[value]])) {
    stop("Aggregation value column `", value, "` must be numeric.", call. = FALSE)
  }

  # Keep the original vector by reference. Calling as.numeric() unconditionally
  # duplicates a several-million-row count column before every aggregation.
  values <- x[[value]]
  if (any(!is.finite(values[!is.na(values)]))) {
    stop("Aggregation value column contains non-finite values.", call. = FALSE)
  }
  before <- sum(values, na.rm = TRUE)

  if (backend == "collapse") {
    require_package("collapse")
    # collapv() is the standard-evaluation programming interface to collap().
    # Selecting only the key and value keeps the C/C++ grouping operation narrow
    # even when the source object still contains record-level working columns.
    selected <- c(by, value)
    # The verified-COD caller already supplies a narrow table in exactly this
    # order. Reuse it directly rather than copying ten several-million-row
    # columns immediately before the grouping operation.
    narrow <- if (identical(names(x), selected)) {
      x
    } else {
      x[, selected, with = FALSE]
    }
    out <- collapse::collapv(
      narrow,
      by = by,
      FUN = "fsum",
      cols = value,
      keep.by = TRUE,
      keep.col.order = FALSE,
      sort = isTRUE(sort),
      return.order = FALSE,
      method = "auto",
      na.rm = TRUE
    )
    out <- data.table::as.data.table(out)
  } else {
    out <- x[
      , .(value_internal = sum(get(value), na.rm = TRUE)),
      by = by
    ]
    data.table::setnames(out, "value_internal", value)
    if (isTRUE(sort)) data.table::setorderv(out, by)
  }

  # A single-function collapv() call retains the original value-column name.
  # Fail explicitly if a future package release changes that contract.
  assert_has_columns(out, c(by, value), "grouped aggregation output")
  data.table::setcolorder(out, c(by, value))
  if (!is.double(out[[value]])) {
    data.table::set(out, j = value, value = as.numeric(out[[value]]))
  }
  # Both collapv() and data.table's grouped j return one row per grouping key
  # by construction. Re-grouping a four-million-row result merely to prove that
  # contract can double the cost of the hot path, so uniqueness is checked in
  # synthetic backend tests and again at the final stable COD boundary.
  assert_total_preserved(
    before,
    sum(out[[value]], na.rm = TRUE),
    tolerance = tolerance,
    label = paste0("grouped sum of ", value)
  )
  out[]
}
# ------------------------------------------------------------------------------
# Module loader
# ------------------------------------------------------------------------------

source_project_functions <- function(root = NULL) {
  if (is.null(root)) root <- find_project_root()
  files <- sort(list.files(
    file.path(root, "R"),
    pattern = "^[0-9]{2}_.*\\.R$",
    full.names = TRUE
  ))
  files <- files[basename(files) != "00_core.R"]
  invisible(lapply(files, sys.source, envir = .GlobalEnv))
}
