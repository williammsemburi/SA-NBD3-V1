# Project discovery, configuration, I/O, and shared helpers -------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

nbd3_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    markers <- c(
      file.path(current, "_targets.R"),
      file.path(current, "R", "00_core.R"),
      file.path(current, "report", "config", "labels.yml")
    )
    if (all(file.exists(markers))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Run this command from inside the NBD3 repository.", call. = FALSE)
    }
    current <- parent
  }
}

# Backward-compatible internal name retained for the extracted VizTool builders.
viztool_root <- nbd3_root

viz_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package, "' is required. Run source('install_packages.R') from the repository root.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

viz_require_packages <- function(packages) {
  invisible(lapply(packages, viz_require))
}

is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\\\\\)", path)
}

normalise_candidate_path <- function(path, root) {
  if (is.null(path) || !length(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (!is_absolute_path(path)) path <- file.path(root, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

first_existing_path <- function(paths, fallback = NULL) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  existing <- paths[file.exists(paths)]
  if (length(existing)) return(existing[[1L]])
  if (!is.null(fallback)) return(fallback)
  if (length(paths)) paths[[1L]] else NA_character_
}

as_flag <- function(x, default = FALSE) {
  if (is.logical(x)) return(ifelse(is.na(x), default, x))
  value <- tolower(trimws(as.character(x)))
  ifelse(value %in% c("true", "t", "1", "yes", "y"), TRUE,
         ifelse(value %in% c("false", "f", "0", "no", "n"), FALSE, default))
}

split_semicolon <- function(x) {
  x <- as.character(x)
  result <- strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE)
  lapply(result, function(values) {
    values <- trimws(values)
    values[nzchar(values)]
  })
}

parse_integer_codes <- function(x) {
  values <- split_semicolon(x)[[1L]]
  if (!length(values)) return(integer())
  out <- suppressWarnings(as.integer(values))
  if (anyNA(out)) stop("Unable to parse integer code list: ", x, call. = FALSE)
  unique(out)
}

slugify <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
  gsub("(^_+|_+$)", "", x)
}

normalise_text <- function(x) {
  # Lower-case before removing non-alphanumeric characters. Applying the
  # lower-case-only regular expression first would strip capital letters from
  # labels such as "KwaZulu Natal", "NBD3", and "Male".
  x <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
  gsub("[^a-z0-9]+", "", x)
}

normalise_geography_label <- function(x) {
  original <- trimws(as.character(x))
  key <- normalise_text(original)
  aliases <- c(
    westerncape = "Western Cape", wc = "Western Cape",
    easterncape = "Eastern Cape", ec = "Eastern Cape",
    northerncape = "Northern Cape", nc = "Northern Cape",
    freestate = "Free State", fs = "Free State",
    kwazulunatal = "KwaZulu-Natal", kzn = "KwaZulu-Natal",
    northwest = "North West", nw = "North West",
    gauteng = "Gauteng", gp = "Gauteng", gt = "Gauteng",
    mpumalanga = "Mpumalanga", mp = "Mpumalanga",
    limpopo = "Limpopo", lp = "Limpopo", lm = "Limpopo",
    southafrica = "South Africa", rsa = "South Africa", national = "South Africa",
    total = "South Africa", all = "South Africa",
    african = "African", blackafrican = "African", black = "African",
    coloured = "Coloured", colored = "Coloured",
    indianasian = "Asian/Indian", asianindian = "Asian/Indian",
    indian = "Asian/Indian", asian = "Asian/Indian",
    white = "White"
  )
  mapped <- unname(aliases[key])
  mapped[is.na(mapped)] <- original[is.na(mapped)]
  mapped
}

normalise_sex_label <- function(x) {
  original <- trimws(as.character(x))
  key <- normalise_text(original)
  aliases <- c(
    male = "Male", males = "Male", m = "Male",
    female = "Female", females = "Female", f = "Female",
    person = "Person", persons = "Person", both = "Person",
    bothsexes = "Person", total = "Person", all = "Person"
  )
  mapped <- unname(aliases[key])
  mapped[is.na(mapped)] <- original[is.na(mapped)]
  mapped
}

rename_first_match <- function(data, new_name, candidates, required = TRUE) {
  matches <- intersect(candidates, names(data))
  if (!length(matches)) {
    if (required) {
      stop(
        "Could not find a column for '", new_name, "'. Tried: ",
        paste(candidates, collapse = ", "), call. = FALSE
      )
    }
    return(data)
  }
  if (matches[[1L]] != new_name) data.table::setnames(data, matches[[1L]], new_name)
  data
}

assert_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

assert_unique_rows <- function(data, key, label = "data") {
  assert_columns(data, key, label)
  duplicate_count <- data[, .N, by = key][N > 1L, .N]
  if (duplicate_count) {
    stop(label, " has ", duplicate_count, " duplicated key combination(s).", call. = FALSE)
  }
  invisible(TRUE)
}

safe_ratio <- function(numerator, denominator, multiplier = 1) {
  lengths <- c(length(numerator), length(denominator), length(multiplier))
  target_length <- max(lengths)
  if (target_length == 0L) return(numeric())

  recycle_numeric <- function(value, name) {
    if (!length(value)) return(rep(NA_real_, target_length))
    if (!(length(value) %in% c(1L, target_length))) {
      stop(
        name, " must have length 1 or ", target_length,
        "; received length ", length(value), ".",
        call. = FALSE
      )
    }
    rep_len(as.numeric(value), target_length)
  }

  numerator <- recycle_numeric(numerator, "numerator")
  denominator <- recycle_numeric(denominator, "denominator")
  multiplier <- recycle_numeric(multiplier, "multiplier")

  out <- rep(NA_real_, target_length)
  valid <- is.finite(numerator) & is.finite(denominator) &
    is.finite(multiplier) & denominator != 0
  out[valid] <- multiplier[valid] * numerator[valid] / denominator[valid]
  out
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

max_or_na <- function(x) {
  values <- x[is.finite(x)]
  if (!length(values)) NA_real_ else max(values)
}

read_viz_config <- function(root = viztool_root()) {
  viz_require_packages(c("data.table", "yaml"))
  labels_path <- file.path(root, "report", "config", "labels.yml")
  labels <- yaml::read_yaml(labels_path)

  cause_path <- file.path(root, "report", "config", "cause_comparison_map.csv")
  age_path <- file.path(root, "report", "config", "age_comparison_map.csv")
  cause_map <- data.table::fread(cause_path, na.strings = c("", "NA"))
  age_map <- data.table::fread(age_path, na.strings = c("", "NA"))

  required_cause <- c(
    "series_id", "display_name", "domain", "hierarchy", "cause_type",
    "za_codes", "denominator_za_codes", "measure", "unit",
    "legacy_compare_names", "legacy_rate_names", "include_explorer",
    "include_comparison", "sort_order"
  )
  required_age <- c(
    "age_id", "age_label", "database_age_codes", "legacy_compare_names",
    "legacy_rate_names", "include_comparison", "include_explorer",
    "metric_type", "sort_order"
  )
  assert_columns(cause_map, required_cause, "cause_comparison_map.csv")
  assert_columns(age_map, required_age, "age_comparison_map.csv")

  cause_map[, `:=`(
    include_explorer = as_flag(include_explorer),
    include_comparison = as_flag(include_comparison),
    sort_order = as.integer(sort_order)
  )]
  age_map[, `:=`(
    include_explorer = as_flag(include_explorer),
    include_comparison = as_flag(include_comparison),
    sort_order = as.integer(sort_order)
  )]

  assert_unique_rows(cause_map, "series_id", "cause_comparison_map.csv")
  assert_unique_rows(age_map, "age_id", "age_comparison_map.csv")
  if (cause_map[
    (include_explorer | include_comparison) &
      (is.na(za_codes) | !nzchar(za_codes)),
    .N
  ]) {
    stop("An enabled cause definition is missing za_codes.", call. = FALSE)
  }
  if (age_map[
    (include_explorer | include_comparison) & metric_type != "legacy_only" &
      (is.na(database_age_codes) | !nzchar(database_age_codes)),
    .N
  ]) {
    stop("An enabled age definition is missing database_age_codes.", call. = FALSE)
  }
  invisible(alias_lookup(cause_map[include_comparison %in% TRUE], "legacy_compare_names"))
  invisible(alias_lookup(cause_map[include_explorer %in% TRUE], "legacy_rate_names"))
  invisible(age_alias_lookup(age_map[include_comparison %in% TRUE], "legacy_compare_names"))
  invisible(age_alias_lookup(age_map, "legacy_rate_names"))

  env_database <- Sys.getenv("NBD3_DATABASE", "")
  database_candidates <- c(
    if (nzchar(env_database)) normalise_candidate_path(env_database, root),
    normalise_candidate_path(labels$paths$nbd3_database, root)
  )

  legacy_env <- Sys.getenv("NBD3_LEGACY_VIZ", "")
  legacy_path <- first_existing_path(c(
    if (nzchar(legacy_env)) normalise_candidate_path(legacy_env, root),
    normalise_candidate_path(labels$paths$legacy_rda, root)
  ))
  database_path <- first_existing_path(database_candidates)

  validation_candidates <- c(
    normalise_candidate_path(labels$paths$nbd3_validation, root)
  )

  labels$paths$legacy_rda <- legacy_path
  labels$paths$nbd3_database <- database_path
  labels$paths$nbd3_validation <- first_existing_path(validation_candidates)
  labels$paths$derived_dir <- normalise_candidate_path(labels$paths$derived_dir, root)
  dir.create(labels$paths$derived_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    root = root,
    labels = labels,
    cause_map = cause_map,
    age_map = age_map,
    files = list(labels = labels_path, cause_map = cause_path, age_map = age_path)
  )
}

geography_catalog <- function(config) {
  groups <- config$labels$geographies
  rows <- lapply(names(groups), function(type) {
    item <- groups[[type]]
    data.table::data.table(
      geography_type = type,
      geography_code = as.integer(unlist(item$codes, use.names = FALSE)),
      geography = as.character(unlist(item$labels, use.names = FALSE))
    )
  })
  data.table::rbindlist(rows, use.names = TRUE)
}

sex_catalog <- function(config) {
  data.table::data.table(
    sex_code = as.integer(unlist(config$labels$sexes$codes, use.names = FALSE)),
    sex = as.character(unlist(config$labels$sexes$labels, use.names = FALSE))
  )
}

alias_lookup <- function(map, alias_column) {
  if (!nrow(map)) return(data.table::data.table(alias = character(), value = character()))
  rows <- lapply(seq_len(nrow(map)), function(index) {
    aliases <- split_semicolon(map[[alias_column]][[index]])[[1L]]
    if (!length(aliases)) return(NULL)
    data.table::data.table(alias = aliases, value = map$series_id[[index]])
  })
  out <- unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  if (!nrow(out)) {
    return(data.table::data.table(alias = character(), value = character()))
  }
  ambiguous <- out[, .(mapped_values = data.table::uniqueN(value)), by = alias][mapped_values > 1L]
  if (nrow(ambiguous)) {
    stop(
      "Ambiguous alias(es) in ", alias_column, ": ",
      paste(ambiguous$alias, collapse = ", "),
      call. = FALSE
    )
  }
  out[]
}

age_alias_lookup <- function(map, alias_column) {
  rows <- lapply(seq_len(nrow(map)), function(index) {
    aliases <- split_semicolon(map[[alias_column]][[index]])[[1L]]
    if (!length(aliases)) return(NULL)
    data.table::data.table(
      alias = aliases,
      age_id = map$age_id[[index]],
      age_label = map$age_label[[index]],
      metric_type = map$metric_type[[index]],
      age_sort_order = map$sort_order[[index]]
    )
  })
  out <- unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  if (!nrow(out)) {
    return(data.table::data.table(
      alias = character(), age_id = character(), age_label = character(),
      metric_type = character(), age_sort_order = integer()
    ))
  }
  ambiguous <- out[, .(mapped_values = data.table::uniqueN(age_id)), by = alias][mapped_values > 1L]
  if (nrow(ambiguous)) {
    stop(
      "Ambiguous age alias(es) in ", alias_column, ": ",
      paste(ambiguous$alias, collapse = ", "),
      call. = FALSE
    )
  }
  out[]
}

expand_code_map <- function(map, code_column, output_code, include_column = NULL) {
  if (!is.null(include_column)) map <- map[get(include_column) %in% TRUE]
  rows <- lapply(seq_len(nrow(map)), function(index) {
    codes <- parse_integer_codes(map[[code_column]][[index]])
    if (!length(codes)) return(NULL)
    data.table::data.table(
      series_id = map$series_id[[index]],
      code = codes
    )
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (nrow(out)) data.table::setnames(out, "code", output_code)
  out
}

expand_age_map <- function(map, include_column = NULL) {
  if (!is.null(include_column)) map <- map[get(include_column) %in% TRUE]
  rows <- lapply(seq_len(nrow(map)), function(index) {
    codes <- parse_integer_codes(map$database_age_codes[[index]])
    if (!length(codes)) return(NULL)
    data.table::data.table(
      age_id = map$age_id[[index]],
      database_age = codes
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

derived_path <- function(config, filename) {
  file.path(config$labels$paths$derived_dir, filename)
}

read_parquet_dt <- function(path) {
  viz_require_packages(c("arrow", "data.table"))
  data.table::as.data.table(arrow::read_parquet(path))
}

write_parquet_dt <- function(data, path, compression = "zstd") {
  viz_require("arrow")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(temporary), add = TRUE)
  arrow::write_parquet(data, temporary, compression = compression)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) stop("Could not write: ", path, call. = FALSE)
  invisible(path)
}

write_rds_atomic <- function(object, path, compress = "gzip") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = compress)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) stop("Could not write: ", path, call. = FALSE)
  invisible(path)
}

load_rda_isolated <- function(path) {
  if (!file.exists(path)) stop("Legacy RDA not found: ", path, call. = FALSE)
  environment <- new.env(parent = baseenv())
  objects <- load(path, envir = environment)
  list(environment = environment, objects = objects)
}

file_metadata <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(data.table::data.table(path = path, exists = FALSE, size_bytes = NA_real_, modified = as.POSIXct(NA)))
  }
  info <- file.info(path)
  data.table::data.table(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    exists = TRUE,
    size_bytes = as.numeric(info$size),
    modified = info$mtime
  )
}

source_report_files <- function(root = nbd3_root()) {
  files <- c(
    file.path(root, "report", "R", "report_data.R"),
    file.path(root, "report", "R", "report_charts.R")
  )
  files[file.exists(files)]
}

source_viztool_files <- function(root = nbd3_root(), include_modules = FALSE) {
  invisible(source_report_files(root))
}

load_viz_runtime_data <- function(config = read_viz_config(), open_rates = TRUE) {
  runtime_path <- derived_path(config, "viz_input_new.rds")
  if (!file.exists(runtime_path)) {
    stop("Report inputs have not been built. Run Rscript run_nbd.R with RUN_REPORT <- TRUE.", call. = FALSE)
  }
  runtime <- readRDS(runtime_path)
  # Resolve the large Parquet relative to the current repository rather than
  # relying on the absolute path stored on the machine that built the RDS.
  runtime$paths$cause_rates <- derived_path(config, "cause_rates.parquet")
  cause_uncertainty_path <- derived_path(config, "nbd3_cause_uncertainty.parquet")
  runtime$paths$cause_uncertainty <- cause_uncertainty_path
  runtime$cause_uncertainty <- if (file.exists(cause_uncertainty_path)) {
    read_parquet_dt(cause_uncertainty_path)
  } else {
    data.table::data.table()
  }
  if (open_rates) {
    viz_require_packages(c("arrow", "dplyr"))
    runtime$cause_rates_dataset <- arrow::open_dataset(runtime$paths$cause_rates, format = "parquet")
  }
  runtime
}
# Runtime filtering, measure selection, and display helpers -------------------

measure_label <- function(measure, config) {
  labels <- unlist(config$labels$measures, use.names = TRUE)
  value <- unname(labels[[measure]])
  if (is.null(value) || is.na(value)) measure else value
}

measure_unit <- function(measure) {
  switch(
    measure,
    deaths = "deaths",
    fraction = "proportion",
    crude_rate = "per 100,000",
    asr = "per 100,000",
    yll00 = "YLLs",
    yll03 = "YLLs",
    yll015 = "YLLs",
    ""
  )
}

measure_column <- function(measure) {
  allowed <- c("deaths", "crude_rate", "asr", "yll00", "yll03", "yll015")
  if (!measure %in% allowed) stop("Unsupported explorer measure: ", measure, call. = FALSE)
  measure
}

available_measure_names <- function(data) {
  candidates <- c("deaths", "crude_rate", "asr", "yll00", "yll03", "yll015")
  candidates[vapply(candidates, function(column) {
    column %in% names(data) && any(is.finite(data[[column]]))
  }, logical(1))]
}

collect_cause_rates <- function(
    dataset,
    models,
    geography_type,
    geography_codes,
    sex_code,
    series_id,
    age_id,
    year_range,
    measure) {
  viz_require_packages(c("dplyr", "data.table"))

  flatten_values <- function(x, mode = c("character", "integer")) {
    mode <- match.arg(mode)
    values <- unlist(x, recursive = TRUE, use.names = FALSE)
    if (mode == "integer") {
      values <- suppressWarnings(as.integer(values))
      return(unique(values[!is.na(values)]))
    }
    values <- trimws(as.character(values))
    unique(values[!is.na(values) & nzchar(values)])
  }

  model_values <- flatten_values(models, "character")
  geography_type_values <- flatten_values(geography_type, "character")
  geography_code_values <- flatten_values(geography_codes, "integer")
  sex_code_value <- flatten_values(sex_code, "integer")
  series_id_value <- flatten_values(series_id, "character")
  age_id_value <- flatten_values(age_id, "character")
  year_values <- flatten_values(year_range, "integer")
  measure_name <- as.character(measure)[[1L]]
  value_column <- measure_column(measure_name)

  if (!length(model_values) || !length(geography_type_values) ||
      !length(geography_code_values) || length(sex_code_value) != 1L ||
      length(series_id_value) != 1L || length(age_id_value) != 1L ||
      length(year_values) < 2L) {
    stop("Invalid report filter values supplied to collect_cause_rates().", call. = FALSE)
  }

  sex_code_value <- sex_code_value[[1L]]
  series_id_value <- series_id_value[[1L]]
  age_id_value <- age_id_value[[1L]]
  year_start <- min(year_values)
  year_end <- max(year_values)

  selected_columns <- c(
    "model", "geography_type", "geography_code", "geography",
    "sex_code", "sex", "year", "age_id", "age_label",
    "series_id", "series_label", "domain", "hierarchy", "cause_type",
    value_column
  )

  if (inherits(dataset, "Dataset")) {
    # Arrow cannot reliably translate the previous expression
    # geography_code %in% as.integer(character_vector). Push only scalar,
    # type-stable predicates to Arrow, collect the small cause/age/sex slice,
    # and apply the multi-geography filter in data.table.
    query <- dplyr::filter(
      dataset,
      .data$sex_code == .env$sex_code_value,
      .data$series_id == .env$series_id_value,
      .data$age_id == .env$age_id_value,
      .data$year >= .env$year_start,
      .data$year <= .env$year_end
    )
    if (length(model_values) == 1L) {
      model_value <- model_values[[1L]]
      query <- dplyr::filter(query, .data$model == .env$model_value)
    }
    if (length(geography_type_values) == 1L) {
      geography_type_value <- geography_type_values[[1L]]
      query <- dplyr::filter(
        query,
        .data$geography_type == .env$geography_type_value
      )
    }
    query <- dplyr::select(query, dplyr::all_of(selected_columns))
    out <- data.table::as.data.table(dplyr::collect(query))
  } else {
    out <- data.table::as.data.table(data.table::copy(dataset))
    missing_columns <- setdiff(selected_columns, names(out))
    if (length(missing_columns)) {
      stop(
        "Cause-rate data are missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }
    out <- out[, ..selected_columns]
  }

  out <- out[
    model %in% model_values &
      geography_type %in% geography_type_values &
      geography_code %in% geography_code_values &
      sex_code == sex_code_value &
      series_id == series_id_value &
      age_id == age_id_value &
      year >= year_start &
      year <= year_end
  ]

  data.table::setnames(out, value_column, "estimate")
  out[, `:=`(
    measure = measure_name,
    estimate = as.numeric(estimate)
  )]
  out[is.finite(estimate)]
}


catalog_subset <- function(runtime, models = NULL, geography_types = NULL, geography_codes = NULL,
                           sex_codes = NULL, hierarchies = NULL, series_ids = NULL) {
  availability <- data.table::copy(runtime$catalogs$availability)
  if (length(models)) availability <- availability[model %in% models]
  if (length(geography_types)) availability <- availability[geography_type %in% geography_types]
  if (length(geography_codes)) availability <- availability[geography_code %in% as.integer(geography_codes)]
  if (length(sex_codes)) availability <- availability[sex_code %in% as.integer(sex_codes)]
  if (length(hierarchies)) availability <- availability[hierarchy %in% hierarchies]
  if (length(series_ids)) availability <- availability[series_id %in% series_ids]
  availability[]
}


ordered_model_names <- function(models, config) {
  preferred <- as.character(unlist(config$labels$models$order, use.names = FALSE))
  c(intersect(preferred, models), setdiff(sort(models), preferred))
}

model_colour <- function(model, config) {
  colours <- unlist(config$labels$models$colours, use.names = TRUE)
  value <- unname(colours[[model]])
  if (is.null(value) || is.na(value)) "#4C566A" else value
}

model_line_type <- function(model, config) {
  styles <- unlist(config$labels$models$line_types, use.names = TRUE)
  value <- unname(styles[[model]])
  if (is.null(value) || is.na(value)) "solid" else value
}

format_metric <- function(value, measure, digits = 1L) {
  if (!length(value) || is.na(value) || !is.finite(value)) return("—")
  if (identical(measure, "fraction")) return(scales::percent(value, accuracy = 0.1))
  format(round(value, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}
# Optional report-specific inputs ---------------------------------------------

read_optional_table <- function(path) {
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) {
    return(data.table::data.table())
  }
  extension <- tolower(tools::file_ext(path))
  if (extension == "parquet") return(read_parquet_dt(path))
  if (extension == "csv") return(data.table::fread(path))
  stop("Unsupported optional report table: ", path, call. = FALSE)
}

load_injury_report_inputs <- function(config = read_viz_config()) {
  root <- config$root
  paths <- config$labels$paths
  source_fractions <- read_optional_table(normalise_candidate_path(
    paths$injury_source_fractions, root
  ))
  model_fractions <- read_optional_table(normalise_candidate_path(
    paths$injury_model_fractions, root
  ))
  survey_model_comparison <- read_optional_table(normalise_candidate_path(
    paths$injury_survey_model_comparison, root
  ))
  diagnostics <- read_optional_table(normalise_candidate_path(
    paths$injury_diagnostics, root
  ))
  final_estimates <- read_optional_table(normalise_candidate_path(
    paths$injury_final_estimates, root
  ))
  historical_comparison <- read_optional_table(file.path(
    root, "report", "data", "legacy", "nbd_injury_comparison_2026-08-07.csv"
  ))
  list(
    source_fractions = source_fractions,
    model_fractions = model_fractions,
    survey_model_comparison = survey_model_comparison,
    diagnostics = diagnostics,
    final_estimates = final_estimates,
    historical_comparison = historical_comparison
  )
}

injury_broad_group <- function(code) {
  code <- as.integer(code)
  data.table::fcase(
    code %in% c(124L, 125L), "Transport injuries",
    code %in% c(127L, 128L, 129L, 130L, 131L, 132L, 135L, 136L, 137L, 138L, 139L),
      "Other unintentional injuries",
    code == 140L, "Self-harm",
    code == 141L, "Interpersonal violence",
    default = "Other"
  )
}

summarise_injury_broad_fractions <- function(injury_inputs) {
  source <- data.table::as.data.table(data.table::copy(
    injury_inputs$source_fractions
  ))
  final <- data.table::as.data.table(data.table::copy(
    injury_inputs$final_estimates
  ))
  out <- list(
    source = data.table::data.table(),
    model = data.table::data.table(),
    comparison = data.table::data.table(),
    detailed_comparison = data.table::data.table()
  )

  stratum_columns <- c("Death_Prov", "Sex", "Popgroup", "age5")

  if (nrow(final) && all(c(
    stratum_columns, "DeathYear", "nbdcode", "Deaths"
  ) %in% names(final))) {
    final[, broad_group := injury_broad_group(nbdcode)]
    out$model <- final[, .(
      deaths = sum(Deaths, na.rm = TRUE)
    ), by = .(year = as.integer(DeathYear), broad_group)]
    out$model[, fraction := safe_ratio(deaths, sum(deaths)), by = year]
  }

  if (nrow(source) && all(c(
    stratum_columns, "year", "nbdcode"
  ) %in% names(source))) {
    source[, broad_group := injury_broad_group(nbdcode)]

    # Place survey and model compositions on the same national injury-envelope
    # weighting basis. The survey points remain observations: the fitted line is
    # not required to pass through them.
    envelope <- data.table::data.table()
    if (nrow(final) && all(c(
      stratum_columns, "DeathYear", "Deaths"
    ) %in% names(final))) {
      envelope <- final[
        DeathYear %in% unique(source$year),
        .(injury_envelope = sum(Deaths, na.rm = TRUE)),
        by = c(stratum_columns, "DeathYear")
      ]
      data.table::setnames(envelope, "DeathYear", "year")
      source <- merge(
        source,
        envelope,
        by = c(stratum_columns, "year"),
        all.x = TRUE,
        sort = FALSE
      )
    }

    if (!"injury_envelope" %in% names(source)) {
      if ("total_count" %in% names(source)) {
        source[, injury_envelope := as.numeric(total_count)]
      } else {
        source[, injury_envelope := NA_real_]
      }
    }
    source[!is.finite(injury_envelope), injury_envelope := 0]

    survey_input <- if ("fraction" %in% names(source)) {
      as.numeric(source$fraction)
    } else if ("fraction_pre_correction" %in% names(source)) {
      as.numeric(source$fraction_pre_correction)
    } else if ("raw_fraction" %in% names(source)) {
      as.numeric(source$raw_fraction)
    } else {
      rep(NA_real_, nrow(source))
    }
    survey_input[!is.finite(survey_input)] <- 0

    legacy_audit <- if ("fraction_corrected" %in% names(source)) {
      as.numeric(source$fraction_corrected)
    } else {
      survey_input
    }
    legacy_audit[!is.finite(legacy_audit)] <- survey_input[
      !is.finite(legacy_audit)
    ]

    raw_observed <- if ("raw_fraction" %in% names(source)) {
      as.numeric(source$raw_fraction)
    } else {
      survey_input
    }

    source[, `:=`(
      survey_input_fraction_cell = survey_input,
      legacy_audit_fraction_cell = legacy_audit,
      raw_observed_fraction_cell = raw_observed,
      survey_input_count = survey_input * injury_envelope,
      legacy_audit_count = legacy_audit * injury_envelope,
      raw_observed_count_weighted = data.table::fifelse(
        is.finite(raw_observed),
        raw_observed * injury_envelope,
        NA_real_
      ),
      raw_observed_envelope = data.table::fifelse(
        is.finite(raw_observed),
        injury_envelope,
        NA_real_
      )
    )]

    if (!"survey" %in% names(source)) {
      source[, survey := as.character(year)]
    }
    if (!"observed_count" %in% names(source)) {
      source[, observed_count := NA_real_]
    }
    if (!"correction_applied" %in% names(source)) {
      source[, correction_applied := FALSE]
    }

    out$source <- source[, .(
      survey_input_count = sum(survey_input_count, na.rm = TRUE),
      legacy_audit_count = sum(legacy_audit_count, na.rm = TRUE),
      raw_observed_count_weighted = sum(
        raw_observed_count_weighted,
        na.rm = TRUE
      ),
      raw_observed_envelope = sum(raw_observed_envelope, na.rm = TRUE),
      observed_count = sum(observed_count, na.rm = TRUE),
      legacy_correction_cells = sum(correction_applied %in% TRUE, na.rm = TRUE),
      survey = paste(sort(unique(as.character(survey))), collapse = "; ")
    ), by = .(year = as.integer(year), broad_group)]

    out$source[, survey_fraction := safe_ratio(
      survey_input_count,
      sum(survey_input_count)
    ), by = year]
    out$source[, legacy_audit_fraction := safe_ratio(
      legacy_audit_count,
      sum(legacy_audit_count)
    ), by = year]
    out$source[, raw_observed_fraction := safe_ratio(
      raw_observed_count_weighted,
      raw_observed_envelope
    )]

    source_detail <- source[, .(
      survey_input_count = sum(survey_input_count, na.rm = TRUE),
      legacy_audit_count = sum(legacy_audit_count, na.rm = TRUE),
      observed_count = sum(observed_count, na.rm = TRUE),
      correction_cells = sum(correction_applied %in% TRUE, na.rm = TRUE),
      survey = paste(sort(unique(as.character(survey))), collapse = "; ")
    ), by = .(year = as.integer(year), nbdcode = as.integer(nbdcode))]
    source_detail[, survey_fraction := safe_ratio(
      survey_input_count,
      sum(survey_input_count)
    ), by = year]
    source_detail[, legacy_audit_fraction := safe_ratio(
      legacy_audit_count,
      sum(legacy_audit_count)
    ), by = year]

    if (nrow(final)) {
      model_detail <- final[
        DeathYear %in% unique(source_detail$year),
        .(model_deaths = sum(Deaths, na.rm = TRUE)),
        by = .(year = as.integer(DeathYear), nbdcode = as.integer(nbdcode))
      ]
      model_detail[, model_fraction := safe_ratio(
        model_deaths,
        sum(model_deaths)
      ), by = year]
      out$detailed_comparison <- merge(
        source_detail,
        model_detail,
        by = c("year", "nbdcode"),
        all.x = TRUE,
        sort = FALSE
      )
      out$detailed_comparison[, `:=`(
        difference = model_fraction - survey_fraction,
        absolute_difference = abs(model_fraction - survey_fraction),
        difference_percentage_points = 100 * (
          model_fraction - survey_fraction
        )
      )]
      data.table::setorder(
        out$detailed_comparison,
        year, -absolute_difference, nbdcode
      )
    }
  }

  if (nrow(out$source) && nrow(out$model)) {
    model_at_survey <- out$model[
      year %in% unique(out$source$year),
      .(year, broad_group, model_fraction = fraction)
    ]
    out$comparison <- merge(
      out$source,
      model_at_survey,
      by = c("year", "broad_group"),
      all.x = TRUE,
      sort = FALSE
    )
    out$comparison[, `:=`(
      difference = model_fraction - survey_fraction,
      absolute_difference = abs(model_fraction - survey_fraction),
      difference_percentage_points = 100 * (
        model_fraction - survey_fraction
      )
    )]
    data.table::setorder(out$comparison, year, broad_group)
  }

  out
}

# Legacy extraction, NBD3-R view construction, assembly, and validation --------

comparison_columns <- function() {
  c(
    "record_type", "domain", "model", "geography_type", "geography_code",
    "geography", "sex_code", "sex", "year", "age_id", "age_label",
    "series_id", "series_label", "hierarchy", "cause_type", "measure", "unit",
    "estimate", "lower", "upper", "series_sort_order", "age_sort_order", "source"
  )
}

cause_rate_columns <- function() {
  c(
    "model", "geography_type", "geography_code", "geography",
    "sex_code", "sex", "year", "age_id", "age_label",
    "series_id", "series_label", "domain", "hierarchy", "cause_type",
    "deaths", "population", "crude_rate", "asr", "yll00", "yll03", "yll015",
    "series_sort_order", "age_sort_order", "source"
  )
}

legacy_object <- function(loaded, name, required = TRUE) {
  if (!exists(name, envir = loaded$environment, inherits = FALSE)) {
    if (required) stop("Legacy RDA is missing object: ", name, call. = FALSE)
    return(NULL)
  }
  data.table::as.data.table(data.table::copy(
    get(name, envir = loaded$environment, inherits = FALSE)
  ))
}

normalise_model_label <- function(x) {
  original <- trimws(as.character(x))
  key <- normalise_text(original)
  aliases <- c(
    nbd2 = "NBD2",
    nbd3 = "NBD3-Stata",
    nbd3stata = "NBD3-Stata",
    nbd3r = "NBD3-R",
    thembisa = "THEMBISA",
    gbd = "GBD2023",
    gbd2023 = "GBD2023"
  )
  mapped <- unname(aliases[key])
  mapped[is.na(mapped)] <- original[is.na(mapped)]
  mapped
}

attach_common_labels <- function(data, geography_column, sex_column, config) {
  # Join on normalised keys rather than punctuation-sensitive display labels.
  # This preserves canonical labels from config while accepting historical
  # spellings such as "KwaZulu Natal" and "KwaZulu-Natal".
  data[, `:=`(
    geography_source_label = trimws(as.character(get(geography_column))),
    sex_source_label = trimws(as.character(get(sex_column)))
  )]
  data[, geography_key := normalise_text(normalise_geography_label(geography_source_label))]
  data[, sex_key := normalise_text(normalise_sex_label(sex_source_label))]

  geographies <- geography_catalog(config)
  geographies[, geography_key := normalise_text(geography)]
  assert_unique_rows(geographies, "geography_key", "geography catalogue")

  sexes <- sex_catalog(config)
  sexes[, sex_key := normalise_text(sex)]
  assert_unique_rows(sexes, "sex_key", "sex catalogue")

  data <- merge(data, geographies, by = "geography_key", all.x = TRUE, sort = FALSE)
  data <- merge(data, sexes, by = "sex_key", all.x = TRUE, sort = FALSE)

  unknown_geographies <- unique(data[is.na(geography_code), geography_source_label])
  unknown_sexes <- unique(data[is.na(sex_code), sex_source_label])
  if (length(unknown_geographies)) {
    stop(
      "Unmapped legacy geography label(s): ",
      paste(sort(unknown_geographies), collapse = ", "),
      call. = FALSE
    )
  }
  if (length(unknown_sexes)) {
    stop(
      "Unmapped legacy sex label(s): ",
      paste(sort(unknown_sexes), collapse = ", "),
      call. = FALSE
    )
  }

  data[, c(
    "geography_key", "sex_key",
    "geography_source_label", "sex_source_label"
  ) := NULL]
  data[]
}

normalise_legacy_comparison <- function(data, domain_name, config) {
  x <- data.table::as.data.table(data.table::copy(data))
  x <- rename_first_match(x, "geography_source", c("Province", "province", "Geography"))
  x <- rename_first_match(x, "sex_source", c("Sex", "sex"))
  x <- rename_first_match(x, "year", c("Year", "DeathYear", "year"))
  x <- rename_first_match(x, "model", c("model", "Model"))
  x <- rename_first_match(x, "age_source", c("Age", "age_name", "age"))
  x <- rename_first_match(x, "legacy_series", c("cause_name", "Cause", "cause"))
  x <- rename_first_match(x, "estimate", c("Deaths", "estimate", "mean"))
  x <- rename_first_match(x, "lower", c("lwr", "lower", "lower95"), required = FALSE)
  x <- rename_first_match(x, "upper", c("uppr", "upper", "upper95"), required = FALSE)
  if (!"lower" %in% names(x)) x[, lower := NA_real_]
  if (!"upper" %in% names(x)) x[, upper := NA_real_]

  x[, `:=`(
    geography_source = as.character(geography_source),
    sex_source = as.character(sex_source),
    age_source = as.character(age_source),
    legacy_series = as.character(legacy_series),
    model = normalise_model_label(model),
    year = as.integer(year),
    estimate = as.numeric(estimate),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  )]

  map_subset <- config$cause_map[include_comparison %in% TRUE & domain == domain_name]
  cause_alias <- alias_lookup(map_subset, "legacy_compare_names")
  x <- merge(x, cause_alias, by.x = "legacy_series", by.y = "alias", all.x = TRUE, sort = FALSE)
  data.table::setnames(x, "value", "series_id")
  unknown_causes <- unique(x[is.na(series_id), legacy_series])
  if (length(unknown_causes)) {
    stop("Unmapped legacy ", domain_name, " series: ", paste(unknown_causes, collapse = ", "), call. = FALSE)
  }

  series_meta <- map_subset[, .(
    series_id,
    series_label = display_name,
    hierarchy,
    cause_type,
    measure,
    unit,
    series_sort_order = sort_order
  )]
  x <- merge(x, series_meta, by = "series_id", all.x = TRUE, sort = FALSE)

  age_alias <- age_alias_lookup(config$age_map[include_comparison %in% TRUE], "legacy_compare_names")
  x <- merge(x, age_alias, by.x = "age_source", by.y = "alias", all.x = TRUE, sort = FALSE)
  unknown_ages <- unique(x[is.na(age_id), age_source])
  if (length(unknown_ages)) {
    stop("Unmapped legacy comparison age(s): ", paste(unknown_ages, collapse = ", "), call. = FALSE)
  }

  x <- attach_common_labels(x, "geography_source", "sex_source", config)

  threshold <- as.numeric(config$labels$build$legacy_percent_auto_threshold %||% 1.5)
  fraction_ids <- unique(x[measure == "fraction", series_id])
  for (series in fraction_ids) {
    values <- x[series_id == series & is.finite(estimate), abs(estimate)]
    if (length(values) && stats::quantile(values, 0.99, na.rm = TRUE, names = FALSE) > threshold) {
      x[series_id == series, `:=`(
        estimate = estimate / 100,
        lower = lower / 100,
        upper = upper / 100
      )]
    }
  }

  x[, `:=`(
    record_type = "comparison",
    domain = domain_name,
    source = "Legacy viz.input.Rda"
  )]
  output_columns <- comparison_columns()
  x <- x[, ..output_columns]
  key <- c("record_type", "domain", "model", "geography_code", "sex_code", "year", "age_id", "series_id", "measure")
  assert_unique_rows(x, key, paste0("legacy ", domain_name, " comparison"))
  data.table::setorder(x, series_sort_order, geography_code, sex_code, age_sort_order, year, model)
  x[]
}

normalise_legacy_rate <- function(data, geography_type_name, config) {
  x <- data.table::as.data.table(data.table::copy(data))
  group_candidates <- if (geography_type_name == "population_group") {
    c("Population", "Popgroup", "PopulationGroup")
  } else {
    c("Province", "province", "Geography")
  }
  x <- rename_first_match(x, "geography_source", group_candidates)
  x <- rename_first_match(x, "sex_source", c("Sex", "sex"))
  x <- rename_first_match(x, "year", c("DeathYear", "Year", "year"))
  x <- rename_first_match(x, "legacy_series", c("cause_name", "Cause", "cause"))
  x <- rename_first_match(x, "cause_type_source", c("cause_type", "CauseType"), required = FALSE)
  x <- rename_first_match(x, "age_source", c("age_name", "Age", "age"))
  x <- rename_first_match(x, "estimate", c("rate", "Rate", "estimate"))
  if (!"cause_type_source" %in% names(x)) x[, cause_type_source := NA_character_]

  x[, `:=`(
    geography_source = as.character(geography_source),
    sex_source = as.character(sex_source),
    legacy_series = as.character(legacy_series),
    cause_type_source = as.character(cause_type_source),
    age_source = as.character(age_source),
    year = as.integer(year),
    estimate = as.numeric(estimate)
  )]

  cause_alias <- alias_lookup(config$cause_map[include_explorer %in% TRUE], "legacy_rate_names")
  x <- merge(x, cause_alias, by.x = "legacy_series", by.y = "alias", all.x = TRUE, sort = FALSE)
  data.table::setnames(x, "value", "series_id")
  x[is.na(series_id), series_id := paste0("legacy_", slugify(legacy_series))]

  series_meta <- config$cause_map[, .(
    series_id,
    series_label = display_name,
    domain,
    hierarchy,
    configured_cause_type = cause_type,
    series_sort_order = sort_order
  )]
  x <- merge(x, series_meta, by = "series_id", all.x = TRUE, sort = FALSE)
  x[is.na(series_label), `:=`(
    series_label = legacy_series,
    domain = "cause_rate",
    hierarchy = "legacy_only",
    configured_cause_type = cause_type_source,
    series_sort_order = 9000L + data.table::frank(legacy_series, ties.method = "dense")
  )]

  age_alias <- age_alias_lookup(config$age_map, "legacy_rate_names")
  x <- merge(x, age_alias, by.x = "age_source", by.y = "alias", all.x = TRUE, sort = FALSE)
  x[is.na(age_id), `:=`(
    age_id = paste0("legacy_", slugify(age_source)),
    age_label = age_source,
    metric_type = data.table::fifelse(grepl("^ASR", age_source), "asr", "standard"),
    age_sort_order = 9000L + data.table::frank(age_source, ties.method = "dense")
  )]

  x <- attach_common_labels(x, "geography_source", "sex_source", config)
  x[, `:=`(
    record_type = "cause_rate",
    model = "NBD3-Stata",
    domain = data.table::fifelse(is.na(domain), "cause_rate", domain),
    cause_type = data.table::fifelse(
      is.na(configured_cause_type) | !nzchar(configured_cause_type),
      cause_type_source,
      configured_cause_type
    ),
    measure = data.table::fifelse(metric_type == "asr", "asr", "crude_rate"),
    unit = "per 100,000",
    lower = NA_real_,
    upper = NA_real_,
    source = "Legacy viz.input.Rda"
  )]
  output_columns <- comparison_columns()
  x <- x[, ..output_columns]

  key <- c("record_type", "model", "geography_type", "geography_code", "sex_code", "year", "age_id", "series_id", "measure")
  if (x[, anyDuplicated(.SD), .SDcols = key]) {
    x <- x[, .(
      geography = geography[[1L]],
      sex = sex[[1L]],
      age_label = age_label[[1L]],
      series_label = series_label[[1L]],
      domain = domain[[1L]],
      hierarchy = hierarchy[[1L]],
      cause_type = cause_type[[1L]],
      unit = unit[[1L]],
      estimate = if (all(is.na(estimate))) NA_real_ else mean(estimate, na.rm = TRUE),
      lower = NA_real_,
      upper = NA_real_,
      series_sort_order = series_sort_order[[1L]],
      age_sort_order = age_sort_order[[1L]],
      source = source[[1L]]
    ), by = key]
    data.table::setcolorder(x, comparison_columns())
  }
  x[]
}

extract_legacy_viz <- function(config = read_viz_config()) {
  loaded <- load_rda_isolated(config$labels$paths$legacy_rda)
  required_objects <- c("compare.hiv", "compare.injury", "provincial_deaths_long", "popgroup_deaths_long")
  missing <- setdiff(required_objects, loaded$objects)
  if (length(missing)) stop("Legacy RDA is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  output <- data.table::rbindlist(list(
    normalise_legacy_comparison(legacy_object(loaded, "compare.hiv"), "hiv", config),
    normalise_legacy_comparison(legacy_object(loaded, "compare.injury"), "injury", config),
    normalise_legacy_rate(legacy_object(loaded, "provincial_deaths_long"), "province", config),
    normalise_legacy_rate(legacy_object(loaded, "popgroup_deaths_long"), "population_group", config)
  ), use.names = TRUE, fill = TRUE)

  path <- derived_path(config, "legacy_comparisons.parquet")
  write_parquet_dt(output, path)
  message("Wrote: ", path)
  invisible(path)
}

standardise_nbd3_database <- function(database, config) {
  x <- data.table::as.data.table(data.table::copy(database))
  required <- c("Grouping", "Sex", "nbdcode", "DeathYear", "Age", "Deaths", "Pop", "YLL00", "YLL03", "YLL015", "ASR")
  assert_columns(x, required, "NBD3 final database")
  x[, `:=`(
    Grouping = as.integer(Grouping),
    Sex = as.integer(Sex),
    nbdcode = as.integer(nbdcode),
    DeathYear = as.integer(DeathYear),
    Age = as.integer(Age),
    Deaths = as.numeric(Deaths),
    Pop = as.numeric(Pop),
    YLL00 = as.numeric(YLL00),
    YLL03 = as.numeric(YLL03),
    YLL015 = as.numeric(YLL015),
    ASR = as.numeric(ASR)
  )]
  x <- x[
    Grouping %in% 1:14 & Sex %in% 1:3 &
      DeathYear >= as.integer(config$labels$build$start_year) &
      DeathYear <= as.integer(config$labels$build$end_year)
  ]
  key <- c("Grouping", "Sex", "nbdcode", "DeathYear", "Age")
  assert_unique_rows(x, key, "NBD3 final database")
  bad <- x[
    !is.finite(Deaths) | Deaths < -1e-10 |
      !is.finite(Pop) | Pop < -1e-10 |
      !is.finite(YLL00) | YLL00 < -1e-10 |
      !is.finite(YLL03) | YLL03 < -1e-10 |
      !is.finite(YLL015) | YLL015 < -1e-10
  ]
  if (nrow(bad)) stop("NBD3 final database contains invalid measures.", call. = FALSE)
  x[]
}

cause_metadata <- function(config, include_column) {
  map <- config$cause_map[get(include_column) %in% TRUE]
  map[, .(
    series_id,
    series_label = display_name,
    domain,
    hierarchy,
    cause_type,
    measure,
    unit,
    series_sort_order = sort_order
  )]
}

age_metadata <- function(config, include_column) {
  map <- config$age_map[get(include_column) %in% TRUE]
  map[, .(
    age_id,
    age_label,
    metric_type,
    age_sort_order = sort_order
  )]
}

add_geography_and_sex_labels <- function(data, config) {
  out <- merge(
    data,
    geography_catalog(config),
    by.x = "Grouping", by.y = "geography_code",
    all.x = TRUE, sort = FALSE
  )
  data.table::setnames(out, "Grouping", "geography_code")
  out <- merge(
    out,
    sex_catalog(config),
    by.x = "Sex", by.y = "sex_code",
    all.x = TRUE, sort = FALSE
  )
  data.table::setnames(out, "Sex", "sex_code")
  if (out[is.na(geography) | is.na(sex), .N]) {
    stop("Failed to attach geography or sex labels to NBD3-R rows.", call. = FALSE)
  }
  out[]
}

build_nbd3_comparisons <- function(database, config) {
  db <- standardise_nbd3_database(database, config)[Grouping %in% 1:10]
  cause_map <- config$cause_map[include_comparison %in% TRUE]
  age_map <- config$age_map[include_comparison %in% TRUE]
  numerator_membership <- expand_code_map(cause_map, "za_codes", "nbdcode")
  age_membership <- expand_age_map(age_map)

  relevant_codes <- unique(numerator_membership$nbdcode)
  relevant_ages <- unique(age_membership$database_age)
  numerator <- merge(
    db[nbdcode %in% relevant_codes & Age %in% relevant_ages],
    numerator_membership,
    by = "nbdcode", allow.cartesian = TRUE, sort = FALSE
  )
  numerator <- merge(
    numerator,
    age_membership,
    by.x = "Age", by.y = "database_age",
    allow.cartesian = TRUE, sort = FALSE
  )
  numerator <- numerator[, .(
    numerator = sum(Deaths, na.rm = TRUE)
  ), by = .(Grouping, Sex, DeathYear, age_id, series_id)]

  denominator_rows <- lapply(seq_len(nrow(cause_map)), function(index) {
    codes <- parse_integer_codes(cause_map$denominator_za_codes[[index]])
    if (!length(codes)) return(NULL)
    data.table::data.table(series_id = cause_map$series_id[[index]], nbdcode = codes)
  })
  denominator_membership <- data.table::rbindlist(denominator_rows, use.names = TRUE, fill = TRUE)
  if (nrow(denominator_membership)) {
    denominator <- merge(
      db[nbdcode %in% unique(denominator_membership$nbdcode) & Age %in% relevant_ages],
      denominator_membership,
      by = "nbdcode", allow.cartesian = TRUE, sort = FALSE
    )
    denominator <- merge(
      denominator,
      age_membership,
      by.x = "Age", by.y = "database_age",
      allow.cartesian = TRUE, sort = FALSE
    )
    denominator <- denominator[, .(
      denominator = sum(Deaths, na.rm = TRUE)
    ), by = .(Grouping, Sex, DeathYear, age_id, series_id)]
  } else {
    denominator <- numerator[0, .(Grouping, Sex, DeathYear, age_id, series_id, denominator = numeric())]
  }

  out <- merge(
    numerator,
    denominator,
    by = c("Grouping", "Sex", "DeathYear", "age_id", "series_id"),
    all.x = TRUE, sort = FALSE
  )
  out <- merge(out, cause_metadata(config, "include_comparison"), by = "series_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, age_metadata(config, "include_comparison"), by = "age_id", all.x = TRUE, sort = FALSE)
  out <- add_geography_and_sex_labels(out, config)
  out[, estimate := data.table::fifelse(
    measure == "fraction",
    safe_ratio(numerator, denominator),
    numerator
  )]
  out[, `:=`(
    record_type = "comparison",
    model = "NBD3-R",
    year = as.integer(DeathYear),
    lower = NA_real_,
    upper = NA_real_,
    source = basename(config$labels$paths$nbd3_database)
  )]
  out[, DeathYear := NULL]
  output_columns <- comparison_columns()
  out <- out[, ..output_columns]
  key <- c("domain", "model", "geography_code", "sex_code", "year", "age_id", "series_id", "measure")
  assert_unique_rows(out, key, "NBD3-R comparisons")
  data.table::setorder(out, domain, series_sort_order, geography_code, sex_code, age_sort_order, year)
  out[]
}

build_nbd3_cause_rates <- function(database, config) {
  db <- standardise_nbd3_database(database, config)
  cause_map <- config$cause_map[include_explorer %in% TRUE]
  age_map <- config$age_map[include_explorer %in% TRUE & metric_type != "legacy_only"]
  cause_membership <- expand_code_map(cause_map, "za_codes", "nbdcode")
  age_membership <- expand_age_map(age_map)

  mapped <- merge(
    db[nbdcode %in% unique(cause_membership$nbdcode) & Age %in% unique(age_membership$database_age)],
    cause_membership,
    by = "nbdcode", allow.cartesian = TRUE, sort = FALSE
  )
  mapped <- merge(
    mapped,
    age_membership,
    by.x = "Age", by.y = "database_age",
    allow.cartesian = TRUE, sort = FALSE
  )
  # Population is stored on every cause row in the final database. For most
  # causes it is identical within geography-sex-year-age, but person estimates
  # for sex-specific causes deliberately use female or male denominators. A
  # generic population table therefore has legitimate duplicate demographic
  # keys. Carry the denominator through the configured cause mapping instead.
  #
  # First collapse across component causes within each source age, retaining a
  # single denominator. Then sum source-age denominators for composite age
  # groups such as <1 year or 20-54 years. This avoids multiplying population by
  # the number of causes in custom cause definitions.
  age_cells <- mapped[, .(
    deaths = sum(Deaths, na.rm = TRUE),
    yll00 = sum(YLL00, na.rm = TRUE),
    yll03 = sum(YLL03, na.rm = TRUE),
    yll015 = sum(YLL015, na.rm = TRUE),
    population_min = min(Pop, na.rm = TRUE),
    population = max(Pop, na.rm = TRUE)
  ), by = .(Grouping, Sex, DeathYear, Age, age_id, series_id)]

  age_cells[, population_scale := pmax(
    1,
    abs(population_min),
    abs(population)
  )]
  inconsistent_population <- age_cells[
    !is.finite(population_min) |
      !is.finite(population) |
      population_min < -1e-10 |
      population < -1e-10 |
      abs(population - population_min) > 1e-10 * population_scale
  ]
  if (nrow(inconsistent_population)) {
    first <- inconsistent_population[1L]
    stop(
      "A configured cause series combines component causes with different ",
      "population denominators in ", nrow(inconsistent_population),
      " source-age cell(s). First series=", first$series_id,
      ", grouping=", first$Grouping,
      ", sex=", first$Sex,
      ", year=", first$DeathYear,
      ", age=", first$Age,
      ". Review config/cause_comparison_map.csv.",
      call. = FALSE
    )
  }

  measures <- age_cells[, .(
    deaths = sum(deaths, na.rm = TRUE),
    yll00 = sum(yll00, na.rm = TRUE),
    yll03 = sum(yll03, na.rm = TRUE),
    yll015 = sum(yll015, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ), by = .(Grouping, Sex, DeathYear, age_id, series_id)]
  rm(mapped, age_cells)
  invisible(gc())

  out <- measures
  out[, crude_rate := safe_ratio(
    deaths,
    population,
    multiplier = as.numeric(config$labels$build$rate_scale)
  )]

  asr_base <- db[Age == 25L & nbdcode %in% unique(cause_membership$nbdcode)]
  asr_mapped <- merge(
    asr_base,
    cause_membership,
    by = "nbdcode", allow.cartesian = TRUE, sort = FALSE
  )
  asr_table <- asr_mapped[, .(
    asr_value = sum_or_na(ASR)
  ), by = .(Grouping, Sex, DeathYear, series_id)]
  out <- merge(
    out,
    asr_table,
    by = c("Grouping", "Sex", "DeathYear", "series_id"),
    all.x = TRUE, sort = FALSE
  )
  out[, asr := data.table::fifelse(age_id == "asr_all", asr_value, NA_real_)]
  out[, asr_value := NULL]

  out <- merge(out, cause_metadata(config, "include_explorer"), by = "series_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, age_metadata(config, "include_explorer"), by = "age_id", all.x = TRUE, sort = FALSE)
  out <- add_geography_and_sex_labels(out, config)
  out[, `:=`(
    model = "NBD3-R",
    year = as.integer(DeathYear),
    source = basename(config$labels$paths$nbd3_database)
  )]
  out[, c("DeathYear", "measure", "unit", "metric_type") := NULL]
  output_columns <- cause_rate_columns()
  out <- out[, ..output_columns]
  key <- c("model", "geography_code", "sex_code", "year", "age_id", "series_id")
  assert_unique_rows(out, key, "NBD3-R cause rates")
  data.table::setorder(out, geography_code, sex_code, series_sort_order, age_sort_order, year)
  out[]
}

build_nbd3_r_views <- function(config = read_viz_config()) {
  database_path <- config$labels$paths$nbd3_database
  if (is.na(database_path) || !file.exists(database_path)) {
    stop(
      "NBD3 database not found. Run the analytical pipeline, set NBD3_DATABASE, or edit report/config/labels.yml.\nResolved path: ",
      database_path, call. = FALSE
    )
  }
  database <- read_parquet_dt(database_path)
  comparisons <- build_nbd3_comparisons(database, config)
  cause_rates <- build_nbd3_cause_rates(database, config)

  comparison_path <- derived_path(config, "nbd3_r_comparisons.parquet")
  rate_path <- derived_path(config, "cause_rates.parquet")
  write_parquet_dt(comparisons, comparison_path)
  write_parquet_dt(cause_rates, rate_path)
  message("Wrote:\n- ", comparison_path, "\n- ", rate_path)
  invisible(c(comparison_path, rate_path))
}

legacy_rates_to_wide <- function(legacy) {
  rates <- legacy[record_type == "cause_rate"]
  if (!nrow(rates)) return(data.table::data.table())
  out <- rates[, .(
    model,
    geography_type,
    geography_code,
    geography,
    sex_code,
    sex,
    year,
    age_id,
    age_label,
    series_id,
    series_label,
    domain,
    hierarchy,
    cause_type,
    deaths = NA_real_,
    population = NA_real_,
    crude_rate = data.table::fifelse(measure == "crude_rate", estimate, NA_real_),
    asr = data.table::fifelse(measure == "asr", estimate, NA_real_),
    yll00 = NA_real_,
    yll03 = NA_real_,
    yll015 = NA_real_,
    series_sort_order,
    age_sort_order,
    source
  )]
  key <- setdiff(cause_rate_columns(), c("crude_rate", "asr", "source"))
  consistency <- out[, .(
    crude_range = if (sum(is.finite(crude_rate)) > 1L) diff(range(crude_rate, na.rm = TRUE)) else 0,
    asr_range = if (sum(is.finite(asr)) > 1L) diff(range(asr, na.rm = TRUE)) else 0
  ), by = key]
  if (consistency[crude_range > 1e-8 | asr_range > 1e-8, .N]) {
    stop("Duplicate legacy national rate views disagree after harmonisation.", call. = FALSE)
  }
  out <- out[, .(
    crude_rate = max_or_na(crude_rate),
    asr = max_or_na(asr),
    source = source[[1L]]
  ), by = key]
  data.table::setcolorder(out, cause_rate_columns())
  out[]
}

rate_long_for_difference <- function(rates) {
  data.table::rbindlist(list(
    rates[is.finite(crude_rate), .(
      model, geography_type, geography_code, geography, sex_code, sex, year,
      age_id, age_label, series_id, series_label, domain, hierarchy, cause_type,
      measure = "crude_rate", unit = "per 100,000", estimate = crude_rate
    )],
    rates[is.finite(asr), .(
      model, geography_type, geography_code, geography, sex_code, sex, year,
      age_id, age_label, series_id, series_label, domain, hierarchy, cause_type,
      measure = "asr", unit = "per 100,000", estimate = asr
    )]
  ), use.names = TRUE, fill = TRUE)
}

add_difference_metrics <- function(data) {
  data[, `:=`(
    coverage = data.table::fcase(
      is.finite(estimate_stata) & is.finite(estimate_r), "matched",
      is.finite(estimate_stata), "stata_only",
      is.finite(estimate_r), "r_only",
      default = "missing"
    ),
    absolute_difference = estimate_r - estimate_stata,
    relative_difference = safe_ratio(estimate_r - estimate_stata, abs(estimate_stata)),
    ratio = safe_ratio(estimate_r, estimate_stata),
    symmetric_percent_difference = safe_ratio(
      2 * (estimate_r - estimate_stata),
      abs(estimate_r) + abs(estimate_stata),
      multiplier = 100
    )
  )]
  data[]
}

build_model_differences <- function(comparisons, cause_rates) {
  id <- c(
    "record_type", "domain", "geography_type", "geography_code", "geography",
    "sex_code", "sex", "year", "age_id", "age_label", "series_id",
    "series_label", "hierarchy", "cause_type", "measure", "unit"
  )

  comparison_stata <- comparisons[model == "NBD3-Stata", c(id, "estimate"), with = FALSE]
  comparison_r <- comparisons[model == "NBD3-R", c(id, "estimate"), with = FALSE]
  data.table::setnames(comparison_stata, "estimate", "estimate_stata")
  data.table::setnames(comparison_r, "estimate", "estimate_r")
  comparison_difference <- merge(comparison_stata, comparison_r, by = id, all = TRUE, sort = FALSE)

  rate_stata <- rate_long_for_difference(cause_rates[model == "NBD3-Stata"])
  rate_r_all <- rate_long_for_difference(cause_rates[model == "NBD3-R"])
  if (nrow(rate_stata)) {
    availability <- unique(rate_stata[, .(geography_type, series_id, age_id, measure)])
    rate_r <- merge(
      rate_r_all,
      availability,
      by = c("geography_type", "series_id", "age_id", "measure"),
      all = FALSE, sort = FALSE
    )
  } else {
    rate_r <- rate_r_all[0]
  }
  rate_stata[, record_type := "cause_rate"]
  rate_r[, record_type := "cause_rate"]
  rate_stata <- rate_stata[, c(id, "estimate"), with = FALSE]
  rate_r <- rate_r[, c(id, "estimate"), with = FALSE]
  data.table::setnames(rate_stata, "estimate", "estimate_stata")
  data.table::setnames(rate_r, "estimate", "estimate_r")
  rate_difference <- merge(rate_stata, rate_r, by = id, all = TRUE, sort = FALSE)

  out <- data.table::rbindlist(list(comparison_difference, rate_difference), use.names = TRUE, fill = TRUE)
  out <- add_difference_metrics(out)
  assert_unique_rows(out, id, "model difference table")
  out[]
}

build_runtime_catalogs <- function(comparisons, cause_rates, config) {
  availability <- unique(cause_rates[, .(
    model, geography_type, geography_code, geography, sex_code, sex,
    age_id, age_label, series_id, series_label, domain, hierarchy, cause_type,
    series_sort_order, age_sort_order
  )])
  data.table::setorder(availability, model, geography_code, sex_code, series_sort_order, age_sort_order)
  list(
    models = ordered_model_names(unique(c(comparisons$model, cause_rates$model)), config),
    geographies = unique(availability[, .(geography_type, geography_code, geography)])[order(geography_type, geography_code)],
    sexes = unique(availability[, .(sex_code, sex)])[order(sex_code)],
    ages = unique(availability[, .(age_id, age_label, age_sort_order)])[order(age_sort_order)],
    causes = unique(availability[, .(domain, series_id, series_label, hierarchy, cause_type, series_sort_order)])[order(domain, series_sort_order)],
    availability = availability,
    comparison_coverage = unique(comparisons[, .(
      domain, model, geography_type, geography_code, geography,
      sex_code, sex, age_id, age_label, series_id, series_label,
      measure, unit, series_sort_order, age_sort_order
    )])
  )
}

first_alias <- function(value, fallback) {
  aliases <- split_semicolon(value)[[1L]]
  if (length(aliases)) aliases[[1L]] else fallback
}

build_compatibility_rda <- function(comparisons, cause_rates, config, path) {
  comparison_names <- config$cause_map[, .(
    series_id,
    compatibility_name = mapply(first_alias, legacy_compare_names, display_name, USE.NAMES = FALSE)
  )]
  comparison_export <- merge(comparisons, comparison_names, by = "series_id", all.x = TRUE, sort = FALSE)

  compare.hiv <- comparison_export[domain == "hiv", .(
    Province = geography, Sex = sex, Year = year, model, Age = age_label,
    cause_name = compatibility_name, Deaths = estimate, lwr = lower, uppr = upper
  )]
  compare.injury <- comparison_export[domain == "injury", .(
    Province = geography, Sex = sex, Year = year, model, Age = age_label,
    cause_name = compatibility_name, Deaths = estimate, lwr = lower, uppr = upper
  )]

  cause_names <- config$cause_map[, .(
    series_id,
    compatibility_cause = mapply(first_alias, legacy_rate_names, display_name, USE.NAMES = FALSE)
  )]
  age_names <- config$age_map[, .(
    age_id,
    compatibility_age = mapply(first_alias, legacy_rate_names, age_label, USE.NAMES = FALSE)
  )]
  rate_export <- merge(cause_rates, cause_names, by = "series_id", all.x = TRUE, sort = FALSE)
  rate_export <- merge(rate_export, age_names, by = "age_id", all.x = TRUE, sort = FALSE)
  rate_export[is.na(compatibility_cause) | !nzchar(compatibility_cause), compatibility_cause := series_label]
  rate_export[is.na(compatibility_age) | !nzchar(compatibility_age), compatibility_age := age_label]

  make_rate_export <- function(data, field) {
    crude <- data[is.finite(crude_rate), .(
      group = geography, Sex = sex, DeathYear = year,
      cause_name = compatibility_cause, cause_type,
      age_name = compatibility_age, rate = crude_rate, model
    )]
    standardised <- data[is.finite(asr), .(
      group = geography, Sex = sex, DeathYear = year,
      cause_name = compatibility_cause, cause_type,
      age_name = compatibility_age, rate = asr, model
    )]
    out <- data.table::rbindlist(list(crude, standardised), use.names = TRUE)
    data.table::setnames(out, "group", field)
    out
  }

  provincial_deaths_long <- make_rate_export(
    rate_export[geography_type %in% c("province", "national")], "Province"
  )
  popgroup_deaths_long <- make_rate_export(
    rate_export[geography_type %in% c("population_group", "national")], "Population"
  )
  save(
    compare.hiv, compare.injury, provincial_deaths_long, popgroup_deaths_long,
    file = path, compress = "xz"
  )
  invisible(path)
}

build_viz_input <- function(config = read_viz_config()) {
  legacy_path <- derived_path(config, "legacy_comparisons.parquet")
  nbd3_path <- derived_path(config, "nbd3_r_comparisons.parquet")
  rates_path <- derived_path(config, "cause_rates.parquet")
  if (!file.exists(legacy_path)) extract_legacy_viz(config)
  if (!file.exists(nbd3_path) || !file.exists(rates_path)) build_nbd3_r_views(config)

  legacy <- read_parquet_dt(legacy_path)
  nbd3 <- read_parquet_dt(nbd3_path)
  comparisons <- data.table::rbindlist(list(
    legacy[record_type == "comparison"],
    nbd3
  ), use.names = TRUE, fill = TRUE)
  key <- c("domain", "model", "geography_code", "sex_code", "year", "age_id", "series_id", "measure")
  assert_unique_rows(comparisons, key, "combined comparison table")

  nbd3_rates <- read_parquet_dt(rates_path)[model == "NBD3-R"]
  legacy_rates <- legacy_rates_to_wide(legacy)
  cause_rates <- data.table::rbindlist(list(nbd3_rates, legacy_rates), use.names = TRUE, fill = TRUE)
  data.table::setcolorder(cause_rates, cause_rate_columns())
  rate_key <- c("model", "geography_code", "sex_code", "year", "age_id", "series_id")
  assert_unique_rows(cause_rates, rate_key, "combined cause-rate table")
  write_parquet_dt(cause_rates, rates_path)

  differences <- build_model_differences(comparisons, cause_rates)
  difference_path <- derived_path(config, "model_differences.parquet")
  write_parquet_dt(differences, difference_path)

  validation_path <- config$labels$paths$nbd3_validation
  pipeline_validation <- if (!is.na(validation_path) && file.exists(validation_path)) {
    data.table::fread(validation_path)
  } else {
    data.table::data.table()
  }

  metadata <- list(
    viztool_version = config$labels$project$version,
    built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    analysis_period = config$labels$project$analysis_period,
    legacy_source = file_metadata(config$labels$paths$legacy_rda),
    nbd3_source = file_metadata(config$labels$paths$nbd3_database),
    pipeline_validation_source = file_metadata(validation_path),
    row_counts = data.table::data.table(
      object = c("comparisons", "cause_rates", "model_differences"),
      rows = c(nrow(comparisons), nrow(cause_rates), nrow(differences))
    )
  )

  runtime <- list(
    comparisons = comparisons,
    differences = differences,
    catalogs = build_runtime_catalogs(comparisons, cause_rates, config),
    pipeline_validation = pipeline_validation,
    metadata = metadata,
    paths = list(
      cause_rates = rates_path,
      legacy_comparisons = legacy_path,
      nbd3_r_comparisons = nbd3_path,
      model_differences = difference_path
    )
  )

  runtime_path <- derived_path(config, "viz_input_new.rds")
  write_rds_atomic(runtime, runtime_path)
  if (isTRUE(as_flag(config$labels$build$write_compatibility_rda, TRUE))) {
    build_compatibility_rda(
      comparisons, cause_rates, config,
      derived_path(config, "viz.input.new.Rda")
    )
  }
  message("Wrote:\n- ", runtime_path, "\n- ", difference_path, "\n- ", rates_path)
  invisible(runtime_path)
}

validation_result <- function(check, status, detail = "") {
  data.table::data.table(check = check, status = status, detail = detail)
}

validate_viz_input <- function(config = read_viz_config(), stop_on_failure = TRUE) {
  required <- c(
    "legacy_comparisons.parquet", "nbd3_r_comparisons.parquet",
    "cause_rates.parquet", "model_differences.parquet", "viz_input_new.rds"
  )
  if (isTRUE(as_flag(config$labels$build$write_compatibility_rda, TRUE))) {
    required <- c(required, "viz.input.new.Rda")
  }
  results <- list()
  add <- function(check, expression) {
    result <- tryCatch({
      detail <- force(expression)
      validation_result(check, "PASS", paste(detail %||% "", collapse = "; "))
    }, error = function(error) validation_result(check, "FAIL", conditionMessage(error)))
    results[[length(results) + 1L]] <<- result
    message("[", result$status, "] ", check, if (nzchar(result$detail)) paste0(": ", result$detail) else "")
  }

  paths <- vapply(required, function(filename) derived_path(config, filename), character(1))
  missing <- required[!file.exists(paths)]
  add("required derived files", {
    if (length(missing)) stop("Missing: ", paste(missing, collapse = ", "))
    paste(length(required), "files present")
  })

  # Do not cascade into Arrow file-not-found errors when a user runs the
  # validator before the build has created its prerequisites.
  if (length(missing)) {
    report <- data.table::rbindlist(results, use.names = TRUE)
    report_path <- derived_path(config, "viz_validation.csv")
    data.table::fwrite(report, report_path)
    if (stop_on_failure) {
      stop(
        "VizTool validation cannot continue because required derived files are missing. ",
        "Run Rscript run_nbd.R with RUN_REPORT <- TRUE. Review ", report_path, ".",
        call. = FALSE
      )
    }
    return(invisible(report))
  }

  legacy <- read_parquet_dt(derived_path(config, "legacy_comparisons.parquet"))
  nbd3 <- read_parquet_dt(derived_path(config, "nbd3_r_comparisons.parquet"))
  rates <- read_parquet_dt(derived_path(config, "cause_rates.parquet"))
  differences <- read_parquet_dt(derived_path(config, "model_differences.parquet"))
  runtime <- readRDS(derived_path(config, "viz_input_new.rds"))

  add("explicit analytical version labels", {
    models <- unique(c(legacy$model, nbd3$model, rates$model))
    if ("NBD3" %in% models) stop("Unqualified NBD3 model label remains")
    if (!all(c("NBD3-Stata", "NBD3-R") %in% models)) stop("NBD3-Stata or NBD3-R is missing")
    paste(ordered_model_names(models, config), collapse = ", ")
  })

  add("comparison key uniqueness", {
    combined <- data.table::rbindlist(list(legacy[record_type == "comparison"], nbd3), use.names = TRUE)
    key <- c("domain", "model", "geography_code", "sex_code", "year", "age_id", "series_id", "measure")
    assert_unique_rows(combined, key, "combined comparisons")
    paste(format(nrow(combined), big.mark = ","), "rows")
  })

  add("fraction bounds", {
    fractions <- data.table::rbindlist(list(
      legacy[record_type == "comparison" & measure == "fraction"],
      nbd3[measure == "fraction"]
    ), use.names = TRUE, fill = TRUE)
    bad <- fractions[is.finite(estimate) & (estimate < -1e-10 | estimate > 1 + 1e-10)]
    if (nrow(bad)) stop(nrow(bad), " fraction rows fall outside 0-1")
    paste(format(nrow(fractions), big.mark = ","), "fraction rows")
  })

  add("NBD3-R comparison fraction arithmetic", {
    key <- c("geography_code", "sex_code", "year", "age_id")
    all_cause <- nbd3[series_id == "hiv_all_causes_deaths", c(key, "estimate"), with = FALSE]
    data.table::setnames(all_cause, "estimate", "all_cause")
    pairs <- data.table::data.table(
      fraction_id = c(
        "hiv_fraction", "injuries_fraction", "transport_fraction",
        "unintentional_fraction", "interpersonal_fraction", "self_harm_fraction"
      ),
      numerator_id = c(
        "hiv_deaths", "injuries_deaths", "transport_deaths",
        "unintentional_deaths", "interpersonal_deaths", "self_harm_deaths"
      )
    )
    errors <- numeric()
    checked <- 0L
    for (index in seq_len(nrow(pairs))) {
      numerator <- nbd3[
        series_id == pairs$numerator_id[[index]],
        c(key, "estimate"),
        with = FALSE
      ]
      fraction <- nbd3[
        series_id == pairs$fraction_id[[index]],
        c(key, "estimate"),
        with = FALSE
      ]
      data.table::setnames(numerator, "estimate", "numerator")
      data.table::setnames(fraction, "estimate", "fraction")
      check <- merge(numerator, all_cause, by = key, all = TRUE, sort = FALSE)
      check <- merge(check, fraction, by = key, all = TRUE, sort = FALSE)
      if (check[is.na(numerator) | is.na(all_cause) | is.na(fraction), .N]) {
        stop("A configured comparison fraction is missing a numerator or denominator row")
      }
      expected <- safe_ratio(check$numerator, check$all_cause)
      errors <- c(errors, abs(check$fraction - expected))
      checked <- checked + nrow(check)
    }
    if (any(errors > 1e-10, na.rm = TRUE)) {
      stop("A configured NBD3-R comparison fraction is arithmetically inconsistent")
    }
    paste(format(checked, big.mark = ","), "fraction rows")
  })

  add("NBD3-R comparison age aggregation", {
    deaths <- nbd3[measure == "deaths"]
    key <- c("domain", "geography_code", "sex_code", "year", "series_id")
    wide <- data.table::dcast(
      deaths,
      stats::as.formula(paste(paste(key, collapse = " + "), "~ age_id")),
      value.var = "estimate"
    )
    needed <- c(
      "age_under1", "age_1_4", "age_under5", "age_5_9",
      "age_10_19", "age_20_54", "age_55_plus", "age_all"
    )
    assert_columns(wide, needed, "NBD3-R comparison age table")
    if (wide[, anyNA(.SD), .SDcols = needed]) {
      stop("NBD3-R comparison age table has missing component estimates")
    }
    under5_error <- abs(wide$age_under5 - wide$age_under1 - wide$age_1_4)
    all_age_error <- abs(
      wide$age_all - wide$age_under1 - wide$age_1_4 - wide$age_5_9 -
        wide$age_10_19 - wide$age_20_54 - wide$age_55_plus
    )
    scale <- pmax(1, abs(wide$age_all), abs(wide$age_under5))
    if (any(under5_error > 1e-8 * scale | all_age_error > 1e-8 * scale, na.rm = TRUE)) {
      stop("NBD3-R comparison age groups do not reproduce their component deaths")
    }
    paste(format(nrow(wide), big.mark = ","), "series-year strata")
  })

  add("legacy uncertainty interval ordering", {
    intervals <- legacy[is.finite(lower) & is.finite(upper) & is.finite(estimate)]
    bad <- intervals[lower > estimate + 1e-10 | upper < estimate - 1e-10 | lower > upper]
    if (nrow(bad)) stop(nrow(bad), " interval rows are invalid")
    paste(format(nrow(intervals), big.mark = ","), "interval rows")
  })

  add("NBD3-R comparison year and age coverage", {
    expected_years <- seq.int(
      as.integer(config$labels$build$start_year),
      as.integer(config$labels$build$end_year)
    )
    if (!identical(sort(unique(nbd3$year)), expected_years)) stop("NBD3-R years are incomplete")
    expected_ages <- sort(config$age_map[include_comparison %in% TRUE, age_id])
    if (!setequal(unique(nbd3$age_id), expected_ages)) stop("NBD3-R comparison ages are incomplete")
    paste(range(expected_years), collapse = "-")
  })

  add("NBD3-R national comparison deaths equal provinces", {
    deaths <- nbd3[measure == "deaths"]
    expected <- deaths[geography_type == "province", .(
      expected = sum(estimate, na.rm = TRUE)
    ), by = .(domain, sex_code, year, age_id, series_id, measure)]
    observed <- deaths[geography_type == "national", .(
      observed = sum(estimate, na.rm = TRUE)
    ), by = .(domain, sex_code, year, age_id, series_id, measure)]
    check <- merge(expected, observed, by = c("domain", "sex_code", "year", "age_id", "series_id", "measure"), all = TRUE)
    check[is.na(expected), expected := 0]
    check[is.na(observed), observed := 0]
    difference <- abs(check$expected - check$observed)
    scale <- pmax(1, abs(check$expected), abs(check$observed))
    if (any(difference > 1e-8 * scale)) stop("National comparison deaths differ from province sums")
    paste(format(nrow(check), big.mark = ","), "keys")
  })

  add("NBD3-R comparison persons equal male plus female", {
    deaths <- nbd3[measure == "deaths"]
    expected <- deaths[sex_code %in% 1:2, .(
      expected = sum(estimate, na.rm = TRUE)
    ), by = .(domain, geography_code, year, age_id, series_id, measure)]
    observed <- deaths[sex_code == 3L, .(
      observed = sum(estimate, na.rm = TRUE)
    ), by = .(domain, geography_code, year, age_id, series_id, measure)]
    check <- merge(expected, observed, by = c("domain", "geography_code", "year", "age_id", "series_id", "measure"), all = TRUE)
    check[is.na(expected), expected := 0]
    check[is.na(observed), observed := 0]
    difference <- abs(check$expected - check$observed)
    scale <- pmax(1, abs(check$expected), abs(check$observed))
    if (any(difference > 1e-8 * scale)) stop("Person comparison deaths differ from male plus female")
    paste(format(nrow(check), big.mark = ","), "keys")
  })

  add("cause-rate key and arithmetic", {
    key <- c("model", "geography_code", "sex_code", "year", "age_id", "series_id")
    assert_unique_rows(rates, key, "cause-rate table")
    r <- rates[model == "NBD3-R" & is.finite(population) & population > 0]
    expected <- r$deaths / r$population * as.numeric(config$labels$build$rate_scale)
    difference <- abs(r$crude_rate - expected)
    scale <- pmax(1, abs(expected), abs(r$crude_rate))
    if (any(difference > 1e-9 * scale, na.rm = TRUE)) stop("NBD3-R crude-rate arithmetic is inconsistent")
    paste(format(nrow(rates), big.mark = ","), "rows")
  })

  add("ASR is limited to the ASR age view", {
    bad <- rates[model == "NBD3-R" & age_id != "asr_all" & is.finite(asr)]
    if (nrow(bad)) stop(nrow(bad), " NBD3-R rows contain ASR outside asr_all")
    paste(rates[model == "NBD3-R" & age_id == "asr_all" & is.finite(asr), .N], "ASR rows")
  })

  add("difference arithmetic", {
    matched <- differences[coverage == "matched"]
    error <- abs(matched$absolute_difference - (matched$estimate_r - matched$estimate_stata))
    if (any(error > 1e-10 * pmax(1, abs(matched$absolute_difference)), na.rm = TRUE)) {
      stop("Stored differences are inconsistent")
    }
    paste(format(nrow(matched), big.mark = ","), "matched rows")
  })

  add("runtime object structure", {
    expected <- c("comparisons", "differences", "catalogs", "pipeline_validation", "metadata", "paths")
    missing <- setdiff(expected, names(runtime))
    if (length(missing)) stop("Runtime RDS is missing: ", paste(missing, collapse = ", "))
    paste(length(expected), "components")
  })

  add("final NBD3-R cause uncertainty", {
    uncertainty_path <- derived_path(config, "nbd3_cause_uncertainty.parquet")
    integration_expected <- !is.null(runtime$metadata$uncertainty) ||
      !is.null(runtime$paths$cause_uncertainty)
    if (!file.exists(uncertainty_path)) {
      if (isTRUE(integration_expected)) {
        stop("Runtime metadata references final-cause uncertainty, but the Parquet file is missing")
      }
      "not attached in this report-input build"
    } else {
      uncertainty <- read_parquet_dt(uncertainty_path)
      required_columns <- c(
        "model", "geography_type", "geography_code", "sex_code", "year",
        "age_id", "series_id", "measure", "estimate", "point",
        "lower", "upper", "n_draws"
      )
      assert_columns(uncertainty, required_columns, "final-cause uncertainty")
      key <- c(
        "model", "geography_type", "geography_code", "sex_code", "year",
        "age_id", "series_id", "measure"
      )
      assert_unique_rows(uncertainty, key, "final-cause uncertainty")
      if (uncertainty[model != "NBD3-R", .N]) {
        stop("Final-cause uncertainty contains a model other than NBD3-R")
      }
      if (uncertainty[
        !measure %in% c("deaths", "crude_rate") |
          !is.finite(estimate) | !is.finite(point) |
          !is.finite(lower) | !is.finite(upper) | lower > upper |
          !is.finite(n_draws) | n_draws < 2,
        .N
      ]) {
        stop("Final-cause uncertainty contains invalid estimates or intervals")
      }
      scale <- pmax(1, abs(uncertainty$estimate), abs(uncertainty$point))
      if (any(abs(uncertainty$estimate - uncertainty$point) >
              pmax(1e-6, 2e-7 * scale), na.rm = TRUE)) {
        stop("Final-cause uncertainty points do not align with NBD3-R")
      }
      paste(
        format(nrow(uncertainty), big.mark = ","), "rows across",
        data.table::uniqueN(uncertainty$series_id), "exactly supported cause series"
      )
    }
  })

  if (isTRUE(as_flag(config$labels$build$write_compatibility_rda, TRUE))) {
    add("compatibility RDA structure", {
      compatibility <- load_rda_isolated(derived_path(config, "viz.input.new.Rda"))
      expected <- c(
        "compare.hiv", "compare.injury",
        "provincial_deaths_long", "popgroup_deaths_long"
      )
      missing <- setdiff(expected, compatibility$objects)
      if (length(missing)) {
        stop("Compatibility RDA is missing: ", paste(missing, collapse = ", "))
      }
      paste(length(expected), "objects")
    })
  }

  report <- data.table::rbindlist(results, use.names = TRUE)
  report_path <- derived_path(config, "viz_validation.csv")
  data.table::fwrite(report, report_path)
  failures <- report[status == "FAIL"]
  if (nrow(failures) && stop_on_failure) {
    stop("VizTool validation failed. Review ", report_path, call. = FALSE)
  }
  message("Validation report: ", report_path)
  invisible(report)
}

build_all_viz_inputs <- function(config = read_viz_config(), validate = TRUE) {
  extract_legacy_viz(config)
  build_nbd3_r_views(config)
  build_viz_input(config)
  if (validate) validate_viz_input(config, stop_on_failure = TRUE)
  invisible(derived_path(config, "viz_input_new.rds"))
}
