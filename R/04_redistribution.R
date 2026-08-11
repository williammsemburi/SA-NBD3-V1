# ==============================================================================
# 04_redistribution: Ill-defined and garbage-code redistribution
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# Core redist() helper
# ------------------------------------------------------------------------------

# Canonical row-wise redistribution --------------------------------------------
#
# This module implements the operation performed by the legacy Stata utility
# redist.ado (W. Msemburi). The public redist() wrapper retains the command's
# consequential interface: without an if-condition, source columns are removed;
# with a condition, they remain and are set to zero only for matching rows.
#
# The internal redistribute_columns() engine is the single wide-column
# implementation used by the NBD3 pipeline. It adds safeguards that the Stata
# utility did not provide: disjoint source and target sets, finite non-negative
# counts, explicit missing-value rules, condition-length checks, double-precision
# target storage, opt-in mutation, and row-level conservation checks.

redist_column_names <- function(columns, argument) {
  valid <- is.character(columns) &&
    length(columns) > 0L &&
    !anyNA(columns) &&
    all(nzchar(columns))
  if (!valid) {
    stop(
      "`", argument, "` must be a non-empty character vector of column names.",
      call. = FALSE
    )
  }
  if (anyDuplicated(columns)) {
    duplicated_names <- unique(columns[duplicated(columns)])
    stop(
      "`", argument, "` contains duplicate column name(s): ",
      paste(duplicated_names, collapse = ", "),
      call. = FALSE
    )
  }
  columns
}

redist_copy_input <- function(data, copy) {
  if (!is.data.frame(data)) {
    stop("`data` must inherit from data.frame.", call. = FALSE)
  }
  if (!is.logical(copy) || length(copy) != 1L || is.na(copy)) {
    stop("`copy` must be TRUE or FALSE.", call. = FALSE)
  }

  is_data_table <- inherits(data, "data.table")
  if (!copy && !is_data_table) {
    stop(
      "`copy = FALSE` is supported only for data.table input, where mutation ",
      "by reference is explicit. Reassign the returned data.frame or tibble.",
      call. = FALSE
    )
  }

  if (is_data_table) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for data.table input.", call. = FALSE)
    }
    if (copy) data.table::copy(data) else data
  } else {
    # Assignments below trigger normal copy-on-modify semantics for data.frames
    # and tibbles, leaving the caller's object unchanged.
    data
  }
}

redist_set_column <- function(data, column, value, rows = NULL) {
  if (inherits(data, "data.table")) {
    data.table::set(data, i = rows, j = column, value = value)
    return(data)
  }

  if (is.null(rows)) {
    data[[column]] <- value
  } else {
    current <- data[[column]]
    current[rows] <- value
    data[[column]] <- current
  }
  data
}

redist_drop_columns <- function(data, columns) {
  if (!length(columns)) return(data)
  if (inherits(data, "data.table")) {
    for (column in columns) {
      data.table::set(data, j = column, value = NULL)
    }
  } else {
    data[columns] <- NULL
  }
  data
}

redist_condition_rows <- function(condition, n_rows, na_action, supplied = TRUE) {
  na_action <- match.arg(na_action, c("error", "exclude", "include"))

  if (!supplied || is.null(condition)) {
    return(seq_len(n_rows))
  }
  if (!is.logical(condition) || !is.null(dim(condition))) {
    stop("`condition` must evaluate to a logical vector.", call. = FALSE)
  }
  if (length(condition) == 1L) {
    condition <- rep(condition, n_rows)
  } else if (length(condition) != n_rows) {
    stop(
      "`condition` must have length 1 or nrow(data) (", n_rows,
      "); received length ", length(condition), ".",
      call. = FALSE
    )
  }

  if (anyNA(condition)) {
    n_missing <- sum(is.na(condition))
    if (na_action == "error") {
      stop(
        "`condition` is missing for ", n_missing, " row(s). Make the rule ",
        "explicit, or set `condition_na` to 'exclude' or 'include'.",
        call. = FALSE
      )
    }
    condition[is.na(condition)] <- identical(na_action, "include")
  }
  which(condition)
}

redist_numeric_matrix <- function(data, rows, columns, missing_counts, tolerance) {
  non_numeric <- columns[
    !vapply(columns, function(column) is.numeric(data[[column]]), logical(1))
  ]
  if (length(non_numeric)) {
    stop(
      "Redistribution columns must be numeric. Non-numeric column(s): ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  if (inherits(data, "data.table")) {
    values <- as.matrix(data[rows, columns, with = FALSE])
  } else {
    values <- as.matrix(data[rows, columns, drop = FALSE])
  }
  storage.mode(values) <- "double"

  if (missing_counts == "error" && anyNA(values)) {
    stop(
      "Redistribution inputs contain missing count values. Replace them ",
      "explicitly or use `missing_counts = 'zero'`.",
      call. = FALSE
    )
  }
  if (missing_counts == "zero") values[is.na(values)] <- 0

  if (any(!is.finite(values))) {
    stop("Redistribution inputs contain non-finite count values.", call. = FALSE)
  }
  if (any(values < -tolerance)) {
    stop("Redistribution inputs contain negative count values.", call. = FALSE)
  }

  # Remove harmless floating-point residues such as -1e-15 while rejecting
  # substantive negative counts above.
  values[values < 0] <- 0
  values
}

#' Redistribute source columns into target columns
#'
#' Safe, vectorised engine for the row-wise operation implemented by the legacy
#' Stata `redist.ado` utility. For each selected row, source columns are summed
#' and added to target columns in proportion to the targets' current values. If
#' the target total is zero, the source total is split equally among targets.
#'
#' @param data A data.frame, tibble, or data.table.
#' @param source_vars Character vector of source column names.
#' @param target_vars Character vector of target column names.
#' @param condition Optional logical vector of length 1 or `nrow(data)`. `NULL`
#'   selects all rows.
#' @param source_action What to do with selected source values after
#'   redistribution: `"zero"` or `"drop"`. Dropping requires all rows to be
#'   selected.
#' @param copy Return an independent object. Set to `FALSE` only for data.table
#'   input when deliberate by-reference mutation is required.
#' @param missing_counts Either `"zero"` (the NBD3/Stata-call-site convention)
#'   or `"error"`.
#' @param condition_na How missing values in `condition` are handled.
#' @param tolerance Relative tolerance used for negative-residue and row-total
#'   conservation checks.
#' @param quiet Suppress the redistribution summary.
#' @param context Label used in messages and errors.
#'
#' @return Modified data in the same class as the input.
redistribute_columns <- function(
    data,
    source_vars,
    target_vars,
    condition = NULL,
    source_action = c("zero", "drop"),
    copy = TRUE,
    missing_counts = c("zero", "error"),
    condition_na = c("error", "exclude", "include"),
    tolerance = 1e-8,
    quiet = TRUE,
    context = "redistribution") {
  source_action <- match.arg(source_action)
  missing_counts <- match.arg(missing_counts)
  condition_na <- match.arg(condition_na)

  valid_tolerance <- is.numeric(tolerance) &&
    length(tolerance) == 1L &&
    is.finite(tolerance) &&
    tolerance >= 0
  if (!valid_tolerance) {
    stop("`tolerance` must be one finite non-negative number.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
  }

  source_vars <- redist_column_names(source_vars, "source_vars")
  target_vars <- redist_column_names(target_vars, "target_vars")
  x <- redist_copy_input(data, copy = copy)

  missing_columns <- setdiff(c(source_vars, target_vars), names(x))
  if (length(missing_columns)) {
    stop(
      "Redistribution column(s) not found in `data`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (identical(source_vars, target_vars)) {
    if (!quiet) {
      message(context, ": source and target lists are identical; no change made")
    }
    return(x)
  }

  overlap <- intersect(source_vars, target_vars)
  if (length(overlap)) {
    stop(
      "Source and target columns must be disjoint. Overlap: ",
      paste(overlap, collapse = ", "),
      call. = FALSE
    )
  }

  condition_supplied <- !is.null(condition)
  rows <- redist_condition_rows(
    condition,
    n_rows = nrow(x),
    na_action = condition_na,
    supplied = condition_supplied
  )

  if (!length(rows)) {
    # An unqualified redistribution on a zero-row data set still consumes the
    # source columns, matching the structural effect of redist.ado.
    if (!condition_supplied && source_action == "drop") {
      x <- redist_drop_columns(x, source_vars)
    }
    if (!quiet) message(context, ": no rows selected; no values changed")
    return(x)
  }

  if (source_action == "drop" && length(rows) != nrow(x)) {
    stop(
      "`source_action = 'drop'` requires all rows to be selected. Use 'zero' ",
      "for a conditional redistribution.",
      call. = FALSE
    )
  }

  source_matrix <- redist_numeric_matrix(
    x, rows, source_vars, missing_counts = missing_counts, tolerance = tolerance
  )
  target_matrix <- redist_numeric_matrix(
    x, rows, target_vars, missing_counts = missing_counts, tolerance = tolerance
  )

  source_total <- rowSums(source_matrix)
  target_total <- rowSums(target_matrix)
  if (any(!is.finite(source_total)) || any(!is.finite(target_total))) {
    stop("Redistribution row totals overflowed numeric range.", call. = FALSE)
  }
  zero_target <- target_total == 0
  positive_target <- target_total > 0

  target_scale <- rep(1, length(rows))
  target_scale[positive_target] <-
    1 + source_total[positive_target] / target_total[positive_target]

  equal_addition <- numeric(length(rows))
  equal_addition[zero_target] <- source_total[zero_target] / length(target_vars)

  # R stores matrices column-major. A vector of nrow(matrix) values is recycled
  # once per column, yielding the intended row-specific scale/addition. Because
  # counts are non-negative, a zero target total implies every target is zero.
  new_target_matrix <- target_matrix * target_scale + equal_addition
  if (any(!is.finite(new_target_matrix))) {
    stop("Redistribution produced non-finite target values.", call. = FALSE)
  }

  before <- source_total + target_total
  after <- rowSums(new_target_matrix)
  scale <- pmax(1, abs(before), abs(after))
  bad <- which(abs(before - after) > tolerance * scale)
  if (length(bad)) {
    preview <- paste(utils::head(rows[bad], 10L), collapse = ", ")
    stop(
      context, " failed to preserve source-plus-target totals in ", length(bad),
      " row(s). First affected row index/indices: ", preview,
      call. = FALSE
    )
  }

  # Promote target columns only after all validation and calculation succeeds.
  # This prevents integer storage from truncating fractional allocations and
  # avoids mutating a copy = FALSE data.table before a validation error.
  for (column in target_vars) {
    if (!is.double(x[[column]])) {
      x <- redist_set_column(x, column, as.numeric(x[[column]]))
    }
  }
  for (j in seq_along(target_vars)) {
    x <- redist_set_column(
      x, target_vars[[j]], new_target_matrix[, j], rows = rows
    )
  }

  if (source_action == "zero") {
    for (column in source_vars) {
      x <- redist_set_column(x, column, 0, rows = rows)
    }
  } else {
    x <- redist_drop_columns(x, source_vars)
  }

  if (!quiet) {
    message(
      context, ": ", length(source_vars), " source column(s) -> ",
      length(target_vars), " target column(s); ", length(rows), " row(s), ",
      sum(positive_target), " proportional and ", sum(zero_target),
      " equal-split; total source mass ", signif(sum(source_total), 12)
    )
  }
  x
}

#' Proportionally redistribute source variable(s) into target variable(s)
#'
#' User-facing R counterpart to `redist.ado`. With no `condition`, source
#' columns are removed after redistribution. With a `condition`, source columns
#' remain and are set to zero only in matching rows.
#'
#' @param data A data.frame, tibble, or data.table.
#' @param source_vars Character vector of source column names.
#' @param target_vars Character vector of target column names.
#' @param condition Optional unquoted logical expression evaluated in `data`.
#' @param copy Return a copy. Set to `FALSE` only for deliberate data.table
#'   by-reference mutation.
#' @param missing_counts Treat missing counts as zero or stop.
#' @param condition_na Stop, exclude, or include rows where the condition is NA.
#' @param tolerance Relative conservation tolerance.
#' @param quiet Suppress the summary message.
#'
#' @details Let `S` be the row sum of source columns and `T` the row sum of
#' target columns. For every selected row, each target `j` becomes
#' `target_j + target_j * S / T` when `T > 0`; when `T == 0`, each target
#' becomes `target_j + S / length(target_vars)`. This is the executable rule in
#' `redist.ado`. The R implementation rejects negative/non-finite counts and
#' overlapping source/target sets rather than allowing silent loss of mass.
#'
#' @return Modified data in the same class as `data`.
redist <- function(
    data,
    source_vars,
    target_vars,
    condition,
    copy = TRUE,
    missing_counts = c("zero", "error"),
    condition_na = c("error", "exclude", "include"),
    tolerance = 1e-8,
    quiet = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must inherit from data.frame.", call. = FALSE)
  }
  has_condition <- !missing(condition)

  if (has_condition) {
    condition_call <- substitute(condition)
    condition_value <- eval(condition_call, envir = data, enclos = parent.frame())
    if (is.null(condition_value)) {
      stop("A supplied `condition` must not evaluate to NULL.", call. = FALSE)
    }
    condition_label <- paste(
      deparse(condition_call, width.cutoff = 500L),
      collapse = " "
    )
    context <- paste0("redist where ", condition_label)
  } else {
    condition_value <- NULL
    context <- "redist"
  }

  redistribute_columns(
    data = data,
    source_vars = source_vars,
    target_vars = target_vars,
    condition = condition_value,
    source_action = if (has_condition) "zero" else "drop",
    copy = copy,
    missing_counts = match.arg(missing_counts),
    condition_na = match.arg(condition_na),
    tolerance = tolerance,
    quiet = quiet,
    context = context
  )
}

# ------------------------------------------------------------------------------
# Generic redistribution utilities
# ------------------------------------------------------------------------------

# Proportional redistribution --------------------------------------------------
#
# Stata's supplied `redist.ado` command is used throughout the legacy pipeline.
# The functions below apply its executable operation in appropriate data shapes:
# source counts are allocated in proportion to existing target counts; where a
# stratum has no target counts, the source is divided equally among targets.

redist_validate_long_counts <- function(data, value, tolerance, context) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance < 0) {
    stop("`tolerance` must be one finite non-negative number.", call. = FALSE)
  }
  if (!is.numeric(data[[value]])) {
    stop(context, " count column `", value, "` must be numeric.", call. = FALSE)
  }

  counts <- as.numeric(data[[value]])
  counts[is.na(counts)] <- 0
  if (any(!is.finite(counts))) {
    stop(context, " contains non-finite count values.", call. = FALSE)
  }
  if (any(counts < -tolerance)) {
    stop(context, " contains negative count values.", call. = FALSE)
  }
  counts[counts < 0] <- 0
  data.table::set(data, j = value, value = counts)
  data
}

redist_assert_group_totals <- function(
    before_data,
    after_data,
    by,
    value,
    tolerance,
    label) {
  if (!length(by)) {
    assert_total_preserved(
      sum(before_data[[value]]),
      sum(after_data[[value]]),
      tolerance,
      label
    )
    return(invisible(TRUE))
  }

  before <- before_data[, .(before = sum(get(value))), by = by]
  after <- after_data[, .(after = sum(get(value))), by = by]
  check <- merge(before, after, by = by, all = TRUE, sort = FALSE)
  check[is.na(before), before := 0]
  check[is.na(after), after := 0]
  check[, scale := pmax(1, abs(before), abs(after))]
  bad <- check[abs(before - after) > tolerance * scale]
  if (nrow(bad)) {
    preview <- utils::capture.output(print(utils::head(bad, 10L)))
    stop(
      label, " failed to preserve totals in ", nrow(bad), " group(s).\n",
      paste(preview, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

redistribute_unknown <- function(
    data,
    category,
    value,
    by,
    unknown,
    targets,
    tolerance = 1e-8) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c(by, category, value))
  x <- redist_validate_long_counts(
    x, value, tolerance, paste0("redistribution of ", category)
  )
  if (!length(targets) || anyDuplicated(targets)) {
    stop("Redistribution targets must be a non-empty unique vector.", call. = FALSE)
  }
  if (length(intersect(unknown, targets))) {
    stop("Unknown and target categories must be disjoint.", call. = FALSE)
  }
  unexpected <- setdiff(unique(x[[category]][!is.na(x[[category]])]), c(unknown, targets))
  if (length(unexpected)) {
    stop(
      "Unexpected ", category, " code(s) would be dropped by redistribution: ",
      paste(sort(unexpected), collapse = ", "),
      call. = FALSE
    )
  }

  before_data <- data.table::copy(x)
  x <- x[, .(value_internal = sum(get(value))), by = c(by, category)]

  unknown_dt <- x[get(category) %in% unknown,
    .(unknown_value = sum(value_internal, na.rm = TRUE)),
    by = by
  ]

  known <- x[get(category) %in% targets]
  groups <- unique(x[, ..by])
  target_table <- data.table::data.table(target_internal = targets)
  grid <- cross_join_dt(groups, target_table)
  data.table::setnames(grid, "target_internal", category)

  known <- merge(grid, known, by = c(by, category), all.x = TRUE, sort = FALSE)
  known[is.na(value_internal), value_internal := 0]
  known <- merge(known, unknown_dt, by = by, all.x = TRUE, sort = FALSE)
  known[is.na(unknown_value), unknown_value := 0]

  known[, target_total := sum(value_internal), by = by]
  n_targets <- length(targets)
  known[, share := data.table::fifelse(
    target_total > 0,
    value_internal / target_total,
    1 / n_targets
  )]
  known[, (value) := value_internal + unknown_value * share]

  out <- known[, c(by, category, value), with = FALSE]
  data.table::setorderv(out, c(by, category))
  redist_assert_group_totals(
    before_data,
    out,
    by = by,
    value = value,
    tolerance = tolerance,
    label = paste0("redistribution of ", category)
  )
  out
}

# Population-group redistribution differs for 1997-1998: the legacy code uses
# the combined 1999-2000 distribution as the reference. Later years use their
# own observed distribution. Unknown rows are removed after allocation.
redistribute_population_group <- function(
    data,
    value = "num",
    year = "DeathYear",
    category = "Popgroup",
    by = c("Death_Prov", "Sex", "nbdcode", "age5"),
    unknown = 8L,
    targets = 1:4,
    reference_years = 1999:2000,
    early_year_max = 1998L,
    tolerance = 1e-8) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c(by, year, category, value))
  x <- redist_validate_long_counts(
    x, value, tolerance, "population-group redistribution"
  )
  if (!length(targets) || anyDuplicated(targets)) {
    stop("Population-group targets must be a non-empty unique vector.", call. = FALSE)
  }
  if (length(intersect(unknown, targets))) {
    stop("Unknown and target population groups must be disjoint.", call. = FALSE)
  }
  unexpected <- setdiff(unique(x[[category]][!is.na(x[[category]])]), c(unknown, targets))
  if (length(unexpected)) {
    stop(
      "Unexpected ", category, " code(s) would be dropped by redistribution: ",
      paste(sort(unexpected), collapse = ", "),
      call. = FALSE
    )
  }
  before_data <- data.table::copy(x)

  x <- x[, .(value_internal = sum(get(value))), by = c(by, year, category)]
  groups <- unique(x[, c(by, year), with = FALSE])
  grid <- cross_join_dt(
    groups,
    data.table::data.table(category_internal = targets)
  )
  data.table::setnames(grid, "category_internal", category)

  known <- x[get(category) %in% targets]
  unknown_dt <- x[get(category) == unknown,
    .(unknown_value = sum(value_internal)),
    by = c(by, year)
  ]
  out <- merge(grid, known, by = c(by, year, category), all.x = TRUE, sort = FALSE)
  out[is.na(value_internal), value_internal := 0]
  out <- merge(out, unknown_dt, by = c(by, year), all.x = TRUE, sort = FALSE)
  out[is.na(unknown_value), unknown_value := 0]

  out[, current_total := sum(value_internal), by = c(by, year)]
  out[, current_share := data.table::fifelse(
    current_total > 0,
    value_internal / current_total,
    1 / length(targets)
  )]

  reference_values <- x[
    get(year) %in% reference_years & get(category) %in% targets,
    .(reference_value = sum(value_internal)),
    by = c(by, category)
  ]
  reference_grid <- cross_join_dt(
    unique(x[, ..by]),
    data.table::data.table(category_internal = targets)
  )
  data.table::setnames(reference_grid, "category_internal", category)
  reference <- merge(
    reference_grid,
    reference_values,
    by = c(by, category),
    all.x = TRUE,
    sort = FALSE
  )
  reference[is.na(reference_value), reference_value := 0]
  reference[, reference_total := sum(reference_value), by = by]
  reference[, reference_share := data.table::fifelse(
    reference_total > 0,
    reference_value / reference_total,
    1 / length(targets)
  )]
  reference <- reference[, c(by, category, "reference_share"), with = FALSE]
  out <- merge(out, reference, by = c(by, category), all.x = TRUE, sort = FALSE)

  out[, selected_share := current_share]
  out[get(year) <= early_year_max, selected_share := reference_share]
  out[, (value) := value_internal + unknown_value * selected_share]

  out <- out[, c(by, year, category, value), with = FALSE]
  data.table::setorderv(out, c(by, year, category))
  redist_assert_group_totals(
    before_data,
    out,
    by = c(by, year),
    value = value,
    tolerance = tolerance,
    label = "population-group redistribution"
  )
  out
}

cause_column <- function(code) paste0("c", as.integer(code))

ensure_cause_columns <- function(data, codes = 1:214, fill = 0, copy = TRUE) {
  require_package("data.table")
  if (!is.data.frame(data)) {
    stop("`data` must inherit from data.frame.", call. = FALSE)
  }
  if (!is.logical(copy) || length(copy) != 1L || is.na(copy)) {
    stop("`copy` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(fill) || length(fill) != 1L || !is.finite(fill)) {
    stop("`fill` must be one finite numeric value.", call. = FALSE)
  }

  if (inherits(data, "data.table")) {
    x <- if (copy) data.table::copy(data) else data
  } else {
    if (!copy) {
      stop("`copy = FALSE` requires data.table input.", call. = FALSE)
    }
    x <- data.table::as.data.table(data.table::copy(data))
  }

  required <- cause_column(codes)
  missing <- setdiff(required, names(x))
  for (column in missing) {
    data.table::set(x, j = column, value = as.numeric(fill))
  }
  for (column in required) {
    value <- x[[column]]
    if (!is.numeric(value)) {
      stop("Cause column `", column, "` must be numeric.", call. = FALSE)
    }
    value <- as.numeric(value)
    value[is.na(value)] <- fill
    data.table::set(x, j = column, value = value)
  }
  x
}

long_to_wide_causes <- function(
    data,
    id = c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup"),
    cause = "nbdcode",
    value = "Deaths",
    codes = 1:214) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c(id, cause, value))
  formula <- stats::as.formula(
    paste(paste(id, collapse = " + "), "~", cause)
  )
  out <- data.table::dcast(
    x,
    formula,
    value.var = value,
    fun.aggregate = sum,
    fill = 0
  )
  cause_names <- setdiff(names(out), id)
  data.table::setnames(out, cause_names, paste0("c", cause_names))
  ensure_cause_columns(out, codes, copy = FALSE)
}

wide_to_long_causes <- function(
    data,
    id = c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup"),
    value = "Count",
    codes = 1:214,
    drop_zero = FALSE) {
  require_package("data.table")
  x <- ensure_cause_columns(data, codes)
  measure <- cause_column(codes)
  out <- data.table::melt(
    x,
    id.vars = id,
    measure.vars = measure,
    variable.name = "nbdcode",
    value.name = value,
    variable.factor = FALSE
  )
  out[, nbdcode := as.integer(sub("^c", "", nbdcode))]
  if (isTRUE(drop_zero)) {
    out <- out[get(value) != 0]
  }
  data.table::setorderv(out, c(id, "nbdcode"))
  out
}

# Redistribute one or more cause columns to target cause columns. A logical
# condition may restrict the rows. Source columns are zeroed only where the
# condition applies. The operation is row-wise and preserves each affected row.
redistribute_cause_columns <- function(
    data,
    sources,
    targets,
    condition = NULL,
    tolerance = 1e-8,
    copy = TRUE) {
  require_package("data.table")
  codes <- unique(c(sources, targets))
  x <- ensure_cause_columns(data, codes, copy = copy)
  source_cols <- cause_column(sources)
  target_cols <- cause_column(targets)

  redistribute_columns(
    data = x,
    source_vars = source_cols,
    target_vars = target_cols,
    condition = condition,
    source_action = "zero",
    copy = FALSE,
    missing_counts = "zero",
    condition_na = "error",
    tolerance = tolerance,
    quiet = TRUE,
    context = paste0(
      "cause redistribution ", paste(sources, collapse = "+"),
      " -> ", paste(targets, collapse = "+")
    )
  )
}

# ------------------------------------------------------------------------------
# Expert-rule redistribution sequence
# ------------------------------------------------------------------------------

# Ill-defined and garbage-code redistribution ----------------------------------
#
# Rules are executed in the order defined by the retained Stata source. Source
# causes are redistributed through the canonical redist engine, then age- and
# sex-plausibility restrictions are applied. Natural and injury totals are
# preserved separately through the plausibility step.
#
# When a positive natural-cause envelope has no surviving valid cause in a row,
# the row borrows a biologically valid distribution from the closest empirical
# reference stratum: province-sex-year-age, national-sex-year-age,
# province-sex-age, then national-sex-age. Equal allocation across valid causes
# is used only when no empirical reference distribution exists. Every fallback
# allocation is written to an audit table.

stage06_redistribution_rules <- function() {
  always <- function(data) rep(TRUE, nrow(data))
  age_equal <- function(value) function(data) data$age5 == value
  age_greater <- function(value) function(data) data$age5 > value
  age_less <- function(value) function(data) data$age5 < value
  age_at_least <- function(value) function(data) data$age5 >= value

  rules <- list(
    list("r01", c(176, 178, 210, 211), 1, always),
    list("r02", 152, 3, always),
    list("r03", c(153, 154), 7, always),
    list("r04", c(193, 209, 212), 8, always),
    list("r05", 191, 10, always),
    list("r06", 175, 11, always),
    list("r07_peritonitis", c(179, 180), c(15, 18, 96, 97, 98, 101, 102), always),
    list("r08_perinatal_neonatal", 149, c(20, 21, 22, 23), age_equal(1L)),
    list("r09_perinatal_postneonatal", 149, 143, age_greater(1L)),
    list("r10_malnutrition_sequelae", 192, 24:27, always),
    list("r11_lip_oral_neoplasm", 155, c(28, 29), always),
    list("r12_digestive_neoplasm", 156, 30:35, always),
    list("r13_respiratory_neoplasm", 157, c(36, 37), always),
    list("r14", 162, 57, always),
    list("r15_mental_disorder", 148, 58:68, always),
    list("r16", 194, 75, always),
    list("r17", 207, 82, always),
    list("r18", 195, 86, always),
    list("r19", c(167, 168), 87, always),
    list("r20_arterial_embolism", 169, c(89, 90), always),
    list("r21_respiratory_failure", 206, 91:95, always),
    list("r22_haematemesis", 184, c(31, 32, 96), always),
    list("r23", c(174, 182, 183), 100, always),
    list("r24", c(170, 181), 103, always),
    list("r25", c(186, 187, 188), 104, always),
    list("r26_atherosclerosis", 208, c(83, 86, 88, 89), always),
    list("r27_ill_defined_cancers", 145, 28:55, always),
    list("r28_hypertension", 147, c(82, 83, 86, 88, 104), always),
    list("r29_ill_defined_cvd", c(146, 204), c(81, 82, 83, 84, 85, 87, 90), always),
    list("r30_heart_failure_child", c(173, 205), c(81, 82, 83, 84, 85, 87, 114), age_less(6L)),
    list("r31_heart_failure_adult", c(173, 205), c(81, 82, 83, 84, 85, 87), age_at_least(6L)),
    list(
      "r32_ill_defined_natural_neonatal",
      c(143, 144, 163, 164, 166, 171, 172, 177, 185, 189, 190, 202, 203),
      NATURAL_TARGETS_NEONATAL,
      age_equal(1L)
    ),
    list(
      "r33_ill_defined_natural_postneonatal",
      c(143, 144, 163, 164, 166, 171, 172, 177, 185, 189, 190, 202, 203),
      NATURAL_TARGETS_POSTNEONATAL,
      age_greater(1L)
    )
  )

  lapply(rules, function(rule) {
    list(
      rule_id = as.character(rule[[1L]]),
      sources = as.integer(rule[[2L]]),
      targets = as.integer(rule[[3L]]),
      condition = rule[[4L]]
    )
  })
}

stage06_validate_target_overrides <- function(rules, target_overrides = NULL) {
  if (is.null(target_overrides)) return(list())
  if (!is.list(target_overrides) || is.null(names(target_overrides)) ||
      any(!nzchar(names(target_overrides)))) {
    stop("Redistribution target overrides must be a named list.", call. = FALSE)
  }
  known <- vapply(rules, `[[`, character(1), "rule_id")
  unknown <- setdiff(names(target_overrides), known)
  if (length(unknown)) {
    stop("Unknown redistribution rule override(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  for (rule in rules) {
    if (!rule$rule_id %in% names(target_overrides)) next
    selected <- sort(unique(as.integer(target_overrides[[rule$rule_id]])))
    if (!length(selected) || anyNA(selected) ||
        length(setdiff(selected, rule$targets))) {
      stop(
        "Rule ", rule$rule_id,
        " must use a non-empty subset of its expert-specified targets.",
        call. = FALSE
      )
    }
    target_overrides[[rule$rule_id]] <- selected
  }
  target_overrides
}

stage06_draw_target_overrides <- function(seed, rules = stage06_redistribution_rules()) {
  if (length(seed) != 1L || !is.finite(as.numeric(seed))) {
    stop("A finite redistribution subset seed is required.", call. = FALSE)
  }
  set.seed(as.integer(seed))
  out <- list()
  for (rule in rules) {
    targets <- as.integer(rule$targets)
    if (length(targets) <= 1L) next
    selected <- integer()
    while (!length(selected)) {
      selected <- targets[stats::runif(length(targets)) < 0.5]
    }
    out[[rule$rule_id]] <- sort(unique(selected))
  }
  out
}

stage06_apply_sequential_redistribution <- function(
    data,
    target_overrides = NULL) {
  x <- ensure_cause_columns(data, 1:214)
  rules <- stage06_redistribution_rules()
  target_overrides <- stage06_validate_target_overrides(
    rules, target_overrides
  )
  audit <- vector("list", length(rules))

  for (index in seq_along(rules)) {
    rule <- rules[[index]]
    targets <- if (rule$rule_id %in% names(target_overrides)) {
      target_overrides[[rule$rule_id]]
    } else {
      rule$targets
    }
    condition <- rule$condition(x)
    x <- redistribute_cause_columns(
      x,
      rule$sources,
      targets,
      condition,
      copy = FALSE
    )
    audit[[index]] <- data.table::data.table(
      rule_id = rule$rule_id,
      source_codes = paste(rule$sources, collapse = ","),
      full_target_count = length(rule$targets),
      selected_target_count = length(targets),
      selected_targets = paste(targets, collapse = ","),
      stochastic_subset = rule$rule_id %in% names(target_overrides)
    )
  }

  attr(x, "redistribution_target_audit") <- data.table::rbindlist(
    audit, use.names = TRUE, fill = TRUE
  )
  x[]
}

stage06_apply_plausibility_checks <- function(data) {
  x <- data

  set_zero <- function(codes, condition = rep(TRUE, nrow(x))) {
    rows <- which(!is.na(condition) & condition)
    if (!length(rows)) return(invisible(NULL))
    for (column in cause_column(codes)) {
      data.table::set(x, i = rows, j = column, value = 0)
    }
    invisible(NULL)
  }

  set_zero(14:19, x$Sex == 1L | x$age5 < 5L | x$age5 >= 14L)
  set_zero(20:23, x$age5 > 1L)
  set_zero(28:54, x$age5 == 1L)
  set_zero(81:90, x$age5 == 1L)
  set_zero(c(13, 55, 72, 73, 82, 92, 94, 97, 101, 109, 110, 140), x$age5 == 1L)
  set_zero(c(33, 45, 46, 50, 51, 61, 62, 91), x$age5 < 5L)
  set_zero(45, x$Sex == 2L)
  set_zero(c(40, 70), x$age5 < 6L)
  set_zero(c(30, 31, 32, 35, 44), x$age5 < 8L)
  set_zero(44, x$Sex == 2L)
  set_zero(c(52, 28, 37, 38, 39, 42, 41, 43), x$age5 < 7L)
  set_zero(c(41, 42, 43), x$Sex == 1L)
  set_zero(53, x$age5 < 3L)
  set_zero(58:68, x$age5 < 4L)
  set_zero(108, x$age5 < 4L)
  set_zero(92, x$age5 > 1L & x$age5 < 6L)
  set_zero(105, x$Sex == 2L | x$age5 < 9L)
  set_zero(c(113, 115, 117), x$age5 >= 9L)
  set_zero(112, x$age5 >= 15L)
  set_zero(c(73, 74, 76, 77, 78))

  x[]
}

stage06_make_reference <- function(data, columns, by) {
  reference <- data[
    , lapply(.SD, sum, na.rm = TRUE),
    by = by,
    .SDcols = columns
  ]
  matrix <- as.matrix(reference[, ..columns])
  storage.mode(matrix) <- "double"
  list(
    by = by,
    keys = do.call(paste, c(reference[, ..by], sep = "\x1f")),
    matrix = matrix,
    total = rowSums(matrix)
  )
}

stage06_key <- function(data, by) {
  do.call(paste, c(data[, ..by], sep = "\x1f"))
}

stage06_valid_columns <- function(sex, age5, columns) {
  natural_candidates <- cause_column(unique(c(
    NATURAL_TARGETS_NEONATAL,
    NATURAL_TARGETS_POSTNEONATAL
  )))
  if (length(intersect(columns, natural_candidates))) {
    candidates <- intersect(columns, natural_candidates)
  } else {
    candidates <- columns
  }

  template <- data.table::data.table(
    Sex = as.integer(sex),
    age5 = as.integer(age5)
  )
  template <- ensure_cause_columns(template, 1:214, copy = FALSE)
  for (column in candidates) data.table::set(template, j = column, value = 1)
  template <- stage06_apply_plausibility_checks(template)
  candidates[vapply(
    candidates,
    function(column) template[[column]][1L] > 0,
    logical(1)
  )]
}

stage06_allocate_zero_denominator <- function(
    data,
    rows,
    columns,
    totals,
    label,
    tolerance = 1e-12) {
  if (!length(rows)) {
    return(data.table::data.table(
      row_index = integer(),
      fallback_level = character(),
      donor_total = numeric(),
      valid_targets = integer()
    ))
  }

  reference_levels <- list(
    province_sex_year_age = c("Death_Prov", "Sex", "DeathYear", "age5"),
    national_sex_year_age = c("Sex", "DeathYear", "age5"),
    province_sex_age = c("Death_Prov", "Sex", "age5"),
    national_sex_age = c("Sex", "age5")
  )
  references <- lapply(reference_levels, function(by) {
    stage06_make_reference(data, columns, by)
  })

  diagnostics <- data.table::data.table(
    row_index = as.integer(rows),
    fallback_level = NA_character_,
    donor_total = NA_real_,
    valid_targets = NA_integer_
  )

  for (position in seq_along(rows)) {
    row_index <- rows[[position]]
    allocated <- FALSE

    for (level_name in names(references)) {
      reference <- references[[level_name]]
      row_key <- stage06_key(data[row_index], reference$by)
      match_index <- match(row_key, reference$keys)
      if (is.na(match_index) || reference$total[[match_index]] <= tolerance) next

      shares <- reference$matrix[match_index, ] / reference$total[[match_index]]
      for (j in seq_along(columns)) {
        data.table::set(
          data,
          i = row_index,
          j = columns[[j]],
          value = totals[[position]] * shares[[j]]
        )
      }
      diagnostics[position, `:=`(
        fallback_level = level_name,
        donor_total = reference$total[[match_index]],
        valid_targets = sum(shares > 0)
      )]
      allocated <- TRUE
      break
    }

    if (!allocated) {
      valid_columns <- stage06_valid_columns(
        data$Sex[[row_index]], data$age5[[row_index]], columns
      )
      if (!length(valid_columns)) {
        stop(
          "No valid ", label, " cause exists for row ", row_index,
          " even after applying the final equal-share fallback.",
          call. = FALSE
        )
      }
      share <- totals[[position]] / length(valid_columns)
      for (column in columns) {
        data.table::set(data, i = row_index, j = column, value = 0)
      }
      for (column in valid_columns) {
        data.table::set(data, i = row_index, j = column, value = share)
      }
      diagnostics[position, `:=`(
        fallback_level = "equal_valid_causes",
        donor_total = 0,
        valid_targets = length(valid_columns)
      )]
    }
  }

  diagnostics[]
}

stage06_normalise_envelope <- function(
    data,
    columns,
    before_total,
    label,
    tolerance = 1e-12) {
  after_total <- rowSums(as.matrix(data[, ..columns]), na.rm = TRUE)
  positive_denominator <- before_total > tolerance & after_total > tolerance
  zero_denominator <- which(before_total > tolerance & after_total <= tolerance)

  if (any(positive_denominator)) {
    scale <- before_total[positive_denominator] / after_total[positive_denominator]
    selected_rows <- which(positive_denominator)
    for (column in columns) {
      data.table::set(
        data,
        i = selected_rows,
        j = column,
        value = data[[column]][selected_rows] * scale
      )
    }
  }

  diagnostics <- stage06_allocate_zero_denominator(
    data = data,
    rows = zero_denominator,
    columns = columns,
    totals = before_total[zero_denominator],
    label = label,
    tolerance = tolerance
  )
  diagnostics[, envelope := label]

  final_total <- rowSums(as.matrix(data[, ..columns]), na.rm = TRUE)
  scale_check <- pmax(1, abs(before_total), abs(final_total))
  bad <- which(abs(before_total - final_total) > 1e-7 * scale_check)
  if (length(bad)) {
    stop(
      label, " normalization failed to preserve totals in ", length(bad),
      " row(s).",
      call. = FALSE
    )
  }

  diagnostics[]
}

stage06_exclusion_reason <- function(code, sex, age5) {
  reasons <- character()
  if (code %in% 14:19 && (sex == 1L || age5 < 5L || age5 >= 14L)) {
    reasons <- c(reasons, "maternal age/sex exclusion")
  }
  if (code %in% 20:23 && age5 > 1L) reasons <- c(reasons, "perinatal age exclusion")
  if (code %in% 28:54 && age5 == 1L) reasons <- c(reasons, "neonatal cancer exclusion")
  if (code %in% 81:90 && age5 == 1L) reasons <- c(reasons, "neonatal circulatory exclusion")
  if (code %in% c(13, 55, 72, 73, 82, 92, 94, 97, 101, 109, 110, 140) && age5 == 1L) {
    reasons <- c(reasons, "additional neonatal exclusion")
  }
  if (code %in% c(33, 45, 46, 50, 51, 61, 62, 91) && age5 < 5L) {
    reasons <- c(reasons, "under-10 exclusion")
  }
  if (code == 45L && sex == 2L) reasons <- c(reasons, "female testis-cancer exclusion")
  if (code %in% c(40, 70) && age5 < 6L) reasons <- c(reasons, "under-15 exclusion")
  if (code %in% c(30, 31, 32, 35, 44) && age5 < 8L) reasons <- c(reasons, "under-25 exclusion")
  if (code == 44L && sex == 2L) reasons <- c(reasons, "female prostate-cancer exclusion")
  if (code %in% c(52, 28, 37, 38, 39, 42, 41, 43) && age5 < 7L) {
    reasons <- c(reasons, "under-20 exclusion")
  }
  if (code %in% c(41, 42, 43) && sex == 1L) reasons <- c(reasons, "male female-cancer exclusion")
  if (code == 53L && age5 < 3L) reasons <- c(reasons, "under-1 leukemia exclusion")
  if (code %in% 58:68 && age5 < 4L) reasons <- c(reasons, "under-5 mental-disorder exclusion")
  if (code == 108L && age5 < 4L) reasons <- c(reasons, "under-5 rheumatoid-arthritis exclusion")
  if (code == 92L && age5 > 1L && age5 < 6L) reasons <- c(reasons, "pneumoconiosis age exclusion")
  if (code == 105L && (sex == 2L || age5 < 9L)) reasons <- c(reasons, "BPH age/sex exclusion")
  if (code %in% c(113, 115, 117) && age5 >= 9L) reasons <- c(reasons, "congenital age exclusion")
  if (code == 112L && age5 >= 15L) reasons <- c(reasons, "neural-tube-defect age exclusion")
  if (code %in% c(73, 74, 76, 77, 78)) reasons <- c(reasons, "not retained as an underlying cause of death")
  if (!length(reasons)) "not identified" else paste(reasons, collapse = "; ")
}

redistribute_garbage_codes <- function(data, cfg = NULL, target_overrides = NULL) {
  require_package("data.table")

  # Remove diagnostics from a previous run so audit files always describe the
  # current inputs. New files are written only when reference allocation is
  # actually required.
  if (!is.null(cfg)) {
    for (filename in c(
      "06_biological_fallback_rows.csv",
      "06_biological_fallback_source_causes.csv"
    )) {
      path <- table_file(cfg, filename)
      if (file.exists(path)) unlink(path)
    }
  }

  x <- stage06_apply_sequential_redistribution(
    data, target_overrides = target_overrides
  )
  target_audit <- attr(x, "redistribution_target_audit")

  id_columns <- c("Death_Prov", "Sex", "DeathYear", "age5", "Popgroup")
  natural_codes <- c(1:123, 150:214)
  natural_columns <- cause_column(natural_codes)
  injury_columns <- cause_column(INJURY_CODES)

  natural_before_matrix <- as.matrix(x[, ..natural_columns])
  injury_before_matrix <- as.matrix(x[, ..injury_columns])
  natural_before <- rowSums(natural_before_matrix, na.rm = TRUE)
  injury_before <- rowSums(injury_before_matrix, na.rm = TRUE)
  all_before <- rowSums(as.matrix(x[, cause_column(1:214), with = FALSE]), na.rm = TRUE)

  x <- stage06_apply_plausibility_checks(x)

  natural_diagnostics <- stage06_normalise_envelope(
    x, natural_columns, natural_before, "natural"
  )
  injury_diagnostics <- stage06_normalise_envelope(
    x, injury_columns, injury_before, "injury"
  )
  fallback_diagnostics <- data.table::rbindlist(
    list(natural_diagnostics, injury_diagnostics),
    use.names = TRUE,
    fill = TRUE
  )

  all_final <- rowSums(as.matrix(x[, cause_column(1:214), with = FALSE]), na.rm = TRUE)
  overall_scale <- pmax(1, abs(all_before), abs(all_final))
  bad_overall <- which(abs(all_before - all_final) > 1e-7 * overall_scale)
  if (length(bad_overall)) {
    stop(
      "Stage 06 failed to preserve all-cause totals in ", length(bad_overall),
      " row(s).",
      call. = FALSE
    )
  }

  if (nrow(fallback_diagnostics) && !is.null(cfg)) {
    id_table <- data.table::copy(x[, ..id_columns])
    id_table[, row_index := .I]
    data.table::setcolorder(id_table, c("row_index", id_columns))
    fallback_diagnostics <- merge(
      fallback_diagnostics,
      id_table,
      by = "row_index",
      all.x = TRUE,
      sort = FALSE
    )
    fallback_diagnostics[, restored_deaths := data.table::fifelse(
      envelope == "natural",
      natural_before[row_index],
      injury_before[row_index]
    )]
    write_csv_table(
      fallback_diagnostics,
      cfg,
      "06_biological_fallback_rows.csv"
    )

    natural_bad <- natural_diagnostics$row_index
    if (length(natural_bad)) {
      bad_matrix <- natural_before_matrix[natural_bad, , drop = FALSE]
      positive <- which(bad_matrix > 1e-12, arr.ind = TRUE)
      if (nrow(positive)) {
        source_detail <- x[natural_bad[positive[, 1L]], ..id_columns]
        source_detail[, nbdcode := natural_codes[positive[, 2L]]]
        source_detail[, precheck_deaths := bad_matrix[positive]]
        source_detail[, exclusion_reason := mapply(
          stage06_exclusion_reason,
          nbdcode,
          Sex,
          age5,
          USE.NAMES = FALSE
        )]
        label_path <- file.path(cfg$paths$lookups, "analysis_codes.csv")
        if (file.exists(label_path)) {
          labels <- read_tabular(label_path)[, .(
            nbdcode = as.integer(analysis_code),
            analysis_label
          )]
          source_detail <- merge(
            source_detail,
            labels,
            by = "nbdcode",
            all.x = TRUE,
            sort = FALSE
          )
        }
        write_csv_table(
          source_detail,
          cfg,
          "06_biological_fallback_source_causes.csv"
        )
      }
    }
  }

  attr(x, "redistribution_target_audit") <- target_audit
  x[]
}

run_garbage_redistribution <- function(hiv_wide, cfg, target_overrides = NULL) {
  wide <- redistribute_garbage_codes(
    hiv_wide, cfg, target_overrides = target_overrides
  )
  long <- wide_to_long_causes(
    wide,
    value = "Count",
    codes = 1:214,
    drop_zero = FALSE
  )
  list(wide = wide, long = long)
}
