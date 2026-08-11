# ==============================================================================
# 02_injuries: Injury survey preparation and annual cause fractions
# ==============================================================================
#
# This file groups related functions so the analytical sequence can be taught
# and reviewed as a small number of coherent modules. Function bodies are
# retained from the validated Version 1 implementation.

# ------------------------------------------------------------------------------
# NIMS, IMS and FAMHIS preparation
# ------------------------------------------------------------------------------

# Injury survey preparation ----------------------------------------------------
#
# NIMS 2000, IMS 2009 and FAMHIS 2017 are harmonised to a common province x
# sex x population-group x age x cause grid. NIMS is national by sex, age and
# cause; it is expanded to province x population group using the observed IMS
# 2009 distribution for the same sex x age x cause. This preserves the NIMS
# national totals while making the spatial information explicitly derived.

standardise_injury_columns <- function(data, aliases) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  for (target in names(aliases)) {
    x <- rename_first_match(x, target, aliases[[target]], required = TRUE)
  }
  x
}


# Build a stable character key from one or more columns. This avoids fragile
# data.table `..` expressions inside nested donor-reference objects and works
# identically for one-column and multi-column reference levels.
injury_reference_key <- function(data, columns) {
  x <- data.table::as.data.table(data)
  if (!length(columns)) return(rep("__national__", nrow(x)))

  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(
      "Cannot construct an injury donor key; missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  values <- lapply(columns, function(column) {
    value <- as.character(x[[column]])
    value[is.na(value)] <- "<NA>"
    value
  })
  do.call(paste, c(values, list(sep = "\x1f")))
}


# Survey-information helpers --------------------------------------------------
#
# Weighted injury totals are not treated as independent sample sizes. For IMS
# and FAMHIS, the available record weights are converted to a Kish
# count-equivalent effective sample size. The survey-wide information is then
# allocated across the final demographic strata in proportion to the weighted
# injury mass retained by the deterministic preparation. NIMS contains count
# cells, so its sex-age total is used as the count-equivalent information size
# and is allocated across the spatial expansion in the same way.

kish_effective_sample_size <- function(weights) {
  w <- suppressWarnings(as.numeric(weights))
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(0)
  denominator <- sum(w^2)
  if (!is.finite(denominator) || denominator <= 0) return(0)
  as.numeric(sum(w)^2 / denominator)
}

allocate_injury_effective_n <- function(
    data,
    count_column,
    effective_total,
    output_column,
    strata = c("Death_Prov", "Sex", "Popgroup", "age5")) {
  require_package("data.table")
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c(strata, "nbdcode", count_column), "injury effective-size allocation")

  effective_total <- suppressWarnings(as.numeric(effective_total))
  if (length(effective_total) != 1L || !is.finite(effective_total) || effective_total < 0) {
    stop("The injury survey effective sample size must be finite and non-negative.", call. = FALSE)
  }

  stratum <- x[, .(
    weighted_total__ = sum(as.numeric(get(count_column)), na.rm = TRUE)
  ), by = strata]
  grand_total <- sum(stratum$weighted_total__, na.rm = TRUE)
  if (nrow(stratum) == 0L) {
    x[, (output_column) := numeric()]
    return(x[])
  }
  if (grand_total > 0 && effective_total > 0) {
    stratum[, effective_n__ := effective_total * weighted_total__ / grand_total]
  } else {
    stratum[, effective_n__ := 0]
  }
  x <- merge(x, stratum[, c(strata, "effective_n__"), with = FALSE],
             by = strata, all.x = TRUE, sort = FALSE)
  x[, (output_column) := pmax(0, as.numeric(effective_n__))]
  x[, effective_n__ := NULL]
  x[]
}

prepare_ims_2009 <- function(data) {
  require_package("data.table")
  x <- standardise_injury_columns(data, list(
    weight = c("Weight", "wht"),
    province = c("Province", "prov"),
    Cause_of_death = c("causeofdeath", "cause_type"),
    age_raw = c("nbd_age", "age5"),
    nbdgr = c("NBDgr", "nbd_group"),
    Population_group = c("populationgroup", "popgroup"),
    Sex = c("sex", "gender")
  ))
  for (column in c("weight", "province", "Cause_of_death", "age_raw", "nbdgr", "Population_group", "Sex")) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
  }

  corrections <- data.table::data.table(
    old = c(2.4806, 2.4768, 4.1042, 2.4828, 4.1042, 2.4930,
            2.4682, 4.1042, 2.6034, 3.9258, 4.1042, 4.1419, 2.4768),
    province = c(4, 4, 4, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8),
    new = c(2.582, 2.578, 3.510, 2.404, 4.529, 2.413,
            2.389, 4.529, 2.520, 4.332, 3.510, 3.542, 2.574)
  )
  for (i in seq_len(nrow(corrections))) {
    x[province == corrections$province[[i]] &
        abs(weight - corrections$old[[i]]) < 1e-7,
      weight := corrections$new[[i]]
    ]
  }

  x <- x[Cause_of_death == 1]
  x[, nbdcode := as.integer(INJURY_NBD_GROUP_MAP[as.character(as.integer(nbdgr))])]
  x[nbdcode == 142L, nbdcode := 141L]
  x[is.na(nbdcode), nbdcode := 999L]
  x[, Death_Prov := as.integer(PROVINCE_FROM_IMS[as.character(as.integer(province))])]
  x[, Popgroup := as.integer(POP_GROUP_FROM_IMS[as.character(as.integer(Population_group))])]
  x[is.na(Popgroup), Popgroup := 5L]
  x[is.na(Sex) | !Sex %in% 1:2, Sex := 3L]
  x[, age5 := as.integer(age_raw)]
  x[is.na(age5), age5 := 20L]
  x[!is.finite(weight) | weight < 0, weight := 0]

  # The raw data do not include PSU/stratum design variables. Kish's weighted
  # effective sample size is therefore retained as the count-equivalent survey
  # information available for the cause-composition uncertainty model.
  ims_effective_n <- kish_effective_sample_size(
    x[nbdcode %in% INJURY_CODES, weight]
  )

  x <- x[, .(Inj2009 = sum(weight, na.rm = TRUE)), by = .(
    Death_Prov, Sex, Popgroup, age5, nbdcode
  )]
  x <- redistribute_unknown(
    x, "Sex", "Inj2009",
    by = c("Popgroup", "Death_Prov", "age5", "nbdcode"),
    unknown = 3L, targets = 1:2
  )
  x <- redistribute_unknown(
    x, "age5", "Inj2009",
    by = c("Popgroup", "Death_Prov", "Sex", "nbdcode"),
    unknown = 20L, targets = 1:19
  )
  x <- redistribute_unknown(
    x, "Popgroup", "Inj2009",
    by = c("age5", "Death_Prov", "Sex", "nbdcode"),
    unknown = 5L, targets = 1:4
  )

  # The IMS ages are indexed one below the mortality pipeline. Add one and copy
  # the all-infant row to the neonatal row, matching the legacy implementation.
  x[, age5 := age5 + 1L]
  neonatal <- data.table::copy(x[age5 == 2L])
  neonatal[, age5 := 1L]
  x <- data.table::rbindlist(list(x, neonatal), use.names = TRUE)
  x <- x[nbdcode %in% INJURY_CODES]
  out <- complete_cells(
    x,
    dimensions = c("Death_Prov", "Sex", "Popgroup", "age5", "nbdcode"),
    values = "Inj2009",
    levels = list(
      Death_Prov = 1:9,
      Sex = 1:2,
      Popgroup = 1:4,
      age5 = 1:20,
      nbdcode = INJURY_CODES
    )
  )
  out <- allocate_injury_effective_n(
    out,
    count_column = "Inj2009",
    effective_total = ims_effective_n,
    output_column = "EffN2009"
  )
  out[]
}


# NIMS 2000 -------------------------------------------------------------------
#
# The supplied workbook is the national NIMS table used by the final Stata
# injury program. In its native wide layout, columns beginning A1 are male age
# columns and columns beginning A2 are female age columns; the trailing digits
# are the source age index. The source index is shifted by one and the all-
# infant row is copied to the neonatal row, matching the executable Stata code.

prepare_nims_2000 <- function(data, count_floor = 1e-6) {
  require_package("data.table")

  x <- data.table::as.data.table(data.table::copy(data))
  if (!is.finite(count_floor) || length(count_floor) != 1L || count_floor <= 0) {
    stop("NIMS count_floor must be finite and greater than zero.", call. = FALSE)
  }

  # Accept a documented long equivalent as well as the original workbook.
  long_aliases <- list(
    nbdcode = c("code", "nbd_code", "analysis_code"),
    Sex = c("sex", "gender"),
    source_age = c("source_age", "age_index", "age"),
    Inj2000 = c("inj2000", "count", "deaths", "weight")
  )
  long_candidate <- data.table::copy(x)
  for (target in names(long_aliases)) {
    long_candidate <- rename_first_match(
      long_candidate,
      target,
      long_aliases[[target]],
      required = FALSE
    )
  }

  if (all(c("nbdcode", "Sex", "source_age", "Inj2000") %in% names(long_candidate))) {
    long <- long_candidate[, .(nbdcode, Sex, source_age, Inj2000)]
  } else {
    x <- rename_first_match(
      x,
      "nbdcode",
      c("code", "nbd_code", "analysis_code"),
      required = TRUE
    )

    original_names <- names(x)
    tokens <- normalise_token(original_names)

    identify_age_columns <- function(prefix) {
      pattern <- paste0("^", prefix, "([0-9]+)$")
      matches <- regexec(pattern, tokens)
      captures <- regmatches(tokens, matches)
      age <- vapply(
        captures,
        function(value) if (length(value) == 2L) value[[2L]] else NA_character_,
        character(1)
      )
      data.table::data.table(
        column = original_names[!is.na(age)],
        source_age = suppressWarnings(as.integer(age[!is.na(age)]))
      )
    }

    male_columns <- identify_age_columns("a1")
    female_columns <- identify_age_columns("a2")
    if (!nrow(male_columns) || !nrow(female_columns)) {
      stop(
        "NIMS workbook must contain male A1<age> and female A2<age> columns, ",
        "or the documented long fields nbdcode, Sex, source_age and Inj2000.",
        call. = FALSE
      )
    }

    make_long <- function(columns, sex) {
      out <- data.table::melt(
        x,
        id.vars = "nbdcode",
        measure.vars = columns$column,
        variable.name = "source_column",
        value.name = "Inj2000",
        variable.factor = FALSE
      )
      out <- merge(
        out,
        columns,
        by.x = "source_column",
        by.y = "column",
        all.x = TRUE,
        sort = FALSE
      )
      out[, `:=`(source_column = NULL, Sex = as.integer(sex))]
      out[]
    }

    long <- data.table::rbindlist(
      list(make_long(male_columns, 1L), make_long(female_columns, 2L)),
      use.names = TRUE
    )
  }

  long[, `:=`(
    nbdcode = suppressWarnings(as.integer(nbdcode)),
    Sex = suppressWarnings(as.integer(Sex)),
    source_age = suppressWarnings(as.integer(source_age)),
    Inj2000 = suppressWarnings(as.numeric(Inj2000))
  )]
  long <- long[
    nbdcode %in% INJURY_CODES & Sex %in% 1:2 &
      is.finite(source_age) & source_age >= 1L
  ]
  if (long[is.finite(Inj2000) & Inj2000 < 0, .N]) {
    stop("NIMS contains negative injury counts.", call. = FALSE)
  }
  nims_effective_by_source_age <- long[, .(
    EffN2000 = sum(pmax(0, Inj2000[is.finite(Inj2000)]), na.rm = TRUE)
  ), by = .(Sex, source_age)]
  # Retain whether the original NIMS cell was zero or missing. Counts receive
  # the legacy 1e-6 numerical floor, but the flag allows the targeted 132/138
  # correction to distinguish a true source zero from a small observed value.
  long[, nims_source_zero := !is.finite(Inj2000) | Inj2000 <= 0]
  long[nims_source_zero %in% TRUE, Inj2000 := count_floor]
  long <- long[, .(
    Inj2000 = sum(Inj2000),
    nims_source_zero = all(nims_source_zero)
  ), by = .(nbdcode, Sex, source_age)]

  long <- merge(
    long,
    nims_effective_by_source_age,
    by = c("Sex", "source_age"),
    all.x = TRUE,
    sort = FALSE
  )
  long[, age5 := as.integer(source_age + 1L)]
  long[, source_age := NULL]
  long <- long[age5 %in% 2:20]
  neonatal <- data.table::copy(long[age5 == 2L])
  neonatal[, age5 := 1L]
  long <- data.table::rbindlist(list(long, neonatal), use.names = TRUE)
  long <- long[, .(
    Inj2000 = sum(Inj2000),
    nims_source_zero = all(nims_source_zero),
    EffN2000 = max(EffN2000, na.rm = TRUE)
  ), by = .(Sex, age5, nbdcode)]

  # The NIMS effective sample size is a property of the complete sex-age
  # composition, not of an individual cause. Capture it before completing the
  # cause grid so causes absent from the workbook inherit the same sex-age
  # information size rather than being assigned zero information.
  nims_effective_by_age <- unique(long[, .(Sex, age5, EffN2000)])
  assert_unique_key(
    nims_effective_by_age,
    c("Sex", "age5"),
    "NIMS sex-age effective size before cause-grid completion"
  )

  out <- complete_cells(
    long[, .(Sex, age5, nbdcode, Inj2000)],
    dimensions = c("Sex", "age5", "nbdcode"),
    values = "Inj2000",
    levels = list(
      Sex = 1:2,
      age5 = 1:20,
      nbdcode = INJURY_CODES
    ),
    fill = count_floor
  )
  zero_flags <- long[, .(
    nims_source_zero = all(nims_source_zero)
  ), by = .(Sex, age5, nbdcode)]
  out <- merge(
    out,
    zero_flags,
    by = c("Sex", "age5", "nbdcode"),
    all.x = TRUE,
    sort = FALSE
  )
  out <- merge(
    out,
    nims_effective_by_age,
    by = c("Sex", "age5"),
    all.x = TRUE,
    sort = FALSE
  )
  out[is.na(nims_source_zero), nims_source_zero := TRUE]
  out[!is.finite(EffN2000) | EffN2000 < 0, EffN2000 := 0]
  data.table::setorder(out, Sex, age5, nbdcode)
  assert_unique_key(out, c("Sex", "age5", "nbdcode"), "national NIMS 2000")
  assert_nonnegative(out, "Inj2000")
  out[]
}

expand_nims_2000_with_ims <- function(
    nims,
    ims,
    tolerance = 1e-10) {
  require_package("data.table")

  national <- data.table::as.data.table(data.table::copy(nims))
  spatial <- data.table::as.data.table(data.table::copy(ims))
  national_key <- c("Sex", "age5", "nbdcode")
  spatial_key <- c("Death_Prov", "Popgroup", national_key)
  assert_has_columns(
    national,
    c(national_key, "Inj2000", "nims_source_zero", "EffN2000"),
    "national NIMS 2000"
  )
  assert_has_columns(spatial, c(spatial_key, "Inj2009"), "IMS 2009")
  assert_unique_key(national, national_key, "national NIMS 2000")
  assert_unique_key(spatial, spatial_key, "IMS 2009 spatial distribution")

  shares <- spatial[, .(
    ims_spatial_count = sum(Inj2009, na.rm = TRUE)
  ), by = spatial_key]
  shares[, ims_spatial_total := sum(ims_spatial_count), by = national_key]
  shares[, nims_spatial_share := data.table::fifelse(
    ims_spatial_total > tolerance,
    ims_spatial_count / ims_spatial_total,
    1 / 36
  )]
  shares[, nims_spatial_status := data.table::fifelse(
    ims_spatial_total > tolerance,
    "IMS 2009 province-population-group fraction",
    "Equal 1/36 fallback"
  )]

  grid <- cross_join_dt(
    national,
    data.table::CJ(Death_Prov = 1:9, Popgroup = 1:4, sorted = FALSE)
  )
  out <- merge(
    grid,
    shares[, c(
      spatial_key,
      "nims_spatial_share",
      "nims_spatial_status"
    ), with = FALSE],
    by = spatial_key,
    all.x = TRUE,
    sort = FALSE
  )
  out[!is.finite(nims_spatial_share), `:=`(
    nims_spatial_share = 1 / 36,
    nims_spatial_status = "Equal 1/36 fallback"
  )]
  out[, Inj2000 := Inj2000 * nims_spatial_share]

  before <- national[, .(before = sum(Inj2000)), by = national_key]
  after <- out[, .(after = sum(Inj2000)), by = national_key]
  check <- merge(before, after, by = national_key, all = TRUE, sort = FALSE)
  check[is.na(before), before := 0]
  check[is.na(after), after := 0]
  check[, scale := pmax(1, abs(before), abs(after))]
  bad <- check[abs(before - after) > tolerance * scale]
  if (nrow(bad)) {
    stop(
      "Expanding NIMS with IMS fractions failed to preserve ",
      nrow(bad), " national sex-age-cause total(s).",
      call. = FALSE
    )
  }

  # NIMS information is count based. Allocate each national sex-age sample size
  # to the final province-population-group strata in proportion to the expanded
  # all-cause NIMS mass, then repeat the stratum information across causes.
  national_eff <- unique(national[, .(Sex, age5, EffN2000)])
  assert_unique_key(national_eff, c("Sex", "age5"), "NIMS sex-age effective size")
  spatial_eff <- out[, .(
    stratum_count__ = sum(Inj2000, na.rm = TRUE)
  ), by = .(Death_Prov, Popgroup, Sex, age5)]
  spatial_eff[, total_count__ := sum(stratum_count__), by = .(Sex, age5)]
  spatial_eff <- merge(
    spatial_eff,
    national_eff,
    by = c("Sex", "age5"),
    all.x = TRUE,
    sort = FALSE
  )
  spatial_eff[, EffN2000 := data.table::fifelse(
    total_count__ > tolerance,
    EffN2000 * stratum_count__ / total_count__,
    EffN2000 / 36
  )]
  out[, EffN2000 := NULL]
  out <- merge(
    out,
    spatial_eff[, .(Death_Prov, Popgroup, Sex, age5, EffN2000)],
    by = c("Death_Prov", "Popgroup", "Sex", "age5"),
    all.x = TRUE,
    sort = FALSE
  )

  out <- out[, .(
    Death_Prov,
    Sex,
    Popgroup,
    age5,
    nbdcode,
    Inj2000,
    EffN2000,
    nims_source_zero,
    nims_spatial_share,
    nims_spatial_status
  )]
  data.table::setorder(out, Death_Prov, Sex, Popgroup, age5, nbdcode)
  assert_unique_key(out, spatial_key, "expanded NIMS 2000")
  assert_nonnegative(out, c("Inj2000", "EffN2000", "nims_spatial_share"))
  out[]
}

redistribute_named_source <- function(
    data,
    source,
    targets,
    tolerance = 1e-8,
    copy = TRUE) {
  require_package("data.table")
  if (inherits(data, "data.table")) {
    x <- if (copy) data.table::copy(data) else data
  } else {
    if (!copy) stop("`copy = FALSE` requires data.table input.", call. = FALSE)
    x <- data.table::as.data.table(data.table::copy(data))
  }

  required <- unique(c(source, targets))
  missing <- setdiff(required, names(x))
  for (column in missing) data.table::set(x, j = column, value = 0.0)
  for (column in required) {
    value <- x[[column]]
    if (!is.numeric(value)) {
      stop("Injury redistribution column `", column, "` must be numeric.", call. = FALSE)
    }
    value <- as.numeric(value)
    value[is.na(value)] <- 0
    data.table::set(x, j = column, value = value)
  }

  redistribute_columns(
    data = x,
    source_vars = source,
    target_vars = targets,
    source_action = "zero",
    copy = FALSE,
    missing_counts = "zero",
    condition_na = "error",
    tolerance = tolerance,
    quiet = TRUE,
    context = paste0(
      "injury mechanism redistribution ", source,
      " -> ", paste(targets, collapse = "+")
    )
  )
}

# Allocate a generic survey mechanism using the most specific empirical donor
# distribution available. This replaces a flat equal split when a fine cell has
# no specific target deaths. The donor hierarchy deliberately omits population
# group first, because that is the sparsest FAMHIS dimension.
redistribute_named_source_with_reference <- function(
    data,
    source,
    targets,
    id_columns = c("Death_Prov", "Sex", "Popgroup", "age5"),
    tolerance = 1e-10,
    copy = TRUE) {
  require_package("data.table")

  if (inherits(data, "data.table")) {
    x <- if (copy) data.table::copy(data) else data
  } else {
    if (!copy) stop("`copy = FALSE` requires data.table input.", call. = FALSE)
    x <- data.table::as.data.table(data.table::copy(data))
  }

  if (!length(source) || length(source) != 1L || !nzchar(source)) {
    stop("FAMHIS redistribution requires one source column.", call. = FALSE)
  }
  if (!length(targets) || anyDuplicated(targets)) {
    stop("FAMHIS redistribution requires unique target columns.", call. = FALSE)
  }
  if (source %in% targets) {
    stop("FAMHIS source and target columns must be disjoint.", call. = FALSE)
  }
  assert_has_columns(x, id_columns, "FAMHIS redistribution identifiers")

  missing_counts <- setdiff(c(source, targets), names(x))
  for (column in missing_counts) data.table::set(x, j = column, value = 0.0)
  for (column in c(source, targets)) {
    values <- suppressWarnings(as.numeric(x[[column]]))
    values[is.na(values)] <- 0
    if (any(!is.finite(values) | values < -tolerance)) {
      stop("Invalid FAMHIS mechanism count in `", column, "`.", call. = FALSE)
    }
    values[values < 0] <- 0
    data.table::set(x, j = column, value = values)
  }

  # Force an independent numeric vector before any by-reference mutation. A
  # direct alias to x[[source]] can be zeroed when data.table::set() updates x.
  source_values <- as.numeric(x[[source]]) + 0
  target_before <- as.matrix(x[, ..targets])
  storage.mode(target_before) <- "double"
  target_total <- rowSums(target_before)
  total_before <- source_values + target_total

  rows <- which(source_values > tolerance)
  if (!length(rows)) {
    data.table::set(x, j = source, value = 0.0)
    return(x[])
  }

  reference_levels <- list(
    province_sex_age = c("Death_Prov", "Sex", "age5"),
    national_sex_age = c("Sex", "age5"),
    national_age = "age5",
    national = character()
  )

  make_reference <- function(by_columns) {
    if (length(by_columns)) {
      ref <- x[, lapply(.SD, sum, na.rm = TRUE),
               by = by_columns, .SDcols = targets]
    } else {
      ref <- x[, lapply(.SD, sum, na.rm = TRUE), .SDcols = targets]
    }
    matrix <- as.matrix(ref[, ..targets])
    storage.mode(matrix) <- "double"
    list(
      by = by_columns,
      keys = injury_reference_key(ref, by_columns),
      matrix = matrix,
      total = rowSums(matrix)
    )
  }

  references <- lapply(reference_levels, make_reference)
  allocated <- numeric(length(rows))
  fallback_level <- character(length(rows))

  for (position in seq_along(rows)) {
    row <- rows[[position]]

    if (target_total[[row]] > tolerance) {
      shares <- target_before[row, ] / target_total[[row]]
      fallback_level[[position]] <- "within_cell"
    } else {
      shares <- NULL
      for (level_name in names(references)) {
        ref <- references[[level_name]]
        row_key <- injury_reference_key(x[row], ref$by)
        match_row <- match(row_key, ref$keys)
        if (!is.na(match_row) && ref$total[[match_row]] > tolerance) {
          shares <- ref$matrix[match_row, ] / ref$total[[match_row]]
          fallback_level[[position]] <- level_name
          break
        }
      }
      if (is.null(shares)) {
        shares <- rep(1 / length(targets), length(targets))
        fallback_level[[position]] <- "equal_final_safeguard"
      }
    }

    shares[!is.finite(shares) | shares < 0] <- 0
    share_total <- sum(shares)
    if (share_total <= tolerance) {
      shares <- rep(1 / length(targets), length(targets))
      fallback_level[[position]] <- "equal_final_safeguard"
    } else {
      shares <- shares / share_total
    }

    addition <- source_values[[row]] * shares
    for (j in seq_along(targets)) {
      data.table::set(
        x,
        i = row,
        j = targets[[j]],
        value = x[[targets[[j]]]][[row]] + addition[[j]]
      )
    }
    allocated[[position]] <- sum(addition)
  }

  data.table::set(x, j = source, value = 0.0)

  target_after <- rowSums(as.matrix(x[, ..targets]))
  scale <- pmax(1, abs(total_before), abs(target_after))
  bad <- which(abs(total_before - target_after) > tolerance * scale)
  if (length(bad)) {
    stop(
      "FAMHIS `", source, "` redistribution failed conservation in ",
      length(bad), " row(s); maximum absolute difference = ",
      signif(max(abs(total_before[bad] - target_after[bad])), 12),
      ". First affected row(s): ", paste(utils::head(bad, 8L), collapse = ", "),
      call. = FALSE
    )
  }

  attr(x, paste0("redistribution_", source)) <- data.table::data.table(
    row = rows,
    source = source,
    fallback_level = fallback_level,
    redistributed = allocated
  )
  x[]
}

prepare_famhis_2017 <- function(data) {
  require_package("data.table")
  x <- standardise_injury_columns(data, list(
    Death_Prov = c("prov", "province"),
    Sex_raw = c("gender_b", "sex", "gender"),
    Population_group = c("ethnicity_b", "popgroup", "population_group"),
    nbd_cod_mech = c("nbdcodmech", "mechanism"),
    age_value = c("age_Non_Natural", "agenonnatural", "age"),
    age_unit = c("age_unitss_Non_Natural", "ageunitsnonnatural", "age_unit"),
    weight = c("wht", "weight")
  ))
  for (column in c(
    "Death_Prov", "Sex_raw", "Population_group",
    "age_value", "age_unit", "weight"
  )) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
  }
  x[, nbd_cod_mech := trimws(as.character(nbd_cod_mech))]
  x <- x[!is.na(nbd_cod_mech) & nzchar(nbd_cod_mech)]
  x[, age5 := age5_from_famhis(age_value, age_unit)]
  x[, Sex := as.integer(Sex_raw + 1L)]
  x[is.na(Sex) | !Sex %in% 1:2, Sex := 3L]
  x[, Popgroup := as.integer(
    POP_GROUP_FROM_IMS[as.character(as.integer(Population_group))]
  )]
  x[is.na(Popgroup), Popgroup := 5L]
  x[is.na(Death_Prov) | !Death_Prov %in% 1:9, Death_Prov := 10L]
  x[!is.finite(weight) | weight < 0, weight := 0]

  # Reproduce the sequential FAMHIS auxiliary-flag overrides used in the final
  # Stata program. The downstream allocation is improved: generic mechanisms
  # borrow empirical target shares instead of receiving a flat detailed split.
  generic_fields <- list(
    trans = "nbdtrans",
    suic = "nbdsuicide",
    homi = "nbdhomicide",
    unint = "nbdunint"
  )
  for (label in names(generic_fields)) {
    source <- names(x)[
      normalise_token(names(x)) == normalise_token(generic_fields[[label]])
    ]
    if (length(source)) {
      flag <- tolower(trimws(as.character(x[[source[[1L]]]])))
      x[!is.na(flag) & flag == "unknown", nbd_cod_mech := label]
    }
  }
  natural_field <- names(x)[
    normalise_token(names(x)) == normalise_token("new_natural")
  ]
  if (length(natural_field)) {
    x[suppressWarnings(as.numeric(get(natural_field[[1L]]))) == 0,
      nbd_cod_mech := "allinj"
    ]
  }
  x <- x[tolower(trimws(nbd_cod_mech)) != "unknown"]
  x[, nbd_cod_mech := gsub("\\.", "", trimws(nbd_cod_mech))]

  famhis_effective_n <- kish_effective_sample_size(x$weight)

  grouped <- x[, .(A = sum(weight, na.rm = TRUE)), by = .(
    Death_Prov, Sex, Popgroup, age5, nbd_cod_mech
  )]

  mechanism_columns <- c(
    "ZA126", "ZA127", "ZA128", "ZA129", "ZA130", "ZA131",
    "ZA1321", "ZA1322", "ZA133", "ZA134", "ZA135", "ZA136",
    "ZA137", "ZA138", "ZA1381", "ZA1382", "ZA1391", "ZA1392",
    "ZA140"
  )
  generic_columns <- c("trans", "suic", "homi", "unint", "allinj")
  allowed_mechanisms <- c(mechanism_columns, generic_columns)
  unexpected_mechanisms <- grouped[
    A > 1e-10 & !nbd_cod_mech %in% allowed_mechanisms,
    .(weighted_count = sum(A)),
    by = nbd_cod_mech
  ]
  if (nrow(unexpected_mechanisms)) {
    stop(
      "FAMHIS contains positive weighted mass in unmapped mechanism(s): ",
      paste(
        paste0(unexpected_mechanisms$nbd_cod_mech, "=", signif(unexpected_mechanisms$weighted_count, 8)),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  wide <- data.table::dcast(
    grouped,
    Death_Prov + Sex + Popgroup + age5 ~ nbd_cod_mech,
    value.var = "A",
    fill = 0
  )

  # Keep observed legal-intervention rows, but combine them with interpersonal
  # violence in the final 15-cause model. The legacy sequence allocated generic
  # homicide/all-injury mass to ZA140 and then dropped it, which systematically
  # depressed violence. Generic homicide is now assigned only to the two
  # observed violence mechanisms, and no survey mass is discarded.
  missing <- setdiff(c(mechanism_columns, generic_columns), names(wide))
  if (length(missing)) wide[, (missing) := 0]

  wide <- redistribute_named_source_with_reference(
    wide, "trans", c("ZA126", "ZA127"), copy = FALSE
  )
  wide <- redistribute_named_source_with_reference(
    wide, "suic", c("ZA138", "ZA1381", "ZA1382"), copy = FALSE
  )
  wide <- redistribute_named_source_with_reference(
    wide, "homi", c("ZA1391", "ZA1392"), copy = FALSE
  )
  wide <- redistribute_named_source_with_reference(
    wide,
    "unint",
    c(
      "ZA128", "ZA129", "ZA130", "ZA131", "ZA1321", "ZA1322",
      "ZA133", "ZA134", "ZA135", "ZA136", "ZA137"
    ),
    copy = FALSE
  )
  wide <- redistribute_named_source_with_reference(
    wide, "allinj", mechanism_columns, copy = FALSE
  )

  long <- data.table::melt(
    wide,
    id.vars = c("Death_Prov", "Sex", "Popgroup", "age5"),
    measure.vars = mechanism_columns,
    variable.name = "mechanism",
    value.name = "Inj2017",
    variable.factor = FALSE
  )
  map <- c(FAMHIS_MECHANISM_MAP, ZA140 = 141L)
  long[, nbdcode := as.integer(map[mechanism])]
  long <- long[!is.na(nbdcode)]
  long <- long[, .(Inj2017 = sum(Inj2017)), by = .(
    Death_Prov, Sex, Popgroup, age5, nbdcode
  )]

  long <- redistribute_unknown(
    long, "Sex", "Inj2017",
    by = c("Popgroup", "Death_Prov", "age5", "nbdcode"),
    unknown = 3L, targets = 1:2
  )
  long <- redistribute_unknown(
    long, "age5", "Inj2017",
    by = c("Popgroup", "Death_Prov", "Sex", "nbdcode"),
    unknown = 21L, targets = 1:20
  )
  long <- redistribute_unknown(
    long, "Popgroup", "Inj2017",
    by = c("age5", "Death_Prov", "Sex", "nbdcode"),
    unknown = 5L, targets = 1:4
  )
  # FAMHIS can contain records without province. The historical program carried
  # them as province 10 and they were subsequently outside the provincial
  # estimation grid. Allocate them across provinces using the observed
  # sex-population-group-age-cause distribution so the national survey mass is
  # retained in the nine-province model.
  long <- redistribute_unknown(
    long, "Death_Prov", "Inj2017",
    by = c("age5", "Sex", "Popgroup", "nbdcode"),
    unknown = 10L, targets = 1:9
  )

  out <- complete_cells(
    long,
    dimensions = c("Death_Prov", "Sex", "Popgroup", "age5", "nbdcode"),
    values = "Inj2017",
    levels = list(
      Death_Prov = 1:9,
      Sex = 1:2,
      Popgroup = 1:4,
      age5 = 1:20,
      nbdcode = INJURY_CODES
    )
  )
  expected_total <- sum(grouped$A, na.rm = TRUE)
  observed_total <- sum(out$Inj2017, na.rm = TRUE)
  assert_total_preserved(
    expected_total,
    observed_total,
    tolerance = 1e-8,
    label = "FAMHIS generic-mechanism redistribution"
  )
  out <- allocate_injury_effective_n(
    out,
    count_column = "Inj2017",
    effective_total = famhis_effective_n,
    output_column = "EffN2017"
  )
  out[]
}

prepare_injury_surveys <- function(cfg) {
  require_package("data.table")

  ims <- prepare_ims_2009(
    read_tabular(raw_file(cfg, "ims_2009", must_exist = TRUE))
  )
  nims_path <- raw_file(cfg, "nims_2000", must_exist = TRUE)
  nims_extension <- tolower(tools::file_ext(nims_path))
  nims_input <- if (nims_extension %in% c("xls", "xlsx")) {
    read_tabular(
      nims_path,
      sheet = cfg$settings$injury_nims_sheet %||% 1,
      .name_repair = "unique_quiet"
    )
  } else {
    read_tabular(nims_path)
  }
  nims_national <- prepare_nims_2000(
    nims_input,
    count_floor = as.numeric(
      cfg$settings$injury_nims_count_floor %||% 1e-6
    )
  )
  nims <- expand_nims_2000_with_ims(nims_national, ims)
  famhis <- prepare_famhis_2017(
    read_tabular(raw_file(cfg, "famhis_2017", must_exist = TRUE))
  )

  keys <- c("Death_Prov", "Sex", "Popgroup", "age5", "nbdcode")
  out <- merge(nims, ims, by = keys, all = TRUE, sort = FALSE)
  out <- merge(out, famhis, by = keys, all = TRUE, sort = FALSE)
  for (column in c("Inj2000", "Inj2009", "Inj2017")) {
    out[is.na(get(column)), (column) := 0]
  }
  for (column in c("EffN2000", "EffN2009", "EffN2017")) {
    out[is.na(get(column)) | !is.finite(get(column)) | get(column) < 0,
        (column) := 0]
  }
  out[is.na(nims_source_zero), nims_source_zero := TRUE]
  out[is.na(nims_spatial_share), nims_spatial_share := 1 / 36]
  out[is.na(nims_spatial_status), nims_spatial_status := "Equal 1/36 fallback"]

  data.table::setorderv(out, keys)
  assert_unique_key(out, keys, "harmonised NIMS-IMS-FAMHIS injury surveys")
  assert_nonnegative(out, c(
    "Inj2000", "Inj2009", "Inj2017",
    "EffN2000", "EffN2009", "EffN2017",
    "nims_spatial_share"
  ))
  out[]
}

# ------------------------------------------------------------------------------
# Interpolation, smoothing and injury allocation
# ------------------------------------------------------------------------------

# Injury cause-fraction model ---------------------------------------------------
#
# Stage 03 uses NIMS 2000, IMS 2009 and FAMHIS 2017. The surveys are not
# perfectly harmonised across detailed mechanisms, so they inform a smooth
# annual composition rather than acting as exact survey-year constraints.
#
# The production estimator is closed form and inexpensive: hierarchical
# additive log ratios are linearly interpolated between the three surveys,
# held constant outside the observed survey range, and then passed through a
# centred triangular moving average, equivalent to applying a centred three-year moving average twice. The annual composition is recovered with
# hierarchical softmax transformations.
#
# Joint injury uncertainty is generated by drawing each survey composition
# using its count/Kish-equivalent information size and rerunning the identical
# interpolation and smoothing calculation. No GAM is fitted and no additional
# between-survey variance parameter is introduced.

injury_model_parameters <- function(cfg) {
  settings <- cfg$settings %||% list()

  requested_method <- tolower(trimws(as.character(
    settings$injury_fraction_method %||%
      "nims_ims_famhis_alr_linear_triangular_ma"
  )))
  supported_method <- "nims_ims_famhis_alr_linear_triangular_ma"
  legacy_alias <- "nims_ims_famhis_robust_bayesian_gam"
  if (length(requested_method) != 1L || is.na(requested_method) ||
      !requested_method %in% c(supported_method, legacy_alias)) {
    stop(
      "Unsupported injury_fraction_method: ", requested_method,
      ". The production method is '", supported_method, "'.",
      call. = FALSE
    )
  }
  # The alias allows an existing targets store to reuse upstream COD outputs
  # when only Stage 03 is replaced. The saved artifact always records the
  # canonical fast method.
  method <- supported_method

  nims_year <- as.integer(settings$injury_nims_year %||% 2000L)
  ims_year <- as.integer(settings$injury_ims_year %||% 2009L)
  famhis_year <- as.integer(settings$injury_famhis_year %||% 2017L)
  start_year <- as.integer(settings$start_year %||% 1997L)
  end_year <- as.integer(settings$end_year %||% 2019L)
  fraction_floor <- as.numeric(settings$injury_fraction_floor %||% 1e-8)
  bias_floor <- as.numeric(settings$injury_bias_correction_floor %||% 1e-5)
  reference_code <- as.integer(settings$injury_alr_reference_code %||% 139L)
  smoothing_window <- as.integer(settings$injury_smoothing_window %||% 5L)

  if (!identical(c(nims_year, ims_year, famhis_year), c(2000L, 2009L, 2017L))) {
    stop(
      "The injury survey years are NIMS 2000, IMS 2009 and FAMHIS 2017.",
      call. = FALSE
    )
  }
  if (length(start_year) != 1L || length(end_year) != 1L ||
      is.na(start_year) || is.na(end_year) || start_year > nims_year ||
      end_year < famhis_year || start_year >= end_year) {
    stop("The injury modelling year range is invalid.", call. = FALSE)
  }
  if (!is.finite(fraction_floor) || length(fraction_floor) != 1L ||
      fraction_floor <= 0 || fraction_floor >= 1 / length(INJURY_CODES)) {
    stop("injury_fraction_floor is outside its valid range.", call. = FALSE)
  }
  if (!is.finite(bias_floor) || length(bias_floor) != 1L ||
      bias_floor <= 0 || bias_floor >= 1 / length(INJURY_CODES)) {
    stop("injury_bias_correction_floor is outside its valid range.", call. = FALSE)
  }
  if (length(reference_code) != 1L || is.na(reference_code) ||
      !reference_code %in% INJURY_CODES) {
    stop("injury_alr_reference_code must be one of INJURY_CODES.", call. = FALSE)
  }
  if (length(smoothing_window) != 1L || is.na(smoothing_window) ||
      smoothing_window < 3L || smoothing_window %% 2L != 1L) {
    stop("injury_smoothing_window must be an odd integer of at least 3.", call. = FALSE)
  }

  years <- seq.int(start_year, end_year)
  interpolation_matrix <- injury_interpolation_matrix(
    years,
    c(nims_year, ims_year, famhis_year)
  )
  smoothing_matrix <- injury_smoothing_matrix(years, smoothing_window)

  list(
    method = method,
    requested_method = requested_method,
    nims_year = nims_year,
    ims_year = ims_year,
    famhis_year = famhis_year,
    anchor_years = c(nims_year, ims_year, famhis_year),
    start_year = start_year,
    end_year = end_year,
    years = years,
    fraction_floor = fraction_floor,
    bias_floor = bias_floor,
    reference_code = reference_code,
    smoothing_window = smoothing_window,
    interpolation_matrix = interpolation_matrix,
    smoothing_matrix = smoothing_matrix,
    interpolation = "Piecewise linear in hierarchical additive-log-ratio space",
    smoother = paste0(
      smoothing_window,
      "-year centred triangular moving average in additive-log-ratio space"
    ),
    tail_policy = "Flat before NIMS 2000 and after FAMHIS 2017 before smoothing",
    survey_policy = "Survey inputs are influential observations, not exact constraints",
    uncertainty = paste(
      "Dirichlet survey-composition draws with total concentration equal to",
      "the available count/Kish effective sample size"
    )
  )
}

injury_age_midpoint <- function(age5) {
  midpoints <- c(
    1 / 24, 0.5, 2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5,
    42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5
  )
  value <- suppressWarnings(as.integer(age5))
  out <- rep(NA_real_, length(value))
  valid <- !is.na(value) & value >= 1L & value <= length(midpoints)
  out[valid] <- midpoints[value[valid]]
  out
}

injury_group_table <- function(cause_codes = INJURY_CODES) {
  cause_codes <- sort(as.integer(cause_codes))
  out <- data.table::data.table(nbdcode = cause_codes)
  out[, broad_group := data.table::fcase(
    nbdcode %in% INJURY_TRANSPORT_CODES, "transport",
    nbdcode %in% INJURY_OTHER_UNINTENTIONAL_CODES, "other_unintentional",
    nbdcode %in% INJURY_SELF_HARM_CODES, "self_harm",
    nbdcode %in% INJURY_VIOLENCE_CODES, "interpersonal_violence",
    default = NA_character_
  )]
  if (out[is.na(broad_group), .N]) {
    stop("An injury cause is not assigned to a broad injury group.", call. = FALSE)
  }
  out[, broad_order := match(
    broad_group,
    c("transport", "other_unintentional", "self_harm", "interpersonal_violence")
  )]
  out[]
}

injury_donor_composition <- function(
    counts,
    cause_codes = INJURY_CODES,
    strata = c("Death_Prov", "Sex", "Popgroup", "age5"),
    tolerance = 1e-12) {
  require_package("data.table")

  x <- data.table::as.data.table(data.table::copy(counts))
  assert_has_columns(x, c(strata, "nbdcode", "survey_count"), "injury counts")
  assert_unique_key(x, c(strata, "nbdcode"), "injury counts")
  if (!"survey_effective_n" %in% names(x)) x[, survey_effective_n := 0]
  x[, survey_effective_n := suppressWarnings(as.numeric(survey_effective_n))]
  x[!is.finite(survey_effective_n) | survey_effective_n < 0,
    survey_effective_n := 0]
  x <- x[nbdcode %in% cause_codes]

  fine_effective <- unique(x[, c(strata, "survey_effective_n"), with = FALSE])
  assert_unique_key(fine_effective, strata, "injury fine-stratum effective sample size")

  out <- x[, c(strata, "nbdcode"), with = FALSE]
  out[, `:=`(
    .row_id = .I,
    donor_fraction = NA_real_,
    donor_level = NA_character_,
    donor_effective_n = NA_real_
  )]

  reference_levels <- list(
    province_sex_age = c("Death_Prov", "Sex", "age5"),
    national_sex_age = c("Sex", "age5"),
    national_age = "age5",
    national = character()
  )

  for (level_name in names(reference_levels)) {
    by <- reference_levels[[level_name]]
    if (length(by)) {
      reference <- x[, .(
        reference_count = sum(survey_count, na.rm = TRUE)
      ), by = c(by, "nbdcode")]
      reference[, reference_total := sum(reference_count), by = by]
      reference[, candidate_fraction := data.table::fifelse(
        reference_total > tolerance,
        reference_count / reference_total,
        NA_real_
      )]
      reference_effective <- fine_effective[, .(
        donor_effective_n = sum(survey_effective_n, na.rm = TRUE)
      ), by = by]
      candidate <- merge(
        out[, c(".row_id", by, "nbdcode"), with = FALSE],
        reference[, c(by, "nbdcode", "reference_total", "candidate_fraction"), with = FALSE],
        by = c(by, "nbdcode"),
        all.x = TRUE,
        sort = FALSE
      )
      candidate <- merge(candidate, reference_effective, by = by, all.x = TRUE, sort = FALSE)
    } else {
      reference <- x[, .(
        reference_count = sum(survey_count, na.rm = TRUE)
      ), by = "nbdcode"]
      reference[, reference_total := sum(reference_count)]
      reference[, candidate_fraction := data.table::fifelse(
        reference_total > tolerance,
        reference_count / reference_total,
        NA_real_
      )]
      donor_effective_n <- sum(fine_effective$survey_effective_n, na.rm = TRUE)
      candidate <- merge(
        out[, .(.row_id, nbdcode)],
        reference[, .(nbdcode, reference_total, candidate_fraction)],
        by = "nbdcode",
        all.x = TRUE,
        sort = FALSE
      )
      candidate[, donor_effective_n := donor_effective_n]
    }

    candidate <- candidate[order(.row_id)]
    eligible <- is.na(out$donor_fraction) &
      is.finite(candidate$candidate_fraction) &
      candidate$reference_total > tolerance
    out[eligible, `:=`(
      donor_fraction = candidate$candidate_fraction[eligible],
      donor_level = level_name,
      donor_effective_n = candidate$donor_effective_n[eligible]
    )]
  }

  if (out[is.na(donor_fraction), .N]) {
    out[is.na(donor_fraction), `:=`(
      donor_fraction = 1 / length(cause_codes),
      donor_level = "equal_final_safeguard",
      donor_effective_n = 0
    )]
  }
  out[!is.finite(donor_effective_n) | donor_effective_n < 0,
    donor_effective_n := 0]
  out[, donor_fraction := donor_fraction / sum(donor_fraction), by = strata]
  out[, .row_id := NULL]
  out[]
}

apply_injury_anchor_corrections <- function(
    fractions,
    parameters,
    strata = c("Death_Prov", "Sex", "Popgroup", "age5"),
    tolerance = 1e-14) {
  require_package("data.table")

  x <- data.table::as.data.table(data.table::copy(fractions))
  key <- c(strata, "nbdcode")
  assert_has_columns(
    x,
    c(key, "year", "fraction_pre_correction", "total_count"),
    "injury anchor fractions"
  )
  assert_unique_key(x, c(key, "year"), "injury anchor fractions")
  if (!"nims_source_zero" %in% names(x)) {
    # Compatibility for historical unit tests or pre-flag inputs. Production
    # Stage 03 supplies the explicit pre-floor source status from NIMS.
    x[, nims_source_zero := year == parameters$nims_year &
        fraction_pre_correction <= tolerance]
  }
  zero_status <- unique(x[, c(key, "nims_source_zero"), with = FALSE])
  assert_unique_key(zero_status, key, "NIMS source-zero status")

  wide <- data.table::dcast(
    x,
    stats::as.formula(paste(
      paste(key, collapse = " + "),
      "~ year"
    )),
    value.var = "fraction_pre_correction"
  )
  year_columns <- as.character(parameters$anchor_years)
  if (length(setdiff(year_columns, names(wide)))) {
    stop("The injury correction table is missing one or more survey years.", call. = FALSE)
  }
  data.table::setnames(
    wide,
    year_columns,
    paste0("fraction_", year_columns)
  )
  wide <- merge(
    wide,
    zero_status,
    by = key,
    all.x = TRUE,
    sort = FALSE
  )
  wide[is.na(nims_source_zero), nims_source_zero := TRUE]

  f2000 <- paste0("fraction_", parameters$nims_year)
  f2009 <- paste0("fraction_", parameters$ims_year)
  f2017 <- paste0("fraction_", parameters$famhis_year)
  wide[, `:=`(
    correction_applied_2000 = FALSE,
    correction_applied_2017 = FALSE,
    correction_note_2000 = "Observed or derived survey fraction",
    correction_note_2017 = "Observed survey fraction"
  )]

  backward_132 <- wide$nbdcode == 132L &
    wide$nims_source_zero & wide[[f2009]] > tolerance
  backward_138 <- wide$nbdcode == 138L &
    wide$nims_source_zero & wide[[f2009]] > tolerance
  forward_136 <- wide$nbdcode == 136L &
    wide[[f2000]] > tolerance & wide[[f2009]] > tolerance

  if (any(backward_132)) {
    wide[backward_132, (f2000) := 2 * get(f2009) - get(f2017)]
    wide[backward_132, `:=`(
      correction_applied_2000 = TRUE,
      correction_note_2000 = "Code 132 backward correction: 2 x 2009 minus 2017"
    )]
  }
  if (any(backward_138)) {
    wide[backward_138, (f2000) := 2 * get(f2009) - get(f2017)]
    wide[backward_138, `:=`(
      correction_applied_2000 = TRUE,
      correction_note_2000 = "Code 138 backward correction: 2 x 2009 minus 2017"
    )]
  }
  if (any(forward_136)) {
    wide[forward_136, (f2017) := 2 * get(f2009) - get(f2000)]
    wide[forward_136, `:=`(
      correction_applied_2017 = TRUE,
      correction_note_2017 = "Code 136 forward correction: 2 x 2009 minus 2000"
    )]
  }

  # The source program applies a 1e-5 floor to these three cause-year pairs,
  # whether or not the extrapolation condition was activated.
  wide[nbdcode %in% c(132L, 138L), (f2000) := pmax(
    get(f2000), parameters$bias_floor
  )]
  wide[nbdcode == 136L, (f2017) := pmax(
    get(f2017), parameters$bias_floor
  )]

  corrected <- data.table::melt(
    wide,
    id.vars = c(
      key,
      "correction_applied_2000", "correction_applied_2017",
      "correction_note_2000", "correction_note_2017"
    ),
    measure.vars = paste0("fraction_", year_columns),
    variable.name = "year_field",
    value.name = "fraction_corrected",
    variable.factor = FALSE
  )
  corrected[, year := as.integer(sub("fraction_", "", year_field))]
  corrected[, year_field := NULL]
  corrected[, `:=`(
    correction_applied = data.table::fifelse(
      year == parameters$nims_year,
      correction_applied_2000,
      data.table::fifelse(
        year == parameters$famhis_year,
        correction_applied_2017,
        FALSE
      )
    ),
    correction_note = data.table::fifelse(
      year == parameters$nims_year,
      correction_note_2000,
      data.table::fifelse(
        year == parameters$famhis_year,
        correction_note_2017,
        "Observed survey fraction"
      )
    )
  )]
  corrected[, c(
    "correction_applied_2000", "correction_applied_2017",
    "correction_note_2000", "correction_note_2017"
  ) := NULL]

  out <- merge(
    x,
    corrected,
    by = c(key, "year"),
    all.x = TRUE,
    sort = FALSE
  )
  out[, fraction_corrected := pmax(fraction_corrected, 0)]
  out[, fraction_corrected := fraction_corrected / sum(fraction_corrected),
      by = eval(c(strata, "year"))]

  # The historical correction formulas are retained as audit fields, but the
  # production trajectory uses the survey/borrowed composition itself. This is
  # deliberate: causes 132, 136 and 138 are not defined identically across the
  # three surveys, so an extrapolated correction must not be treated as a
  # high-precision observed value. Zeros receive only the numerical floor and
  # therefore have large delta-method ALR variance and correspondingly low
  # model weight.
  out[, model_input_fraction := pmax(
    fraction_pre_correction,
    parameters$fraction_floor
  )]
  out[, model_input_fraction := model_input_fraction / sum(model_input_fraction),
      by = eval(c(strata, "year"))]
  out[, numerical_floor_applied :=
        fraction_pre_correction < parameters$fraction_floor]
  out[, fraction := model_input_fraction]
  out[, harmonised_count := fraction * total_count]
  out[, survey_count := harmonised_count]
  out[, model_input_policy :=
        "Raw or borrowed survey composition; legacy correction retained for audit only"]
  out[]
}

survey_counts_to_fractions <- function(
    surveys,
    nims_year = 2000L,
    ims_year = 2009L,
    famhis_year = 2017L,
    fraction_floor = 1e-8,
    bias_floor = 1e-5,
    reference_code = 139L) {
  require_package("data.table")

  keys <- c("Death_Prov", "Sex", "Popgroup", "age5", "nbdcode")
  count_columns <- c("Inj2000", "Inj2009", "Inj2017")
  effective_columns <- c("EffN2000", "EffN2009", "EffN2017")
  x <- data.table::as.data.table(data.table::copy(surveys))
  assert_has_columns(
    x,
    c(keys, count_columns, effective_columns, "nims_source_zero"),
    "harmonised injury surveys"
  )
  assert_unique_key(x, keys, "harmonised injury surveys")

  for (column in c(count_columns, effective_columns)) {
    x[, (column) := suppressWarnings(as.numeric(get(column)))]
    if (x[!is.finite(get(column)) | get(column) < 0, .N]) {
      stop("Injury survey values must be finite and non-negative: ", column,
           call. = FALSE)
    }
  }
  if (!"nims_spatial_status" %in% names(x)) {
    x[, nims_spatial_status := "IMS 2009 province-population-group fraction"]
  }

  parameters <- list(
    anchor_years = c(as.integer(nims_year), as.integer(ims_year), as.integer(famhis_year)),
    nims_year = as.integer(nims_year),
    ims_year = as.integer(ims_year),
    famhis_year = as.integer(famhis_year),
    fraction_floor = as.numeric(fraction_floor),
    bias_floor = as.numeric(bias_floor),
    reference_code = as.integer(reference_code)
  )

  long <- data.table::melt(
    x,
    id.vars = c(
      keys, effective_columns,
      "nims_spatial_status", "nims_source_zero"
    ),
    measure.vars = count_columns,
    variable.name = "source",
    value.name = "observed_count",
    variable.factor = FALSE
  )
  year_map <- c(
    Inj2000 = parameters$nims_year,
    Inj2009 = parameters$ims_year,
    Inj2017 = parameters$famhis_year
  )
  source_map <- c(
    Inj2000 = "NIMS 2000",
    Inj2009 = "IMS 2009",
    Inj2017 = "FAMHIS 2017"
  )
  long[, `:=`(
    year = as.integer(year_map[source]),
    survey = unname(source_map[source]),
    survey_effective_n = data.table::fcase(
      source == "Inj2000", EffN2000,
      source == "Inj2009", EffN2009,
      source == "Inj2017", EffN2017,
      default = 0
    )
  )]
  long[, c("source", effective_columns) := NULL]

  strata <- c("Death_Prov", "Sex", "Popgroup", "age5")
  long[, total_count := sum(observed_count), by = eval(c(strata, "year"))]
  long[, survey_effective_n := {
    value <- survey_effective_n[is.finite(survey_effective_n)]
    if (length(value)) max(value) else 0
  }, by = eval(c(strata, "year"))]
  long[, raw_fraction := data.table::fifelse(
    total_count > 0,
    observed_count / total_count,
    NA_real_
  )]

  pieces <- lapply(parameters$anchor_years, function(selected_year) {
    selected <- long[year == selected_year]
    selected[, survey_effective_n_raw := survey_effective_n]
    donor <- injury_donor_composition(
      selected[, .(
        Death_Prov, Sex, Popgroup, age5, nbdcode,
        survey_count = observed_count,
        survey_effective_n
      )],
      cause_codes = INJURY_CODES,
      strata = strata
    )
    selected <- merge(
      selected,
      donor,
      by = c(strata, "nbdcode"),
      all.x = TRUE,
      sort = FALSE
    )
    selected[, fraction_pre_correction := data.table::fifelse(
      total_count > 0,
      raw_fraction,
      donor_fraction
    )]
    selected[, fraction_source := data.table::fifelse(
      total_count > 0,
      "Survey fraction",
      paste0("Borrowed from ", donor_level)
    )]
    selected[, survey_effective_n := data.table::fifelse(
      total_count > 0,
      survey_effective_n_raw,
      donor_effective_n
    )]
    selected[, effective_n_source := data.table::fifelse(
      total_count > 0,
      "Fine survey stratum",
      paste0("Borrowed from ", donor_level)
    )]
    selected[]
  })
  prepared <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  prepared[, anchor_provenance := data.table::fifelse(
    year == parameters$nims_year,
    paste0("NIMS national composition; spatial allocation: ", nims_spatial_status),
    paste0(survey, " observed at province-population-group level")
  )]

  out <- apply_injury_anchor_corrections(
    prepared,
    parameters = parameters,
    strata = strata
  )
  closure <- out[, .(
    fraction_sum = sum(fraction),
    n_effective_sizes = data.table::uniqueN(survey_effective_n)
  ), by = eval(c(strata, "year"))]
  if (closure[abs(fraction_sum - 1) > 1e-12 | n_effective_sizes != 1L, .N]) {
    stop(
      "Harmonised injury anchors do not close or have inconsistent effective sample sizes.",
      call. = FALSE
    )
  }
  data.table::setorder(out, Death_Prov, Sex, Popgroup, age5, year, nbdcode)
  out[]
}



# Fast hierarchical ALR interpolation model -----------------------------------

injury_alr_variance <- function(effective_n, numerator, denominator) {
  n <- as.numeric(effective_n)
  p <- as.numeric(numerator)
  r <- as.numeric(denominator)
  out <- rep(Inf, length(n))
  valid <- is.finite(n) & n > 0 & is.finite(p) & p > 0 &
    is.finite(r) & r > 0
  out[valid] <- 1 / (n[valid] * p[valid]) + 1 / (n[valid] * r[valid])
  out
}

injury_stable_log_ratio <- function(numerator, denominator) {
  p <- as.numeric(numerator)
  r <- as.numeric(denominator)
  if (length(p) != length(r) ||
      any(!is.finite(p)) || any(p <= 0) ||
      any(!is.finite(r)) || any(r <= 0)) {
    stop(
      "Injury ALR inputs must be finite, positive, and have equal lengths.",
      call. = FALSE
    )
  }

  # Computing log(p / r) can overflow when a valid Dirichlet draw places a
  # component extremely close to zero. The equivalent log(p) - log(r) remains
  # finite for every positive representable p and r and does not alter the
  # statistical draw or the deterministic trajectory.
  out <- log(p) - log(r)
  if (any(!is.finite(out))) {
    stop("Injury ALR calculation produced a non-finite value.", call. = FALSE)
  }
  out
}

build_injury_hierarchical_observations <- function(
    observed,
    reference_code = 139L,
    strata = c("Death_Prov", "Sex", "Popgroup", "age5")) {
  require_package("data.table")

  x <- data.table::as.data.table(data.table::copy(observed))
  assert_has_columns(
    x,
    c(
      strata, "year", "nbdcode", "fraction", "harmonised_count",
      "total_count", "survey_effective_n"
    ),
    "injury survey fractions"
  )
  x[, survey_effective_n := as.numeric(survey_effective_n)]
  groups <- injury_group_table(INJURY_CODES)
  x <- merge(x, groups, by = "nbdcode", all.x = TRUE, sort = FALSE)

  broad_by <- c(strata, "year", "broad_group", "broad_order")
  broad <- x[, .(
    broad_fraction = sum(fraction),
    broad_count = sum(harmonised_count),
    total_count = max(total_count),
    survey_effective_n = max(survey_effective_n),
    survey = if ("survey" %in% names(x)) as.character(survey[[1L]]) else as.character(year[[1L]])
  ), by = broad_by]
  reference_group <- groups[nbdcode == reference_code, broad_group][[1L]]
  broad_reference <- broad[broad_group == reference_group, .(
    broad_reference_fraction = broad_fraction
  ), by = eval(c(strata, "year"))]
  broad <- merge(
    broad,
    broad_reference,
    by = c(strata, "year"),
    all.x = TRUE,
    sort = FALSE
  )
  broad[, `:=`(
    log_ratio = injury_stable_log_ratio(
      broad_fraction,
      broad_reference_fraction
    ),
    is_reference = broad_group == reference_group
  )]
  broad[, alr_variance := injury_alr_variance(
    survey_effective_n,
    broad_fraction,
    broad_reference_fraction
  )]
  broad[, `:=`(
    alr_se = sqrt(alr_variance),
    reference_group = reference_group
  )]
  broad[is_reference == TRUE, `:=`(
    log_ratio = 0,
    alr_variance = 0,
    alr_se = 0
  )]

  within_reference <- groups[, .(
    within_reference_code = if (reference_code %in% nbdcode) {
      reference_code
    } else {
      max(nbdcode)
    }
  ), by = broad_group]
  x <- merge(x, within_reference, by = "broad_group", all.x = TRUE, sort = FALSE)
  x[, within_fraction := fraction / sum(fraction),
    by = eval(c(strata, "year", "broad_group"))]
  reference <- x[nbdcode == within_reference_code, .(
    within_reference_fraction = within_fraction,
    within_reference_overall_fraction = fraction
  ), by = c(strata, "year", "broad_group")]
  within <- merge(
    x,
    reference,
    by = c(strata, "year", "broad_group"),
    all.x = TRUE,
    sort = FALSE
  )
  within[, `:=`(
    log_ratio = injury_stable_log_ratio(
      within_fraction,
      within_reference_fraction
    ),
    is_reference = nbdcode == within_reference_code
  )]
  within[, alr_variance := injury_alr_variance(
    survey_effective_n,
    fraction,
    within_reference_overall_fraction
  )]
  within[, alr_se := sqrt(alr_variance)]
  within[is_reference == TRUE, `:=`(
    log_ratio = 0,
    alr_variance = 0,
    alr_se = 0
  )]

  list(
    source = x[],
    broad = broad[],
    within = within[],
    reference_group = reference_group,
    within_reference = within_reference[]
  )
}

injury_interpolation_matrix <- function(years, anchor_years) {
  years <- as.integer(years)
  anchors <- as.integer(anchor_years)
  if (length(anchors) != 3L || any(!is.finite(anchors)) ||
      any(diff(anchors) <= 0)) {
    stop("Injury interpolation requires three increasing survey years.", call. = FALSE)
  }
  if (!length(years) || any(!is.finite(years))) {
    stop("Injury interpolation years are invalid.", call. = FALSE)
  }

  out <- matrix(0, nrow = length(years), ncol = 3L)
  for (i in seq_along(years)) {
    year <- years[[i]]
    if (year <= anchors[[1L]]) {
      out[i, ] <- c(1, 0, 0)
    } else if (year <= anchors[[2L]]) {
      weight <- (year - anchors[[1L]]) / (anchors[[2L]] - anchors[[1L]])
      out[i, ] <- c(1 - weight, weight, 0)
    } else if (year < anchors[[3L]]) {
      weight <- (year - anchors[[2L]]) / (anchors[[3L]] - anchors[[2L]])
      out[i, ] <- c(0, 1 - weight, weight)
    } else {
      out[i, ] <- c(0, 0, 1)
    }
  }
  rownames(out) <- as.character(years)
  colnames(out) <- as.character(anchors)
  out
}

injury_triangular_kernel <- function(window) {
  window <- as.integer(window)
  if (length(window) != 1L || is.na(window) || window < 3L ||
      window %% 2L != 1L) {
    stop("The injury smoothing window must be an odd integer of at least 3.", call. = FALSE)
  }
  half <- (window - 1L) %/% 2L
  c(seq_len(half + 1L), seq.int(half, 1L))
}

injury_smoothing_matrix <- function(years, window = 5L) {
  years <- as.integer(years)
  if (length(years) < 3L || any(diff(years) != 1L)) {
    stop("Injury smoothing years must be consecutive integers.", call. = FALSE)
  }
  kernel <- injury_triangular_kernel(window)
  half <- (length(kernel) - 1L) %/% 2L
  out <- matrix(0, nrow = length(years), ncol = length(years))
  for (i in seq_along(years)) {
    offsets <- seq.int(-half, half)
    positions <- i + offsets
    keep <- positions >= 1L & positions <= length(years)
    weights <- kernel[keep]
    weights <- weights / sum(weights)
    out[i, positions[keep]] <- weights
  }
  rownames(out) <- as.character(years)
  colnames(out) <- as.character(years)
  out
}

injury_trajectory_matrices <- function(anchor_matrix, parameters) {
  anchor_matrix <- as.matrix(anchor_matrix)
  if (ncol(anchor_matrix) != length(parameters$anchor_years) ||
      any(!is.finite(anchor_matrix))) {
    stop("Injury anchor log-ratio matrix is incomplete or non-finite.", call. = FALSE)
  }
  linear <- anchor_matrix %*% t(parameters$interpolation_matrix)
  smoothed <- linear %*% t(parameters$smoothing_matrix)
  colnames(linear) <- as.character(parameters$years)
  colnames(smoothed) <- as.character(parameters$years)
  list(linear = linear, smoothed = smoothed)
}

injury_anchor_matrix <- function(data, id_columns, anchor_years) {
  x <- data.table::as.data.table(data.table::copy(data))
  assert_has_columns(x, c(id_columns, "year", "log_ratio"), "injury ALR anchors")
  assert_unique_key(x, c(id_columns, "year"), "injury ALR anchors")
  formula <- stats::as.formula(paste(
    paste(id_columns, collapse = " + "),
    "~ year"
  ))
  wide <- data.table::dcast(x, formula, value.var = "log_ratio")
  year_columns <- as.character(as.integer(anchor_years))
  missing <- setdiff(year_columns, names(wide))
  if (length(missing)) {
    stop(
      "Injury ALR anchors are missing survey year(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  matrix <- as.matrix(wide[, ..year_columns])
  storage.mode(matrix) <- "double"
  if (any(!is.finite(matrix))) {
    stop("Injury ALR anchor matrix contains non-finite values.", call. = FALSE)
  }
  list(ids = wide[, ..id_columns], anchors = matrix)
}

injury_matrix_to_long <- function(ids, matrix, years, value_name) {
  x <- data.table::as.data.table(data.table::copy(ids))
  values <- data.table::as.data.table(matrix)
  data.table::setnames(values, as.character(as.integer(years)))
  x <- cbind(x, values)
  data.table::melt(
    x,
    id.vars = names(ids),
    measure.vars = as.character(as.integer(years)),
    variable.name = "year",
    value.name = value_name,
    variable.factor = FALSE
  )[, year := as.integer(year)][]
}

injury_interpolate_trajectory_table <- function(
    anchors,
    id_columns,
    parameters,
    trajectory_level) {
  packed <- injury_anchor_matrix(anchors, id_columns, parameters$anchor_years)
  matrices <- injury_trajectory_matrices(packed$anchors, parameters)
  linear <- injury_matrix_to_long(
    packed$ids,
    matrices$linear,
    parameters$years,
    "log_ratio_linear"
  )
  smoothed <- injury_matrix_to_long(
    packed$ids,
    matrices$smoothed,
    parameters$years,
    "log_ratio_smoothed"
  )
  out <- merge(
    linear,
    smoothed,
    by = c(id_columns, "year"),
    all = TRUE,
    sort = FALSE
  )
  if (out[!is.finite(log_ratio_linear) | !is.finite(log_ratio_smoothed), .N]) {
    stop("Annual injury ALR trajectory contains non-finite values.", call. = FALSE)
  }
  out[, `:=`(
    trajectory_level = trajectory_level,
    record_type = "Linear ALR interpolation plus triangular moving average",
    interpolation = "Piecewise linear in ALR space",
    smoother = paste0(parameters$smoothing_window, "-year centred triangular moving average"),
    tail_policy = "Flat survey-boundary tails before smoothing"
  )]
  data.table::setorderv(out, c(id_columns, "year"))
  out[]
}

injury_build_annual_log_ratios <- function(observed, cfg, parameters = NULL) {
  parameters <- parameters %||% injury_model_parameters(cfg)
  observations <- build_injury_hierarchical_observations(
    observed,
    reference_code = parameters$reference_code
  )
  strata <- c("Death_Prov", "Sex", "Popgroup", "age5")

  broad_anchors <- observations$broad[is_reference == FALSE, .(
    Death_Prov, Sex, Popgroup, age5, year,
    broad_group, log_ratio
  )]
  broad <- injury_interpolate_trajectory_table(
    broad_anchors,
    id_columns = c(strata, "broad_group"),
    parameters = parameters,
    trajectory_level = "Broad group"
  )

  within_anchors <- observations$within[is_reference == FALSE, .(
    Death_Prov, Sex, Popgroup, age5, year,
    broad_group, nbdcode, within_reference_code, log_ratio
  )]
  within <- injury_interpolate_trajectory_table(
    within_anchors,
    id_columns = c(strata, "broad_group", "nbdcode", "within_reference_code"),
    parameters = parameters,
    trajectory_level = "Within broad group"
  )

  list(
    observations = observations,
    broad = broad[],
    within = within[]
  )
}

injury_compose_annual_fractions <- function(annual, parameters) {
  require_package("data.table")
  strata_year <- c("Death_Prov", "Sex", "Popgroup", "age5", "year")
  broad_groups <- c(
    "transport", "other_unintentional", "self_harm",
    "interpersonal_violence"
  )

  broad <- annual$broad[, c(
    strata_year, "broad_group", "log_ratio_smoothed"
  ), with = FALSE]
  data.table::setnames(broad, "log_ratio_smoothed", "log_ratio")
  broad_keys <- unique(broad[, ..strata_year])
  broad_reference <- data.table::copy(broad_keys)
  broad_reference[, `:=`(
    broad_group = annual$observations$reference_group,
    log_ratio = 0
  )]
  broad <- data.table::rbindlist(
    list(broad, broad_reference),
    use.names = TRUE,
    fill = TRUE
  )
  broad <- broad[broad_group %in% broad_groups]
  assert_unique_key(broad, c(strata_year, "broad_group"), "annual broad injury ALRs")
  broad[, score__ := exp(log_ratio - max(log_ratio)), by = strata_year]
  broad[, broad_fraction := score__ / sum(score__), by = strata_year]
  broad[, score__ := NULL]

  within <- annual$within[, c(
    strata_year, "broad_group", "nbdcode", "log_ratio_smoothed"
  ), with = FALSE]
  data.table::setnames(within, "log_ratio_smoothed", "log_ratio")
  within_reference <- data.table::copy(annual$observations$within_reference)
  within_reference[, nbdcode := as.integer(within_reference_code)]
  within_reference[, within_reference_code := NULL]
  reference_rows <- cross_join_dt(broad_keys, within_reference)
  reference_rows[, log_ratio := 0]
  within <- data.table::rbindlist(
    list(within, reference_rows),
    use.names = TRUE,
    fill = TRUE
  )
  groups <- injury_group_table(INJURY_CODES)[, .(broad_group, nbdcode)]
  within <- merge(
    within,
    groups,
    by = c("broad_group", "nbdcode"),
    all.x = TRUE,
    sort = FALSE
  )
  if (within[is.na(year) | !is.finite(log_ratio), .N]) {
    stop("The annual within-group injury ALR panel is incomplete.", call. = FALSE)
  }
  assert_unique_key(
    within,
    c(strata_year, "broad_group", "nbdcode"),
    "annual within-group injury ALRs"
  )
  within[, score__ := exp(log_ratio - max(log_ratio)),
         by = c(strata_year, "broad_group")]
  within[, within_fraction := score__ / sum(score__),
         by = c(strata_year, "broad_group")]
  within[, score__ := NULL]

  fractions <- merge(
    within,
    broad[, c(strata_year, "broad_group", "broad_fraction"), with = FALSE],
    by = c(strata_year, "broad_group"),
    all.x = TRUE,
    sort = FALSE
  )
  fractions[, cf_final := broad_fraction * within_fraction]
  fractions[, cf_final := cf_final / sum(cf_final), by = strata_year]
  fractions[, `:=`(
    broad_order = match(broad_group, broad_groups),
    period = paste0(
      "Linear ALR interpolation + ",
      parameters$smoothing_window,
      "-year triangular moving average"
    )
  )]
  data.table::setorder(
    fractions,
    year, Death_Prov, Popgroup, Sex, age5, broad_order, nbdcode
  )
  fractions[]
}

injury_fraction_panel_from_surveys <- function(
    observed,
    cfg,
    parameters = NULL,
    return_details = FALSE) {
  parameters <- parameters %||% injury_model_parameters(cfg)
  annual <- injury_build_annual_log_ratios(observed, cfg, parameters)
  fractions <- injury_compose_annual_fractions(annual, parameters)
  validate_injury_fraction_panel(fractions, cfg)
  if (isTRUE(return_details)) {
    return(list(annual = annual, fractions = fractions))
  }
  fractions
}

injury_draw_survey_compositions <- function(source_fractions, seed) {
  require_package("data.table")
  if (length(seed) != 1L || !is.finite(as.numeric(seed))) {
    stop("A finite injury survey seed is required.", call. = FALSE)
  }
  set.seed(as.integer(seed))
  x <- data.table::as.data.table(data.table::copy(source_fractions))
  group <- c("Death_Prov", "Sex", "Popgroup", "age5", "year")
  assert_has_columns(
    x,
    c(
      group, "nbdcode", "fraction", "survey_effective_n", "total_count",
      "fraction_source", "donor_level"
    ),
    "Stage 03 survey composition artifact"
  )
  assert_unique_key(x, c(group, "nbdcode"), "Stage 03 survey composition artifact")

  x[, sampling_group__ := data.table::fcase(
    fraction_source == "Survey fraction",
    paste("fine", year, Death_Prov, Sex, Popgroup, age5, sep = "|"),
    donor_level == "province_sex_age",
    paste("province_sex_age", year, Death_Prov, Sex, age5, sep = "|"),
    donor_level == "national_sex_age",
    paste("national_sex_age", year, Sex, age5, sep = "|"),
    donor_level == "national_age",
    paste("national_age", year, age5, sep = "|"),
    donor_level == "national",
    paste("national", year, sep = "|"),
    default = paste("fixed", year, Death_Prov, Sex, Popgroup, age5, sep = "|")
  )]

  basis <- x[, .(
    fraction__ = mean(as.numeric(fraction)),
    fraction_range__ = max(as.numeric(fraction)) - min(as.numeric(fraction)),
    effective_n__ = mean(as.numeric(survey_effective_n)),
    effective_n_range__ = max(as.numeric(survey_effective_n)) -
      min(as.numeric(survey_effective_n))
  ), by = .(sampling_group__, nbdcode)]
  if (basis[
    !is.finite(fraction__) | fraction__ <= 0 |
      !is.finite(effective_n__) | effective_n__ < 0 |
      fraction_range__ > 1e-12 | effective_n_range__ > 1e-8,
    .N
  ]) {
    stop(
      "Borrowed injury survey compositions are inconsistent within a shared donor group.",
      call. = FALSE
    )
  }
  basis[, fraction__ := fraction__ / sum(fraction__), by = sampling_group__]
  basis[, draw_fraction__ := {
    p <- as.numeric(fraction__)
    p <- pmax(p, .Machine$double.eps)
    p <- p / sum(p)
    n <- suppressWarnings(as.numeric(effective_n__[[1L]]))
    if (!is.finite(n) || n <= 0) {
      p
    } else {
      shape <- pmax(p * n, .Machine$double.eps)
      value <- stats::rgamma(.N, shape = shape, rate = 1)
      value[!is.finite(value) | value < 0] <- 0
      value <- pmax(value, .Machine$double.xmin)
      total <- sum(value)
      if (!is.finite(total) || total <= 0) p else value / total
    }
  }, by = sampling_group__]

  x <- merge(
    x,
    basis[, .(sampling_group__, nbdcode, draw_fraction__)],
    by = c("sampling_group__", "nbdcode"),
    all.x = TRUE,
    sort = FALSE
  )
  if (x[!is.finite(draw_fraction__) | draw_fraction__ <= 0, .N]) {
    stop("The injury survey-composition draw is incomplete.", call. = FALSE)
  }
  x[, fraction := draw_fraction__]
  x[, c("sampling_group__", "draw_fraction__") := NULL]
  x[, harmonised_count := fraction * total_count]
  x[, survey_count := harmonised_count]
  data.table::setorder(x, Death_Prov, Sex, Popgroup, age5, year, nbdcode)
  x[]
}

injury_compose_fraction_panel <- function(artifact, survey_fractions = NULL) {
  if (!is.list(artifact) || is.null(artifact$parameters) ||
      is.null(artifact$source_fractions) || is.null(artifact$start_year) ||
      is.null(artifact$end_year)) {
    stop("The Stage 03 interpolation artifact is incomplete.", call. = FALSE)
  }
  observed <- survey_fractions %||% artifact$source_fractions
  cfg <- list(settings = list(
    start_year = artifact$start_year,
    end_year = artifact$end_year,
    injury_fraction_method = artifact$parameters$method,
    injury_nims_year = artifact$parameters$nims_year,
    injury_ims_year = artifact$parameters$ims_year,
    injury_famhis_year = artifact$parameters$famhis_year,
    injury_fraction_floor = artifact$parameters$fraction_floor,
    injury_bias_correction_floor = artifact$parameters$bias_floor,
    injury_alr_reference_code = artifact$parameters$reference_code,
    injury_smoothing_window = artifact$parameters$smoothing_window
  ))
  injury_fraction_panel_from_surveys(
    observed,
    cfg,
    parameters = artifact$parameters,
    return_details = FALSE
  )
}

injury_draw_fraction_panel <- function(artifact, seed) {
  drawn_surveys <- injury_draw_survey_compositions(
    artifact$source_fractions,
    seed = seed
  )
  injury_compose_fraction_panel(artifact, drawn_surveys)
}

injury_model_summary_table <- function(details, parameters) {
  broad <- unique(details$annual$broad[, .(
    model_id = paste0("broad:", broad_group),
    trajectory_level,
    broad_group,
    nbdcode = NA_integer_
  )])
  within <- unique(details$annual$within[, .(
    model_id = paste0("within:", broad_group, ":", nbdcode),
    trajectory_level,
    broad_group,
    nbdcode = as.integer(nbdcode)
  )])
  out <- data.table::rbindlist(list(broad, within), use.names = TRUE, fill = TRUE)
  out[, `:=`(
    method = parameters$method,
    interpolation = "Piecewise linear in hierarchical ALR space",
    smoother = paste0(parameters$smoothing_window, "-year centred triangular moving average"),
    tail_policy = "Flat survey-boundary tails before smoothing",
    survey_years = paste(parameters$anchor_years, collapse = ","),
    uncertainty = "Dirichlet survey-composition draws using count/Kish effective N",
    fit_status = "closed_form"
  )]
  data.table::setorder(out, trajectory_level, broad_group, nbdcode)
  out[]
}

fit_predict_injury_interpolation_panel <- function(observed, cfg) {
  parameters <- injury_model_parameters(cfg)
  details <- injury_fraction_panel_from_surveys(
    observed,
    cfg,
    parameters = parameters,
    return_details = TRUE
  )
  artifact <- list(
    version = "injury-alr-linear-triangular-ma-v1",
    parameters = parameters,
    start_year = as.integer(cfg$settings$start_year),
    end_year = as.integer(cfg$settings$end_year),
    source_fractions = data.table::copy(observed),
    trajectory_count = 14L
  )
  list(
    parameters = parameters,
    details = details,
    artifact = artifact,
    fractions = details$fractions,
    model_summaries = injury_model_summary_table(details, parameters)
  )
}

summarise_injury_logit_trends <- function(
    observed,
    reference_code = 139L,
    strata = c("Death_Prov", "Sex", "Popgroup", "age5")) {
  anchors <- build_injury_hierarchical_observations(
    observed,
    reference_code = reference_code,
    strata = strata
  )
  broad <- data.table::copy(anchors$broad)
  broad[, `:=`(
    trajectory_level = "Broad group",
    nbdcode = NA_integer_,
    reference = anchors$reference_group,
    record_type = "Survey ALR observation"
  )]
  within <- data.table::copy(anchors$within)
  within[, `:=`(
    trajectory_level = "Within broad group",
    reference = as.character(within_reference_code),
    record_type = "Survey ALR observation"
  )]
  data.table::rbindlist(
    list(broad, within),
    use.names = TRUE,
    fill = TRUE
  )[]
}

injury_survey_model_comparison <- function(source_fractions, fractions) {
  require_package("data.table")
  key <- c("Death_Prov", "Sex", "Popgroup", "age5", "year", "nbdcode")
  source <- data.table::as.data.table(data.table::copy(source_fractions))
  model <- data.table::as.data.table(data.table::copy(fractions))[
    year %in% sort(unique(source$year)),
    c(key, "cf_final"),
    with = FALSE
  ]
  data.table::setnames(model, "cf_final", "model_fraction")
  keep <- intersect(c(
    key, "survey", "observed_count", "total_count", "survey_effective_n",
    "survey_effective_n_raw", "raw_fraction", "fraction_pre_correction",
    "fraction_corrected", "fraction", "correction_applied",
    "correction_note", "fraction_source", "effective_n_source"
  ), names(source))
  out <- merge(
    source[, ..keep],
    model,
    by = key,
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setnames(out, "fraction", "survey_fraction", skip_absent = TRUE)
  out[, `:=`(
    difference = model_fraction - survey_fraction,
    absolute_difference = abs(model_fraction - survey_fraction)
  )]
  data.table::setorder(out, year, Death_Prov, Sex, Popgroup, age5, nbdcode)
  out[]
}

validate_injury_fraction_panel <- function(fractions, cfg, tolerance = 1e-10) {
  require_package("data.table")

  x <- data.table::as.data.table(fractions)
  key <- c("Death_Prov", "Sex", "Popgroup", "age5", "year", "nbdcode")
  assert_has_columns(x, c(key, "cf_final"), "modelled injury fractions")
  assert_unique_key(x, key, "modelled injury fractions")
  if (x[!is.finite(cf_final) | cf_final <= 0, .N]) {
    stop("Modelled injury fractions must be finite and strictly positive.", call. = FALSE)
  }

  expected_years <- seq.int(
    as.integer(cfg$settings$start_year),
    as.integer(cfg$settings$end_year)
  )
  expected_levels <- list(
    Death_Prov = 1:9,
    Sex = 1:2,
    Popgroup = 1:4,
    age5 = 1:20,
    year = expected_years,
    nbdcode = sort(INJURY_CODES)
  )
  for (column in names(expected_levels)) {
    observed_levels <- sort(unique(as.integer(x[[column]])))
    if (!identical(observed_levels, as.integer(expected_levels[[column]]))) {
      stop("Modelled injury fractions have unexpected levels for ", column, ".", call. = FALSE)
    }
  }

  closure <- x[, .(
    n_causes = data.table::uniqueN(nbdcode),
    fraction_sum = sum(cf_final)
  ), by = .(Death_Prov, Sex, Popgroup, age5, year)]
  if (closure[n_causes != length(INJURY_CODES), .N] ||
      closure[abs(fraction_sum - 1) > tolerance, .N]) {
    stop("Modelled injury compositions are incomplete or do not close.", call. = FALSE)
  }
  invisible(TRUE)
}

fit_injury_fraction_model <- function(surveys, cfg) {
  require_package("data.table")

  parameters <- injury_model_parameters(cfg)
  message(
    "[Stage 03 injury] Preparing NIMS 2000, IMS 2009 and FAMHIS 2017 ",
    "cause compositions..."
  )
  observed <- survey_counts_to_fractions(
    surveys,
    nims_year = parameters$nims_year,
    ims_year = parameters$ims_year,
    famhis_year = parameters$famhis_year,
    fraction_floor = parameters$fraction_floor,
    bias_floor = parameters$bias_floor,
    reference_code = parameters$reference_code
  )
  message(
    "[Stage 03 injury] Applying hierarchical ALR linear interpolation and the ",
    parameters$smoothing_window,
    "-year triangular moving average..."
  )
  fitted <- fit_predict_injury_interpolation_panel(observed, cfg)
  fractions <- fitted$fractions
  message(
    "[Stage 03 injury] Completed closed-form injury fractions: ",
    format(nrow(fractions), big.mark = ",", scientific = FALSE),
    " rows."
  )
  survey_comparison <- injury_survey_model_comparison(observed, fractions)

  survey_alr <- summarise_injury_logit_trends(
    observed,
    reference_code = parameters$reference_code
  )
  annual_broad <- data.table::copy(fitted$details$annual$broad)
  annual_broad[, `:=`(
    nbdcode = NA_integer_,
    reference = fitted$details$annual$observations$reference_group
  )]
  annual_within <- data.table::copy(fitted$details$annual$within)
  annual_within[, reference := as.character(within_reference_code)]
  model_summary <- data.table::copy(fitted$model_summaries)
  model_summary[, record_type := "Interpolation specification"]
  logit_trends <- data.table::rbindlist(
    list(survey_alr, annual_broad, annual_within, model_summary),
    use.names = TRUE,
    fill = TRUE
  )

  list(
    parameters = parameters,
    source_fractions = observed,
    logit_trends = logit_trends[],
    model_summaries = fitted$model_summaries,
    model_artifact = fitted$artifact,
    survey_model_comparison = survey_comparison,
    fractions = fractions[]
  )
}

estimate_injury_fractions <- function(surveys, cfg) {
  fit_injury_fraction_model(surveys, cfg)$fractions
}


apply_injury_fractions <- function(injury_cod, fractions) {
  require_package("data.table")

  mortality <- data.table::as.data.table(data.table::copy(injury_cod))
  assert_has_columns(
    mortality,
    c("Death_Prov", "Sex", "Popgroup", "age5", "DeathYear", "Deaths"),
    "injury mortality envelope"
  )
  mortality[, Deaths := suppressWarnings(as.numeric(Deaths))]
  if (mortality[!is.finite(Deaths) | Deaths < 0, .N]) {
    stop("Injury mortality counts must be finite and non-negative.", call. = FALSE)
  }

  totals <- mortality[, .(
    total_injury_deaths = sum(Deaths)
  ), by = .(Death_Prov, Sex, Popgroup, age5, DeathYear)]
  keys_total <- c("Death_Prov", "Sex", "Popgroup", "age5", "DeathYear")

  f <- data.table::as.data.table(data.table::copy(fractions))
  assert_has_columns(
    f,
    c("Death_Prov", "Sex", "Popgroup", "age5", "year", "nbdcode", "cf_final"),
    "injury fractions"
  )
  f <- f[, .(
    Death_Prov, Sex, Popgroup, age5,
    DeathYear = as.integer(year),
    nbdcode,
    cf_final
  )]
  assert_unique_key(
    f,
    c(keys_total, "nbdcode"),
    "injury fractions"
  )
  if (f[!is.finite(cf_final) | cf_final <= 0, .N]) {
    stop("Injury fractions must be finite and strictly positive.", call. = FALSE)
  }
  fraction_check <- f[, .(
    fraction_sum = sum(cf_final)
  ), by = keys_total]
  if (fraction_check[abs(fraction_sum - 1) > 1e-10, .N]) {
    stop("Injury fractions must sum to one within every mortality stratum.", call. = FALSE)
  }

  out <- merge(
    totals,
    f,
    by = keys_total,
    all.x = TRUE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  missing_strata <- out[is.na(cf_final), unique(.SD), .SDcols = keys_total]
  if (nrow(missing_strata)) {
    stop(
      "Injury fractions are missing for ", nrow(missing_strata),
      " mortality stratum/strata. First missing row: ",
      paste(unlist(missing_strata[1]), collapse = "/"),
      call. = FALSE
    )
  }
  out[, Deaths := total_injury_deaths * cf_final]

  envelope_check <- out[, .(
    expected_deaths = unique(total_injury_deaths),
    n_expected_values = data.table::uniqueN(total_injury_deaths),
    allocated_deaths = sum(Deaths)
  ), by = keys_total]
  if (envelope_check[n_expected_values != 1L, .N]) {
    stop("The injury mortality envelope is not unique by stratum.", call. = FALSE)
  }
  envelope_check[, tolerance := 1e-8 * pmax(1, abs(expected_deaths))]
  if (envelope_check[
    abs(allocated_deaths - expected_deaths) > tolerance,
    .N
  ]) {
    stop("Cause allocation did not preserve every injury mortality stratum.", call. = FALSE)
  }

  assert_total_preserved(
    sum(totals$total_injury_deaths),
    sum(out$Deaths),
    tolerance = 1e-8,
    label = "injury mortality envelope"
  )
  out[, `:=`(cf_final = NULL, total_injury_deaths = NULL, Inj = 1L)]
  out <- out[, .(
    Death_Prov, Sex, DeathYear, Popgroup, age5, nbdcode, Deaths, Inj
  )]
  assert_nonnegative(out, "Deaths")
  assert_unique_key(
    out,
    c("Death_Prov", "Sex", "DeathYear", "Popgroup", "age5", "nbdcode", "Inj"),
    "final injury estimates"
  )
  out[]
}


injury_model_diagnostics <- function(
    surveys,
    fractions,
    cfg = NULL,
    fitted_model = NULL) {
  require_package("data.table")

  if (is.null(cfg)) cfg <- list(settings = list(
    start_year = min(fractions$year),
    end_year = max(fractions$year),
    injury_fraction_method = "nims_ims_famhis_alr_linear_triangular_ma",
    injury_nims_year = 2000L,
    injury_ims_year = 2009L,
    injury_famhis_year = 2017L,
    injury_fraction_floor = 1e-8,
    injury_bias_correction_floor = 1e-5,
    injury_alr_reference_code = 139L,
    injury_smoothing_window = 5L
  ))
  model <- fitted_model %||% fit_injury_fraction_model(surveys, cfg)
  parameters <- model$parameters
  observed <- model$source_fractions

  source_summary <- observed[, .(
    observed_count = sum(observed_count, na.rm = TRUE),
    harmonised_count = sum(harmonised_count, na.rm = TRUE),
    effective_n = max(survey_effective_n, na.rm = TRUE),
    cells = .N,
    empty_cells_borrowed = sum(grepl("^Borrowed", fraction_source)),
    bias_corrected_cells = sum(correction_applied, na.rm = TRUE),
    numerical_floor_cells = sum(numerical_floor_applied, na.rm = TRUE)
  ), by = .(survey, year)]
  source_summary[, record_type := "Survey input"]
  data.table::setcolorder(source_summary, c("record_type", "survey", "year"))

  comparison <- model$survey_model_comparison %||%
    injury_survey_model_comparison(observed, fractions)
  survey_fit <- comparison[, .(
    mean_absolute_difference = mean(absolute_difference, na.rm = TRUE),
    p95_absolute_difference = as.numeric(stats::quantile(
      absolute_difference, 0.95, na.rm = TRUE
    )),
    maximum_absolute_difference = max(absolute_difference, na.rm = TRUE)
  ), by = .(survey, year)]
  survey_fit[, record_type := "Survey-model comparison"]
  data.table::setcolorder(survey_fit, c("record_type", "survey", "year"))

  closure <- fractions[, .(fraction_sum = sum(cf_final)),
    by = .(Death_Prov, Sex, Popgroup, age5, year)]
  panel_summary <- data.table::data.table(
    record_type = "Modelled panel",
    method = parameters$method,
    interpolation = "Piecewise linear in hierarchical ALR space",
    smoother = paste0(parameters$smoothing_window, "-year centred triangular moving average"),
    tail_policy = "Flat survey-boundary tails before smoothing",
    start_year = min(fractions$year),
    end_year = max(fractions$year),
    rows = nrow(fractions),
    minimum_fraction = min(fractions$cf_final),
    maximum_fraction = max(fractions$cf_final),
    maximum_closure_error = max(abs(closure$fraction_sum - 1)),
    survey_policy = "Survey fractions inform the curve but are not exact constraints",
    uncertainty_policy = paste(
      "Dirichlet survey-composition draws with total concentration equal to",
      "count/Kish effective N; the same interpolation and smoother are rerun"
    )
  )

  trajectory_summary <- data.table::copy(model$model_summaries)
  trajectory_summary[, record_type := "Interpolation specification"]

  data.table::rbindlist(
    list(source_summary, survey_fit, panel_summary, trajectory_summary),
    use.names = TRUE,
    fill = TRUE
  )[]
}
