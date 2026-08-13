make_fast_injury_cfg <- function(window = 5L) {
  list(settings = list(
    start_year = 1997L,
    end_year = 2019L,
    injury_fraction_method = "nims_ims_famhis_alr_linear_triangular_ma",
    injury_nims_year = 2000L,
    injury_ims_year = 2009L,
    injury_famhis_year = 2017L,
    injury_fraction_floor = 1e-8,
    injury_bias_correction_floor = 1e-5,
    injury_alr_reference_code = 139L,
    injury_smoothing_window = as.integer(window)
  ))
}

add_test_effective_sizes <- function(data, n = 100) {
  data[, `:=`(EffN2000 = n, EffN2009 = n, EffN2017 = n)]
  data
}

make_test_anchor_compositions <- function() {
  codes <- sort(INJURY_CODES)
  base <- seq_along(codes)
  p2000 <- base / sum(base)
  p2009 <- rev(base) / sum(base)
  p2017 <- (1 + (base %% 5L)^2) / sum(1 + (base %% 5L)^2)
  data.table::rbindlist(list(
    data.table::data.table(year = 2000L, nbdcode = codes, fraction = p2000),
    data.table::data.table(year = 2009L, nbdcode = codes, fraction = p2009),
    data.table::data.table(year = 2017L, nbdcode = codes, fraction = p2017)
  ))[, `:=`(
    Death_Prov = 1L,
    Sex = 1L,
    Popgroup = 1L,
    age5 = 8L,
    harmonised_count = 200 * fraction,
    total_count = 200,
    survey_effective_n = 200,
    survey = paste0("Survey ", year)
  )][]
}

testthat::test_that("Stage 03 exposes the fast fixed interpolation method", {
  parameters <- injury_model_parameters(make_fast_injury_cfg())

  testthat::expect_identical(
    parameters$method,
    "nims_ims_famhis_alr_linear_triangular_ma"
  )
  testthat::expect_identical(parameters$anchor_years, c(2000L, 2009L, 2017L))
  testthat::expect_identical(parameters$smoothing_window, 5L)
  testthat::expect_match(parameters$interpolation, "Piecewise linear")
  testthat::expect_match(parameters$smoother, "triangular moving average")
  testthat::expect_match(parameters$survey_policy, "not exact constraints")

  legacy <- injury_model_parameters(list(settings = list(
    start_year = 1997L,
    end_year = 2019L,
    injury_fraction_method = "nims_ims_famhis_robust_bayesian_gam"
  )))
  testthat::expect_identical(legacy$method, parameters$method)
  testthat::expect_identical(
    legacy$requested_method,
    "nims_ims_famhis_robust_bayesian_gam"
  )

  testthat::expect_error(
    injury_model_parameters(list(settings = list(
      injury_fraction_method = "unsupported_injury_method"
    ))),
    "Unsupported injury_fraction_method"
  )
  testthat::expect_error(
    injury_model_parameters(make_fast_injury_cfg(window = 4L)),
    "odd integer"
  )
})

testthat::test_that("linear ALR interpolation has exact raw anchors and flat raw tails", {
  years <- 1997:2019
  interpolation <- injury_interpolation_matrix(years, c(2000L, 2009L, 2017L))

  testthat::expect_equal(rowSums(interpolation), rep(1, length(years)), tolerance = 1e-15)
  testthat::expect_equal(interpolation[as.character(1997:2000), 1], rep(1, 4))
  testthat::expect_equal(interpolation[as.character(1997:2000), 2:3], base::matrix(0, 4, 2))
  testthat::expect_equal(interpolation["2009", ], c(0, 1, 0))
  testthat::expect_equal(interpolation[as.character(2017:2019), 3], rep(1, 3))
  testthat::expect_equal(interpolation["2005", ], c(4 / 9, 5 / 9, 0), tolerance = 1e-15)
  testthat::expect_equal(interpolation["2013", ], c(0, 0.5, 0.5), tolerance = 1e-15)
})

testthat::test_that("the triangular moving average is normalized and inexpensive", {
  years <- 1997:2019
  smoother <- injury_smoothing_matrix(years, window = 5L)
  index_2009 <- match(2009L, years)
  expected <- rep(0, length(years))
  expected[match(2007:2011, years)] <- c(1, 2, 3, 2, 1) / 9

  testthat::expect_equal(rowSums(smoother), rep(1, length(years)), tolerance = 1e-15)
  testthat::expect_equal(smoother[index_2009, ], expected, tolerance = 1e-15)
  testthat::expect_equal(injury_triangular_kernel(5L), c(1L, 2L, 3L, 2L, 1L))
})

testthat::test_that("smoothing reduces survey-year slope changes without resetting anchors", {
  parameters <- injury_model_parameters(make_fast_injury_cfg())
  matrices <- injury_trajectory_matrices(
    matrix(c(0, 3, -1), nrow = 1L),
    parameters
  )
  years <- parameters$years

  kink <- function(values, year) {
    index <- match(year, years)
    abs((values[index + 1L] - values[index]) -
          (values[index] - values[index - 1L]))
  }

  linear <- as.numeric(matrices$linear[1, ])
  smooth <- as.numeric(matrices$smoothed[1, ])
  testthat::expect_lt(kink(smooth, 2009L), kink(linear, 2009L))
  testthat::expect_lt(kink(smooth, 2017L), kink(linear, 2017L))
  testthat::expect_gt(abs(smooth[match(2009L, years)] - 3), 1e-12)
  testthat::expect_gt(abs(smooth[match(2017L, years)] + 1), 1e-12)
})

testthat::test_that(
  "cross-survey cause harmonisation defines the production anchors",
  {
    causes <- c(132L, 136L, 138L, 139L)
    surveys <- data.table::data.table(
      Death_Prov = 1L,
      Sex = 1L,
      Popgroup = 1L,
      age5 = 8L,
      nbdcode = causes,
      Inj2000 = c(0, 10, 0, 90),
      Inj2009 = c(10, 20, 10, 60),
      Inj2017 = c(20, 40, 20, 20),
      nims_spatial_status = "IMS 2009 province-population-group fraction",
      nims_source_zero = c(TRUE, FALSE, TRUE, FALSE)
    )
    surveys <- add_test_effective_sizes(surveys)

    out <- survey_counts_to_fractions(surveys)

    testthat::expect_true(
      out[year == 2000L & nbdcode == 132L, correction_applied]
    )
    testthat::expect_true(
      out[year == 2000L & nbdcode == 138L, correction_applied]
    )
    testthat::expect_true(
      out[year == 2017L & nbdcode == 136L, correction_applied]
    )
    testthat::expect_equal(out$fraction, out$model_input_fraction)
    testthat::expect_match(
      unique(out$model_input_policy),
      "Harmonised survey anchors"
    )
    closure <- out[, .(fraction_sum = sum(fraction)), by = year]
    testthat::expect_equal(closure$fraction_sum, rep(1, 3), tolerance = 1e-12)
    testthat::expect_equal(unique(out$survey_effective_n), 100)
  }
)

testthat::test_that("empty survey cells borrow an observed composition", {
  causes <- c(124L, 125L, 140L, 141L)
  surveys <- data.table::rbindlist(list(
    data.table::data.table(
      Death_Prov = 1L, Sex = 1L, Popgroup = 1L, age5 = 5L,
      nbdcode = causes,
      Inj2000 = c(20, 30, 10, 40),
      Inj2009 = c(25, 25, 10, 40),
      Inj2017 = c(30, 20, 10, 40),
      nims_spatial_status = "IMS 2009 province-population-group fraction",
      nims_source_zero = FALSE
    ),
    data.table::data.table(
      Death_Prov = 1L, Sex = 1L, Popgroup = 2L, age5 = 5L,
      nbdcode = causes,
      Inj2000 = 0,
      Inj2009 = 0,
      Inj2017 = 0,
      nims_spatial_status = "Equal 1/36 fallback",
      nims_source_zero = TRUE
    )
  ))
  surveys <- add_test_effective_sizes(surveys)

  out <- survey_counts_to_fractions(surveys)
  borrowed <- out[Popgroup == 2L]
  testthat::expect_true(all(grepl("^Borrowed", borrowed$fraction_source)))
  testthat::expect_equal(
    borrowed[, .(fraction_sum = sum(fraction)), by = year]$fraction_sum,
    rep(1, 3),
    tolerance = 1e-12
  )
})

testthat::test_that("ALR calculations remain finite for extreme positive survey draws", {
  tiny <- 1e-310

  testthat::expect_true(is.infinite(1 / tiny))
  testthat::expect_true(is.infinite(log(1 / tiny)))

  value <- injury_stable_log_ratio(1, tiny)
  testthat::expect_true(is.finite(value))
  testthat::expect_equal(value, log(1) - log(tiny), tolerance = 0)
  testthat::expect_equal(
    injury_stable_log_ratio(c(0.75, tiny), c(tiny, 0.75)),
    log(c(0.75, tiny)) - log(c(tiny, 0.75)),
    tolerance = 0
  )
})

testthat::test_that("hierarchical log ratios use explicit zero reference rows", {
  observed <- make_test_anchor_compositions()
  result <- build_injury_hierarchical_observations(
    observed,
    reference_code = 139L
  )

  testthat::expect_true(all(result$broad[is_reference == TRUE, log_ratio] == 0))
  testthat::expect_true(all(result$broad[is_reference == TRUE, alr_variance] == 0))
  testthat::expect_true(all(result$within[is_reference == TRUE, log_ratio] == 0))
  testthat::expect_true(all(result$within[is_reference == TRUE, alr_variance] == 0))
})

testthat::test_that("the smoothed annual composition is positive, closed, and not hard anchored", {
  cfg <- make_fast_injury_cfg()
  parameters <- injury_model_parameters(cfg)
  observed <- make_test_anchor_compositions()
  annual <- injury_build_annual_log_ratios(observed, cfg, parameters)
  fractions <- injury_compose_annual_fractions(annual, parameters)

  closure <- fractions[, .(
    causes = data.table::uniqueN(nbdcode),
    total = sum(cf_final)
  ), by = year]
  testthat::expect_equal(closure$causes, rep(length(INJURY_CODES), nrow(closure)))
  testthat::expect_equal(closure$total, rep(1, nrow(closure)), tolerance = 1e-12)
  testthat::expect_true(all(fractions$cf_final > 0))

  comparison <- injury_survey_model_comparison(observed, fractions)
  testthat::expect_gt(comparison[absolute_difference > 1e-12, .N], 0L)
  testthat::expect_setequal(unique(comparison$year), c(2000L, 2009L, 2017L))
})

testthat::test_that("survey-composition uncertainty is reproducible and closed", {
  observed <- make_test_anchor_compositions()
  observed[, `:=`(
    fraction_source = "Survey fraction",
    donor_level = NA_character_,
    raw_fraction = fraction,
    fraction_pre_correction = fraction,
    fraction_corrected = fraction,
    correction_applied = FALSE,
    correction_note = "",
    effective_n_source = "Fine survey stratum"
  )]

  draw_a <- injury_draw_survey_compositions(observed, seed = 910L)
  draw_b <- injury_draw_survey_compositions(observed, seed = 910L)
  draw_c <- injury_draw_survey_compositions(observed, seed = 911L)

  testthat::expect_equal(draw_a$fraction, draw_b$fraction)
  testthat::expect_gt(max(abs(draw_a$fraction - draw_c$fraction)), 0)
  testthat::expect_equal(
    draw_a[, sum(fraction), by = year]$V1,
    rep(1, 3),
    tolerance = 1e-12
  )
})

testthat::test_that("cells sharing a borrowed donor receive one common survey draw", {
  base <- make_test_anchor_compositions()
  borrowed <- data.table::rbindlist(list(
    data.table::copy(base)[, `:=`(Death_Prov = 1L, Popgroup = 1L)],
    data.table::copy(base)[, `:=`(Death_Prov = 2L, Popgroup = 4L)]
  ))
  borrowed[, `:=`(
    fraction_source = "Borrowed from national_age",
    donor_level = "national_age",
    raw_fraction = NA_real_,
    fraction_pre_correction = fraction,
    fraction_corrected = fraction,
    correction_applied = FALSE,
    correction_note = "",
    effective_n_source = "Borrowed from national_age"
  )]

  draw <- injury_draw_survey_compositions(borrowed, seed = 700L)
  wide <- data.table::dcast(
    draw,
    year + nbdcode ~ Death_Prov,
    value.var = "fraction"
  )
  testthat::expect_equal(wide[["1"]], wide[["2"]], tolerance = 0)
})


testthat::test_that(
  "NIMS is sampled before IMS spatial expansion and cross-survey harmonisation",
  {
    codes <- sort(INJURY_CODES)
    base_count <- seq_along(codes) + 2
    counts <- data.table::CJ(
      Death_Prov = 1:9,
      Sex = 1L,
      Popgroup = 1:4,
      age5 = 5L,
      nbdcode = codes,
      sorted = FALSE
    )
    counts[, `:=`(
      cause_index__ = match(nbdcode, codes),
      spatial_index__ = (Death_Prov - 1L) * 4L + Popgroup
    )]
    counts[, `:=`(
      Inj2000 = base_count[cause_index__] / 36,
      EffN2000 = 100 / 36,
      nims_source_zero = nbdcode %in% c(132L, 138L),
      Inj2009 = base_count[cause_index__] * spatial_index__,
      Inj2017 = rev(base_count)[cause_index__] * (37L - spatial_index__),
      EffN2009 = 0,
      EffN2017 = 0
    )]
    counts[, c("cause_index__", "spatial_index__") := NULL]

    draw_a <- injury_draw_nims_count_panel(counts, seed = 1401L)
    draw_b <- injury_draw_nims_count_panel(counts, seed = 1401L)
    draw_c <- injury_draw_nims_count_panel(counts, seed = 1402L)

    testthat::expect_equal(draw_a$Inj2000, draw_b$Inj2000)
    testthat::expect_gt(max(abs(draw_a$Inj2000 - draw_c$Inj2000)), 0)
    before <- counts[, .(total = sum(Inj2000)), by = .(Sex, age5)]
    after <- draw_a[, .(total = sum(Inj2000)), by = .(Sex, age5)]
    testthat::expect_equal(after$total, before$total, tolerance = 1e-12)

    # The sampled IMS cause profile controls NIMS spatial expansion.
    spatial <- draw_a[nbdcode == 124L, .(
      Death_Prov,
      Popgroup,
      nims_share = Inj2000 / sum(Inj2000),
      ims_share = Inj2009 / sum(Inj2009)
    )]
    testthat::expect_equal(
      spatial$nims_share,
      spatial$ims_share,
      tolerance = 1e-12
    )

    # Harmonisation is applied after the common NIMS draw. It may legitimately
    # produce different code-132 anchors in fine cells sharing the same raw
    # national NIMS composition, and it must no longer trigger a donor error.
    observed <- survey_counts_to_fractions(draw_a)
    testthat::expect_true(all(is.finite(observed$fraction)))
    testthat::expect_equal(
      observed[, sum(fraction), by = .(Death_Prov, Popgroup, year)]$V1,
      rep(1, 108),
      tolerance = 1e-12
    )
    testthat::expect_gt(
      data.table::uniqueN(
        round(observed[year == 2000L & nbdcode == 132L, fraction], 12)
      ),
      1L
    )
  }
)

testthat::test_that("injury fractions preserve each mortality envelope", {
  mortality <- data.table::data.table(
    Death_Prov = c(1L, 2L),
    Sex = 1L,
    Popgroup = 1L,
    age5 = 3L,
    DeathYear = 2000L,
    nbdcode = 124L,
    Deaths = c(100, 25)
  )
  fractions <- data.table::rbindlist(lapply(c(1L, 2L), function(province) {
    data.table::data.table(
      Death_Prov = province,
      Sex = 1L,
      Popgroup = 1L,
      age5 = 3L,
      year = 2000L,
      nbdcode = c(124L, 125L, 141L),
      cf_final = c(0.2, 0.3, 0.5)
    )
  }))

  out <- apply_injury_fractions(mortality, fractions)
  totals <- out[, .(Deaths = sum(Deaths)), by = Death_Prov]
  testthat::expect_equal(totals[Death_Prov == 1L, Deaths], 100)
  testthat::expect_equal(totals[Death_Prov == 2L, Deaths], 25)
  testthat::expect_equal(out[Death_Prov == 1L & nbdcode == 141L, Deaths], 50)
})

testthat::test_that("broad injury groups partition the final causes", {
  partition <- sort(c(
    INJURY_TRANSPORT_CODES,
    INJURY_OTHER_UNINTENTIONAL_CODES,
    INJURY_SELF_HARM_CODES,
    INJURY_VIOLENCE_CODES
  ))
  testthat::expect_identical(partition, sort(INJURY_CODES))
  testthat::expect_false(anyDuplicated(partition) > 0L)
})

make_injury_envelope_cfg <- function() {
  list(settings = list(
    start_year = 1997L,
    end_year = 2019L,
    injury_ims_year = 2009L,
    injury_famhis_year = 2017L,
    injury_envelope_method =
      "ims2009_famhis2017_level_plus_relative_profile",
    injury_envelope_age_smoothing_window = 5L,
    injury_envelope_profile_floor = 1e-10,
    injury_envelope_alpha_floor = 1e-8
  ))
}

testthat::test_that("injury level and profile hold/interpolate over time", {
  cfg <- make_injury_envelope_cfg()
  anchors <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    age5 = 5L,
    survey_year = c(2009L, 2017L),
    smoothed_log_profile_scalar = log(c(1.5, 0.8)),
    point_log_level_ratio = log(c(1.1, 0.9)),
    point_level_ratio = c(1.1, 0.9),
    survey_level_total = c(55, 54),
    routine_level_total = c(50, 60)
  )
  annual <- injury_envelope_annual_factors(anchors, cfg)

  testthat::expect_equal(
    annual[DeathYear %in% 1997:2009, profile_factor],
    rep(1.5, 13),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    annual[DeathYear %in% 1997:2009, point_level_ratio],
    rep(1.1, 13),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    annual[DeathYear == 2013L, profile_factor],
    sqrt(1.5 * 0.8),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    annual[DeathYear == 2013L, point_level_ratio],
    sqrt(1.1 * 0.9),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    annual[DeathYear %in% 2017:2019, point_level_ratio],
    rep(0.9, 3),
    tolerance = 1e-12
  )
})

testthat::test_that("level-plus-profile adjustment preserves cell totals and reaches national target", {
  mortality <- data.table::data.table(
    Death_Prov = c(1L, 1L, 2L, 2L),
    Sex = 1L,
    DeathYear = 2009L,
    age5 = 8L,
    Popgroup = 1L,
    nbdcode = c(124L, 85L, 124L, 85L),
    Deaths = c(20, 80, 30, 70)
  )
  factors <- data.table::data.table(
    Death_Prov = c(1L, 2L),
    Sex = 1L,
    DeathYear = 2009L,
    age5 = 8L,
    profile_factor = c(2, 0.5),
    point_level_ratio = 1.2,
    calibration_active = TRUE
  )
  result <- apply_injury_envelope_adjustment(
    mortality,
    factors,
    profile_column = "profile_factor",
    level_column = "point_level_ratio"
  )

  totals <- result$data[, .(Deaths = sum(Deaths)),
                        by = .(Death_Prov, Popgroup)]
  testthat::expect_equal(totals[Death_Prov == 1L, Deaths], 100,
                         tolerance = 1e-10)
  testthat::expect_equal(totals[Death_Prov == 2L, Deaths], 100,
                         tolerance = 1e-10)
  testthat::expect_equal(
    result$data[nbdcode %in% INJURY_CODES, sum(Deaths)],
    60,
    tolerance = 1e-8
  )
  testthat::expect_gt(
    result$data[Death_Prov == 1L & nbdcode %in% INJURY_CODES, sum(Deaths)],
    result$data[Death_Prov == 2L & nbdcode %in% INJURY_CODES, sum(Deaths)]
  )
  testthat::expect_lte(
    result$diagnostics$maximum_total_absolute_error,
    1e-8
  )
  testthat::expect_true("DeathYear" %in% names(result$audit))
  testthat::expect_false(any(
    c("DeathYear.x", "DeathYear.y") %in% names(result$audit)
  ))
})

testthat::test_that("national survey level is reproduced when some ages remain fixed", {
  mortality <- data.table::data.table(
    Death_Prov = c(1L, 1L, 1L, 1L),
    Sex = 1L,
    DeathYear = 2009L,
    age5 = c(2L, 2L, 8L, 8L),
    Popgroup = 1L,
    nbdcode = c(124L, 85L, 124L, 85L),
    Deaths = c(5, 95, 45, 55)
  )
  factors <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2009L,
    age5 = c(2L, 8L),
    profile_factor = c(1, 1),
    point_level_ratio = 1.2,
    calibration_active = c(FALSE, TRUE)
  )
  result <- apply_injury_envelope_adjustment(
    mortality,
    factors,
    profile_column = "profile_factor",
    level_column = "point_level_ratio"
  )

  testthat::expect_equal(
    result$data[nbdcode %in% INJURY_CODES, sum(Deaths)],
    60,
    tolerance = 1e-8
  )
  testthat::expect_equal(
    result$data[age5 == 2L & nbdcode %in% INJURY_CODES, sum(Deaths)],
    5,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    result$data[, sum(Deaths)],
    200,
    tolerance = 1e-10
  )
})

testthat::test_that("survey weight scale does not change the relative profile", {
  survey_a <- c(10, 20, 30)
  survey_b <- survey_a * 100
  routine <- c(20, 20, 20)
  share_a <- injury_normalise_profile(survey_a)
  share_b <- injury_normalise_profile(survey_b)
  routine_share <- injury_normalise_profile(routine)
  testthat::expect_equal(share_a, share_b, tolerance = 1e-12)
  testthat::expect_equal(
    log(share_a) - log(routine_share),
    log(share_b) - log(routine_share),
    tolerance = 1e-12
  )
})

testthat::test_that("joint survey-design injury draws are reproducible", {
  make_design <- function(name, year, offset = 0) {
    psus <- data.table::data.table(
      stratum_id = c("a", "a", "b", "b"),
      psu_id = paste0(name, "_", 1:4)
    )
    level_domain <- data.table::data.table(
      stratum_id = psus$stratum_id,
      psu_id = psus$psu_id,
      Death_Prov = c(1L, 2L, 1L, 2L),
      Sex = c(1L, 1L, 2L, 2L),
      age5 = c(8L, 8L, 9L, 9L),
      weighted_injuries = c(10, 20, 30, 40) + offset
    )
    cause_domain <- data.table::rbindlist(lapply(seq_len(nrow(psus)), function(i) {
      data.table::data.table(
        stratum_id = psus$stratum_id[[i]],
        psu_id = psus$psu_id[[i]],
        Death_Prov = level_domain$Death_Prov[[i]],
        Sex = level_domain$Sex[[i]],
        Popgroup = 1L,
        age5 = level_domain$age5[[i]],
        nbdcode = c(124L, 139L),
        weighted_causes = c(0.4, 0.6) *
          level_domain$weighted_injuries[[i]]
      )
    }))
    list(
      survey = name,
      survey_year = as.integer(year),
      psus = psus,
      level_domain = level_domain,
      cause_domain = cause_domain,
      level_effective_n = 80,
      cause_effective_n = 70
    )
  }
  artifact <- list(
    ims = make_design("IMS 2009", 2009L),
    famhis = make_design("FAMHIS 2017", 2017L, offset = 5)
  )

  draw_a <- injury_survey_design_draw(artifact, seed = 91L, stochastic = TRUE)
  draw_b <- injury_survey_design_draw(artifact, seed = 91L, stochastic = TRUE)
  draw_c <- injury_survey_design_draw(artifact, seed = 92L, stochastic = TRUE)

  testthat::expect_equal(draw_a$envelope_surveys, draw_b$envelope_surveys)
  testthat::expect_equal(draw_a$cause_surveys, draw_b$cause_surveys)
  testthat::expect_false(identical(
    draw_a$diagnostics$level_total,
    draw_c$diagnostics$level_total
  ))
  testthat::expect_true(all(is.finite(draw_a$diagnostics$level_total)))
  testthat::expect_true(all(draw_a$diagnostics$level_total > 0))
  envelope_totals <- draw_a$envelope_surveys[
    , .(level_total = sum(survey_injury_total)),
    by = survey_year
  ][order(survey_year)]
  diagnostic_totals <- draw_a$diagnostics[order(survey_year), level_total]
  testthat::expect_equal(
    envelope_totals$level_total,
    diagnostic_totals,
    tolerance = 1e-10
  )
  testthat::expect_true(all(draw_a$cause_surveys$EffN2009 == 0))
  testthat::expect_true(all(draw_a$cause_surveys$EffN2017 == 0))
})

testthat::test_that("external level references do not create hidden point defaults", {
  cfg <- make_injury_envelope_cfg()
  parameters <- injury_envelope_model_parameters(cfg)
  testthat::expect_true(is.na(parameters$level_reference[["2009"]]$estimate))
  testthat::expect_true(is.na(parameters$level_reference[["2017"]]$estimate))
})

testthat::test_that("extreme sparse profile draws remain finite in log space", {
  shares <- c(1 - 1e-12, rep(1e-12 / 9, 9))
  draw <- injury_draw_dirichlet_profile(
    shares,
    effective_n = 0.01,
    alpha_floor = 1e-12
  )
  testthat::expect_true(all(is.finite(draw)))
  testthat::expect_true(all(draw > 0))
  testthat::expect_equal(sum(draw), 1, tolerance = 1e-12)
  log_factor <- log(draw) - log(injury_normalise_profile(rep(1, 10)))
  testthat::expect_true(all(is.finite(log_factor)))
  testthat::expect_true(all(injury_safe_exp(log_factor) > 0))
})


testthat::test_that("log-space envelope adjustment tolerates machine-small profile factors", {
  mortality <- data.table::data.table(
    Death_Prov = c(1L, 1L, 2L, 2L),
    Sex = 1L,
    DeathYear = 2009L,
    age5 = 8L,
    Popgroup = 1L,
    nbdcode = c(124L, 85L, 124L, 85L),
    Deaths = c(20, 80, 30, 70)
  )
  factors <- data.table::data.table(
    Death_Prov = c(1L, 2L),
    Sex = 1L,
    DeathYear = 2009L,
    age5 = 8L,
    draw_log_profile_factor = c(-800, 0),
    draw_profile_factor = injury_safe_exp(c(-800, 0)),
    draw_log_level_ratio = log(1.1),
    draw_level_ratio = 1.1,
    calibration_active = TRUE
  )
  result <- apply_injury_envelope_adjustment(
    mortality,
    factors,
    profile_column = "draw_profile_factor",
    level_column = "draw_level_ratio",
    profile_log_column = "draw_log_profile_factor",
    level_log_column = "draw_log_level_ratio"
  )
  testthat::expect_equal(
    result$data[, sum(Deaths)],
    mortality[, sum(Deaths)],
    tolerance = 1e-8
  )
  testthat::expect_equal(
    result$data[nbdcode %in% INJURY_CODES, sum(Deaths)],
    55,
    tolerance = 1e-8
  )
})


testthat::test_that(
  "FAMHIS cause-composition effective size uses the retained injury weights",
  {
    famhis <- data.table::data.table(
      prov = c(1, 1, 2, 2),
      gender_b = c(0, 1, 0, 1),
      ethnicity_b = c(1, 2, 3, 4),
      nbdcodmech = c("ZA126", "ZA131", "ZA138", "ZA1391"),
      non_nat_undert = NA_real_,
      age_Non_Natural = c(30, 40, 25, 35),
      age_unitss_Non_Natural = rep(2, 4),
      wht = c(1, 2, 3, 4),
      fps_code_updated = c(11, 12, 21, 22),
      stratprov = c(1, 1, 2, 2)
    )

    out <- prepare_famhis_2017(famhis)
    expected_effective_n <- kish_effective_sample_size(famhis$wht)
    allocated_effective_n <- unique(
      out[EffN2017 > 0, .(Death_Prov, Sex, Popgroup, age5, EffN2017)]
    )[, sum(EffN2017)]

    testthat::expect_equal(
      sum(out$Inj2017),
      sum(famhis$wht),
      tolerance = 1e-12
    )
    testthat::expect_equal(
      allocated_effective_n,
      expected_effective_n,
      tolerance = 1e-12
    )
    testthat::expect_true(all(is.finite(out$EffN2017)))
    testthat::expect_true(all(out$EffN2017 >= 0))
  }
)


testthat::test_that("well-specified cause fractions exclude unresolved injury mass", {
  text <- paste(deparse(body(injury_build_ims_design)), collapse = " ")
  testthat::expect_match(text, "cause_eligible", fixed = TRUE)
  testthat::expect_match(text, "nbdcode %in% INJURY_CODES", fixed = TRUE)
  famhis_text <- paste(deparse(body(injury_build_famhis_design)), collapse = " ")
  testthat::expect_match(famhis_text, "cause_eligible", fixed = TRUE)
  testthat::expect_match(famhis_text, "level_eligible", fixed = TRUE)
})

testthat::test_that("cause harmonisation is the production survey input", {
  text <- paste(deparse(body(survey_counts_to_fractions)), collapse = " ")
  testthat::expect_match(text, "fraction_corrected", fixed = TRUE)
  testthat::expect_match(text, "model_input_fraction", fixed = TRUE)
  testthat::expect_match(text, "nbdcode %in% c(132L, 138L)", fixed = TRUE)
  testthat::expect_match(text, "nbdcode == 136L", fixed = TRUE)
})
