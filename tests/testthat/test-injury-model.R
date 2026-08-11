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
  "legacy survey corrections are audited but are not imposed on the trajectory input",
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
    testthat::expect_gt(
      out[year == 2000L & nbdcode == 132L, fraction_corrected],
      out[year == 2000L & nbdcode == 132L, fraction]
    )
    testthat::expect_match(unique(out$model_input_policy), "audit only")
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
