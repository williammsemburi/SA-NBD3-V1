testthat::test_that("completeness dispersion uses annual province totals over time", {
  scalars <- data.table::CJ(
    Death_Prov = 1:9,
    Sex = 1:2,
    DeathYear = 2000:2002,
    age5 = 2:4,
    sorted = TRUE
  )
  province_level <- seq(-0.20, 0.20, length.out = 9)
  province_time_scale <- seq(0.03, 0.19, length.out = 9)
  # The age term is deliberately large. It must disappear when the estimator
  # collapses age before measuring temporal dispersion.
  scalars[, S2 := exp(
    province_level[Death_Prov] +
      province_time_scale[Death_Prov] * (DeathYear - 2001L) +
      0.60 * (age5 - 3L) +
      0.02 * (Sex - 1L)
  )]
  scalars[, pre_deaths := 100 + 8 * age5 + 5 * Sex]

  stage04 <- scalars[, .(
    Death_Prov,
    Sex,
    DeathYear,
    Popgroup = 1L,
    age5,
    nbdcode = 85L,
    Deaths = pre_deaths * S2
  )]

  copied_neonatal <- data.table::copy(scalars[age5 == 2L])
  copied_neonatal[, age5 := 1L]
  post_freeze <- data.table::copy(scalars[DeathYear == 2002L])
  post_freeze[, DeathYear := 2003L]
  scalar_input <- data.table::rbindlist(
    list(
      scalars[, .(Death_Prov, Sex, DeathYear, age5, S2)],
      copied_neonatal[, .(Death_Prov, Sex, DeathYear, age5, S2)],
      post_freeze[, .(Death_Prov, Sex, DeathYear, age5, S2)]
    ),
    use.names = TRUE
  )

  cfg <- list(settings = list(completeness_freeze_year = 2002L))
  distribution <- uc_estimate_completeness_distribution(
    scalar_input,
    stage04,
    cfg
  )

  expected_cells <- scalars[, .(
    adjusted = sum(pre_deaths * S2),
    pre = sum(pre_deaths)
  ), by = .(Death_Prov, DeathYear)]
  expected_cells[, `:=`(
    S2 = adjusted / pre,
    log_s2 = log(adjusted / pre)
  )]
  expected <- expected_cells[, {
    weighted_mean <- sum(pre * log_s2) / sum(pre)
    list(
      aggregate_s2 = sum(adjusted) / sum(pre),
      log_sd = sqrt(sum(pre * (log_s2 - weighted_mean)^2) / sum(pre))
    )
  }, by = Death_Prov]

  testthat::expect_equal(
    distribution$province_summary$aggregate_s2,
    expected$aggregate_s2,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    distribution$province_summary$log_sd,
    expected$log_sd,
    tolerance = 1e-12
  )
  testthat::expect_gt(
    data.table::uniqueN(round(distribution$province_summary$log_sd, 10)),
    1L
  )
  testthat::expect_equal(
    distribution$province_summary$factor_mean,
    rep(1, 9),
    tolerance = 1e-12
  )
  testthat::expect_true(all(
    distribution$province_summary$factor_q025 < 1 &
      distribution$province_summary$factor_q975 > 1
  ))
  testthat::expect_equal(distribution$eligible_rows, 162L)
  testthat::expect_equal(distribution$eligible_strata, 27L)
  testthat::expect_equal(
    distribution$province_summary$eligible_time_cells,
    rep(3L, 9)
  )
  testthat::expect_identical(distribution$provinces, 1:9)
})

make_completeness_draw_input <- function() {
  data.table::CJ(
    Death_Prov = 1:9,
    Sex = 1L,
    DeathYear = 2000L,
    Popgroup = 1:2,
    age5 = 5L,
    nbdcode = c(2L, 85L, 124L),
    sorted = TRUE
  )[, Deaths := 100]
}

testthat::test_that("completeness draws apply one province-specific factor", {
  province_summary <- data.table::data.table(
    Death_Prov = 1:9,
    log_sd = seq(0.02, 0.18, length.out = 9)
  )
  distribution <- list(
    log_sd = sqrt(mean(province_summary$log_sd^2)),
    log_sd_min = min(province_summary$log_sd),
    log_sd_median = stats::median(province_summary$log_sd),
    log_sd_max = max(province_summary$log_sd),
    province_summary = province_summary
  )
  input <- make_completeness_draw_input()

  draw_a <- uc_draw_completeness(input, distribution, TRUE, seed = 71L)
  draw_b <- uc_draw_completeness(input, distribution, TRUE, seed = 71L)

  testthat::expect_equal(draw_a$data, draw_b$data)
  testthat::expect_equal(draw_a$factors, draw_b$factors)
  testthat::expect_equal(nrow(draw_a$factors), 9L)
  testthat::expect_gt(
    data.table::uniqueN(round(draw_a$factors$factor, 12)),
    1L
  )

  comparison <- merge(
    input,
    draw_a$data,
    by = UC_KEY,
    suffixes = c("_point", "_draw"),
    sort = FALSE
  )
  comparison <- merge(
    comparison,
    draw_a$factors[, .(Death_Prov, factor)],
    by = "Death_Prov",
    all.x = TRUE,
    sort = FALSE
  )
  testthat::expect_equal(
    comparison[
      Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
      Deaths_draw
    ],
    comparison[
      Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
      Deaths_point * factor
    ],
    tolerance = 1e-12
  )
  testthat::expect_true(comparison[
    Popgroup != 1L | nbdcode %in% UC_INJURY_CODES,
    all(abs(Deaths_draw - Deaths_point) <= 1e-12)
  ])

  african_natural <- comparison[
    Popgroup == 1L & nbdcode %in% c(2L, 85L),
    .(ratio = Deaths_draw / Deaths_point),
    by = .(Death_Prov, nbdcode)
  ]
  testthat::expect_true(african_natural[, .(
    identical_within_province = diff(range(ratio)) <= 1e-12
  ), by = Death_Prov][, all(identical_within_province)])
})

testthat::test_that("redistribution uncertainty uses continuous approved-target weights", {
  rules <- list(
    list(
      rule_id = "three_targets",
      sources = 1L,
      targets = c(10L, 20L, 30L),
      condition = function(data) rep(TRUE, nrow(data))
    ),
    list(
      rule_id = "one_target",
      sources = 2L,
      targets = 40L,
      condition = function(data) rep(TRUE, nrow(data))
    )
  )

  draw_a <- stage06_draw_target_weight_overrides(123L, rules)
  draw_b <- stage06_draw_target_weight_overrides(123L, rules)
  testthat::expect_identical(draw_a, draw_b)
  testthat::expect_true("three_targets" %in% names(draw_a))
  testthat::expect_false("one_target" %in% names(draw_a))
  testthat::expect_length(draw_a$three_targets, 3L)
  testthat::expect_true(all(is.finite(draw_a$three_targets)))
  testthat::expect_true(all(draw_a$three_targets > 0))
  testthat::expect_equal(mean(draw_a$three_targets), 1, tolerance = 1e-12)
  testthat::expect_setequal(names(draw_a$three_targets), c("10", "20", "30"))

  validated <- stage06_validate_target_weight_overrides(rules, draw_a)
  testthat::expect_equal(validated, draw_a)
  testthat::expect_error(
    stage06_validate_target_weight_overrides(
      rules,
      list(three_targets = c(1, -1, 1))
    ),
    "strictly positive multiplier"
  )
  testthat::expect_equal(
    stage06_subset_allocation_gamma_shape(2L),
    0.25,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    stage06_subset_allocation_gamma_shape(3L),
    0.2888888888888889,
    tolerance = 1e-12
  )
})

testthat::test_that("injury uncertainty uses one joint survey-design replicate", {
  testthat::expect_identical(NBD3_UNCERTAINTY_VERSION, "1.7.1")
  draw_text <- paste(deparse(body(uc_run_one_draw)), collapse = " ")
  testthat::expect_match(draw_text, "injury_survey_design_draw", fixed = TRUE)
  testthat::expect_match(draw_text, "survey_draw$envelope_surveys", fixed = TRUE)
  testthat::expect_match(draw_text, "survey_draw$cause_surveys", fixed = TRUE)
  testthat::expect_match(draw_text, "stage06_draw_target_weight_overrides", fixed = TRUE)
})

testthat::test_that("joint uncertainty configuration records the structured models", {
  config <- yaml::read_yaml(file.path(
    .test_root,
    "config",
    "uncertainty_joint.yml"
  ))
  testthat::expect_identical(
    as.character(unlist(config$run$scenarios)),
    "joint"
  )
  testthat::expect_identical(
    config$components$completeness$distribution,
    "province_specific_time_log_sd"
  )
  testthat::expect_identical(
    config$components$completeness$draw_level,
    "province"
  )
  testthat::expect_identical(
    config$components$redistribution$target_weight_distribution,
    "subset_allocation_variance_matched_gamma"
  )
  testthat::expect_identical(
    config$components$injury$model_file,
    "03_injury_fraction_model.rds"
  )
  testthat::expect_identical(
    config$components$injury$sampling_distribution,
    "stratified_psu_bootstrap_joint"
  )
  testthat::expect_identical(
    config$components$injury$inter_survey_path,
    "deterministic_linear_plus_smoother"
  )
  testthat::expect_identical(
    config$components$injury$envelope_model_file,
    "04_injury_envelope_model.rds"
  )
  testthat::expect_identical(
    config$components$injury$envelope_sampling_distribution,
    "stratified_psu_bootstrap_joint_level_profile"
  )
  testthat::expect_identical(
    config$components$injury$survey_design_file,
    "03_injury_survey_design.rds"
  )
  testthat::expect_identical(
    config$components$injury$pre_2009_policy,
    "hold_ims_2009"
  )
  testthat::expect_identical(
    config$components$injury$between_surveys_policy,
    "log_linear"
  )
  testthat::expect_identical(
    config$components$injury$post_2017_policy,
    "hold_famhis_2017"
  )
  testthat::expect_identical(config$reporting$top_n_causes, 214L)

  testthat::expect_equal(
    config$components$redistribution$point_multiplier,
    1
  )
})

testthat::test_that("the full cause catalogue retains every Stage 06 analysis cause", {
  point <- data.table::data.table(
    nbdcode = 1:214,
    Deaths = seq_len(214)
  )
  catalog <- uc_build_cause_catalog(.test_root, point, top_n = 214L)
  testthat::expect_setequal(catalog$nbdcode, 1:214)
  testthat::expect_equal(data.table::uniqueN(catalog$cause_id), 214L)
})

testthat::test_that("HIV covariance diagnostics expose reciprocal donor-cause movement", {
  draws <- data.table::CJ(
    scenario = "joint",
    draw_id = 1:5,
    Death_Prov = 10L,
    Sex = 3L,
    DeathYear = 2010L,
    age_group = "all_ages",
    cause_id = c("nbd_1", "nbd_2", "nbd_85"),
    sorted = TRUE
  )
  hiv_values <- c(80, 90, 100, 110, 120)
  draws[cause_id == "nbd_2", Deaths := hiv_values]
  draws[cause_id == "nbd_1", Deaths := 300 - hiv_values]
  draws[cause_id == "nbd_85", Deaths := 50]
  catalog <- data.table::data.table(
    cause_id = c("nbd_1", "nbd_2", "nbd_85"),
    nbdcode = c(1L, 2L, 85L),
    label = c("Tuberculosis", "HIV/AIDS", "Ischaemic heart disease")
  )

  result <- uc_hiv_cause_covariance_diagnostics(draws, catalog)
  tb <- result[cause_id == "nbd_1"]
  ihd <- result[cause_id == "nbd_85"]

  testthat::expect_equal(tb$correlation_with_hiv, -1, tolerance = 1e-12)
  testthat::expect_equal(tb$cause_change_per_hiv_death, -1, tolerance = 1e-12)
  testthat::expect_true(tb$hiv_linked_destination)
  testthat::expect_true(is.na(ihd$correlation_with_hiv))
  testthat::expect_false(ihd$hiv_linked_destination)
})

testthat::test_that("population-group reporting is retained in the joint profile", {
  config <- uc_validate_config(uc_default_config())
  testthat::expect_true(config$reporting$include_population_groups)

  point <- data.table::CJ(
    Death_Prov = 1:2,
    Sex = 1:2,
    DeathYear = 2010L,
    Popgroup = 1:4,
    age5 = 1:2,
    nbdcode = c(1L, 2L, 124L),
    sorted = TRUE
  )
  point[, Deaths := as.numeric(
    Death_Prov + Sex + Popgroup + age5 + nbdcode / 100
  )]
  catalog <- data.table::data.table(
    nbdcode = c(1L, 2L, 124L),
    cause_id = c("nbd_1", "nbd_2", "nbd_124")
  )
  config$reporting$age_groups <- list(all_ages = 1:2)
  config$reporting$sexes <- 3L

  result <- uc_report_population_draw(
    point, catalog, config, scenario = "point", draw_id = 0L
  )
  testthat::expect_setequal(unique(result$Popgroup), 1:4)
  testthat::expect_identical(unique(result$Sex), 3L)
  testthat::expect_true(all(c(
    "all_causes", "all_injuries", "nbd_1", "nbd_2", "nbd_124"
  ) %in% result$cause_id))
  testthat::expect_equal(
    result[cause_id == "all_causes", sum(Deaths)],
    point[, sum(Deaths)]
  )
})
