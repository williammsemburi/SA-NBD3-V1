testthat::test_that("completeness support is the exact cluster bootstrap of the national S2 mean", {
  scalars <- data.table::CJ(
    Death_Prov = 1:9,
    Sex = 1:2,
    DeathYear = 2000:2001,
    age5 = 2:3,
    sorted = TRUE
  )
  province_effect <- seq(-0.25, 0.25, length.out = 9)
  scalars[, S2 := exp(
    province_effect[Death_Prov] +
      0.03 * (Sex - 1L) +
      0.01 * (DeathYear - 2000L) +
      0.02 * (age5 - 2L)
  )]
  scalars[, pre_deaths := 100 + 5 * Sex + 2 * age5]

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
  post_freeze <- data.table::copy(scalars[DeathYear == 2001L])
  post_freeze[, DeathYear := 2002L]
  scalars <- data.table::rbindlist(
    list(
      scalars[, .(Death_Prov, Sex, DeathYear, age5, S2)],
      copied_neonatal[, .(Death_Prov, Sex, DeathYear, age5, S2)],
      post_freeze[, .(Death_Prov, Sex, DeathYear, age5, S2)]
    ),
    use.names = TRUE
  )

  cfg <- list(settings = list(completeness_freeze_year = 2001L))
  distribution <- uc_estimate_completeness_distribution(
    scalars,
    stage04,
    cfg
  )

  province_aggregate <- stage04[, .(
    adjusted = sum(Deaths),
    pre = sum(Deaths / exp(
      province_effect[Death_Prov] +
        0.03 * (Sex - 1L) +
        0.01 * (DeathYear - 2000L) +
        0.02 * (age5 - 2L)
    ))
  ), by = Death_Prov]
  province_aggregate[, aggregate_s2 := adjusted / pre]
  national_s2 <- sum(province_aggregate$adjusted) /
    sum(province_aggregate$pre)

  testthat::expect_equal(
    distribution$province_summary$aggregate_s2,
    province_aggregate$aggregate_s2,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    distribution$national_aggregate_s2,
    national_s2,
    tolerance = 1e-12
  )
  testthat::expect_equal(nrow(distribution$factor_support), 24310L)
  testthat::expect_equal(sum(distribution$factor_support$probability), 1,
                         tolerance = 1e-12)
  testthat::expect_equal(
    sum(
      distribution$factor_support$probability *
        distribution$factor_support$factor
    ),
    1,
    tolerance = 1e-12
  )
  testthat::expect_lt(
    distribution$q975 - distribution$q025,
    diff(range(distribution$province_summary$relative_to_national))
  )
  testthat::expect_equal(distribution$eligible_strata, 8L)
  testthat::expect_equal(distribution$eligible_rows, 72L)
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

testthat::test_that("completeness draws apply one shared national factor", {
  support <- data.table::data.table(
    bootstrap_id = 1:3,
    factor = c(0.9, 1.0, 1.1),
    probability = c(0.25, 0.50, 0.25)
  )
  distribution <- list(
    log_sd = stats::sd(log(support$factor)),
    factor_support = support
  )
  input <- make_completeness_draw_input()

  draw_a <- uc_draw_completeness(input, distribution, TRUE, seed = 71L)
  draw_b <- uc_draw_completeness(input, distribution, TRUE, seed = 71L)

  testthat::expect_equal(draw_a$data, draw_b$data)
  testthat::expect_equal(draw_a$factors, draw_b$factors)
  testthat::expect_length(unique(draw_a$factors$factor), 1L)
  testthat::expect_true(unique(draw_a$factors$factor) %in% support$factor)

  sampled_factors <- vapply(1:50, function(seed) {
    unique(uc_draw_completeness(
      input, distribution, TRUE, seed = seed
    )$factors$factor)
  }, numeric(1))
  testthat::expect_gt(data.table::uniqueN(sampled_factors), 1L)

  comparison <- merge(
    input,
    draw_a$data,
    by = UC_KEY,
    suffixes = c("_point", "_draw"),
    sort = FALSE
  )
  shared_factor <- unique(draw_a$factors$factor)
  testthat::expect_equal(
    comparison[
      Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
      Deaths_draw
    ],
    comparison[
      Popgroup == 1L & !nbdcode %in% UC_INJURY_CODES,
      Deaths_point * shared_factor
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
  testthat::expect_lte(diff(range(african_natural$ratio)), 1e-12)
})

testthat::test_that("redistribution uncertainty uses only non-empty expert-target subsets", {
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

  draw_a <- stage06_draw_target_overrides(123L, rules)
  draw_b <- stage06_draw_target_overrides(123L, rules)

  testthat::expect_identical(draw_a, draw_b)
  testthat::expect_true("three_targets" %in% names(draw_a))
  testthat::expect_false("one_target" %in% names(draw_a))
  testthat::expect_gt(length(draw_a$three_targets), 0L)
  testthat::expect_length(
    setdiff(draw_a$three_targets, c(10L, 20L, 30L)),
    0L
  )
  subset_keys <- vapply(1:200, function(seed) {
    paste(
      stage06_draw_target_overrides(seed, rules)$three_targets,
      collapse = ","
    )
  }, character(1))
  expected_keys <- c("10", "20", "30", "10,20", "10,30", "20,30", "10,20,30")
  testthat::expect_setequal(unique(subset_keys), expected_keys)

  validated <- stage06_validate_target_overrides(rules, draw_a)
  testthat::expect_identical(validated, draw_a)
  testthat::expect_error(
    stage06_validate_target_overrides(
      rules,
      list(three_targets = integer())
    ),
    "non-empty subset"
  )
  testthat::expect_error(
    stage06_validate_target_overrides(
      rules,
      list(three_targets = 99L)
    ),
    "non-empty subset"
  )
})

testthat::test_that("joint uncertainty configuration contains no free scale parameters", {
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
    "cluster_bootstrap_national_mean"
  )
  testthat::expect_identical(
    config$components$completeness$draw_level,
    "national_shared"
  )
  testthat::expect_identical(
    config$components$redistribution$target_subset_distribution,
    "uniform_nonempty"
  )
  testthat::expect_identical(
    config$components$injury$model_file,
    "03_injury_fraction_model.rds"
  )
  testthat::expect_identical(
    config$components$injury$sampling_distribution,
    "dirichlet_effective_n"
  )
  testthat::expect_identical(
    config$components$injury$inter_survey_path,
    "deterministic_linear_plus_smoother"
  )
  testthat::expect_identical(config$reporting$top_n_causes, 214L)

  forbidden <- c(
    "log_sd", "sigma", "concentration", "multiplier", "bridge",
    "correlation"
  )
  flattened_names <- names(unlist(config, recursive = TRUE, use.names = TRUE))
  testthat::expect_false(any(vapply(
    forbidden,
    function(term) any(grepl(term, flattened_names, ignore.case = TRUE)),
    logical(1)
  )))
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
