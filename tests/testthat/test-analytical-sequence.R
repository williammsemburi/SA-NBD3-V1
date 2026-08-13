testthat::test_that("the cached target graph preserves the scientific dependency order", {
  text <- paste(readLines("_targets.R", warn = FALSE), collapse = "\n")
  positions <- c(
    cod = regexpr("tar_target\\(cod_file", text)[[1L]],
    injury_profile = regexpr("tar_target\\(\\s*injury_outputs", text, perl = TRUE)[[1L]],
    completed_envelope = regexpr("tar_target\\(\\s*investigation_file", text, perl = TRUE)[[1L]],
    hiv = regexpr("tar_target\\(\\s*hiv_outputs", text, perl = TRUE)[[1L]],
    redistribution = regexpr("tar_target\\(\\s*redistributed_file", text, perl = TRUE)[[1L]],
    yll = regexpr("tar_target\\(\\s*deaths_yll_file", text, perl = TRUE)[[1L]],
    database = regexpr("tar_target\\(\\s*database_file_output", text, perl = TRUE)[[1L]]
  )
  testthat::expect_true(all(positions > 0))
  testthat::expect_true(all(diff(positions) > 0))
})

testthat::test_that("joint draws use the four uncertainty drivers in order", {
  text <- paste(readLines("R/06_uncertainty.R", warn = FALSE), collapse = "\n")
  start <- regexpr("uc_run_one_draw <- function", text, fixed = TRUE)[[1L]]
  testthat::expect_gt(start, 0)
  draw_text <- substr(text, start, nchar(text))
  positions <- c(
    completeness = regexpr("uc_draw_completeness(", draw_text, fixed = TRUE)[[1L]],
    injury_survey = regexpr("injury_survey_design_draw(", draw_text, fixed = TRUE)[[1L]],
    injury_level = regexpr("uc_draw_injury_envelope(", draw_text, fixed = TRUE)[[1L]],
    injury_profile = regexpr("uc_draw_injury_fraction_panel(", draw_text, fixed = TRUE)[[1L]],
    hiv = regexpr("run_hiv_reallocation_from_covariance(", draw_text, fixed = TRUE)[[1L]],
    redistribution = regexpr("stage06_draw_target_weight_overrides(", draw_text, fixed = TRUE)[[1L]]
  )
  testthat::expect_true(all(positions > 0))
  testthat::expect_true(all(diff(positions) > 0))
})

testthat::test_that("injury calibration separates national level and relative profile", {
  text <- paste(readLines("R/02_injuries.R", warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "survey_level_total")
  testthat::expect_match(text, "routine_level_total")
  testthat::expect_match(text, "point_level_ratio")
  testthat::expect_match(text, "survey_share")
  testthat::expect_match(text, "routine_share")
  testthat::expect_match(text, "target_total_injury_year")
  testthat::expect_match(text, "total_error")
})

testthat::test_that("redistribution uses continuous weights on approved targets", {
  text <- paste(readLines("R/04_redistribution.R", warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "stage06_draw_target_weight_overrides")
  testthat::expect_match(text, "stats::rgamma")
  testthat::expect_match(text, "multiplier / mean(multiplier)", fixed = TRUE)
  testthat::expect_false(grepl("uniform_nonempty", text, fixed = TRUE))
})

testthat::test_that("configuration uses time-only completeness and population-group draws", {
  cfg <- yaml::read_yaml("config/uncertainty_joint.yml")
  testthat::expect_identical(
    cfg$components$completeness$distribution,
    "province_specific_time_log_sd"
  )
  testthat::expect_identical(
    cfg$components$injury$envelope_sampling_distribution,
    "stratified_psu_bootstrap_joint_level_profile"
  )
  testthat::expect_identical(
    cfg$components$redistribution$target_weight_distribution,
    "subset_allocation_variance_matched_gamma"
  )
  testthat::expect_true(isTRUE(cfg$reporting$include_population_groups))
})
