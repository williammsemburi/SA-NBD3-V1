report_data_path <- file.path(.test_root, "report", "R", "report_data.R")
report_cache_path <- file.path(.test_root, "report", "R", "report_cache.R")
source(report_data_path, local = environment())
source(report_cache_path, local = environment())

testthat::test_that("report ratio calculations handle scalar and vector inputs", {
  testthat::expect_equal(
    safe_ratio(1:3, 2),
    c(0.5, 1, 1.5)
  )
  testthat::expect_equal(
    safe_ratio(c(1, 2), c(2, 4), multiplier = 100),
    c(50, 50)
  )
  testthat::expect_true(is.na(safe_ratio(1, 0)))
  testthat::expect_error(
    safe_ratio(1:3, 1:2),
    "must have length 1 or 3"
  )
})

testthat::test_that("legacy geography and model labels are harmonised", {
  testthat::expect_identical(
    normalise_geography_label(c("KwaZulu Natal", "KZN")),
    c("KwaZulu-Natal", "KwaZulu-Natal")
  )
  testthat::expect_identical(
    normalise_geography_label(c("Asian", "Indian", "Indian/Asian", "Asian/Indian")),
    rep("Asian/Indian", 4L)
  )
  testthat::expect_identical(
    normalise_model_label(c("NBD3", "NBD3-R", "GBD")),
    c("NBD3-Stata", "NBD3-R", "GBD2023")
  )
  testthat::expect_identical(
    normalise_sex_label(c("Both sexes", "Males", "Female")),
    c("Person", "Male", "Female")
  )
})

testthat::test_that("the collaborator report configuration loads the unified release", {
  config <- read_viz_config(.test_root)
  testthat::expect_identical(config$labels$project$version, "NBD3 Version 1")
  testthat::expect_true(any(config$cause_map$include_comparison %in% TRUE))
  testthat::expect_true(any(config$cause_map$include_explorer %in% TRUE))
  testthat::expect_true(any(config$age_map$include_comparison %in% TRUE))
  testthat::expect_identical(
    config$labels$geographies$population_group$labels,
    c("African", "White", "Asian/Indian", "Coloured")
  )
})

testthat::test_that("the interactive tool exposes mortality measures only", {
  testthat::expect_identical(
    vapply(c("deaths", "crude_rate", "asr"), measure_column, character(1)),
    c(deaths = "deaths", crude_rate = "crude_rate", asr = "asr")
  )
  testthat::expect_error(measure_column("yll00"), "Unsupported explorer measure")
  labels <- unlist(read_viz_config(.test_root)$labels$measures, use.names = TRUE)
  testthat::expect_false(any(names(labels) %in% c("yll00", "yll03", "yll015")))
})

testthat::test_that("full uncertainty age definitions cover report ages", {
  config <- read_viz_config(.test_root)
  death_spec <- full_uncertainty_age_spec(config, "age_all", "deaths")
  under5_spec <- full_uncertainty_age_spec(config, "age_under5", "crude_rate")
  asr_spec <- full_uncertainty_age_spec(config, "asr_all", "asr")

  testthat::expect_identical(death_spec$death_codes, 0:19)
  testthat::expect_identical(under5_spec$death_codes, 0:2)
  testthat::expect_identical(asr_spec$columns, paste0("age_", 0:19))
  testthat::expect_error(
    full_uncertainty_age_spec(config, "age_all", "asr"),
    "age_id='asr_all'"
  )
})


testthat::test_that("full uncertainty runtime uses the complete per-draw stores", {
  config <- read_viz_config(.test_root)
  profile <- yaml::read_yaml(file.path(.test_root, "config", "uncertainty_joint.yml"))
  testthat::expect_true(isTRUE(profile$reporting$full_ui_enabled))
  testthat::expect_identical(as.integer(profile$run$n_draws), 1000L)
  testthat::expect_identical(
    as.character(profile$run$output_name),
    "nbd3_v1_joint_full_ui_1000"
  )
  testthat::expect_setequal(
    names(config$labels$measures),
    c("deaths", "fraction", "crude_rate", "asr")
  )
  testthat::expect_false(any(grepl("yll", names(config$labels$measures), ignore.case = TRUE)))
})
