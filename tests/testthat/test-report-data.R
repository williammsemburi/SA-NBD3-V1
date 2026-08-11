report_data_path <- file.path(.test_root, "report", "R", "report_data.R")
source(report_data_path, local = environment())

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
