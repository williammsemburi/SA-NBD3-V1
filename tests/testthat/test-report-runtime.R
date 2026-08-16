report_data_path <- file.path(.test_root, "report", "R", "report_data.R")
report_cache_path <- file.path(.test_root, "report", "R", "report_cache.R")
report_charts_path <- file.path(.test_root, "report", "R", "report_charts.R")
source(report_data_path, local = environment())
source(report_cache_path, local = environment())
source(report_charts_path, local = environment())

runtime_test_config <- list(
  labels = list(
    models = list(
      colours = list("NBD3-R" = "#0071BC", "GBD2023" = "#7A5195"),
      line_types = list("NBD3-R" = "Solid", "GBD2023" = "ShortDash")
    )
  )
)

testthat::test_that("line charts do not confuse interval arguments with data columns", {
  testthat::skip_if_not_installed("highcharter")
  d <- data.table::data.table(
    year = rep(2018:2019, 2L),
    model = rep(c("NBD3-R", "GBD2023"), each = 2L),
    estimate = c(100, 105, 95, 101),
    lower = c(90, 94, 85, 91),
    upper = c(110, 116, 106, 112),
    plot_lower = c(90, 94, 85, 91),
    plot_upper = c(110, 116, 106, 112)
  )

  chart <- NULL
  testthat::expect_silent(chart <- hc_nbd_lines(
    d,
    y = "estimate",
    series = "model",
    lower = "plot_lower",
    upper = "plot_upper",
    config = runtime_test_config
  ))
  testthat::expect_s3_class(chart, "highchart")
})

cause_rate_fixture <- function() {
  data.table::data.table(
    model = "NBD3-R",
    geography_type = "province",
    geography_code = rep(c(1L, 2L), each = 2L),
    geography = rep(c("Eastern Cape", "Free State"), each = 2L),
    sex_code = 3L,
    sex = "Person",
    year = rep(2018:2019, 2L),
    age_id = "age_all",
    age_label = "All ages",
    series_id = "za_1",
    series_label = "All natural causes",
    domain = "natural",
    hierarchy = "detailed",
    cause_type = "natural",
    crude_rate = c(700, 690, 650, 640)
  )
}

testthat::test_that("cause-rate filtering accepts selectize character geography codes", {
  out <- collect_cause_rates(
    cause_rate_fixture(),
    models = "NBD3-R",
    geography_type = "province",
    geography_codes = c("1", "2"),
    sex_code = "3",
    series_id = "za_1",
    age_id = "age_all",
    year_range = c(2018, 2019),
    measure = "crude_rate"
  )
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_identical(sort(unique(out$geography_code)), c(1L, 2L))
  testthat::expect_true(all(out$measure == "crude_rate"))
})

testthat::test_that("Arrow cause-rate filtering collects before multi-value filtering", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("dplyr")

  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  arrow::write_parquet(cause_rate_fixture(), path)
  dataset <- arrow::open_dataset(path, format = "parquet")

  out <- NULL
  testthat::expect_silent(out <- collect_cause_rates(
    dataset,
    models = "NBD3-R",
    geography_type = "province",
    geography_codes = list(c("1", "2")),
    sex_code = "3",
    series_id = "za_1",
    age_id = "age_all",
    year_range = c("2018", "2019"),
    measure = "crude_rate"
  ))
  testthat::expect_equal(nrow(out), 4L)
})

testthat::test_that("the explorer exposes mortality measures and rejects YLL measures", {
  testthat::expect_identical(measure_column("deaths"), "deaths")
  testthat::expect_identical(measure_column("crude_rate"), "crude_rate")
  testthat::expect_identical(measure_column("asr"), "asr")
  testthat::expect_error(
    measure_column("yll00"),
    "Unsupported explorer measure"
  )
})

testthat::test_that("Highcharter plots animate by default and allow reduced-motion opt-out", {
  testthat::skip_if_not_installed("highcharter")
  d <- data.table::data.table(
    year = 2017:2019,
    model = "NBD3-R",
    estimate = c(100, 103, 106)
  )

  chart <- hc_nbd_lines(
    d,
    y = "estimate",
    series = "model",
    config = runtime_test_config
  )
  testthat::expect_true(is.list(chart$x$hc_opts$chart$animation))
  testthat::expect_gt(chart$x$hc_opts$chart$animation$duration, 0)
  testthat::expect_true(is.list(chart$x$hc_opts$plotOptions$series$animation))
  testthat::expect_gt(chart$x$hc_opts$plotOptions$series$animation$duration, 0)

  old_options <- options(nbd3.highcharts.animation = FALSE)
  on.exit(options(old_options), add = TRUE)
  reduced_motion_chart <- hc_nbd_lines(
    d,
    y = "estimate",
    series = "model",
    config = runtime_test_config
  )
  testthat::expect_false(reduced_motion_chart$x$hc_opts$chart$animation)
  testthat::expect_false(
    reduced_motion_chart$x$hc_opts$plotOptions$series$animation
  )
})

testthat::test_that("publication report includes the fast deployment cache layer", {
  testthat::expect_true(file.exists(file.path(
    .test_root, "report", "R", "report_cache.R"
  )))
  testthat::expect_true(file.exists(file.path(
    .test_root, "dev", "build_shiny_ui_cache.R"
  )))
  testthat::expect_true(file.exists(file.path(
    .test_root, "dev", "prepare_shinyapps_bundle.R"
  )))
  testthat::expect_true(file.exists(file.path(
    .test_root, "docs", "SHINY_DEPLOYMENT.md"
  )))
})

testthat::test_that("fast cache paths are rooted in report data", {
  config <- read_viz_config(.test_root)
  paths <- nbd_fast_ui_cache_paths(config)
  testthat::expect_true(grepl(
    "output/report-data/ui_uncertainty_cache$",
    gsub("\\\\", "/", paths$root)
  ))
  testthat::expect_identical(
    basename(paths$comparison),
    "model_comparison_uncertainty.parquet"
  )
})

testthat::test_that("deployment cache prewarms the default report partitions", {
  path <- file.path(root, "report", "R", "report_cache.R")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "NBD3_PREWARM_UI_CACHE", fixed = TRUE)
  testthat::expect_match(text, 'c("age_all", "asr_all")', fixed = TRUE)
  testthat::expect_match(text, "sex_code = 3L", fixed = TRUE)
})


testthat::test_that("deployment partition-cache keys are cachem compatible", {
  keys <- c(
    nbd_fast_ui_partition_cache_key("province", 3L, "age_0"),
    nbd_fast_ui_partition_cache_key("province", 3L, "asr_all"),
    nbd_fast_ui_partition_cache_key("population_group", 1L, "age_85_plus")
  )
  testthat::expect_true(all(grepl("^[a-z0-9]+$", keys)))
  testthat::expect_length(unique(keys), length(keys))
})
