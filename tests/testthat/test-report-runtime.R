report_data_path <- file.path(.test_root, "report", "R", "report_data.R")
report_charts_path <- file.path(.test_root, "report", "R", "report_charts.R")
source(report_data_path, local = environment())
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
