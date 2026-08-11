testthat::test_that("final-cause uncertainty mapping is exact and conservative", {
  available <- c(
    "all_causes", "all_injuries", "nbd_2",
    paste0("nbd_", c(
      124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L,
      135L, 136L, 137L, 138L, 139L, 140L, 141L
    ))
  )
  result <- nbd_build_cause_uncertainty_mapping(.test_root, available)

  testthat::expect_true(all(
    c("za_2", "za_126", "za_138", "za_139", "za_171", "za_172") %in%
      result$coverage[supported %in% TRUE, series_id]
  ))
  testthat::expect_false(result$coverage[series_id == "za_1", supported])
  testthat::expect_identical(
    sort(result$mapping[series_id == "za_132", source_cause_id]),
    c("nbd_131", "nbd_137")
  )
  testthat::expect_identical(
    result$mapping[series_id == "za_171", source_cause_id],
    "all_injuries"
  )
  testthat::expect_identical(
    result$mapping[series_id == "za_172", source_cause_id],
    "all_causes"
  )
})

testthat::test_that("invalid final-cause intervals are recomputed exactly", {
  keys <- data.table::data.table(
    Death_Prov = c(1L, 1L),
    Sex = c(3L, 3L),
    DeathYear = c(2018L, 2019L),
    age_group = c("all_ages", "all_ages"),
    series_id = c("za_1", "za_1"),
    series_sort_order = c(1L, 1L)
  )
  draws <- rbind(
    seq(10, 109),
    seq(20, 119)
  )
  summary <- data.table::copy(keys)
  summary[, `:=`(
    n_draws = 100L,
    draw_mean = rowMeans(draws),
    draw_median = c(NA_real_, 69.5),
    draw_sd = c(NA_real_, stats::sd(draws[2, ])),
    lower = c(NA_real_, 110),
    upper = c(NA_real_, 30)
  )]
  collected <- list(keys = keys, values = draws, n_draws = 100L)
  diagnostic <- tempfile(fileext = ".csv")

  result <- nbd_recompute_invalid_interval_rows(
    summary = summary,
    collected = collected,
    key_columns = names(keys),
    label = "test",
    diagnostic_path = diagnostic
  )

  expected_1 <- stats::quantile(
    draws[1, ], c(0.025, 0.5, 0.975), names = FALSE, type = 8
  )
  expected_2 <- stats::quantile(
    draws[2, ], c(0.025, 0.5, 0.975), names = FALSE, type = 8
  )
  testthat::expect_equal(result$repaired_rows, 2L)
  testthat::expect_equal(result$data$lower, c(expected_1[1], expected_2[1]))
  testthat::expect_equal(result$data$draw_median, c(expected_1[2], expected_2[2]))
  testthat::expect_equal(result$data$upper, c(expected_1[3], expected_2[3]))
  testthat::expect_true(all(result$data$lower <= result$data$upper))
  testthat::expect_true(file.exists(diagnostic))
  testthat::expect_true(all(data.table::fread(diagnostic)$repaired))
})

testthat::test_that("population-group cause uncertainty preserves its report key", {
  source <- data.table::data.table(
    draw_id = rep(1L, 4L),
    Popgroup = rep(c(1L, 2L), each = 2L),
    Sex = 3L,
    DeathYear = 2010L,
    age_group = "all_ages",
    cause_id = rep(c("nbd_1", "nbd_2"), 2L),
    Deaths = c(10, 20, 30, 40)
  )
  mapping <- data.table::data.table(
    source_cause_id = c("nbd_1", "nbd_2"),
    series_id = "za_test",
    series_sort_order = 1L,
    weight = 1
  )
  result <- nbd_derive_population_cause_uncertainty_series(
    source,
    mapping,
    value_column = "Deaths",
    include_draw_id = TRUE
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(result[Popgroup == 1L, value], 30)
  testthat::expect_equal(result[Popgroup == 2L, value], 70)
  testthat::expect_true(all(c(
    "draw_id", "Popgroup", "Sex", "DeathYear", "age_group",
    "series_id", "series_sort_order", "value"
  ) %in% names(result)))
})
