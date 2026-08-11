testthat::test_that("input manifest creates a literal key column", {
  manifest <- required_input_manifest(list())

  testthat::expect_s3_class(manifest, "data.table")
  testthat::expect_true("key" %in% names(manifest))
  testthat::expect_equal(nrow(manifest), 14L)
  testthat::expect_equal(
    manifest$key[1:4],
    c(
      "population_current", "population_workbook",
      "cod_raw", "icd_to_nbd_lookup"
    )
  )
  testthat::expect_null(data.table::key(manifest))
})

testthat::test_that("cross_join_dt works on current data.table releases", {
  left <- data.table::data.table(group = c("a", "b"), value = c(10L, 20L))
  right <- data.table::data.table(target = 1:3)

  out <- cross_join_dt(left, right)
  testthat::expect_s3_class(out, "data.table")
  testthat::expect_equal(names(out), c("group", "value", "target"))
  testthat::expect_false(any(grepl("^\\.nbd3_cross_join", names(out))))
  testthat::expect_equal(nrow(out), 6L)
  testthat::expect_equal(out[, .N, by = group]$N, c(3L, 3L))
  testthat::expect_equal(sort(unique(out$target)), 1:3)

  # The helper must not alter either input by reference.
  testthat::expect_equal(names(left), c("group", "value"))
  testthat::expect_equal(names(right), "target")
})

testthat::test_that("cross_join_dt handles empty and overlapping inputs safely", {
  empty_left <- data.table::data.table(group = character())
  right <- data.table::data.table(target = 1:2)
  empty_out <- cross_join_dt(empty_left, right)

  testthat::expect_equal(nrow(empty_out), 0L)
  testthat::expect_equal(names(empty_out), c("group", "target"))

  testthat::expect_error(
    cross_join_dt(
      data.table::data.table(code = 1L),
      data.table::data.table(code = 2L)
    ),
    "disjoint column names"
  )
})

testthat::test_that("long-form redistribution expands targets without by-null merge", {
  x <- data.table::data.table(
    stratum = c(1L, 1L, 1L, 2L),
    category = c(1L, 2L, 8L, 8L),
    count = c(3, 1, 4, 6)
  )

  out <- redistribute_unknown(
    x,
    category = "category",
    value = "count",
    by = "stratum",
    unknown = 8L,
    targets = 1:2
  )

  testthat::expect_equal(out[stratum == 1L & category == 1L, count], 6)
  testthat::expect_equal(out[stratum == 1L & category == 2L, count], 2)
  testthat::expect_equal(out[stratum == 2L, count], c(3, 3))
  testthat::expect_equal(out[, sum(count), by = stratum]$V1, c(8, 6))
})
