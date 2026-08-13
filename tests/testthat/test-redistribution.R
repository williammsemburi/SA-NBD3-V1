testthat::test_that("redist reproduces proportional and zero-denominator allocation", {
  original <- data.frame(
    source = c(20L, 10L),
    target_a = c(30L, 0L),
    target_b = c(10L, 0L)
  )

  out <- redist(
    original,
    source_vars = "source",
    target_vars = c("target_a", "target_b"),
    quiet = TRUE
  )

  testthat::expect_false("source" %in% names(out))
  testthat::expect_equal(out$target_a, c(45, 5))
  testthat::expect_equal(out$target_b, c(15, 5))
  testthat::expect_true("source" %in% names(original))
  testthat::expect_type(out$target_a, "double")
})

testthat::test_that("conditional redist zeros sources only in matching rows", {
  x <- data.frame(
    age = c(10, 20),
    source = c(20, 8),
    target_a = c(30, 1),
    target_b = c(10, 3)
  )

  out <- redist(
    x,
    "source",
    c("target_a", "target_b"),
    age < 15,
    quiet = TRUE
  )

  testthat::expect_equal(out$source, c(0, 8))
  testthat::expect_equal(out$target_a, c(45, 1))
  testthat::expect_equal(out$target_b, c(15, 3))
})

testthat::test_that("scalar conditions are applied to every row", {
  x <- data.frame(source = c(2, 4), target_a = 0, target_b = 0)
  out <- redist(x, "source", c("target_a", "target_b"), TRUE, quiet = TRUE)

  testthat::expect_equal(out$source, c(0, 0))
  testthat::expect_equal(out$target_a, c(1, 2))
  testthat::expect_equal(out$target_b, c(1, 2))
})

testthat::test_that("missing count cells follow the pipeline zero convention", {
  x <- data.frame(source = 10, target_a = NA_real_, target_b = 0)
  out <- redist(x, "source", c("target_a", "target_b"), quiet = TRUE)

  testthat::expect_equal(out$target_a, 5)
  testthat::expect_equal(out$target_b, 5)
})

testthat::test_that("redist fails fast on ambiguous or invalid inputs", {
  overlap <- data.frame(source = 1, target = 2)
  testthat::expect_error(
    redist(overlap, c("source", "target"), "target", quiet = TRUE),
    "disjoint"
  )

  negative <- data.frame(source = -1, target = 2)
  testthat::expect_error(
    redist(negative, "source", "target", quiet = TRUE),
    "negative"
  )

  missing_condition <- data.frame(
    age = c(10, NA), source = c(1, 1), target = c(1, 1)
  )
  testthat::expect_error(
    redist(missing_condition, "source", "target", age >= 15, quiet = TRUE),
    "condition.*missing"
  )
})

testthat::test_that("condition NA handling can be made explicit", {
  x <- data.frame(age = c(10, NA), source = c(2, 4), target = c(2, 4))
  out <- redist(
    x,
    "source",
    "target",
    age >= 15,
    condition_na = "exclude",
    quiet = TRUE
  )

  testthat::expect_equal(out$source, c(2, 4))
  testthat::expect_equal(out$target, c(2, 4))
})

testthat::test_that("data.table mutation is opt-in and preserves fractional values", {
  x <- data.table::data.table(source = 1L, target_a = 1L, target_b = 2L)
  out <- redist(
    x,
    "source",
    c("target_a", "target_b"),
    copy = FALSE,
    quiet = TRUE
  )

  testthat::expect_false("source" %in% names(x))
  testthat::expect_equal(x$target_a, 4 / 3)
  testthat::expect_equal(x$target_b, 8 / 3)
  testthat::expect_identical(out, x)
})

testthat::test_that("unknown categories are redistributed proportionally", {
  x <- data.table::data.table(
    stratum = 1L,
    category = c(1L, 2L, 8L),
    value = c(30, 10, 20)
  )

  out <- redistribute_unknown(
    x,
    category = "category",
    value = "value",
    by = "stratum",
    unknown = 8L,
    targets = 1:2
  )

  testthat::expect_equal(out[category == 1L, value], 45)
  testthat::expect_equal(out[category == 2L, value], 15)
  testthat::expect_equal(sum(out$value), sum(x$value))
  testthat::expect_false(any(out$category == 8L))
})

testthat::test_that("unknown categories use equal shares when targets are zero", {
  x <- data.table::data.table(
    stratum = 1L,
    category = c(1L, 2L, 8L),
    value = c(0, 0, 20)
  )

  out <- redistribute_unknown(
    x, "category", "value", "stratum", 8L, 1:2
  )

  testthat::expect_equal(out$value, c(10, 10))
})

testthat::test_that("early population groups use the 1999-2000 reference shares", {
  x <- data.table::data.table(
    stratum = 1L,
    DeathYear = c(1997L, 1997L, 1997L, 1999L, 1999L, 2000L, 2000L),
    Popgroup = c(1L, 2L, 8L, 1L, 2L, 1L, 2L),
    num = c(0, 0, 40, 80, 20, 80, 20)
  )

  out <- redistribute_population_group(
    x,
    value = "num",
    year = "DeathYear",
    category = "Popgroup",
    by = "stratum",
    unknown = 8L,
    targets = 1:2,
    reference_years = 1999:2000,
    early_year_max = 1998L
  )

  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 1L, num], 32)
  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 2L, num], 8)
  testthat::expect_equal(sum(out$num), sum(x$num))
})

testthat::test_that("row-wise cause redistribution preserves totals", {
  x <- data.table::data.table(
    id = 1:2,
    c10 = c(12, 10),
    c20 = c(3, 0),
    c21 = c(1, 0)
  )

  out <- redistribute_cause_columns(x, sources = 10, targets = c(20, 21))

  testthat::expect_equal(out$c10, c(0, 0))
  testthat::expect_equal(out$c20, c(12, 5))
  testthat::expect_equal(out$c21, c(4, 5))
  testthat::expect_equal(rowSums(out[, .(c10, c20, c21)]), c(16, 10))
})

testthat::test_that("absent reference categories receive zero rather than equal early-year share", {
  x <- data.table::data.table(
    stratum = 1L,
    DeathYear = c(1997L, 1997L, 1997L, 1997L, 1999L, 1999L, 2000L, 2000L),
    Popgroup = c(1L, 2L, 3L, 8L, 1L, 2L, 1L, 2L),
    num = c(0, 0, 0, 30, 40, 10, 40, 10)
  )

  out <- redistribute_population_group(
    x,
    value = "num",
    year = "DeathYear",
    category = "Popgroup",
    by = "stratum",
    unknown = 8L,
    targets = 1:3,
    reference_years = 1999:2000,
    early_year_max = 1998L
  )

  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 1L, num], 24)
  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 2L, num], 6)
  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 3L, num], 0)
  testthat::expect_equal(sum(out$num), sum(x$num))
})

testthat::test_that("redistribution rejects unrecognised categories", {
  x <- data.table::data.table(stratum = 1L, category = c(1L, 7L, 8L), value = c(1, 2, 3))
  testthat::expect_error(
    redistribute_unknown(x, "category", "value", "stratum", 8L, 1:2),
    "Unexpected category"
  )
})

testthat::test_that("redist matches the Stata formula across synthetic valid rows", {
  set.seed(20260805)
  n <- 200L
  x <- data.frame(
    source_a = stats::runif(n, 0, 50),
    source_b = stats::runif(n, 0, 25),
    target_a = stats::runif(n, 0, 100),
    target_b = stats::runif(n, 0, 100),
    target_c = stats::runif(n, 0, 100)
  )
  x[1:20, c("target_a", "target_b", "target_c")] <- 0

  source_total <- rowSums(x[c("source_a", "source_b")])
  target_total <- rowSums(x[c("target_a", "target_b", "target_c")])
  expected <- as.matrix(x[c("target_a", "target_b", "target_c")])
  proportional <- target_total > 0
  expected[proportional, ] <- expected[proportional, , drop = FALSE] *
    (1 + source_total[proportional] / target_total[proportional])
  expected[!proportional, ] <- source_total[!proportional] / 3

  out <- redist(
    x,
    c("source_a", "source_b"),
    c("target_a", "target_b", "target_c"),
    quiet = TRUE
  )

  testthat::expect_equal(
    as.matrix(out[c("target_a", "target_b", "target_c")]),
    expected,
    tolerance = 1e-12
  )
  testthat::expect_equal(rowSums(expected), source_total + target_total)
})

testthat::test_that("redist validates conditions and preserves no-match inputs", {
  x <- data.frame(source = c(2, 4), target = c(3, 6))

  testthat::expect_error(
    redist(x, "source", "target", c(TRUE, FALSE, TRUE), quiet = TRUE),
    "length 1 or nrow"
  )

  out <- redist(x, "source", "target", FALSE, quiet = TRUE)
  testthat::expect_identical(out, x)
})

testthat::test_that("data.table copying is safe by default", {
  x <- data.table::data.table(source = 3L, target_a = 1L, target_b = 2L)
  out <- redist(x, "source", c("target_a", "target_b"), quiet = TRUE)

  testthat::expect_true("source" %in% names(x))
  testthat::expect_equal(x$target_a, 1)
  testthat::expect_false("source" %in% names(out))
  testthat::expect_equal(out$target_a, 2)
  testthat::expect_equal(out$target_b, 4)
})

testthat::test_that("identical source and target lists are an explicit no-op", {
  x <- data.frame(a = c(1, 2), b = c(3, 4))
  out <- redist(x, c("a", "b"), c("a", "b"), quiet = TRUE)
  testthat::expect_identical(out, x)
})

testthat::test_that("missing-count policy can reject incomplete inputs", {
  x <- data.frame(source = 1, target = NA_real_)
  testthat::expect_error(
    redist(x, "source", "target", missing_counts = "error", quiet = TRUE),
    "missing count"
  )
})

testthat::test_that("long-form redistribution rejects negative count cells", {
  x <- data.table::data.table(
    stratum = 1L,
    category = c(1L, 8L),
    value = c(2, -1)
  )
  testthat::expect_error(
    redistribute_unknown(x, "category", "value", "stratum", 8L, 1:2),
    "negative count"
  )
})

testthat::test_that("long-form redistribution preserves every group separately", {
  x <- data.table::data.table(
    stratum = rep(1:2, each = 3),
    category = rep(c(1L, 2L, 8L), 2),
    value = c(4, 1, 5, 0, 0, 8)
  )
  out <- redistribute_unknown(
    x, "category", "value", "stratum", 8L, 1:2
  )

  before <- x[, .(total = sum(value)), by = stratum]
  after <- out[, .(total = sum(value)), by = stratum]
  testthat::expect_equal(after$total, before$total)
})

testthat::test_that("redist rejects duplicate names and non-finite counts", {
  x <- data.frame(source = 1, target = 2)
  testthat::expect_error(
    redist(x, c("source", "source"), "target", quiet = TRUE),
    "duplicate column"
  )

  infinite <- data.frame(source = Inf, target = 2)
  testthat::expect_error(
    redist(infinite, "source", "target", quiet = TRUE),
    "non-finite"
  )
})

testthat::test_that("unqualified zero-row redist still consumes source columns", {
  x <- data.frame(source = numeric(), target = numeric())
  out <- redist(x, "source", "target", quiet = TRUE)
  testthat::expect_false("source" %in% names(out))
  testthat::expect_true("target" %in% names(out))
})

testthat::test_that("condition missingness can be included deliberately", {
  x <- data.frame(
    flag = c(TRUE, NA),
    source = c(2, 4),
    target = c(3, 6)
  )
  out <- redist(
    x,
    "source",
    "target",
    flag,
    condition_na = "include",
    quiet = TRUE
  )
  testthat::expect_equal(out$source, c(0, 0))
  testthat::expect_equal(out$target, c(5, 10))
})

testthat::test_that("a no-match data.table condition does not mutate storage", {
  x <- data.table::data.table(source = 1L, target = 2L)
  out <- redist(x, "source", "target", FALSE, copy = FALSE, quiet = TRUE)
  testthat::expect_type(x$target, "integer")
  testthat::expect_identical(out, x)
})


testthat::test_that("target multipliers continuously reweight approved destinations", {
  x <- data.frame(source = 60, target_a = 30, target_b = 30, target_c = 0)
  out <- redist(
    x,
    source_vars = "source",
    target_vars = c("target_a", "target_b", "target_c"),
    target_multipliers = c(2, 1, 1),
    quiet = TRUE
  )
  weighted_before <- c(60, 30, 0)
  expected_add <- 60 * weighted_before / sum(weighted_before)
  testthat::expect_equal(
    unlist(out[c("target_a", "target_b", "target_c")]),
    c(30, 30, 0) + expected_add,
    tolerance = 1e-12
  )
  testthat::expect_equal(sum(out), 120, tolerance = 1e-12)
})

testthat::test_that("all-one target multipliers reproduce deterministic redistribution", {
  x <- data.frame(source = c(12, 8), target_a = c(3, 0), target_b = c(1, 0))
  point <- redist(x, "source", c("target_a", "target_b"), quiet = TRUE)
  weighted <- redist(
    x,
    "source",
    c("target_a", "target_b"),
    target_multipliers = c(1, 1),
    quiet = TRUE
  )
  testthat::expect_equal(point, weighted)
})
