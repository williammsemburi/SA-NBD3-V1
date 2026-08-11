testthat::test_that("collapse and data.table grouped sums agree", {
  testthat::skip_if_not_installed("collapse")

  x <- data.table::data.table(
    group_a = c(1L, 1L, 1L, 2L, 2L, 2L),
    group_b = c("a", "a", "b", "a", "a", "b"),
    num = c(1, 2, 4, 8, 16, 32)
  )

  collapse_out <- fast_group_sum(
    x,
    by = c("group_a", "group_b"),
    value = "num",
    backend = "collapse",
    sort = TRUE
  )
  data_table_out <- fast_group_sum(
    x,
    by = c("group_a", "group_b"),
    value = "num",
    backend = "data.table",
    sort = TRUE
  )

  data.table::setorderv(collapse_out, c("group_a", "group_b"))
  data.table::setorderv(data_table_out, c("group_a", "group_b"))
  testthat::expect_equal(collapse_out$num, data_table_out$num)
  testthat::expect_equal(
    collapse_out[, .(group_a, group_b)],
    data_table_out[, .(group_a, group_b)]
  )
  testthat::expect_equal(sum(collapse_out$num), sum(x$num))
})


testthat::test_that("collapse retains observed factor combinations only", {
  testthat::skip_if_not_installed("collapse")

  x <- data.table::data.table(
    group_a = factor(c("a", "a"), levels = c("a", "b")),
    group_b = c(1L, 1L),
    num = c(2, 3)
  )
  out <- fast_group_sum(
    x,
    by = c("group_a", "group_b"),
    value = "num",
    backend = "collapse",
    sort = FALSE
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(as.character(out$group_a), "a")
  testthat::expect_equal(out$num, 5)
})


testthat::test_that("verified COD records collapse to the exact maximum key", {
  testthat::skip_if_not_installed("collapse")

  x <- data.table::data.table(
    DeathType = c(1L, 1L, 1L),
    Death_Prov = c(1L, 1L, 1L),
    Sex = c(1L, 1L, 2L),
    DeathYear = c(2001L, 2001L, 2001L),
    DeathMonth = c(1L, 1L, 1L),
    nbdcode = c(10L, 10L, 10L),
    age_ = c(25, 25, 25),
    age_u5 = c(9L, 9L, 9L),
    Popgroup = c(1L, 1L, 1L)
  )
  cfg <- list(settings = list(cod_aggregation_backend = "collapse"))

  out <- collapse_verified_cod(x, cfg)

  testthat::expect_equal(names(out), c(COD_VERIFIED_GROUP_KEY, "num"))
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(sort(out$num), c(1, 2))
  testthat::expect_equal(sum(out$num), nrow(x))
  testthat::expect_equal(anyDuplicated(out, by = COD_VERIFIED_GROUP_KEY), 0L)
})


testthat::test_that("production collapse can release certificate columns by reference", {
  testthat::skip_if_not_installed("collapse")

  x <- data.table::data.table(
    DeathType = 1L,
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2001L,
    DeathMonth = 1L,
    nbdcode = 10L,
    age_ = 25,
    age_u5 = 9L,
    Popgroup = 1L,
    certificate_working_field = "discard me"
  )
  cfg <- list(settings = list(cod_aggregation_backend = "collapse"))

  out <- collapse_verified_cod(x, cfg, copy = FALSE)

  testthat::expect_false("certificate_working_field" %in% names(x))
  testthat::expect_equal(names(x), c(COD_VERIFIED_GROUP_KEY, "num"))
  testthat::expect_equal(out$num, 1)
})


testthat::test_that("unknown sex and age use compact wide redistribution", {
  cfg <- list(settings = list(cod_drop_zero_cells = TRUE))

  sex_data <- data.table::data.table(
    Death_Prov = 1L,
    Popgroup = 1L,
    DeathYear = 2001L,
    nbdcode = c(10L, 10L, 10L, 11L),
    age5 = 5L,
    Sex = c(1L, 2L, 8L, 8L),
    num = c(3, 1, 4, 6)
  )
  sex_out <- redistribute_cod_sex(sex_data, cfg)
  data.table::setorder(sex_out, nbdcode, Sex)

  testthat::expect_equal(
    sex_out[nbdcode == 10L, num],
    c(6, 2)
  )
  testthat::expect_equal(
    sex_out[nbdcode == 11L, num],
    c(3, 3)
  )
  testthat::expect_equal(sum(sex_out$num), sum(sex_data$num))

  age_data <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2001L,
    nbdcode = c(10L, 10L, 10L, 11L),
    Popgroup = 1L,
    age5 = c(1L, 2L, 999L, 999L),
    num = c(3, 1, 4, 20)
  )
  age_out <- redistribute_cod_age(age_data, cfg)
  data.table::setorder(age_out, nbdcode, age5)

  testthat::expect_equal(age_out[nbdcode == 10L, num], c(6, 2))
  testthat::expect_equal(nrow(age_out[nbdcode == 11L]), 20L)
  testthat::expect_equal(age_out[nbdcode == 11L, unique(num)], 1)
  testthat::expect_equal(sum(age_out$num), sum(age_data$num))
})


testthat::test_that("population redistribution uses 1999-2000 reference shares", {
  cfg <- list(settings = list(cod_drop_zero_cells = TRUE))
  x <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    nbdcode = 10L,
    age5 = 5L,
    DeathYear = c(
      1997L, 1997L, 1997L,
      1999L, 1999L,
      2000L, 2000L,
      2001L, 2001L, 2001L
    ),
    Popgroup = c(
      1L, 2L, 8L,
      1L, 2L,
      1L, 2L,
      1L, 2L, 8L
    ),
    num = c(
      5, 5, 8,
      30, 10,
      20, 20,
      1, 3, 4
    )
  )

  out <- redistribute_cod_population_group(x, cfg)
  data.table::setorder(out, DeathYear, Popgroup)

  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 1L, num], 10)
  testthat::expect_equal(out[DeathYear == 1997L & Popgroup == 2L, num], 8)
  testthat::expect_equal(out[DeathYear == 2001L & Popgroup == 1L, num], 2)
  testthat::expect_equal(out[DeathYear == 2001L & Popgroup == 2L, num], 6)
  testthat::expect_equal(sum(out$num), sum(x$num))
})


testthat::test_that("early population redistribution uses equal shares when reference years are absent", {
  cfg <- list(settings = list(cod_drop_zero_cells = TRUE))
  x <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    nbdcode = 99L,
    age5 = 5L,
    DeathYear = c(1997L, 1997L, 1997L),
    Popgroup = c(1L, 2L, 8L),
    num = c(3, 1, 4)
  )

  out <- redistribute_cod_population_group(x, cfg)
  data.table::setorder(out, Popgroup)

  testthat::expect_equal(out[Popgroup == 1L, num], 4)
  testthat::expect_equal(out[Popgroup == 2L, num], 2)
  testthat::expect_equal(out[Popgroup == 3L, num], 1)
  testthat::expect_equal(out[Popgroup == 4L, num], 1)
  testthat::expect_equal(sum(out$num), sum(x$num))
})


testthat::test_that("annual aggregation corrects only the affected 2004 cells", {
  cfg <- list(settings = list(cod_aggregation_backend = "data.table"))
  x <- data.table::data.table(
    Death_Prov = c(1L, 1L, 1L, 1L),
    Sex = 1L,
    DeathYear = c(2003L, 2004L, 2006L, 2004L),
    DeathMonth = c(1L, 1L, 1L, 1L),
    nbdcode = c(143L, 143L, 143L, 10L),
    age5 = c(3L, 3L, 3L, 3L),
    Popgroup = 1L,
    num = c(30, 100, 60, 7)
  )

  out <- aggregate_cod_annual_groups(x, cfg)

  testthat::expect_equal(
    out[DeathYear == 2004L & nbdcode == 143L & age5 == 3L, num],
    40
  )
  testthat::expect_equal(out[DeathYear == 2004L & nbdcode == 10L, num], 7)
  testthat::expect_false("DeathMonth" %in% names(out))

  missing_endpoint <- x[DeathYear != 2006L]
  missing_out <- aggregate_cod_annual_groups(missing_endpoint, cfg)
  testthat::expect_equal(
    missing_out[DeathYear == 2004L & nbdcode == 143L & age5 == 3L, num],
    20
  )
})


testthat::test_that("age grouping preserves under-five detail for numeric missing age", {
  x <- data.table::data.table(
    DeathType = 1L,
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2001L,
    DeathMonth = 1L,
    nbdcode = 10L,
    age_ = c(NA_real_, 999),
    age_u5 = c(2L, 2L),
    Popgroup = 1L,
    num = 1
  )

  out <- prepare_cod_analysis_groups(x)
  data.table::setorder(out, age_)

  testthat::expect_equal(out[is.na(age_), age5], 1L)
  testthat::expect_equal(out[age_ == 999, age5], 999L)
})

testthat::test_that("non-live and missing death types are excluded after early collapse", {
  x <- data.table::data.table(
    DeathType = c(1L, 2L, NA_integer_),
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2001L,
    DeathMonth = 1L,
    nbdcode = 10L,
    age_ = 25,
    age_u5 = 9L,
    Popgroup = 1L,
    num = c(2, 3, 4)
  )

  out <- prepare_cod_analysis_groups(x)

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$num, 2)
})
