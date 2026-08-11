testthat::test_that("calendar ANC prevalence reproduces legacy age-specific lags", {
  source <- data.table::data.table(Year = 1992:2008)
  for (province in 1:9) {
    source[, (paste0("P", province)) := Year + province / 100]
  }
  # The legacy source explicitly overwrites Y2008 with Y2007. A supplied 2008
  # value must therefore have no influence on the derived 2009 age-2 estimate.
  source[Year == 2008L, P1 := 999]
  cfg <- list(settings = list(start_year = 1997L, end_year = 2010L))

  out <- build_calendar_anc_prevalence(source, cfg)

  testthat::expect_equal(
    out[Death_Prov == 1L & DeathYear == 1997L & age5 == 2L, ANCPrev],
    1996.01
  )
  testthat::expect_equal(
    out[Death_Prov == 1L & DeathYear == 1997L & age5 == 3L, ANCPrev],
    1994.01
  )
  testthat::expect_equal(
    out[Death_Prov == 1L & DeathYear == 1997L & age5 == 4L, ANCPrev],
    1992.01
  )
  # The legacy source sets 2008 equal to 2007 before deriving the 2009 age-2 row.
  testthat::expect_equal(
    out[Death_Prov == 1L & DeathYear == 2009L & age5 == 2L, ANCPrev],
    2007.01
  )
})


testthat::test_that("missing 2009 calendar prevalence falls back to the 2008 estimate", {
  source <- data.table::data.table(Year = 1992:2007)
  for (province in 1:9) {
    source[, (paste0("P", province)) := Year + province / 100]
  }
  source[Year == 2006L, P1 := NA_real_]
  cfg <- list(settings = list(start_year = 1997L, end_year = 2009L))

  out <- build_calendar_anc_prevalence(source, cfg)

  testthat::expect_equal(
    out[Death_Prov == 1L & DeathYear == 2009L & age5 == 3L, ANCPrev],
    out[Death_Prov == 1L & DeathYear == 2008L & age5 == 3L, ANCPrev]
  )
  testthat::expect_equal(
    out[Death_Prov == 1L & DeathYear == 2009L & age5 == 3L, ANCPrev],
    2005.01
  )
})

testthat::test_that("prepared HIV prevalence panel freezes ANC prevalence after 2009", {
  source <- data.table::data.table(Year = 1992:2007)
  for (province in 1:9) {
    source[, (paste0("P", province)) := (Year - 1990) / 100 + province / 1000]
  }
  population <- data.table::CJ(
    Death_Prov = 1:9,
    Sex = 1:2,
    DeathYear = 1997:2010,
    age5 = 1:20,
    Popgroup = 1:4,
    sorted = FALSE
  )
  population[, Pop := 100]
  cfg <- list(settings = list(start_year = 1997L, end_year = 2010L))

  out <- prepare_prevalence_population(source, population, cfg)

  value_2009 <- out[
    Death_Prov == 1L & Sex == 1L & DeathYear == 2009L & age5 == 2L,
    ANCPrev
  ]
  value_2010 <- out[
    Death_Prov == 1L & Sex == 1L & DeathYear == 2010L & age5 == 2L,
    ANCPrev
  ]
  testthat::expect_equal(value_2010, value_2009)
  testthat::expect_equal(
    out[Death_Prov == 1L & Sex == 1L & DeathYear == 2010L & age5 == 2L, ASSABase],
    400
  )
  testthat::expect_true(any(out$Death_Prov == 10L))
  testthat::expect_true(any(out$Sex == 3L))
})
