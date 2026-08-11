testthat::test_that("child completeness requires ages 2 and 3 and derives neonatal values", {
  temp <- tempfile(fileext = ".csv")
  years <- 1997:1998
  source <- data.table::CJ(Death_Prov = 1:9, age5 = 2:3, sorted = FALSE)
  for (year in years) source[, (paste0("y", year)) := 0.8 + age5 / 100]
  data.table::fwrite(source, temp)

  cfg <- list(
    root = dirname(temp),
    paths = list(raw = dirname(temp)),
    files = list(completeness_child = basename(temp)),
    settings = list(
      start_year = 1997L,
      completeness_freeze_year = 1998L,
      strict_checks = TRUE
    )
  )

  out <- read_child_completeness(cfg)
  testthat::expect_equal(nrow(out), 9L * 2L * 20L)
  testthat::expect_setequal(unique(out$age5), 1:20)
  testthat::expect_equal(
    out[age5 == 1L][order(Death_Prov, DeathYear), completeness],
    out[age5 == 2L][order(Death_Prov, DeathYear), completeness]
  )
  testthat::expect_true(all(out[age5 >= 4L, completeness] == 1))
})

testthat::test_that("child completeness still stops when age 2 or 3 is missing", {
  temp <- tempfile(fileext = ".csv")
  source <- data.table::CJ(Death_Prov = 1:9, age5 = 2L, sorted = FALSE)
  source[, y1997 := 0.9]
  data.table::fwrite(source, temp)

  cfg <- list(
    root = dirname(temp),
    paths = list(raw = dirname(temp)),
    files = list(completeness_child = basename(temp)),
    settings = list(
      start_year = 1997L,
      completeness_freeze_year = 1997L,
      strict_checks = TRUE
    )
  )

  testthat::expect_error(
    read_child_completeness(cfg),
    "required province-year-age values for ages 2-3"
  )
})

testthat::test_that("machine-precision negative completeness residuals are treated as zero", {
  natural <- data.table::data.table(
    Death_Prov = 7L,
    Sex = 1L,
    DeathYear = 1997L,
    age5 = 6L,
    Popgroup = c(1L, 2L),
    nbdcode = c(1L, 1L),
    Deaths = c(172.66343527719874, 28.429808426639784)
  )
  injury <- data.table::data.table(
    Death_Prov = 7L,
    Sex = 1L,
    DeathYear = 1997L,
    age5 = 6L,
    Popgroup = c(1L, 2L),
    nbdcode = c(124L, 124L),
    Deaths = c(505.00669282924656, 141.91098150754331),
    Inj = 1L
  )
  adjusted <- data.table::data.table(
    Death_Prov = 7L,
    Sex = 1L,
    DeathYear = 1997L,
    age5 = 6L,
    Adj = 649.44315007162834
  )
  cfg <- list(settings = list(
    completeness_freeze_year = 1997L,
    strict_checks = TRUE
  ))

  out <- testthat::expect_silent(
    derive_completeness_scalars(natural, injury, adjusted, cfg)
  )

  testthat::expect_equal(out$S2, 0)
  testthat::expect_true(all(out$S1 >= 0))
})
