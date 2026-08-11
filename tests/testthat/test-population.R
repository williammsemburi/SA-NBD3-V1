testthat::test_that("population output is restricted to configured years", {
  temp <- tempfile(fileext = ".csv")
  source <- data.table::CJ(
    Popgroup = 1:4,
    Sex = 1:2,
    DeathYear = 1997:2022,
    age5 = 2:20,
    Death_Prov = 1:9,
    sorted = FALSE
  )
  source[, Pop := 100]
  data.table::fwrite(source, temp)

  cfg <- list(
    root = dirname(temp),
    paths = list(raw = dirname(temp)),
    files = list(
      population_current = basename(temp),
      population_workbook = "not-used.xls"
    ),
    settings = list(
      start_year = 1997L,
      end_year = 2019L,
      strict_checks = TRUE
    )
  )

  out <- prepare_population(cfg)

  testthat::expect_setequal(unique(out$DeathYear), 1997:2019)
  testthat::expect_setequal(unique(out$age5), 2:20)
  testthat::expect_equal(nrow(out), 9L * 2L * 4L * 23L * 19L)
})
