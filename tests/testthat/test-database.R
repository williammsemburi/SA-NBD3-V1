testthat::test_that("database shifts pipeline ages before merging population", {
  deaths_yll <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    Popgroup = 1L,
    age = 2L,
    nbdcode = 1L,
    Count = 5,
    YLL00 = 50,
    YLL03 = 40,
    YLL015 = 45
  )
  population <- data.table::data.table(
    Popgroup = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    age5 = 2L,
    Death_Prov = 1L,
    Pop = 100
  )
  factors <- data.table::data.table(age = 1L, F = 1)

  out <- build_final_database(deaths_yll, population, factors)
  row <- out[Grouping == 1L & Sex == 1L & nbdcode == 1L & DeathYear == 2000L & Age == 1L]

  testthat::expect_equal(nrow(row), 1L)
  testthat::expect_equal(row$Deaths, 5)
  testthat::expect_equal(row$Pop, 100)
})

testthat::test_that("combined ZA 140/141 ASR is recalculated from combined deaths", {
  deaths_yll <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    Popgroup = 1L,
    age = 2L,
    nbdcode = c(140L, 141L),
    Count = c(5, 10),
    YLL00 = c(50, 100),
    YLL03 = c(40, 80),
    YLL015 = c(45, 90)
  )
  population <- data.table::data.table(
    Popgroup = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    age5 = 2L,
    Death_Prov = 1L,
    Pop = 100
  )
  factors <- data.table::data.table(age = 1L, F = 1)

  out <- build_final_database(deaths_yll, population, factors)
  row <- out[
    Grouping == 1L & Sex == 1L & nbdcode == 140L &
      DeathYear == 2000L & Age == 1L
  ]

  testthat::expect_equal(nrow(row), 1L)
  testthat::expect_equal(row$Deaths, 15)
  testthat::expect_equal(row$Pop, 100)
  testthat::expect_equal(row$ASR, 15000)
})

testthat::test_that("database population includes a structural neonatal denominator", {
  population <- data.table::data.table(
    Popgroup = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    age5 = 2:20,
    Death_Prov = 1L,
    Pop = seq_len(19L)
  )

  out <- prepare_database_population(population)

  testthat::expect_equal(
    out[Grouping == 1L & Sex == 1L & DeathYear == 2000L & age == 0L, Pop],
    0
  )
  testthat::expect_equal(
    out[Grouping == 11L & Sex == 1L & DeathYear == 2000L & age == 0L, Pop],
    0
  )
})
