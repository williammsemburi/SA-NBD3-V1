testthat::test_that("observed pseudo-cause cells outside the HIV model are retained", {
  pop_pseudo <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2012L,
    age5 = 1L,
    Popgroup = c(1L, 2L),
    nbdcode = 222L,
    NBD = c(12, 8)
  )
  misclassified <- data.table::data.table(
    Death_Prov = integer(),
    Sex = integer(),
    DeathYear = integer(),
    age5 = integer(),
    nbdcode = integer(),
    Misclassified = numeric()
  )

  out <- allocate_hiv_to_population_groups(pop_pseudo, misclassified)

  testthat::expect_equal(sum(out$NBD), 20)
  testthat::expect_equal(sum(out$AIDSDeaths), 0)
  testthat::expect_equal(sum(out$NonHIV), 20)
  testthat::expect_equal(out[Popgroup == 1L, NonHIV], 12)
  testthat::expect_equal(out[Popgroup == 2L, NonHIV], 8)
})
