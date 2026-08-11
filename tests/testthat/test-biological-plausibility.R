testthat::test_that("biological plausibility preserves natural and injury envelopes", {
  x <- data.table::data.table(
    Death_Prov = c(1L, 1L, 2L),
    Sex = c(1L, 1L, 1L),
    DeathYear = c(2015L, 2015L, 2015L),
    age5 = c(4L, 4L, 1L),
    Popgroup = c(1L, 2L, 1L),
    c1 = c(0, 10, 0),
    c39 = c(2, 0, 0),
    c140 = c(0, 0, 3),
    c141 = c(0, 0, 7)
  )

  out <- redistribute_garbage_codes(x)

  testthat::expect_equal(out[Popgroup == 1L & age5 == 4L, c1], 2)
  testthat::expect_equal(out[Popgroup == 1L & age5 == 4L, c39], 0)
  testthat::expect_equal(out[age5 == 1L, c140], 0)
  testthat::expect_equal(out[age5 == 1L, c141], 10)

  before <- c(2, 10, 10)
  after <- rowSums(as.matrix(out[, paste0("c", 1:214), with = FALSE]))
  testthat::expect_equal(after, before, tolerance = 1e-12)
})
