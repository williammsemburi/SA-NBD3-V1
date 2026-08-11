testthat::test_that("HIV reallocation preserves the total number of deaths", {
  original <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    age5 = 8L,
    Popgroup = 1L,
    nbdcode = c(1L, 176L, 2L, 50L),
    Deaths = c(100, 20, 5, 7)
  )

  allocation <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    age5 = 8L,
    Popgroup = 1L,
    nbdcode = c(215L, 2L),
    NBD = c(120, 5),
    AIDSDeaths = c(30, 5),
    NonHIV = c(90, 0)
  )

  out <- construct_hiv_reallocated_long(original, allocation)

  testthat::expect_equal(sum(out$Deaths), sum(original$Deaths))
  testthat::expect_equal(out[nbdcode == 1L, Deaths], 90)
  testthat::expect_equal(out[nbdcode == 2L, Deaths], 35)
  testthat::expect_equal(out[nbdcode == 50L, Deaths], 7)
})

testthat::test_that("pseudo-cause 234 is excluded from background trends by default", {
  cfg <- list(settings = list(hiv_include_pseudo_234_in_background_model = FALSE))
  testthat::expect_false(234L %in% hiv_background_model_codes(cfg))
  cfg$settings$hiv_include_pseudo_234_in_background_model <- TRUE
  testthat::expect_true(234L %in% hiv_background_model_codes(cfg))
})

testthat::test_that("zero 1997 pseudo-cause totals do not invent cause composition", {
  panel <- data.table::CJ(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 1997L,
    age5 = 4L,
    nbdcode = HIV_PSEUDO_CODES,
    sorted = FALSE
  )
  panel[, NBD := 0]
  intercepts <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    age5 = 4L,
    all_cause_intercept = 100
  )

  out <- allocate_intercept_to_causes(panel, intercepts)
  testthat::expect_true(all(is.na(out$cause_intercept)))
})

testthat::test_that("HIV coefficient draws use the fitted covariance matrix", {
  coefficient <- c(0.4, -0.2)
  zero_covariance <- matrix(0, nrow = 2L, ncol = 2L)
  testthat::expect_equal(
    hiv_draw_mvn(coefficient, zero_covariance),
    coefficient,
    tolerance = 1e-12
  )

  covariance <- matrix(c(0.04, 0.01, 0.01, 0.09), 2L, 2L)
  set.seed(4401L)
  first <- hiv_draw_mvn(coefficient, covariance)
  set.seed(4401L)
  second <- hiv_draw_mvn(coefficient, covariance)
  testthat::expect_equal(first, second, tolerance = 1e-12)
  testthat::expect_false(isTRUE(all.equal(first, coefficient)))
})

testthat::test_that("one HIV model draw is shared across its linked cells", {
  artifact <- list(
    models = list(list(
      nbdcode = 215L,
      cells = data.table::data.table(
        Death_Prov = c(1L, 2L),
        Sex = c(1L, 1L),
        g1_point = c(0.10, 0.20),
        constant_point = c(0, 0)
      ),
      model_status = c("full_interaction", "full_interaction"),
      full = list(
        coefficient = c(0.5, -0.1),
        coefficient_names = c("b1", "b2"),
        covariance = matrix(c(0.04, 0.01, 0.01, 0.04), 2L, 2L),
        contrast = matrix(c(1, 0, 1, 1), nrow = 2L, byrow = TRUE),
        valid_rows = c(1L, 2L)
      ),
      fallback = NULL
    ))
  )

  point <- draw_hiv_background_trends(
    artifact, seed = 71L, stochastic = FALSE
  )
  draw_a <- draw_hiv_background_trends(
    artifact, seed = 71L, stochastic = TRUE
  )
  draw_b <- draw_hiv_background_trends(
    artifact, seed = 71L, stochastic = TRUE
  )

  testthat::expect_equal(point$g1, c(0.10, 0.20))
  testthat::expect_equal(draw_a$g1, draw_b$g1, tolerance = 1e-12)
  testthat::expect_false(isTRUE(all.equal(draw_a$g1, point$g1)))
})
