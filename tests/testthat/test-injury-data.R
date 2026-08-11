testthat::test_that("IMS 2009 is harmonised to the complete production grid", {
  ims <- data.table::data.table(
    weight = 10,
    province = 9L,
    Cause_of_death = 1L,
    nbd_age = 1L,
    nbdgr = 4L,
    Population_group = 1L,
    Sex = 1L
  )

  out <- prepare_ims_2009(ims)

  testthat::expect_equal(nrow(out), 9L * 2L * 4L * 20L * length(INJURY_CODES))
  testthat::expect_equal(
    out[
      Death_Prov == 1L & Sex == 1L & Popgroup == 1L &
        age5 == 2L & nbdcode == 124L,
      Inj2009
    ],
    10
  )
  testthat::expect_equal(
    out[
      Death_Prov == 1L & Sex == 1L & Popgroup == 1L &
        age5 == 1L & nbdcode == 124L,
      Inj2009
    ],
    10
  )
})

testthat::test_that("NIMS wide columns are read as national 2000 counts", {
  nims <- data.table::data.table(
    code = c(124L, 141L),
    A11 = c(20, 80),
    A21 = c(10, 90)
  )

  out <- prepare_nims_2000(nims)

  testthat::expect_equal(nrow(out), 2L * 20L * length(INJURY_CODES))
  testthat::expect_equal(out[Sex == 1L & age5 == 2L & nbdcode == 124L, Inj2000], 20)
  testthat::expect_equal(out[Sex == 2L & age5 == 2L & nbdcode == 141L, Inj2000], 90)
  testthat::expect_equal(out[Sex == 1L & age5 == 1L & nbdcode == 124L, Inj2000], 20)

  # Effective information belongs to the whole sex-age composition. Causes
  # absent from the workbook must inherit that same information size.
  testthat::expect_equal(
    data.table::uniqueN(out[Sex == 1L & age5 == 2L, EffN2000]),
    1L
  )
  testthat::expect_equal(
    unique(out[Sex == 1L & age5 == 2L, EffN2000]),
    100
  )
  testthat::expect_equal(
    out[Sex == 1L & age5 == 2L & nbdcode == 125L, EffN2000],
    100
  )
})



testthat::test_that("NIMS zero and missing cells are floored with an explicit source-zero flag", {
  nims <- data.table::data.table(
    code = c(124L, 132L, 138L),
    A11 = c(20, 0, NA_real_),
    A21 = c(10, 1, 2)
  )

  out <- prepare_nims_2000(nims, count_floor = 1e-6)

  zero_132 <- out[Sex == 1L & age5 == 2L & nbdcode == 132L]
  missing_138 <- out[Sex == 1L & age5 == 2L & nbdcode == 138L]
  observed_124 <- out[Sex == 1L & age5 == 2L & nbdcode == 124L]

  testthat::expect_true(zero_132$nims_source_zero)
  testthat::expect_true(missing_138$nims_source_zero)
  testthat::expect_false(observed_124$nims_source_zero)
  testthat::expect_equal(zero_132$Inj2000, 1e-6, tolerance = 0)
  testthat::expect_equal(missing_138$Inj2000, 1e-6, tolerance = 0)
  testthat::expect_equal(observed_124$Inj2000, 20)
})

testthat::test_that("IMS fractions expand NIMS without changing national totals", {
  nims <- data.table::data.table(
    Sex = 1L,
    age5 = 3L,
    nbdcode = c(124L, 141L),
    Inj2000 = c(40, 60),
    EffN2000 = 100,
    nims_source_zero = FALSE
  )
  ims <- data.table::CJ(
    Death_Prov = 1:9,
    Popgroup = 1:4,
    Sex = 1L,
    age5 = 3L,
    nbdcode = c(124L, 141L),
    sorted = FALSE
  )
  ims[, Inj2009 := data.table::fifelse(Death_Prov == 1L & Popgroup == 1L, 1, 0)]

  out <- expand_nims_2000_with_ims(nims, ims)
  totals <- out[, .(expanded = sum(Inj2000)), by = .(Sex, age5, nbdcode)]

  testthat::expect_equal(totals[nbdcode == 124L, expanded], 40)
  testthat::expect_equal(totals[nbdcode == 141L, expanded], 60)
  testthat::expect_equal(
    out[Death_Prov == 1L & Popgroup == 1L & nbdcode == 124L, Inj2000],
    40
  )

  eff_by_stratum <- unique(out[, .(
    Death_Prov, Popgroup, Sex, age5, EffN2000
  )])
  testthat::expect_equal(sum(eff_by_stratum$EffN2000), 100)
  testthat::expect_equal(
    out[, data.table::uniqueN(EffN2000),
        by = .(Death_Prov, Popgroup, Sex, age5)]$V1,
    rep(1L, 36L)
  )
})

testthat::test_that("FAMHIS 2017 generic mechanisms preserve survey mass", {
  famhis <- data.table::data.table(
    prov = c(1L, 1L),
    gender_b = c(0L, 0L),
    ethnicity_b = c(1L, 1L),
    nbd_cod_mech = c("ZA126", "unknown"),
    nbdtrans = c("", "unknown"),
    age_Non_Natural = c(10, 10),
    age_unitss_Non_Natural = c(2L, 2L),
    wht = c(10, 4)
  )

  out <- prepare_famhis_2017(famhis)

  testthat::expect_equal(nrow(out), 9L * 2L * 4L * 20L * length(INJURY_CODES))
  testthat::expect_equal(
    out[
      Death_Prov == 1L & Sex == 1L & Popgroup == 1L &
        age5 == 5L & nbdcode == 124L,
      Inj2017
    ],
    14
  )
  testthat::expect_equal(sum(out$Inj2017), 14)
})

testthat::test_that("generic FAMHIS redistribution reaches a one-column donor level", {
  toy <- data.table::data.table(
    Death_Prov = c(1L, 2L),
    Sex = c(1L, 2L),
    Popgroup = c(1L, 1L),
    age5 = c(6L, 6L),
    generic = c(1, 0),
    target_a = c(0, 3),
    target_b = c(0, 1)
  )

  out <- redistribute_named_source_with_reference(
    toy,
    source = "generic",
    targets = c("target_a", "target_b")
  )

  testthat::expect_equal(out$generic, c(0, 0))
  testthat::expect_equal(out$target_a[1], 0.75, tolerance = 1e-12)
  testthat::expect_equal(out$target_b[1], 0.25, tolerance = 1e-12)
  testthat::expect_equal(
    sum(out$target_a + out$target_b),
    5,
    tolerance = 1e-12
  )
})
