testthat::test_that("diabetes multiple-cause recoding follows Stata statement priority", {
  record <- data.table::data.table(
    DeathType = 1L,
    DeathYear = 2010L,
    AgeYear = 50,
    Sex = 1L,
    Popgroup = 1L,
    Death_Prov = 1L,
    UnderlyingCause = "E10",
    CauseA = "I20",
    CauseB = NA_character_,
    CauseC = NA_character_,
    CauseD = "I12"
  )
  lookup <- data.table::data.table(
    icd10 = c("E10", "I12", "I20", "R99"),
    nbdcode = c(56L, 82L, 83L, 143L)
  )

  out <- apply_icd10_verification(record, lookup)

  testthat::expect_equal(out$uc_recode, "I20")
  testthat::expect_equal(out$nbdcode, 83L)
})
