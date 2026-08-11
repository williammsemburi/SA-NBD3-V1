testthat::test_that("ICD classification uses three-character categories", {
  lookup <- data.table::data.table(
    icd10 = c("000", "888", "999", "A00", "B20"),
    nbdcode = c(143L, 143L, 143L, 4L, 2L)
  )
  x <- data.table::data.table(
    code = c("A00.1", "a009", "B20", NA_character_, "000"),
    age_ = c(10, 10, 30, 40, 40),
    age_u5 = 9L
  )

  out <- map_icd_to_nbd(x, "code", lookup)

  testthat::expect_equal(out$nbdcode, c(4L, 4L, 2L, 143L, 143L))
})

testthat::test_that("ICD classification reproduces age-dependent Stata overrides", {
  lookup <- data.table::data.table(
    icd10 = c("000", "888", "999", "A33", "A50"),
    nbdcode = c(143L, 143L, 143L, 5L, 3L)
  )
  x <- data.table::data.table(
    code = c(
      "A34", "A34", "A34", "A34", "A33", "A50", "A50",
      "P23", "P23", "P23", "P23", "G80", "G80", "G80", "P23"
    ),
    age_ = c(0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 2, 19, 20, NA, NA),
    age_u5 = c(1L, 2L, 4L, 5L, 2L, 3L, 5L, 2L, 3L, 4L, 5L, 9L, 9L, 9L, NA)
  )

  out <- map_icd_to_nbd(x, "code", lookup)

  testthat::expect_equal(
    out$nbdcode,
    c(
      NA, 22L, 5L, 5L, 22L, 22L, 3L,
      20L, 11L, 11L, 11L, 149L, 194L, 194L, 11L
    )
  )
})

testthat::test_that("ICD classification preserves late source overrides", {
  lookup <- data.table::fread(
    file.path(.test_root, "data", "lookups", "icd10_to_nbd.csv")
  )
  x <- data.table::data.table(
    code = c("B94", "O98", "Y35", "B33"),
    age_ = 40,
    age_u5 = 9L
  )

  out <- map_icd_to_nbd(x, "code", lookup)

  testthat::expect_equal(out$nbdcode, c(191L, 213L, 214L, 215L))
})

testthat::test_that("age-dependent ICD categories require age fields", {
  lookup <- data.table::data.table(
    icd10 = c("000", "888", "999"),
    nbdcode = c(143L, 143L, 143L)
  )
  x <- data.table::data.table(code = "P23")

  testthat::expect_error(
    map_icd_to_nbd(x, "code", lookup),
    "requires column"
  )
})

testthat::test_that("ZA mapping creates detailed and aggregate causes", {
  x <- data.table::data.table(
    Death_Prov = 1L,
    Sex = 1L,
    DeathYear = 2000L,
    age5 = 1L,
    Popgroup = 1L,
    nbdcode = c(1L, 2L),
    Count = c(3, 7)
  )
  lookup <- data.table::data.table(
    analysis_code = c(1L, 1L, 1L, 2L, 2L, 2L),
    za_code = c(1L, 150L, 172L, 2L, 150L, 172L),
    weight = 1,
    hierarchy = c(
      "detailed", "cause_group", "broad_group",
      "detailed", "cause_group", "broad_group"
    )
  )

  out <- apply_analysis_to_za(x, lookup, value = "Count")

  testthat::expect_equal(out[nbdcode == 1L, Count], 3)
  testthat::expect_equal(out[nbdcode == 2L, Count], 7)
  testthat::expect_equal(out[nbdcode == 150L, Count], 10)
  testthat::expect_equal(out[nbdcode == 172L, Count], 10)
})

testthat::test_that("two analysis causes can combine into one detailed ZA cause", {
  x <- data.table::data.table(
    stratum = 1L,
    nbdcode = c(131L, 137L),
    Count = c(2, 5)
  )
  lookup <- data.table::data.table(
    analysis_code = c(131L, 137L),
    za_code = c(132L, 132L),
    weight = 1,
    hierarchy = "detailed"
  )

  out <- apply_analysis_to_za(x, lookup, value = "Count")

  testthat::expect_equal(out[nbdcode == 132L, Count], 7)
})

testthat::test_that("ZA mapping permits omitted structural zeros", {
  x <- data.table::data.table(
    stratum = 1L,
    nbdcode = c(1L, 214L),
    Count = c(5, 0)
  )
  lookup <- data.table::data.table(
    analysis_code = 1L,
    za_code = 1L,
    weight = 1,
    hierarchy = "detailed"
  )

  out <- apply_analysis_to_za(x, lookup, value = "Count")

  testthat::expect_equal(out$Count, 5)
})

testthat::test_that("ZA mapping rejects omitted non-zero causes", {
  x <- data.table::data.table(
    stratum = 1L,
    nbdcode = 214L,
    Count = 3
  )
  lookup <- data.table::data.table(
    analysis_code = 1L,
    za_code = 1L,
    weight = 1,
    hierarchy = "detailed"
  )

  testthat::expect_error(
    apply_analysis_to_za(x, lookup, value = "Count", strict = TRUE),
    "omits non-zero analysis cause"
  )
})

testthat::test_that("committed lookup tables have the expected source-derived structure", {
  icd <- data.table::fread(file.path(.test_root, "data", "lookups", "icd10_to_nbd.csv"))
  rules <- data.table::fread(file.path(.test_root, "data", "lookups", "nbd_rule_manifest.csv"))
  za <- data.table::fread(file.path(.test_root, "data", "lookups", "analysis_to_za.csv"))
  diagnostics <- data.table::fread(
    file.path(.test_root, "data", "lookups", "lookup_build_diagnostics.csv")
  )

  testthat::expect_equal(nrow(icd), 1847L)
  testthat::expect_false(any(c("A34", "A35", "P23", "G80") %in% icd$icd10))
  testthat::expect_equal(nrow(rules), 241L)
  testthat::expect_equal(sum(rules$age_sensitive), 10L)
  testthat::expect_equal(nrow(za), 700L)
  testthat::expect_setequal(unique(za$za_code), c(1:139, 145:181))
  testthat::expect_false(214L %in% za$analysis_code)
  testthat::expect_equal(
    diagnostics[metric == "icd_source_design_semantic_cases", as.integer(value)],
    2947L
  )
})
