if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install testthat before running the tests.", call. = FALSE)
}

testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
