find_test_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "R", "00_core.R"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not locate the project root.")
    current <- parent
  }
}

.test_root <- find_test_root()
.test_modules <- sort(list.files(
  file.path(.test_root, "R"),
  pattern = "^[0-9]{2}_.*\\.R$",
  full.names = TRUE
))
invisible(lapply(.test_modules, sys.source, envir = .GlobalEnv))
