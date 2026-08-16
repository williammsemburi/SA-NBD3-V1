#!/usr/bin/env Rscript

# Launch the completed publication report without rebuilding analytical inputs.

locate_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "_targets.R")) &&
        file.exists(file.path(current, "report", "nbd3_results_report.Rmd"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the SA-NBD3 repository root.", call. = FALSE)
    }
    current <- parent
  }
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Package 'rmarkdown' is required. Run Rscript install_packages.R.",
       call. = FALSE)
}

root <- locate_root()
setwd(root)
manifest <- file.path(
  root,
  "output",
  "report-data",
  "ui_uncertainty_cache",
  "manifest.yml"
)
if (!file.exists(manifest)) {
  stop(
    "The query-optimised report cache is missing. Run ",
    "Rscript dev/build_shiny_ui_cache.R first.",
    call. = FALSE
  )
}

rmarkdown::run(
  file.path(root, "report", "nbd3_results_report.Rmd"),
  shiny_args = list(
    launch.browser = TRUE,
    host = "127.0.0.1"
  )
)
