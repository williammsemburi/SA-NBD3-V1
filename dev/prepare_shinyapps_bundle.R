#!/usr/bin/env Rscript

# Prepare a minimal, publication-facing shinyapps.io bundle.
#
# The live application needs the report code, lookup/configuration files, point
# estimates, and the query-optimised uncertainty cache. It does not need raw
# mortality data, deterministic checkpoints, targets metadata, databases, or
# the 1,000 per-draw uncertainty files.

options(stringsAsFactors = FALSE, scipen = 999)

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

as_flag <- function(x, default = FALSE) {
  value <- tolower(trimws(as.character(x)))
  if (!length(value) || is.na(value) || !nzchar(value)) return(default)
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  default
}

copy_file_safe <- function(source, target) {
  if (!file.exists(source)) {
    stop("Required deployment file not found: ", source, call. = FALSE)
  }
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(source, target, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(ok)) stop("Could not copy: ", source, call. = FALSE)
  invisible(target)
}

copy_tree_safe <- function(source, target, exclude = character()) {
  if (!dir.exists(source)) {
    stop("Required deployment directory not found: ", source, call. = FALSE)
  }
  files <- list.files(
    source,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (length(exclude)) {
    relative <- substring(files, nchar(source) + 2L)
    keep <- !vapply(relative, function(path) {
      any(vapply(exclude, function(prefix) {
        identical(path, prefix) || startsWith(path, paste0(prefix, "/"))
      }, logical(1)))
    }, logical(1))
    files <- files[keep]
  }
  directories <- files[dir.exists(files)]
  for (directory in directories) {
    relative <- substring(directory, nchar(source) + 2L)
    dir.create(file.path(target, relative), recursive = TRUE, showWarnings = FALSE)
  }
  regular <- files[file.exists(files) & !dir.exists(files)]
  for (path in regular) {
    relative <- substring(path, nchar(source) + 2L)
    copy_file_safe(path, file.path(target, relative))
  }
  invisible(target)
}

root <- locate_root()
setwd(root)

bundle_root <- normalizePath(
  file.path(root, "deployment", "shinyapps"),
  winslash = "/",
  mustWork = FALSE
)
overwrite <- as_flag(Sys.getenv("NBD3_DEPLOY_OVERWRITE", "true"), TRUE)
max_bundle_gb <- suppressWarnings(as.numeric(
  Sys.getenv("NBD3_DEPLOY_MAX_GB", "0.95")
))
if (!is.finite(max_bundle_gb) || max_bundle_gb <= 0) max_bundle_gb <- 0.95

cache_manifest <- file.path(
  root,
  "output",
  "report-data",
  "ui_uncertainty_cache",
  "manifest.yml"
)
required_report_data <- c(
  file.path(root, "output", "report-data", "viz_input_new.rds"),
  file.path(root, "output", "report-data", "cause_rates.parquet"),
  cache_manifest
)
missing <- required_report_data[!file.exists(required_report_data)]
if (length(missing)) {
  stop(
    "The deployment data are incomplete. Missing:\n- ",
    paste(missing, collapse = "\n- "),
    "\nRun Rscript dev/build_shiny_ui_cache.R first.",
    call. = FALSE
  )
}

if (dir.exists(bundle_root)) {
  if (!overwrite) {
    stop(
      "Deployment bundle already exists: ", bundle_root,
      "\nSet NBD3_DEPLOY_OVERWRITE=true to rebuild it.",
      call. = FALSE
    )
  }
  unlink(bundle_root, recursive = TRUE, force = TRUE)
}
dir.create(bundle_root, recursive = TRUE, showWarnings = FALSE)

# Publication metadata and lightweight repository-root markers. The analytical
# targets graph and R modules are intentionally not bundled; the report only
# checks these two marker paths when locating its root.
for (filename in c("README.md", "LICENSE", "VERSION")) {
  copy_file_safe(file.path(root, filename), file.path(bundle_root, filename))
}
writeLines(
  "# Deployment root marker; the analytical targets graph is not bundled.",
  file.path(bundle_root, "_targets.R")
)
dir.create(file.path(bundle_root, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines(
  "# Deployment root marker; analytical modules remain in the research archive.",
  file.path(bundle_root, "R", "00_core.R")
)

# Small, version-controlled inputs needed by the report.
copy_tree_safe(file.path(root, "config"), file.path(bundle_root, "config"))
copy_tree_safe(
  file.path(root, "data", "lookups"),
  file.path(bundle_root, "data", "lookups")
)

# Report code and assets. Raw legacy inputs are not needed because the report
# runtime contains the approved comparison tables.
copy_tree_safe(
  file.path(root, "report"),
  file.path(bundle_root, "report"),
  exclude = c("data/legacy")
)

# Publication-ready point estimates and precomputed uncertainty summaries.
copy_tree_safe(
  file.path(root, "output", "report-data"),
  file.path(bundle_root, "output", "report-data"),
  exclude = c(".ui_uncertainty_work")
)

# Deployment guard: if somebody later deploys this directory recursively, the
# analytical archive is still absent by construction.
writeLines(
  c(
    ".git",
    ".Rproj.user",
    "deployment_manifest.csv",
    "README_DEPLOYMENT.txt"
  ),
  file.path(bundle_root, ".rscignore"),
  useBytes = TRUE
)

files <- list.files(
  bundle_root,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)
files <- files[file.exists(files) & !dir.exists(files)]
info <- file.info(files)
manifest <- data.frame(
  path = substring(files, nchar(bundle_root) + 2L),
  size_bytes = as.numeric(info$size),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$path), , drop = FALSE]
utils::write.csv(
  manifest,
  file.path(bundle_root, "deployment_manifest.csv"),
  row.names = FALSE,
  na = ""
)

bundle_bytes <- sum(manifest$size_bytes, na.rm = TRUE)
bundle_gb <- bundle_bytes / 1024^3
readme <- c(
  "SA-NBD3 shinyapps.io deployment bundle",
  "",
  paste0("Prepared: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Files: ", format(nrow(manifest), big.mark = ",")),
  paste0("Uncompressed size: ", sprintf("%.3f GiB", bundle_gb)),
  "",
  "Primary document: report/nbd3_results_report.Rmd",
  "",
  "This bundle contains query-optimised report data and excludes raw data,",
  "derived analytical checkpoints, the targets store, databases, and raw",
  "uncertainty draws. Rebuild it from the analytical repository whenever the",
  "report data or uncertainty cache changes."
)
writeLines(readme, file.path(bundle_root, "README_DEPLOYMENT.txt"))

message("shinyapps.io bundle prepared: ", bundle_root)
message("Files: ", format(nrow(manifest), big.mark = ","))
message("Uncompressed size: ", sprintf("%.3f GiB", bundle_gb))
if (bundle_gb > max_bundle_gb) {
  stop(
    "The prepared bundle exceeds NBD3_DEPLOY_MAX_GB=", max_bundle_gb,
    ". Reduce the deployment data or set an appropriate limit for the ",
    "shinyapps.io plan before deploying.",
    call. = FALSE
  )
}
message("Bundle is within the configured deployment-size limit.")
