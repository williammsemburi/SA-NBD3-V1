#!/usr/bin/env Rscript

# Prepare a minimal shinyapps.io bundle for the SA-NBD3 Shiny-enabled R Markdown
# report. shinyapps.io renders the Rmd as the primary application document; a
# pre-rendered static HTML file is neither required nor used as the application.

options(stringsAsFactors = FALSE, scipen = 999)

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

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
  if (!isTRUE(file.copy(source, target, overwrite = TRUE, copy.mode = TRUE))) {
    stop("Could not copy: ", source, call. = FALSE)
  }
  invisible(target)
}

copy_tree_safe <- function(source, target, exclude = character()) {
  if (!dir.exists(source)) {
    stop("Required deployment directory not found: ", source, call. = FALSE)
  }
  files <- list.files(
    source, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, no.. = TRUE
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
  for (path in files[dir.exists(files)]) {
    relative <- substring(path, nchar(source) + 2L)
    dir.create(file.path(target, relative), recursive = TRUE,
               showWarnings = FALSE)
  }
  for (path in files[file.exists(files) & !dir.exists(files)]) {
    relative <- substring(path, nchar(source) + 2L)
    copy_file_safe(path, file.path(target, relative))
  }
  invisible(target)
}

root <- locate_root()
setwd(root)
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required. Run Rscript install_packages.R.",
       call. = FALSE)
}

cache_root <- file.path(root, "output", "report-data", "ui_uncertainty_cache")
cache_manifest_path <- file.path(cache_root, "manifest.yml")
asr_validation_path <- file.path(root, "output", "tables",
                                 "asr_cache_validation.csv")
required <- c(
  file.path(root, "output", "report-data", "viz_input_new.rds"),
  file.path(root, "output", "report-data", "cause_rates.parquet"),
  cache_manifest_path,
  asr_validation_path
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(
    "Deployment inputs are incomplete. Missing:\n- ",
    paste(missing, collapse = "\n- "),
    "\nRun Rscript dev/repair_asr_ui_cache.R before preparing deployment.",
    call. = FALSE
  )
}

cache_manifest <- yaml::read_yaml(cache_manifest_path)
formula_version <- as.character(cache_manifest$asr_formula_version %||% "")
if (!identical(formula_version, "1.0.1")) {
  stop(
    "The UI cache does not contain the corrected ASR formula. Found version '",
    formula_version, "'. Run Rscript dev/repair_asr_ui_cache.R.",
    call. = FALSE
  )
}
asr_validation <- utils::read.csv(asr_validation_path, stringsAsFactors = FALSE)
formula_rows <- asr_validation[
  asr_validation$validation_type == "deterministic_formula", , drop = FALSE
]
if (!nrow(formula_rows) ||
    any(!is.finite(formula_rows$max_relative_error)) ||
    any(formula_rows$max_relative_error > 1e-8)) {
  stop(
    "ASR cache validation has not passed deterministic equivalence. Review: ",
    asr_validation_path,
    call. = FALSE
  )
}

bundle_root <- normalizePath(
  file.path(root, "deployment", "shinyapps"),
  winslash = "/", mustWork = FALSE
)
overwrite <- as_flag(Sys.getenv("NBD3_DEPLOY_OVERWRITE", "true"), TRUE)
max_bundle_gb <- suppressWarnings(as.numeric(
  Sys.getenv("NBD3_DEPLOY_MAX_GB", "0.95")
))
if (!is.finite(max_bundle_gb) || max_bundle_gb <= 0) max_bundle_gb <- 0.95

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

for (filename in c("README.md", "LICENSE", "VERSION")) {
  copy_file_safe(file.path(root, filename), file.path(bundle_root, filename))
}

# Lightweight root markers used by the report's root locator.
writeLines(
  "# Deployment root marker; the analytical targets graph is not bundled.",
  file.path(bundle_root, "_targets.R")
)
dir.create(file.path(bundle_root, "R"), recursive = TRUE,
           showWarnings = FALSE)
writeLines(
  "# Deployment root marker; analytical modules remain in the research archive.",
  file.path(bundle_root, "R", "00_core.R")
)

copy_tree_safe(file.path(root, "config"), file.path(bundle_root, "config"))
copy_tree_safe(
  file.path(root, "data", "lookups"),
  file.path(bundle_root, "data", "lookups")
)
copy_tree_safe(
  file.path(root, "report"),
  file.path(bundle_root, "report"),
  exclude = c("data/legacy")
)

# Copy only the approved runtime report data. Top-level files support point
# estimates and comparison panels; the optimized uncertainty cache supplies all
# interactive uncertainty intervals. Old cache implementations are excluded.
source_report_data <- file.path(root, "output", "report-data")
target_report_data <- file.path(bundle_root, "output", "report-data")
dir.create(target_report_data, recursive = TRUE, showWarnings = FALSE)
for (path in list.files(
  source_report_data, full.names = TRUE, recursive = FALSE,
  all.files = TRUE, no.. = TRUE
)) {
  if (file.exists(path) && !dir.exists(path)) {
    copy_file_safe(path, file.path(target_report_data, basename(path)))
  }
}
copy_tree_safe(
  cache_root,
  file.path(target_report_data, "ui_uncertainty_cache")
)
methods_root <- file.path(source_report_data, "methods")
if (dir.exists(methods_root)) {
  copy_tree_safe(methods_root, file.path(target_report_data, "methods"))
}

# Include the ASR validation record used to approve the deployment cache.
dir.create(file.path(bundle_root, "output", "tables"), recursive = TRUE,
           showWarnings = FALSE)
copy_file_safe(
  asr_validation_path,
  file.path(bundle_root, "output", "tables", basename(asr_validation_path))
)

writeLines(
  c(
    ".git", ".Rproj.user", "deployment_manifest.csv",
    "README_DEPLOYMENT.txt"
  ),
  file.path(bundle_root, ".rscignore"),
  useBytes = TRUE
)

files <- list.files(
  bundle_root, recursive = TRUE, full.names = TRUE,
  all.files = TRUE, no.. = TRUE
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
writeLines(
  c(
    "SA-NBD3 shinyapps.io deployment bundle",
    "",
    paste0("Prepared: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Files: ", format(nrow(manifest), big.mark = ",")),
    paste0("Uncompressed size: ", sprintf("%.3f GiB", bundle_gb)),
    "",
    "Primary application document: report/nbd3_results_report.Rmd",
    "Application mode: Shiny-enabled R Markdown (rmd-shiny)",
    "",
    "shinyapps.io renders the Rmd as the interactive HTML application. A",
    "pre-rendered static HTML file is intentionally not bundled.",
    "",
    "The bundle includes point-estimate report data and the optimized",
    "uncertainty cache. It excludes raw data, derived analytical checkpoints,",
    "the targets store, the final analytical database, and all 1,000 draw files."
  ),
  file.path(bundle_root, "README_DEPLOYMENT.txt")
)

message("shinyapps.io bundle prepared: ", bundle_root)
message("Primary document: report/nbd3_results_report.Rmd")
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
