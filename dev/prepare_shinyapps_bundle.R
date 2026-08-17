#!/usr/bin/env Rscript

# Prepare a root-primary shinyapps.io bundle for the current NBD3 report.
#
# The active document is copied to nbd3viztool.Rmd at the bundle root. The
# report's startup code is copied into a root global.R (not sourced through a
# wrapper), which avoids both the nested-document stack overflow and a nested
# primary-document health-check path.

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
  relative <- gsub("\\\\", "/", substring(files, nchar(source) + 2L))

  if (length(exclude)) {
    keep <- !vapply(relative, function(path) {
      any(vapply(exclude, function(prefix) {
        prefix <- gsub("\\\\", "/", prefix)
        identical(path, prefix) || startsWith(path, paste0(prefix, "/"))
      }, logical(1)))
    }, logical(1))
    files <- files[keep]
    relative <- relative[keep]
  }

  directories <- dir.exists(files)
  for (path in relative[directories]) {
    dir.create(file.path(target, path), recursive = TRUE, showWarnings = FALSE)
  }
  for (i in which(!directories)) {
    copy_file_safe(files[[i]], file.path(target, relative[[i]]))
  }
  invisible(target)
}

sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required. Run Rscript install_packages.R.",
         call. = FALSE)
  }
  unname(digest::digest(file = path, algo = "sha256", serialize = FALSE))
}

patch_deployment_rmd <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub(
    "^([[:space:]]*)self_contained:[[:space:]]*true[[:space:]]*$",
    "\\1self_contained: false",
    lines,
    ignore.case = TRUE
  )
  if (!any(grepl(
    "^[[:space:]]*runtime:[[:space:]]*shiny[[:space:]]*$",
    lines,
    ignore.case = TRUE
  ))) {
    stop("The current report is not configured with runtime: shiny.",
         call. = FALSE)
  }
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

write_root_global <- function(source, target) {
  lines <- readLines(source, warn = FALSE, encoding = "UTF-8")
  start <- grep(
    "^nbd_report_locate_dirs[[:space:]]*<-[[:space:]]*function\\(\\)[[:space:]]*\\{",
    lines,
    perl = TRUE
  )[1L]
  startup <- grep("^startup_started[[:space:]]*<-", lines, perl = TRUE)[1L]
  if (is.na(start) || is.na(startup) || startup <= start) {
    stop(
      "Could not patch report/global.R for root-primary deployment.",
      call. = FALSE
    )
  }

  replacement <- c(
    "nbd_report_locate_dirs <- function() {",
    "  application_root <- normalizePath(",
    "    getwd(), winslash = \"/\", mustWork = TRUE",
    "  )",
    "  report_dir <- file.path(application_root, \"report\")",
    "  if (!dir.exists(report_dir)) {",
    "    stop(\"The bundled report directory is missing.\", call. = FALSE)",
    "  }",
    "  list(",
    "    report_dir = normalizePath(",
    "      report_dir, winslash = \"/\", mustWork = TRUE",
    "    ),",
    "    root = application_root",
    "  )",
    "}",
    ""
  )

  before <- if (start > 1L) lines[seq_len(start - 1L)] else character()
  after <- lines[startup:length(lines)]
  output <- c(before, replacement, after)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, target, useBytes = TRUE)
  invisible(target)
}

root <- locate_root()
setwd(root)

# Rebuild only the compact report state. This does not run point estimates,
# uncertainty draws, ASR repair, or the UI-cache builder.
state_builder <- file.path(root, "build_report_state.R")
if (file.exists(state_builder)) {
  build_env <- new.env(parent = globalenv())
  sys.source(state_builder, envir = build_env)
  if (exists("build_report_state_main", envir = build_env, inherits = FALSE)) {
    build_env$build_report_state_main(root)
  } else {
    status <- system2(file.path(R.home("bin"), "Rscript"), state_builder)
    if (!identical(status, 0L)) {
      stop("build_report_state.R failed with status ", status, ".",
           call. = FALSE)
    }
  }
}

source_rmd <- file.path(root, "report", "nbd3_results_report.Rmd")
source_global <- file.path(root, "report", "global.R")
state_path <- file.path(root, "report", "data", "report_state.rds")
comparison_manifest <- file.path(
  root, "report", "data", "model_comparison", "manifest.csv"
)
cache_root <- file.path(root, "output", "report-data", "ui_uncertainty_cache")
cache_manifest <- file.path(cache_root, "manifest.yml")
required <- c(
  source_rmd,
  source_global,
  file.path(root, "report", "R", "report_panels.R"),
  file.path(root, "report", "R", "report_data.R"),
  file.path(root, "report", "R", "report_cache.R"),
  file.path(root, "report", "R", "report_charts.R"),
  state_path,
  comparison_manifest,
  cache_manifest
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(
    "Deployment inputs are incomplete. Missing:\n- ",
    paste(missing, collapse = "\n- "),
    call. = FALSE
  )
}

state_mb <- file.info(state_path)$size / 1024^2
if (!is.finite(state_mb) || state_mb > 64) {
  stop(
    "The compact report state is too large for deployment: ",
    round(state_mb, 1), " MB.",
    call. = FALSE
  )
}

bundle_root <- normalizePath(
  file.path(root, "deployment", "shinyapps"),
  winslash = "/",
  mustWork = FALSE
)
overwrite <- as_flag(Sys.getenv("NBD3_DEPLOY_OVERWRITE", "true"), TRUE)
if (dir.exists(bundle_root)) {
  if (!overwrite) {
    stop("Deployment bundle already exists: ", bundle_root, call. = FALSE)
  }
  unlink(bundle_root, recursive = TRUE, force = TRUE)
}
dir.create(bundle_root, recursive = TRUE, showWarnings = FALSE)

for (filename in c("README.md", "LICENSE", "VERSION")) {
  path <- file.path(root, filename)
  if (file.exists(path)) copy_file_safe(path, file.path(bundle_root, filename))
}

# Lightweight markers required by report path discovery helpers.
writeLines(
  "# Deployment root marker; analytical targets are not bundled.",
  file.path(bundle_root, "_targets.R")
)
dir.create(file.path(bundle_root, "R"), recursive = TRUE,
           showWarnings = FALSE)
writeLines(
  "# Deployment root marker; analytical modules remain in the research archive.",
  file.path(bundle_root, "R", "00_core.R")
)

# Put the complete current report at the application root. This is not a
# wrapper: it is the actual working Rmd with only self_contained disabled.
primary_relative <- "nbd3viztool.Rmd"
primary <- file.path(bundle_root, primary_relative)
copy_file_safe(source_rmd, primary)
patch_deployment_rmd(primary)

# Execute the report startup code directly as root/global.R. Do not source it
# through another global.R wrapper.
write_root_global(source_global, file.path(bundle_root, "global.R"))

# Keep helper code, compact state, catalogues and model-comparison partitions in
# report/. The second Rmd and the second global.R are intentionally excluded.
copy_tree_safe(
  file.path(root, "report"),
  file.path(bundle_root, "report"),
  exclude = c(
    "data/legacy",
    "R/report_state_builder.R",
    "R/report_state_source.R",
    "rsconnect",
    "nbd3_results_report.Rmd",
    "nbd3_results_report.html",
    "nbd3_results_report_files",
    "global.R",
    "www",
    "README.md"
  )
)

# The root Rmd retains css: www/report.css.
copy_tree_safe(
  file.path(root, "report", "www"),
  file.path(bundle_root, "www")
)

copy_tree_safe(
  cache_root,
  file.path(bundle_root, "output", "report-data", "ui_uncertainty_cache")
)

# Remove stale deployment metadata and any nested deployable document.
for (stale in c("rsconnect", file.path("report", "rsconnect"))) {
  path <- file.path(bundle_root, stale)
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  if (file.exists(path)) unlink(path, force = TRUE)
}

source_hash <- sha256_file(source_rmd)
deployment_hash <- sha256_file(primary)
source_lines <- readLines(source_rmd, warn = FALSE, encoding = "UTF-8")
deployment_lines <- readLines(primary, warn = FALSE, encoding = "UTF-8")
normalize_rmd <- function(x) sub(
  "^([[:space:]]*)self_contained:[[:space:]]*(true|false)[[:space:]]*$",
  "\\1self_contained: <deployment>",
  x,
  ignore.case = TRUE
)
if (!identical(normalize_rmd(source_lines), normalize_rmd(deployment_lines))) {
  stop("The root deployment Rmd differs unexpectedly from the current report.",
       call. = FALSE)
}

# Confirm that only one Rmd is deployable.
rmd_files <- list.files(
  bundle_root,
  pattern = "\\.[Rr][Mm][Dd]$",
  recursive = TRUE,
  full.names = FALSE
)
if (!identical(gsub("\\\\", "/", rmd_files), primary_relative)) {
  stop(
    "The bundle must contain exactly one Rmd at the root. Found: ",
    paste(rmd_files, collapse = ", "),
    call. = FALSE
  )
}

build_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
writeLines(
  c(
    paste0("build_id=", build_id),
    "source_report=report/nbd3_results_report.Rmd",
    paste0("primary_document=", primary_relative),
    paste0("source_sha256=", source_hash),
    paste0("deployment_sha256=", deployment_hash),
    paste0("state_mb=", sprintf("%.1f", state_mb)),
    "startup_layout=root-primary-direct-global"
  ),
  file.path(bundle_root, "DEPLOY_BUILD.txt")
)

files <- list.files(
  bundle_root,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)
files <- files[file.exists(files) & !dir.exists(files)]
relative <- gsub("\\\\", "/", substring(files, nchar(bundle_root) + 2L))
info <- file.info(files)
manifest <- data.frame(
  file = relative,
  size_bytes = as.numeric(info$size),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$file), , drop = FALSE]
utils::write.csv(
  manifest,
  file.path(bundle_root, "deployment_manifest.csv"),
  row.names = FALSE,
  na = ""
)

bundle_gb <- sum(manifest$size_bytes, na.rm = TRUE) / 1024^3
max_bundle_gb <- suppressWarnings(as.numeric(
  Sys.getenv("NBD3_DEPLOY_MAX_GB", "0.95")
))
if (!is.finite(max_bundle_gb) || max_bundle_gb <= 0) max_bundle_gb <- 0.95
if (bundle_gb > max_bundle_gb) {
  stop(
    "The prepared bundle is ", sprintf("%.3f", bundle_gb),
    " GiB, above NBD3_DEPLOY_MAX_GB=", max_bundle_gb, ".",
    call. = FALSE
  )
}

writeLines(
  c(
    "SA-NBD3 shinyapps.io deployment bundle",
    "",
    paste0("Build ID: ", build_id),
    paste0("Files: ", format(nrow(manifest), big.mark = ",")),
    paste0("Uncompressed size: ", sprintf("%.3f GiB", bundle_gb)),
    paste0("Report-state size: ", sprintf("%.1f MB", state_mb)),
    "",
    "Primary document: nbd3viztool.Rmd",
    "Application mode: rmd-shiny",
    "Startup layout: root primary document plus direct root global.R",
    "Source report: report/nbd3_results_report.Rmd"
  ),
  file.path(bundle_root, "README_DEPLOYMENT.txt")
)

message("shinyapps.io bundle prepared: ", bundle_root)
message("Build ID: ", build_id)
message("Primary document: ", primary_relative)
message("Startup layout: root-primary-direct-global")
message("Compact state: ", sprintf("%.1f MB", state_mb))
message("Files: ", format(nrow(manifest), big.mark = ","))
message("Uncompressed size: ", sprintf("%.3f GiB", bundle_gb))
