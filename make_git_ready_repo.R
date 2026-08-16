#!/usr/bin/env Rscript

# Build a clean, publication-ready Git repository from the working SA-NBD3-V1
# analysis folder. The working folder is never modified.
#
# Run from the project root:
#   Rscript make_git_ready_repo.R
#
# Optional:
#   Rscript make_git_ready_repo.R --output="C:/path/to/SA-NBD3-V1-Git"
#   Rscript make_git_ready_repo.R --overwrite

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[[length(hit)]], fixed = TRUE)
}

flag <- function(x) x %in% args

root <- arg_value("--root=", getwd())
root <- normalizePath(root, winslash = "/", mustWork = TRUE)

required_markers <- c(
  "run_nbd.R",
  "_targets.R",
  "R/00_core.R",
  "report/nbd3_results_report.Rmd"
)
missing_markers <- required_markers[!file.exists(file.path(root, required_markers))]
if (length(missing_markers)) {
  stop(
    "This script must be run from the SA-NBD3-V1 project root. Missing: ",
    paste(missing_markers, collapse = ", "),
    call. = FALSE
  )
}

output_default <- file.path(dirname(root), paste0(basename(root), "-Git"))
out <- arg_value("--output=", output_default)
out <- normalizePath(out, winslash = "/", mustWork = FALSE)
overwrite <- flag("--overwrite")

if (identical(tolower(root), tolower(out))) {
  stop(
    "The output folder must be different from the working analysis folder. ",
    "This script intentionally creates a clean copy rather than deleting files in place.",
    call. = FALSE
  )
}

if (dir.exists(out)) {
  if (!overwrite) {
    stop(
      "Output folder already exists: ", out,
      "\nRerun with --overwrite to replace it.",
      call. = FALSE
    )
  }
  unlink(out, recursive = TRUE, force = TRUE)
}

dir.create(out, recursive = TRUE, showWarnings = FALSE)

copied <- character()
skipped_missing <- character()

copy_file_rel <- function(rel, required = FALSE) {
  src <- file.path(root, rel)
  dst <- file.path(out, rel)
  if (!file.exists(src) || dir.exists(src)) {
    if (required) stop("Required file is missing: ", src, call. = FALSE)
    skipped_missing <<- c(skipped_missing, rel)
    return(invisible(FALSE))
  }
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (!ok) stop("Could not copy: ", src, call. = FALSE)
  copied <<- c(copied, rel)
  invisible(TRUE)
}

copy_tree_rel <- function(rel, keep = NULL, exclude_regex = NULL, required = FALSE) {
  src_dir <- file.path(root, rel)
  if (!dir.exists(src_dir)) {
    if (required) stop("Required directory is missing: ", src_dir, call. = FALSE)
    skipped_missing <<- c(skipped_missing, paste0(rel, "/"))
    return(invisible(FALSE))
  }

  files <- list.files(src_dir, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE, include.dirs = FALSE)
  if (!length(files)) return(invisible(TRUE))
  rel_files <- substring(files, nchar(src_dir) + 2L)

  keep_idx <- rep(TRUE, length(files))
  if (!is.null(keep)) keep_idx <- keep_idx & grepl(keep, rel_files, perl = TRUE)
  if (!is.null(exclude_regex)) keep_idx <- keep_idx & !grepl(exclude_regex, rel_files, perl = TRUE)

  files <- files[keep_idx]
  rel_files <- rel_files[keep_idx]
  for (i in seq_along(files)) {
    dst_rel <- file.path(rel, rel_files[[i]])
    dst <- file.path(out, dst_rel)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(files[[i]], dst, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (!ok) stop("Could not copy: ", files[[i]], call. = FALSE)
    copied <<- c(copied, gsub("\\\\", "/", dst_rel))
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 1. Core publication repository files
# -----------------------------------------------------------------------------
core_files <- c(
  ".gitattributes",
  ".gitignore",
  ".rscignore",
  "CONTRIBUTING.md",
  "LICENSE",
  "README.md",
  "VERSION",
  "SA-NBD3-V1.Rproj",
  "_targets.R",
  "install_packages.R",
  "run_nbd.R"
)

optional_root_files <- c(
  "CITATION.cff",
  "CODE_OF_CONDUCT.md",
  "SECURITY.md",
  "renv.lock",
  "DESCRIPTION",
  "NAMESPACE",
  "build_report_database.R",
  "deploy_shinyapps.R"
)

for (f in core_files) copy_file_rel(f, required = f %in% required_markers)
for (f in optional_root_files) copy_file_rel(f, required = FALSE)

# Analytical modules and configuration.
copy_tree_rel("R", keep = "\\.[Rr]$", required = TRUE)
copy_tree_rel("config", exclude_regex = "(^|/)(Thumbs\\.db|\\.DS_Store)$", required = TRUE)

# Version-controlled lookup tables only.
copy_tree_rel("data/lookups", exclude_regex = "(^|/)(Thumbs\\.db|\\.DS_Store)$", required = TRUE)

# Restricted/generated directories are represented by documentation and placeholders only.
for (f in c(
  "data/raw/.gitkeep",
  "data/raw/README.md",
  "data/derived/.gitkeep",
  "output/database/.gitkeep",
  "output/figures/.gitkeep",
  "output/report-data/.gitkeep",
  "output/tables/.gitkeep",
  "output/uncertainty/.gitkeep",
  "report/data/legacy/.gitkeep",
  "report/data/legacy/README.md"
)) copy_file_rel(f, required = FALSE)

# Report source. Exclude rendered HTML, caches, and generated supporting directories.
copy_tree_rel(
  "report",
  exclude_regex = paste0(
    "(^|/)(Thumbs\\.db|\\.DS_Store|\\.Rhistory|\\.RData)$|",
    "\\.(html|log|tmp)$|",
    "(^|/)data/legacy(/|$)|",
    "(^|/)(nbd3_results_report_files|nbd3_results_report_cache)(/|$)"
  ),
  required = TRUE
)

# Scientific/publication documentation. Development and workshop material is omitted.
publication_docs <- c(
  "ANALYTICAL_SEQUENCE.md",
  "INJURY_SURVEY_AUDIT.md",
  "INPUTS.md",
  "METHODS.md",
  "REPORT.md",
  "REPORT_DATABASE.md",
  "SHINY_DEPLOYMENT.md",
  "UNCERTAINTY.md"
)
for (f in publication_docs) copy_file_rel(file.path("docs", f), required = FALSE)

# Retain only operational app/cache/deployment utilities. Diagnostic and validation scripts are omitted.
operational_dev <- c(
  "build_shiny_ui_cache.R",
  "run_cached_report.R",
  "benchmark_shiny_cache.R",
  "prepare_shinyapps_bundle.R",
  "deploy_shinyapps.R"
)
for (f in operational_dev) copy_file_rel(file.path("dev", f), required = FALSE)

# Preserve an existing GitHub configuration if present.
if (dir.exists(file.path(root, ".github"))) {
  copy_tree_rel(".github", exclude_regex = "(^|/)(Thumbs\\.db|\\.DS_Store)$")
}

# -----------------------------------------------------------------------------
# 2. Make copied documentation consistent with the streamlined repository
# -----------------------------------------------------------------------------
read_text <- function(path) {
  if (!file.exists(path)) return(NULL)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

write_text <- function(path, text) {
  writeLines(enc2utf8(text), path, useBytes = TRUE)
}

readme_path <- file.path(out, "README.md")
readme <- read_text(readme_path)
if (!is.null(readme)) {
  readme <- gsub(
    "dev/                 Extended diagnostics and release validation",
    "dev/                 Operational report-cache and deployment utilities",
    readme,
    fixed = TRUE
  )
  readme <- gsub(
    "(?m)^tests/\\s+Focused automated tests\\n",
    "",
    readme,
    perl = TRUE
  )
  readme <- gsub(
    "Methods, functions, configuration, tests, and report structure",
    "Methods, functions, configuration, and report structure",
    readme,
    fixed = TRUE
  )

  new_validation <- paste0(
    "The released repository retains the runtime input checks, analytical invariants, ",
    "point-reconstruction checks, and uncertainty diagnostics used by the production ",
    "pipeline. The larger development test suite and one-off diagnostic scripts are ",
    "maintained outside the publication repository to keep the public codebase focused."
  )
  readme <- sub(
    paste0(
      "(?s)Focused automated tests are stored in `tests/testthat/`\\..*?",
      "(?=\\n\\n## Data governance)"
    ),
    new_validation,
    readme,
    perl = TRUE
  )
  readme <- gsub(
    "Analytical changes should include updated documentation, focused tests, and review of point-estimate and uncertainty diagnostics.",
    "Analytical changes should include updated documentation and review of point-estimate and uncertainty diagnostics.",
    readme,
    fixed = TRUE
  )
  write_text(readme_path, readme)
}

contrib_path <- file.path(out, "CONTRIBUTING.md")
contrib <- read_text(contrib_path)
if (!is.null(contrib)) {
  contrib <- gsub(
    "4. Add or update unit tests for statistical rules, mappings, redistribution logic and output invariants.",
    "4. Add or update concise validation checks for statistical rules, mappings, redistribution logic and output invariants.",
    contrib,
    fixed = TRUE
  )
  contrib <- gsub(
    "5. Run the affected target, the unit tests and `dev/full_validation.R` before requesting review.",
    "5. Run the affected target and review the relevant point-estimate, uncertainty and report diagnostics before requesting review.",
    contrib,
    fixed = TRUE
  )
  write_text(contrib_path, contrib)
}

# The publication README already documents the separation between public code
# and restricted/generated analytical files. No extra cleanup notes are added.

# -----------------------------------------------------------------------------
# 3. Remove empty directories except intended placeholders and write inventory
# -----------------------------------------------------------------------------
# Ensure standard placeholder directories exist even if the source lacked .gitkeep files.
placeholder_dirs <- c(
  "data/raw", "data/derived",
  "output/database", "output/figures", "output/report-data", "output/tables", "output/uncertainty",
  "report/data/legacy"
)
for (d in placeholder_dirs) {
  dir.create(file.path(out, d), recursive = TRUE, showWarnings = FALSE)
  keep <- file.path(out, d, ".gitkeep")
  if (!file.exists(keep)) file.create(keep)
}

# Count retained files for the completion summary.
all_files <- list.files(out, recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE)
all_files <- all_files[!dir.exists(file.path(out, all_files))]
all_files <- sort(gsub("\\\\", "/", all_files))

# Sanity checks.
required_output <- c(
  "README.md", "run_nbd.R", "_targets.R",
  "R/00_core.R", "R/08_pipeline.R",
  "config/config.yml", "config/uncertainty_joint.yml",
  "report/nbd3_results_report.Rmd"
)
missing_output <- required_output[!file.exists(file.path(out, required_output))]
if (length(missing_output)) {
  stop("The clean repository is incomplete. Missing: ", paste(missing_output, collapse = ", "), call. = FALSE)
}

if (dir.exists(file.path(out, "tests"))) {
  stop("Cleanup check failed: tests/ was copied unexpectedly.", call. = FALSE)
}

forbidden_dirs <- c("_targets", "deployment")
forbidden_present <- forbidden_dirs[dir.exists(file.path(out, forbidden_dirs))]
if (length(forbidden_present)) {
  stop("Cleanup check failed: generated directory copied: ", paste(forbidden_present, collapse = ", "), call. = FALSE)
}

size_bytes <- sum(file.info(list.files(out, recursive = TRUE, full.names = TRUE,
                                      all.files = TRUE, no.. = TRUE))$size, na.rm = TRUE)

cat("\nGit-ready repository created successfully.\n")
cat("Source analytical workspace: ", root, "\n", sep = "")
cat("Clean Git repository:       ", out, "\n", sep = "")
cat("Files retained:             ", length(all_files), "\n", sep = "")
cat("Approximate size:           ", sprintf("%.2f MB", size_bytes / 1024^2), "\n", sep = "")
cat("\nThe original data, draws, caches and outputs were not modified.\n")
cat("\nNext commands:\n")
cat("  cd /d \"", gsub("/", "\\\\", out), "\"\n", sep = "")
cat("  git init\n")
cat("  git add .\n")
cat("  git status\n")
