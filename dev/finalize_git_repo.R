#!/usr/bin/env Rscript

# Final in-place cleanup for the Git-connected SA-NBD3-V1 repository.
#
# The script separates the publication source tree from local analytical state.
# It deletes obsolete development artefacts, preserves completed data/draws/cache
# locally, updates ignore rules, and optionally removes generated files from the
# Git index without deleting them from disk.
#
# Preview only (preserves the full scientific test suite):
#   Rscript dev/finalize_git_repo.R
#
# Apply and untrack generated state:
#   Rscript dev/finalize_git_repo.R --apply --untrack-generated
#
# Optional: add --lean-tests only when a deliberately reduced public test
# suite is preferred after reviewing the dry-run manifest.
#
# Keep every current dev script instead of pruning to the operational allow-list:
#   Rscript dev/finalize_git_repo.R --apply --keep-dev --untrack-generated

options(stringsAsFactors = FALSE, warn = 1)

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(x) x %in% args
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[[length(hit)]], fixed = TRUE)
}

apply_changes <- has_flag("--apply")
lean_tests <- has_flag("--lean-tests")
untrack_generated <- has_flag("--untrack-generated")
prune_dev <- !has_flag("--keep-dev")
root <- normalizePath(arg_value("--root=", getwd()), winslash = "/", mustWork = TRUE)

required <- c(
  ".git",
  "run_nbd.R",
  "_targets.R",
  "R/00_core.R",
  "report/nbd3_results_report.Rmd",
  "report/global.R",
  "report/R/report_data.R",
  "report/R/report_cache.R",
  "report/R/report_charts.R",
  "report/R/report_panels.R",
  "dev/prepare_shinyapps_bundle.R",
  "dev/deploy_shinyapps.R"
)
missing <- required[
  !file.exists(file.path(root, required)) &
    !dir.exists(file.path(root, required))
]
if (length(missing)) {
  stop(
    "Run this script from the working SA-NBD3-V1 Git repository root. Missing: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

rel_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

path_size <- function(path) {
  if (file.exists(path) && !dir.exists(path)) {
    value <- file.info(path)$size
    return(ifelse(is.na(value), 0, value))
  }
  if (!dir.exists(path)) return(0)
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  if (!length(files)) return(0)
  sizes <- file.info(files)$size
  sum(sizes[is.finite(sizes)], na.rm = TRUE)
}

# These paths are analytically valuable local state. They may be removed from
# Git tracking, but this cleanup never deletes them from disk.
protected_local <- c(
  ".git",
  "data/raw",
  "data/derived",
  "_targets",
  "output/database",
  "output/uncertainty",
  "output/report-data/ui_uncertainty_cache",
  "report/data/report_state.rds",
  "report/data/report_state_manifest.csv",
  "report/data/report_state_object_sizes.csv",
  "report/data/model_comparison",
  "report/data/legacy",
  "deployment/shinyapps"
)

is_protected <- function(rel) {
  rel %in% protected_local ||
    any(startsWith(rel, paste0(protected_local, "/")))
}

candidates <- data.frame(
  path = character(),
  type = character(),
  reason = character(),
  bytes = numeric(),
  stringsAsFactors = FALSE
)

add_candidate <- function(path, reason) {
  if (!file.exists(path) && !dir.exists(path)) return(invisible(FALSE))
  rel <- rel_path(path)
  if (is_protected(rel)) return(invisible(FALSE))
  candidates <<- rbind(
    candidates,
    data.frame(
      path = rel,
      type = if (dir.exists(path)) "directory" else "file",
      reason = reason,
      bytes = path_size(path),
      stringsAsFactors = FALSE
    )
  )
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 1. Obsolete root-level installers, wrappers, duplicates, and troubleshooting.
# -----------------------------------------------------------------------------
obsolete_root <- c(
  "apply_report_state_fix.R",
  "apply_shiny_deploy_startup_fix.R",
  "clean_git_repo.R",
  "clean_repo_in_place.R",
  "make_git_ready_repo.R",
  "make_git_ready_repo.R",
  "build_report_database.R",
  "deploy_shinyapps.R",
  "global.R",
  "nbd3viztool.Rmd",
  "APPLY_THIS_UPDATE.txt",
  "latest_deploy_task.log",
  "DEPLOY_BUILD.txt",
  "MANIFEST.sha256",
  "GIT_READY_FILE_LIST.txt",
  "SA-NBD3-V1-consolidation-audit.md"
)
for (name in obsolete_root) {
  add_candidate(file.path(root, name), "Completed installer, duplicate wrapper, or troubleshooting file")
}

root_entries <- list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
root_files <- root_entries[file.exists(root_entries) & !dir.exists(root_entries)]
root_names <- basename(root_files)

artifact_file <- grepl(
  paste0(
    "(?i)(\\.zip$|\\.sha256$|\\.patch$|\\.tar\\.gz$|",
    "(^|[-_ ])validation([-_ .]|$)|changed[-_ ]files|file[-_ ]list|",
    "update[-_ ]notes|fix[-_ ]readme|^Pasted (text|markdown)|",
    "^child_task_.*\\.log$|^latest_.*\\.log$)"
  ),
  root_names,
  perl = TRUE
)
for (path in root_files[artifact_file]) {
  add_candidate(path, "Patch delivery, checksum, pasted diagnostic, or deployment log")
}

# Extracted update/fix folders left in the repository root.
reserved_dirs <- c(
  ".git", "R", "config", "data", "deployment", "dev", "docs", "output",
  "report", "tests", "_targets", "renv"
)
root_dirs <- root_entries[dir.exists(root_entries)]
root_dir_names <- basename(root_dirs)
artifact_dir <- !root_dir_names %in% reserved_dirs & grepl(
  paste0(
    "(?i)(^_deliver|^_delivery|^_inspect|^_tmp|",
    "(^|[-_])(fix|update|patch|delivery|recovery|validation)([-_]|$)|",
    "^NBD3-|^SA-NBD3-V1-)"
  ),
  root_dir_names,
  perl = TRUE
)
for (path in root_dirs[artifact_dir]) {
  add_candidate(path, "Extracted update, patch, recovery, or temporary folder")
}

# -----------------------------------------------------------------------------
# 2. Retain only the final operational dev utilities unless --keep-dev is used.
# -----------------------------------------------------------------------------
keep_dev <- c(
  "README.md",
  "build_shiny_ui_cache.R",
  "run_cached_report.R",
  "benchmark_shiny_cache.R",
  "prepare_shinyapps_bundle.R",
  "deploy_shinyapps.R",
  "full_validation.R",
  "finalize_git_repo.R"
)
if (prune_dev && dir.exists(file.path(root, "dev"))) {
  dev_root <- file.path(root, "dev")
  dev_files <- list.files(
    dev_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  for (path in dev_files) {
    inside <- gsub("\\\\", "/", substring(path, nchar(dev_root) + 2L))
    if (!inside %in% keep_dev) {
      add_candidate(path, "Completed diagnostic, repair, migration, or non-operational dev script")
    }
  }
}

# -----------------------------------------------------------------------------
# 3. Remove stale rsconnect metadata. Deployment identity is now explicit in
#    dev/deploy_shinyapps.R, and stale metadata previously selected the old app.
# -----------------------------------------------------------------------------
all_dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)
rsconnect_dirs <- all_dirs[basename(all_dirs) == "rsconnect"]
for (path in rsconnect_dirs) {
  rel <- rel_path(path)
  if (!startsWith(rel, "deployment/shinyapps/")) {
    add_candidate(path, "Stale local rsconnect deployment metadata")
  }
}

# -----------------------------------------------------------------------------
# 4. Optional lean, publication-facing scientific test suite.
# -----------------------------------------------------------------------------
if (lean_tests && dir.exists(file.path(root, "tests"))) {
  tests_root <- file.path(root, "tests")
  keep_test_regex <- paste0(
    "(^testthat\\.R$)|",
    "(^testthat/helper.*\\.[Rr]$)|",
    "(^testthat/test-(population|cod|mapping|database|hiv|injury-model|",
    "redistribution|structured-uncertainty|yll|report-runtime).*\\.[Rr]$)"
  )
  test_files <- list.files(
    tests_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  for (path in test_files) {
    inside <- gsub("\\\\", "/", substring(path, nchar(tests_root) + 2L))
    if (!grepl(keep_test_regex, inside, perl = TRUE, ignore.case = TRUE)) {
      add_candidate(path, "Non-core development regression test")
    }
  }
}

# -----------------------------------------------------------------------------
# 5. Local renders, backups, old caches, IDE state, and one-off diagnostics.
# -----------------------------------------------------------------------------
for (path in c(
  file.path(root, "output", "backups"),
  file.path(root, "output", "report-data", "app_database"),
  file.path(root, "output", "report-data", "ui_uncertainty_cache_work"),
  file.path(root, "output", "report-data", ".ui_uncertainty_cache_work"),
  file.path(root, "report", "nbd3_results_report_files"),
  file.path(root, "report", "nbd3_results_report_cache"),
  file.path(root, ".Rproj.user")
)) {
  add_candidate(path, "Generated backup, superseded cache, local render, or IDE state")
}

if (dir.exists(file.path(root, "report"))) {
  report_files <- list.files(
    file.path(root, "report"),
    full.names = TRUE,
    recursive = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  rendered <- report_files[
    file.exists(report_files) &
      !dir.exists(report_files) &
      grepl("(?i)\\.(html|utf8\\.md|knit\\.md|log)$", basename(report_files), perl = TRUE)
  ]
  for (path in rendered) add_candidate(path, "Locally rendered report output")
}

for (name in c(".Rhistory", ".RData", "Thumbs.db", ".DS_Store")) {
  add_candidate(file.path(root, name), "Local R or operating-system state")
}

one_off_outputs <- c(
  "asr_total_cache_check.csv",
  "repository_cleanup_manifest.csv",
  "git_cleanup_manifest.csv",
  "completeness_sd_assumptions_by_province.csv",
  "completeness_sd_assumptions_summary.csv",
  "completeness_sd_assumptions_national_simulation.csv",
  "completeness_sd_assumption_definitions.csv",
  "completeness_sd_eligible_input_cells.csv"
)
for (name in one_off_outputs) {
  add_candidate(file.path(root, "output", "tables", name), "Completed one-off diagnostic output")
}

for (path in c(
  file.path(root, "docs", "WORKSHOP_GUIDE.md"),
  file.path(root, "docs", "MIGRATION.md"),
  file.path(root, "docs", "RELEASE_NOTES_INTERNAL.md")
)) {
  add_candidate(path, "Internal workshop, migration, or development-only document")
}

# De-duplicate and delete deepest paths first.
if (nrow(candidates)) {
  candidates <- candidates[!duplicated(candidates$path), , drop = FALSE]
  candidates$depth <- lengths(strsplit(candidates$path, "/", fixed = TRUE))
  candidates <- candidates[order(-candidates$depth, candidates$path), , drop = FALSE]
} else {
  candidates$depth <- integer()
}

# -----------------------------------------------------------------------------
# 6. Idempotent publication-repository ignore rules.
# -----------------------------------------------------------------------------
gitignore_path <- file.path(root, ".gitignore")
begin_marker <- "# BEGIN SA-NBD3 local analytical state"
end_marker <- "# END SA-NBD3 local analytical state"
gitignore_block <- c(
  begin_marker,
  "",
  "# Local R, IDE, and operating-system state",
  ".Rhistory",
  ".RData",
  ".Rproj.user/",
  "Thumbs.db",
  ".DS_Store",
  "*.log",
  "",
  "# Restricted source data and generated analytical checkpoints",
  "data/raw/*",
  "!data/raw/.gitkeep",
  "!data/raw/README.md",
  "data/derived/*",
  "!data/derived/.gitkeep",
  "!data/derived/README.md",
  "_targets/",
  "",
  "# Generated analysis and report outputs",
  "output/*",
  "!output/README.md",
  "!output/**/.gitkeep",
  "",
  "# Deployment/runtime artefacts generated from the report source",
  "deployment/shinyapps/",
  "report/data/report_state.rds",
  "report/data/report_state_manifest.csv",
  "report/data/report_state_object_sizes.csv",
  "report/data/model_comparison/",
  "report/data/legacy/",
  "report/*.html",
  "report/*_files/",
  "report/*_cache/",
  "**/rsconnect/",
  "",
  "# Update-delivery artefacts",
  "*.zip",
  "*.sha256",
  "*.patch",
  end_marker
)

update_marked_file <- function(path, start_marker, finish_marker, block) {
  existing <- if (file.exists(path)) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }
  start <- match(start_marker, existing)
  finish <- match(finish_marker, existing)
  if (!is.na(start) && !is.na(finish) && finish >= start) {
    existing <- existing[-seq.int(start, finish)]
  }
  while (length(existing) && !nzchar(existing[[length(existing)]])) {
    existing <- existing[-length(existing)]
  }
  updated <- c(existing, if (length(existing)) "" else character(), block)
  writeLines(enc2utf8(updated), path, useBytes = TRUE)
}

rscignore_path <- file.path(root, ".rscignore")
rsc_begin <- "# BEGIN SA-NBD3 root deployment exclusions"
rsc_end <- "# END SA-NBD3 root deployment exclusions"
rsc_block <- c(
  rsc_begin,
  "^\\.git$",
  "^data/raw$",
  "^data/derived$",
  "^_targets$",
  "^output/database$",
  "^output/uncertainty$",
  "^deployment$",
  "^tests$",
  "^.*\\.log$",
  rsc_end
)

# -----------------------------------------------------------------------------
# 7. Report, apply, and optionally untrack generated local state.
# -----------------------------------------------------------------------------
manifest_dir <- file.path(root, "output", "tables")
dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
manifest_path <- file.path(manifest_dir, "final_git_cleanup_manifest.csv")

candidates$size_mb <- round(candidates$bytes / 1024^2, 3)
candidates$action <- if (apply_changes) "delete" else "would_delete"
write.csv(
  candidates[, c("path", "type", "reason", "size_mb", "action"), drop = FALSE],
  manifest_path,
  row.names = FALSE,
  na = ""
)

cat("SA-NBD3 final Git cleanup\n")
cat("Repository: ", root, "\n", sep = "")
cat("Mode: ", if (apply_changes) "APPLY" else "DRY RUN", "\n", sep = "")
cat("Prune dev folder: ", prune_dev, "\n", sep = "")
cat("Lean test suite: ", lean_tests, "\n", sep = "")
cat("Candidate paths: ", nrow(candidates), "\n", sep = "")
cat(sprintf("Approximate removable size: %.2f MB\n", sum(candidates$bytes, na.rm = TRUE) / 1024^2))
cat("Manifest: ", manifest_path, "\n\n", sep = "")

if (nrow(candidates)) {
  print(candidates[, c("path", "type", "reason", "size_mb"), drop = FALSE], row.names = FALSE)
}

if (!apply_changes) {
  cat("\nNo files were deleted and Git tracking was not changed.\n")
  cat("Apply after reviewing the manifest with:\n")
  cat("  Rscript dev/finalize_git_repo.R --apply")
  if (lean_tests) cat(" --lean-tests")
  cat(" --untrack-generated\n")
  quit(status = 0L)
}

failures <- character()
for (i in seq_len(nrow(candidates))) {
  path <- file.path(root, candidates$path[[i]])
  if (!file.exists(path) && !dir.exists(path)) next
  ok <- tryCatch({
    unlink(path, recursive = TRUE, force = TRUE)
    !file.exists(path) && !dir.exists(path)
  }, error = function(error) FALSE)
  if (!isTRUE(ok)) failures <- c(failures, candidates$path[[i]])
}

update_marked_file(gitignore_path, begin_marker, end_marker, gitignore_block)
update_marked_file(rscignore_path, rsc_begin, rsc_end, rsc_block)

if (untrack_generated) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    warning("Git was not found; generated paths remain in the Git index.")
  } else {
    # Remove generated data from the index while preserving the local files.
    generated_paths <- c(
      "data/raw",
      "data/derived",
      "_targets",
      "output",
      "deployment/shinyapps",
      "report/data/report_state.rds",
      "report/data/report_state_manifest.csv",
      "report/data/report_state_object_sizes.csv",
      "report/data/model_comparison",
      "report/data/legacy",
      "report/rsconnect",
      "rsconnect"
    )
    for (rel in generated_paths) {
      tracked <- tryCatch(
        system2(
          git,
          c("-C", root, "ls-files", "--", rel),
          stdout = TRUE,
          stderr = FALSE
        ),
        error = function(error) character()
      )
      if (length(tracked)) {
        output <- suppressWarnings(system2(
          git,
          c("-C", root, "rm", "-r", "--cached", "--ignore-unmatch", "--", rel),
          stdout = TRUE,
          stderr = TRUE
        ))
        if (length(output)) cat(paste(output, collapse = "\n"), "\n")
      }
    }
  }
}

if (length(failures)) {
  stop(
    "Cleanup completed with deletion failures: ",
    paste(failures, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 8. Final Git release audit. This checks the staged index, not local ignored
#    analytical state. The audit is written under output/tables and remains
#    local because output is ignored.
# -----------------------------------------------------------------------------
audit <- data.frame(
  check = character(),
  status = character(),
  detail = character(),
  stringsAsFactors = FALSE
)
add_audit <- function(check, status, detail = "") {
  audit <<- rbind(
    audit,
    data.frame(
      check = as.character(check),
      status = as.character(status),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  )
}

git <- Sys.which("git")
if (!nzchar(git)) {
  add_audit("Git executable", "WARN", "Git was not found; index checks were skipped.")
} else {
  tracked <- tryCatch(
    system2(git, c("-C", root, "ls-files"), stdout = TRUE, stderr = TRUE),
    error = function(error) character()
  )
  tracked <- gsub("\\\\", "/", tracked)

  forbidden_prefixes <- c(
    "data/raw/", "data/derived/", "_targets/", "output/",
    "deployment/shinyapps/", "report/data/model_comparison/",
    "report/data/legacy/", "report/rsconnect/", "rsconnect/"
  )
  forbidden_exact <- c(
    "report/data/report_state.rds",
    "report/data/report_state_manifest.csv",
    "report/data/report_state_object_sizes.csv"
  )
  forbidden <- tracked[
    tracked %in% forbidden_exact |
      vapply(tracked, function(path) {
        any(startsWith(path, forbidden_prefixes))
      }, logical(1))
  ]
  # Documentation/placeholders are allowed even though they live under a
  # generated directory tree.
  allowed_placeholders <- c(
    "data/raw/README.md", "data/raw/.gitkeep",
    "data/derived/README.md", "data/derived/.gitkeep",
    "output/README.md"
  )
  forbidden <- setdiff(forbidden, allowed_placeholders)
  add_audit(
    "No restricted or generated analytical files tracked",
    if (length(forbidden)) "FAIL" else "PASS",
    paste(forbidden, collapse = "; ")
  )

  obsolete_tracked <- intersect(
    tracked,
    c(
      "apply_report_state_fix.R", "apply_shiny_deploy_startup_fix.R",
      "global.R", "nbd3viztool.Rmd", "build_report_database.R",
      "deploy_shinyapps.R", "latest_deploy_task.log",
      "MANIFEST.sha256", "SA-NBD3-V1-consolidation-audit.md"
    )
  )
  add_audit(
    "No obsolete root installers or duplicate wrappers tracked",
    if (length(obsolete_tracked)) "FAIL" else "PASS",
    paste(obsolete_tracked, collapse = "; ")
  )

  required_source <- c(
    ".gitignore", ".gitattributes", ".rscignore",
    "README.md", "LICENSE", "VERSION", "run_nbd.R", "_targets.R",
    "install_packages.R", "config/config.yml",
    "config/uncertainty_joint.yml",
    sprintf("R/%02d_%s.R", 0:8, c(
      "core", "population_cod", "injuries", "completeness_hiv",
      "redistribution", "yll_database", "uncertainty", "reporting",
      "pipeline"
    )),
    "data/lookups/analysis_codes.csv", "data/lookups/analysis_to_za.csv",
    "data/lookups/icd10_to_nbd.csv", "data/lookups/za_codes.csv",
    "report/nbd3_results_report.Rmd", "report/global.R",
    "report/config/labels.yml", "report/config/age_comparison_map.csv",
    "report/config/cause_comparison_map.csv",
    "report/R/report_data.R", "report/R/report_cache.R",
    "report/R/report_charts.R", "report/R/report_panels.R",
    "report/R/report_state_builder.R",
    "dev/build_shiny_ui_cache.R", "dev/run_cached_report.R",
    "dev/benchmark_shiny_cache.R",
    "dev/prepare_shinyapps_bundle.R", "dev/deploy_shinyapps.R",
    "docs/SHINY_DEPLOYMENT.md"
  )
  missing_source <- required_source[!file.exists(file.path(root, required_source))]
  add_audit(
    "Required publication source files present",
    if (length(missing_source)) "FAIL" else "PASS",
    paste(missing_source, collapse = "; ")
  )

  tracked_existing <- tracked[file.exists(file.path(root, tracked))]
  tracked_sizes <- if (length(tracked_existing)) {
    file.info(file.path(root, tracked_existing))$size
  } else {
    numeric()
  }
  large <- tracked_existing[is.finite(tracked_sizes) & tracked_sizes > 50 * 1024^2]
  add_audit(
    "No tracked file exceeds 50 MiB",
    if (length(large)) "WARN" else "PASS",
    paste(large, collapse = "; ")
  )

  text_candidates <- tracked_existing[
    grepl("(?i)\\.(r|rmd|md|txt|yml|yaml|csv|json|dcf|gitignore|rscignore)$",
          tracked_existing, perl = TRUE) |
      basename(tracked_existing) %in% c("README", "LICENSE", "VERSION")
  ]
  definite_credential_hits <- character()
  possible_credential_hits <- character()
  definite_pattern <- "AWSAccessKeyId=|x-amz-security-token="
  possible_pattern <- paste0(
    "(?i)(password|passwd|api[_-]?key|secret[_-]?key)",
    "[[:space:]]*[:=][[:space:]]*['\"][^'\"]{8,}['\"]"
  )
  for (rel in text_candidates) {
    lines <- tryCatch(
      readLines(file.path(root, rel), warn = FALSE, encoding = "UTF-8"),
      error = function(error) character()
    )
    definite <- grep(definite_pattern, lines, perl = TRUE)
    possible <- grep(possible_pattern, lines, perl = TRUE)
    if (length(definite)) {
      definite_credential_hits <- c(
        definite_credential_hits,
        paste0(rel, ":", paste(head(definite, 5L), collapse = ","))
      )
    }
    if (length(possible)) {
      possible_credential_hits <- c(
        possible_credential_hits,
        paste0(rel, ":", paste(head(possible, 5L), collapse = ","))
      )
    }
  }
  add_audit(
    "No signed deployment credentials in tracked text",
    if (length(definite_credential_hits)) "FAIL" else "PASS",
    paste(definite_credential_hits, collapse = "; ")
  )
  add_audit(
    "No possible embedded secrets in tracked text",
    if (length(possible_credential_hits)) "REVIEW" else "PASS",
    paste(possible_credential_hits, collapse = "; ")
  )
}

licence_path <- file.path(root, "LICENSE")
licence_text <- if (file.exists(licence_path)) {
  paste(readLines(licence_path, warn = FALSE), collapse = " ")
} else {
  ""
}
placeholder_licence <- grepl(
  "(?i)all rights reserved|licen[cs]e.*(to be determined|pending|placeholder)",
  licence_text,
  perl = TRUE
)
add_audit(
  "Public software licence approved",
  if (placeholder_licence || !nzchar(licence_text)) "REVIEW" else "PASS",
  if (placeholder_licence || !nzchar(licence_text)) {
    "Replace the placeholder/all-rights-reserved notice before making the repository public."
  } else {
    ""
  }
)

legacy_present <- dir.exists(file.path(root, "report", "data", "legacy")) &&
  length(list.files(file.path(root, "report", "data", "legacy"),
                    recursive = TRUE, all.files = TRUE, no.. = TRUE)) > 0L
add_audit(
  "Legacy comparison-data redistribution reviewed",
  if (legacy_present) "REVIEW" else "PASS",
  if (legacy_present) {
    "The files remain local and ignored; confirm permissions before distributing them separately."
  } else {
    ""
  }
)

audit_path <- file.path(manifest_dir, "final_git_release_audit.csv")
write.csv(audit, audit_path, row.names = FALSE, na = "")

cat("\nCleanup completed.\n")
cat("Preserved locally: raw/derived data, targets state, 1,000 draws, UI cache, compact report state, model-comparison partitions, legacy comparison inputs, and shinyapps bundle.\n")
cat("Excluded from Git: generated analytical state, restricted inputs, and deployment artefacts.\n")
cat("Release audit: ", audit_path, "\n", sep = "")
print(audit, row.names = FALSE)

if (any(audit$status == "FAIL")) {
  stop(
    "The final Git audit contains FAIL items. Resolve them before committing.",
    call. = FALSE
  )
}

cat("\nReview before committing:\n")
cat("  git status --short\n")
cat("  git diff -- .gitignore .rscignore\n")
cat("\nThen commit the clean source tree, for example:\n")
cat('  git add -A\n  git commit -m "Finalise publication-ready SA-NBD3 Version 1 repository"\n')
