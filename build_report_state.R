#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, scipen = 999)

locate_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "report", "nbd3_results_report.Rmd")) &&
        file.exists(file.path(current, "report", "R", "report_state_builder.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the SA-NBD3 repository root.", call. = FALSE)
    }
    current <- parent
  }
}

build_report_state_main <- function(root = locate_root()) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  builder_path <- file.path(root, "report", "R", "report_state_builder.R")
  builder_env <- new.env(parent = globalenv())
  sys.source(builder_path, envir = builder_env)
  if (!exists("build_nbd_report_state", envir = builder_env,
              inherits = FALSE)) {
    stop(
      "report_state_builder.R does not define build_nbd_report_state().",
      call. = FALSE
    )
  }

  message("Building compact NBD3 report state...")
  state <- builder_env$build_nbd_report_state(root)
  if (!is.list(state$objects) || length(state$objects) < 25L) {
    stop("Refusing to write an incomplete report state.", call. = FALSE)
  }

  state_dir <- file.path(root, "report", "data")
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  state_path <- file.path(state_dir, "report_state.rds")
  temp_path <- paste0(state_path, ".tmp")

  # The compact state should load quickly enough that compression is unnecessary
  # at worker startup. The explicit size guard below prevents another oversized
  # state from reaching deployment.
  saveRDS(state, temp_path, compress = FALSE, version = 3)
  state_bytes <- file.info(temp_path)$size
  max_state_mb <- suppressWarnings(as.numeric(
    Sys.getenv("NBD3_MAX_REPORT_STATE_MB", "64")
  ))
  if (!is.finite(max_state_mb) || max_state_mb < 8) max_state_mb <- 64
  if (state_bytes > max_state_mb * 1024^2) {
    unlink(temp_path, force = TRUE)
    stop(
      "The prepared report state is ", round(state_bytes / 1024^2, 1),
      " MB; the hosted-worker limit is ", max_state_mb,
      " MB. Review report/data/report_state_object_sizes.csv.",
      call. = FALSE
    )
  }

  if (file.exists(state_path)) unlink(state_path, force = TRUE)
  if (!file.rename(temp_path, state_path)) {
    stop("Could not install the rebuilt report state.", call. = FALSE)
  }

  object_sizes <- data.frame(
    object = names(state$objects),
    size_bytes = as.numeric(state$object_sizes[names(state$objects)]),
    stringsAsFactors = FALSE
  )
  object_sizes$size_mb <- round(object_sizes$size_bytes / 1024^2, 3)
  object_sizes <- object_sizes[order(object_sizes$size_bytes, decreasing = TRUE), ]
  utils::write.csv(
    object_sizes,
    file.path(state_dir, "report_state_object_sizes.csv"),
    row.names = FALSE,
    na = ""
  )

  manifest <- data.frame(
    schema_version = as.integer(state$schema_version),
    built_at = as.character(state$built_at),
    r_version = as.character(state$r_version),
    object_count = length(state$objects),
    object_names = paste(names(state$objects), collapse = ";"),
    size_bytes = as.numeric(state_bytes),
    size_mb = round(as.numeric(state_bytes) / 1024^2, 3),
    source_signature = as.character(state$source_signature),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest,
    file.path(state_dir, "report_state_manifest.csv"),
    row.names = FALSE,
    na = ""
  )

  message("Report state written: ", state_path)
  message("Objects: ", format(length(state$objects), big.mark = ","))
  message("Size: ", sprintf("%.1f MB", state_bytes / 1024^2))
  invisible(state_path)
}

if (sys.nframe() == 0L) build_report_state_main()
