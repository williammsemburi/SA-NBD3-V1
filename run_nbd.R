#!/usr/bin/env Rscript

# Third South African National Burden of Disease Study -------------------------
# Version 1: reproducible mortality estimates for South Africa, 1997-2019.
# The scientific model follows six operations: COD cleaning, African natural
# completeness, total injury calibration, HIV/AIDS estimation, detailed injury
# allocation and redistribution of ill-defined causes.
#
# Run from the repository root:
#   Rscript run_nbd.R
#
# In RStudio:
#   source("run_nbd.R")
#
# Normal use requires changing only the five controls below. Set completed
# phases to FALSE to resume from existing outputs.

RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE

UNCERTAINTY_CONFIG   <- file.path("config", "uncertainty_joint.yml")
UNCERTAINTY_SCENARIO <- "joint"
OVERWRITE_UNCERTAINTY <- TRUE

nbd_start_directory <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- arguments[grepl("^--file=", arguments)]
  if (length(file_argument)) {
    script_path <- sub("^--file=", "", file_argument[[1L]])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }
  getwd()
}

nbd_locate_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    markers <- c(
      file.path(current, "_targets.R"),
      file.path(current, "R", "00_core.R"),
      file.path(current, "config", "config.yml")
    )
    if (all(file.exists(markers))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not locate the NBD3 Version 1 repository root. Start R inside ",
        "the folder containing run_nbd.R, _targets.R, R/, and config/.",
        call. = FALSE
      )
    }
    current <- parent
  }
}

nbd_module_files <- function(root) {
  files <- sort(list.files(
    file.path(root, "R"),
    pattern = "^[0-9]{2}_.*\\.R$",
    full.names = TRUE
  ))
  if (!length(files)) stop("No numbered R modules were found.", call. = FALSE)
  files
}

nbd_source_modules <- function(root, envir = .GlobalEnv) {
  invisible(lapply(nbd_module_files(root), sys.source, envir = envir))
}

nbd_timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
nbd_log <- function(...) message("[", nbd_timestamp(), "] ", paste0(..., collapse = ""))

nbd_phase <- function(title, expression) {
  bar <- paste(rep("=", 78L), collapse = "")
  cat("\n", bar, "\n", title, "\n", bar, "\n", sep = "")
  started <- proc.time()[[3L]]
  value <- force(expression)
  elapsed <- proc.time()[[3L]] - started
  nbd_log(title, " completed in ", round(elapsed, 1), " seconds")
  invisible(value)
}

root <- nbd_locate_root(nbd_start_directory())
setwd(root)
nbd_source_modules(root)
cfg <- read_project_config(root)

nbd_log("Third South African NBD Study — Version 1")
nbd_log("Project root: ", root)

tryCatch({
  nbd_phase("0. INPUT CHECK", {
    status <- check_required_inputs(cfg)
    message(
      "Required analytical inputs are ready. Full manifest: ",
      table_file(cfg, "input_status.csv")
    )
    if (isTRUE(RUN_REPORT)) {
      legacy_path <- file.path(root, "report", "data", "legacy", "viz.input.Rda")
      if (!file.exists(legacy_path)) {
        stop(
          "The report comparison input is missing: ", legacy_path,
          "\nPlace viz.input.Rda in report/data/legacy/.",
          call. = FALSE
        )
      }
    }
    invisible(status)
  })

  if (isTRUE(RUN_POINT_ESTIMATES)) {
    nbd_phase("A. POINT-ESTIMATE ANALYSIS", {
      require_package("targets")
      require_package("tidyselect")
      targets::tar_make(
        names = tidyselect::any_of("database_file_output"),
        reporter = "balanced"
      )
    })
  }

  if (isTRUE(RUN_FULL_VALIDATION)) {
    nbd_phase("A. FULL DEVELOPMENT VALIDATION", {
      sys.source(file.path(root, "dev", "full_validation.R"), envir = .GlobalEnv)
    })
  }

  if (isTRUE(RUN_UNCERTAINTY)) {
    nbd_phase("B. JOINT UNCERTAINTY", {
      require_package("yaml")
      uncertainty_path <- if (grepl(
        "^(?:[A-Za-z]:[/\\\\]|/|\\\\\\\\)",
        UNCERTAINTY_CONFIG
      )) UNCERTAINTY_CONFIG else file.path(root, UNCERTAINTY_CONFIG)
      if (!file.exists(uncertainty_path)) {
        stop("Uncertainty configuration not found: ", uncertainty_path, call. = FALSE)
      }
      uncertainty_cfg <- yaml::read_yaml(uncertainty_path)
      if (isTRUE(OVERWRITE_UNCERTAINTY)) {
        if (is.null(uncertainty_cfg$run)) uncertainty_cfg$run <- list()
        uncertainty_cfg$run$overwrite <- TRUE
      }
      run_nbd3_uncertainty(
        root = root,
        cfg = cfg,
        uncertainty_cfg = uncertainty_cfg,
        uncertainty_config_path = uncertainty_path
      )
    })
  }

  if (isTRUE(RUN_REPORT)) {
    nbd_phase("C. RESULTS REPORT", {
      source(file.path(root, "report", "R", "report_data.R"), local = FALSE)
      report_config <- read_viz_config(root)

      message("[1/5] Reading NBD2, NBD3-Stata, THEMBISA, and GBD2023 inputs")
      extract_legacy_viz(report_config)

      message("[2/5] Building NBD3-R comparison and cause-rate views")
      build_nbd3_r_views(report_config)

      message("[3/5] Building the report runtime")
      build_viz_input(report_config)

      message("[4/5] Attaching NBD3-R joint uncertainty intervals")
      nbd_attach_uncertainty_to_report(
        root = root,
        uncertainty_config_path = UNCERTAINTY_CONFIG,
        scenario = UNCERTAINTY_SCENARIO
      )

      message("[5/5] Checking report inputs")
      validate_viz_input(report_config, stop_on_failure = TRUE)
    })
  }

  nbd_log("Pipeline completed successfully.")
  nbd_log("Database: ", database_file(cfg, "NBD_database_1997_2019.parquet"))
  nbd_log("Report data: ", file.path(root, "output", "report-data"))

  if (isTRUE(OPEN_REPORT)) {
    if (!isTRUE(RUN_REPORT)) {
      stop("OPEN_REPORT requires RUN_REPORT = TRUE.", call. = FALSE)
    }
    nbd_phase("OPEN INTERACTIVE REPORT", {
      require_package("rmarkdown")
      report_path <- file.path(root, "report", "nbd3_results_report.Rmd")
      rmarkdown::run(
        file = report_path,
        shiny_args = list(launch.browser = TRUE),
        auto_reload = TRUE
      )
    })
  }
}, error = function(error) {
  cat(
    "\n", paste(rep("=", 78L), collapse = ""),
    "\nNBD3 VERSION 1 PIPELINE FAILED\n",
    paste(rep("=", 78L), collapse = ""),
    "\n", conditionMessage(error), "\n",
    sep = ""
  )
  stop(error)
})
