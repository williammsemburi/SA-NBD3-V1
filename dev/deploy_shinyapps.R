#!/usr/bin/env Rscript

# Deploy the root-primary NBD3 R Markdown Shiny application over the existing
# shinyapps.io application.

options(stringsAsFactors = FALSE)

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

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Run Rscript install_packages.R.",
       call. = FALSE)
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required. Run Rscript install_packages.R.",
       call. = FALSE)
}

root <- locate_root()
bundle_root <- file.path(root, "deployment", "shinyapps")
primary_relative <- "nbd3viztool.Rmd"
primary <- file.path(bundle_root, primary_relative)
source_rmd <- file.path(root, "report", "nbd3_results_report.Rmd")
build_marker <- file.path(bundle_root, "DEPLOY_BUILD.txt")
state_path <- file.path(bundle_root, "report", "data", "report_state.rds")
required <- c(
  primary,
  file.path(bundle_root, "global.R"),
  file.path(bundle_root, "report", "R", "report_panels.R"),
  state_path,
  file.path(
    bundle_root,
    "output", "report-data", "ui_uncertainty_cache", "manifest.yml"
  ),
  build_marker,
  file.path(bundle_root, "deployment_manifest.csv")
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(
    "Prepared deployment bundle is incomplete. Missing:\n- ",
    paste(missing, collapse = "\n- "),
    "\nRun Rscript dev/prepare_shinyapps_bundle.R first.",
    call. = FALSE
  )
}

# The corrected bundle has one root Rmd and no nested Rmd/global.R pair.
rmd_files <- gsub("\\\\", "/", list.files(
  bundle_root,
  pattern = "\\.[Rr][Mm][Dd]$",
  recursive = TRUE,
  full.names = FALSE
))
if (!identical(rmd_files, primary_relative)) {
  stop(
    "The deployment bundle is not root-primary-only. Rebuild it. Found: ",
    paste(rmd_files, collapse = ", "),
    call. = FALSE
  )
}
if (file.exists(file.path(bundle_root, "report", "global.R"))) {
  stop(
    "A nested report/global.R remains in the bundle. Rebuild it with the ",
    "root-primary health-check fix.",
    call. = FALSE
  )
}

marker <- readLines(build_marker, warn = FALSE)
marker_value <- function(prefix) {
  values <- sub(paste0("^", prefix), "", marker[grepl(paste0("^", prefix), marker)])
  if (length(values)) values[[1L]] else ""
}
if (!identical(
  marker_value("startup_layout="),
  "root-primary-direct-global"
)) {
  stop("The deployment bundle has the wrong startup layout. Rebuild it.",
       call. = FALSE)
}

source_hash <- unname(digest::digest(
  file = source_rmd,
  algo = "sha256",
  serialize = FALSE
))
if (!identical(source_hash, marker_value("source_sha256="))) {
  stop(
    "The deployment bundle was built from an older report. Run ",
    "Rscript dev/prepare_shinyapps_bundle.R again.",
    call. = FALSE
  )
}

rmd_header <- readLines(primary, n = 50L, warn = FALSE, encoding = "UTF-8")
if (!any(grepl(
  "^[[:space:]]*runtime:[[:space:]]*shiny[[:space:]]*$",
  rmd_header,
  ignore.case = TRUE
))) {
  stop("The deployment primary document is not runtime: shiny.",
       call. = FALSE)
}
if (any(grepl(
  "^[[:space:]]*self_contained:[[:space:]]*true[[:space:]]*$",
  rmd_header,
  ignore.case = TRUE
))) {
  stop("The deployment primary document is still self_contained: true.",
       call. = FALSE)
}

app_name <- Sys.getenv("NBD3_SHINY_APP_NAME", "nbd3viztool")
app_id <- Sys.getenv("NBD3_SHINY_APP_ID", "16178524")
account <- Sys.getenv("NBD3_SHINY_ACCOUNT", "")
server <- Sys.getenv("NBD3_SHINY_SERVER", "shinyapps.io")
force_update <- tolower(Sys.getenv("NBD3_SHINY_FORCE_UPDATE", "true")) %in%
  c("true", "t", "1", "yes", "y")
max_bundle_gb <- suppressWarnings(as.numeric(
  Sys.getenv("NBD3_DEPLOY_MAX_GB", "0.95")
))
if (!is.finite(max_bundle_gb) || max_bundle_gb <= 0) max_bundle_gb <- 0.95
options(rsconnect.max.bundle.size = max_bundle_gb * 1024^3)

app_files <- list.files(
  bundle_root,
  recursive = TRUE,
  all.files = TRUE,
  no.. = TRUE,
  include.dirs = FALSE
)
app_files <- setdiff(app_files, c(
  "deployment_manifest.csv", "README_DEPLOYMENT.txt"
))
app_files <- app_files[!grepl("(^|/)rsconnect(/|$)", app_files)]

state_mb <- file.info(state_path)$size / 1024^2
build_id <- marker_value("build_id=")
if (!nzchar(build_id)) build_id <- "unknown"

arguments <- list(
  appDir = bundle_root,
  appFiles = app_files,
  appPrimaryDoc = primary_relative,
  appMode = "rmd-shiny",
  appName = app_name,
  server = server,
  forceUpdate = force_update,
  launch.browser = FALSE,
  logLevel = "verbose",
  lint = FALSE,
  quarto = FALSE
)
if (nzchar(account)) arguments$account <- account
if (nzchar(app_id)) arguments$appId <- app_id

message("Deploying current NBD3 report to '", app_name, "'.")
message("Application ID: ", if (nzchar(app_id)) app_id else "resolved by name")
message("Build ID: ", build_id)
message("Primary document: ", primary_relative)
message("Startup layout: root-primary-direct-global")
message("Compact state: ", sprintf("%.1f MB", state_mb))
message("Bundled files: ", format(length(app_files), big.mark = ","))

result <- tryCatch(
  do.call(rsconnect::deployApp, arguments),
  error = function(error) {
    message("\nDeployment failed: ", conditionMessage(error))
    message(
      "The existing shinyapps.io version remains active when an update task ",
      "fails."
    )
    stop(error)
  }
)

message("")
message("Deployment completed. Open:")
message("https://msemburi.shinyapps.io/", app_name, "/")
invisible(result)
