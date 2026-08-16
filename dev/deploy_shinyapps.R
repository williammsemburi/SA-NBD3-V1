#!/usr/bin/env Rscript

# Deploy the minimal SA-NBD3 application bundle to shinyapps.io.
# Authentication should be configured once with rsconnect::setAccountInfo().

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

root <- locate_root()
bundle_root <- file.path(root, "deployment", "shinyapps")
primary <- file.path(bundle_root, "report", "nbd3_results_report.Rmd")
if (!file.exists(primary)) {
  stop(
    "Prepared deployment bundle not found. Run ",
    "Rscript dev/prepare_shinyapps_bundle.R first.",
    call. = FALSE
  )
}

app_name <- Sys.getenv("NBD3_SHINY_APP_NAME", "nbd3viztool")
account <- Sys.getenv("NBD3_SHINY_ACCOUNT", "")
server <- Sys.getenv("NBD3_SHINY_SERVER", "shinyapps.io")
force_update <- tolower(Sys.getenv("NBD3_SHINY_FORCE_UPDATE", "true")) %in%
  c("true", "t", "1", "yes", "y")

max_bundle_gb <- suppressWarnings(as.numeric(
  Sys.getenv("NBD3_DEPLOY_MAX_GB", "0.95")
))
if (!is.finite(max_bundle_gb) || max_bundle_gb <= 0) max_bundle_gb <- 0.95
options(rsconnect.max.bundle.size = max_bundle_gb * 1024^3)

arguments <- list(
  appDir = bundle_root,
  appPrimaryDoc = "report/nbd3_results_report.Rmd",
  appMode = "rmd-shiny",
  appName = app_name,
  server = server,
  forceUpdate = force_update,
  launch.browser = FALSE
)
if (nzchar(account)) arguments$account <- account

message("Deploying application '", app_name, "' from: ", bundle_root)
do.call(rsconnect::deployApp, arguments)
