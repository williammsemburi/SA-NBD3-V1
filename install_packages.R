packages <- c(
  "arrow",
  "collapse",
  "data.table",
  "dplyr",
  "haven",
  "highcharter",
  "htmltools",
  "knitr",
  "readxl",
  "rmarkdown",
  "scales",
  "shiny",
  "targets",
  "testthat",
  "tidyselect",
  "yaml"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

message("All required pipeline and report packages are installed.")
