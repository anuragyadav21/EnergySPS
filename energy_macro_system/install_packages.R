# install_packages.R — minimal runtime packages for Shiny app (app.R + R/*.R)
# cpp11 first (tidyr and others depend on it). Install with full dependencies.

packages <- c(
  "cpp11",
  "shiny",
  "httr",
  "jsonlite",
  "dplyr",
  "ggplot2",
  "tidyr",
  "readr",
  "bslib",
  "DT",
  "lubridate",
  "vars",
  "MASS",
  "Matrix"
)

packages <- unique(packages)
cat("Installing", length(packages), "packages:", paste(packages, collapse = ", "), "\n")

install.packages(
  packages,
  repos = "https://cloud.r-project.org",
  dependencies = TRUE,
  Ncpus = max(1L, parallel::detectCores() - 1L)
)

# Stop if any required package failed to install
failed <- character(0)
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    failed <- c(failed, pkg)
  }
}
if (length(failed) > 0) {
  stop("Required package(s) failed to install: ", paste(failed, collapse = ", "))
}
cat("Done.\n")
