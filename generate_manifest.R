# Generate manifest.json for Posit Connect (git-backed deployment)
# Run from repo root: Rscript generate_manifest.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}
library(rsconnect)
rsconnect::writeManifest(appDir = "energy_macro_system")
