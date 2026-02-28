# 00_load_env.R
# API keys are read from the process environment via Sys.getenv().
# Use FRED_API_KEY and EIA_API_KEY (e.g. set in Posit Connect or shell).
# No .env file is required; these stubs exist for backward compatibility.

load_dotenv <- function(path = ".env") {
  # No-op: do not read any file. Keys must be set via environment (e.g. Posit Connect).
  invisible(NULL)
}

get_env_path <- function() {
  # No-op: no file path; callers should use Sys.getenv() only.
  NULL
}
