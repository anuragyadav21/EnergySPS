# 00_load_env.R
# Load .env into environment. Source this from scripts that need API keys.
# Handles KEY=value and "# KEY=value" (commented) lines.

load_dotenv <- function(path = ".env") {
  if (!file.exists(path)) return(invisible(NULL))
  lines <- readLines(path, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    if (substr(line, 1L, 1L) == "#") line <- trimws(substr(line, 2L, nchar(line)))
    idx <- regexpr("=", line, fixed = TRUE)
    if (idx < 1) next
    key <- trimws(substr(line, 1L, idx - 1L))
    val <- trimws(substr(line, idx + 1L, nchar(line)))
    if (nzchar(key) && nzchar(val)) do.call(Sys.setenv, setNames(list(val), key))
  }
  invisible(NULL)
}

# Resolve .env path relative to project root (R/ or energy_macro_system/)
get_env_path <- function() {
  env_here <- ".env"
  env_parent <- file.path("..", ".env")
  if (file.exists(env_here)) return(env_here)
  if (file.exists(env_parent)) return(env_parent)
  env_ems <- "energy_macro_system/.env"
  if (file.exists(env_ems)) return(env_ems)
  env_here
}
