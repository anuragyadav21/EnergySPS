# test_api_keys.R — Verify FRED, EIA, and OpenAI API keys with minimal requests
# Run from project root: Rscript energy_macro_system/scripts/test_api_keys.R
# Or from energy_macro_system: Rscript scripts/test_api_keys.R
# Keys are read from environment only (e.g. Posit Connect). No .env file required.

options(warn = 1)

fred_key <- Sys.getenv("FRED_API_KEY")
eia_key  <- Sys.getenv("EIA_API_KEY")
openai_key <- Sys.getenv("OPENAI_API_KEY")

cat("--- API key presence ---\n")
cat("FRED_API_KEY:   ", if (nzchar(fred_key)) "set" else "NOT SET", "\n")
cat("EIA_API_KEY:    ", if (nzchar(eia_key)) "set" else "NOT SET", "\n")
cat("OPENAI_API_KEY: ", if (nzchar(openai_key)) "set" else "NOT SET", "\n\n")

# ---- FRED test ----
test_fred <- function() {
  if (!nzchar(fred_key)) return(list(ok = FALSE, msg = "FRED_API_KEY not set"))
  url <- sprintf(
    "https://api.stlouisfed.org/fred/series/observations?series_id=GDPC1&api_key=%s&file_type=json&limit=2",
    URLencode(fred_key, reserved = TRUE)
  )
  res <- tryCatch(
    httr::GET(url, httr::timeout(10)),
    error = function(e) list(status_code = 0, message = conditionMessage(e))
  )
  if (inherits(res, "response")) {
    if (httr::status_code(res) != 200) {
      return(list(ok = FALSE, msg = paste0("HTTP ", httr::status_code(res), ": ", httr::content(res, "text", encoding = "UTF-8"))))
    }
    body <- httr::content(res, "parsed")
    n_obs <- length(body$observations %||% list())
    return(list(ok = TRUE, msg = paste0("OK — got ", n_obs, " observation(s) for GDPC1")))
  }
  list(ok = FALSE, msg = res$message %||% "Request failed")
}

# ---- EIA test (v2 electricity retail sales, 1 row) ----
test_eia <- function() {
  if (!nzchar(eia_key)) return(list(ok = FALSE, msg = "EIA_API_KEY not set"))
  url <- sprintf(
    "https://api.eia.gov/v2/electricity/retail-sales/data?api_key=%s&frequency=monthly&data[0]=revenue&length=1",
    URLencode(eia_key, reserved = TRUE)
  )
  res <- tryCatch(
    httr::GET(url, httr::timeout(10), httr::add_headers(Accept = "application/json")),
    error = function(e) list(status_code = 0, message = conditionMessage(e))
  )
  if (inherits(res, "response")) {
    if (httr::status_code(res) != 200) {
      return(list(ok = FALSE, msg = paste0("HTTP ", httr::status_code(res), ": ", substr(httr::content(res, "text", encoding = "UTF-8"), 1L, 200))))
    }
    body <- httr::content(res, "parsed")
    n <- length(body$response$data %||% list())
    return(list(ok = TRUE, msg = paste0("OK — got ", n, " record(s) from electricity/retail-sales")))
  }
  list(ok = FALSE, msg = res$message %||% "Request failed")
}

# ---- OpenAI test (list models) ----
test_openai <- function() {
  if (!nzchar(openai_key)) return(list(ok = FALSE, msg = "OPENAI_API_KEY not set"))
  res <- tryCatch(
    httr::GET(
      "https://api.openai.com/v1/models",
      httr::timeout(15),
      httr::add_headers(Authorization = paste0("Bearer ", openai_key))
    ),
    error = function(e) list(status_code = 0, message = conditionMessage(e))
  )
  if (inherits(res, "response")) {
    if (httr::status_code(res) != 200) {
      return(list(ok = FALSE, msg = paste0("HTTP ", httr::status_code(res), ": ", substr(httr::content(res, "text", encoding = "UTF-8"), 1L, 200))))
    }
    body <- httr::content(res, "parsed")
    n <- length(body$data %||% list())
    return(list(ok = TRUE, msg = paste0("OK — list models returned ", n, " model(s)")))
  }
  list(ok = FALSE, msg = res$message %||% "Request failed")
}

# Need these packages
if (!requireNamespace("httr", quietly = TRUE)) stop("Install package: httr")
`%||%` <- function(x, y) if (is.null(x)) y else x

cat("--- Test queries ---\n")
cat("FRED:   ", (r_fred <- test_fred())$msg, "\n")
cat("EIA:    ", (r_eia <- test_eia())$msg, "\n")
cat("OpenAI: ", (r_openai <- test_openai())$msg, "\n\n")

all_ok <- r_fred$ok && r_eia$ok && r_openai$ok
cat("--- Result ---\n")
if (all_ok) cat("All API keys working.\n") else cat("One or more checks failed. Set FRED_API_KEY, EIA_API_KEY, OPENAI_API_KEY in environment.\n")

# Sample query: show a few rows from FRED and EIA
if (all_ok && r_fred$ok) {
  url_fred <- sprintf(
    "https://api.stlouisfed.org/fred/series/observations?series_id=GDPC1&api_key=%s&file_type=json&limit=5",
    URLencode(fred_key, reserved = TRUE)
  )
  sample_fred <- httr::content(httr::GET(url_fred, httr::timeout(10)), "parsed")
  cat("\n--- Sample FRED (GDPC1, real GDP) ---\n")
  for (o in sample_fred$observations %||% list()) cat("  ", o$date, " ", o$value, "\n")
}
if (all_ok && r_eia$ok) {
  url_eia <- sprintf(
    "https://api.eia.gov/v2/electricity/retail-sales/data?api_key=%s&frequency=monthly&data[0]=revenue&length=3",
    URLencode(eia_key, reserved = TRUE)
  )
  sample_eia <- httr::content(httr::GET(url_eia, httr::timeout(10)), "parsed")
  cat("\n--- Sample EIA (retail sales revenue) ---\n")
  for (d in sample_eia$response$data %||% list()) cat("  ", d$period, " ", d$revenue, " ", d$type, "\n")
}

invisible(!all_ok)


