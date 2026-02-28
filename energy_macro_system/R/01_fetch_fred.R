# 01_fetch_fred.R
# Energy–Macro Shock Propagation Simulator — FRED data ingestion
# Fetches: DCOILWTICO, CPIAUCSL, FEDFUNDS, INDPRO, UNRATE, GDPC1
# Output: data/fred_raw.csv

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# API key from environment only (e.g. Posit Connect). No .env file required.
FRED_BASE <- "https://api.stlouisfed.org/fred/series/observations"
FRED_RATE_LIMIT_DELAY <- 0.5  # seconds between requests (FRED allows 120/min)

#' Fetch one FRED series with error and rate-limit handling
#' @param series_id FRED series code
#' @param api_key API key (default from env)
#' @return tibble with columns date, value, series_id
fetch_fred_series <- function(series_id,
                              api_key = Sys.getenv("FRED_API_KEY")) {
  if (is.null(api_key) || !nzchar(api_key)) {
    stop("FRED_API_KEY not set. Set FRED_API_KEY in environment (e.g. Posit Connect).")
  }
  url <- sprintf(
    "%s?series_id=%s&api_key=%s&file_type=json",
    FRED_BASE,
    URLencode(series_id, reserved = TRUE),
    URLencode(api_key, reserved = TRUE)
  )
  resp <- tryCatch(
    httr::GET(url, httr::timeout(30)),
    error = function(e) stop("FRED request failed: ", conditionMessage(e))
  )
  if (httr::status_code(resp) == 429) {
    message("FRED rate limit hit; waiting 60s.")
    Sys.sleep(60)
    return(fetch_fred_series(series_id, api_key))
  }
  if (httr::status_code(resp) != 200) {
    stop("FRED API error ", httr::status_code(resp), ": ", httr::content(resp, "text", encoding = "UTF-8"))
  }
  body <- httr::content(resp, "parsed")
  obs <- body$observations %||% list()
  if (length(obs) == 0) {
    return(tibble(date = as.Date(character()), value = numeric(), series_id = character()))
  }
  out <- tibble(
    date = vapply(obs, function(x) x$date %||% NA_character_, character(1)),
    value = as.numeric(vapply(obs, function(x) x$value %||% NA_character_, character(1))),
    series_id = series_id
  )
  out$date <- as.Date(out$date)
  out
}

#' Convert quarterly GDP to monthly by repeating each value for 3 months
quarterly_to_monthly <- function(df, date_col = "date", value_col = "value") {
  df <- dplyr::arrange(df, .data[[date_col]])
  qtr <- df[[date_col]]
  # End-of-quarter months: 3,6,9,12
  mon <- seq(min(qtr, na.rm = TRUE), max(qtr, na.rm = TRUE), by = "month")
  mon <- as.Date(mon)
  vals <- approx(
    x = as.numeric(qtr),
    y = df[[value_col]],
    xout = as.numeric(mon),
    method = "constant",
    f = 0
  )$y
  tibble(!!date_col := mon, !!value_col := vals)
}

#' Fetch all FRED series, convert GDP to monthly, write data/fred_raw.csv
run_fetch_fred <- function(out_dir = "data", env_path = NULL) {
  api_key <- Sys.getenv("FRED_API_KEY")
  if (is.null(api_key) || !nzchar(api_key)) stop("FRED_API_KEY not set.")

  series_list <- c(
    "DCOILWTICO",  # WTI oil price
    "CPIAUCSL",    # CPI
    "FEDFUNDS",    # Federal funds rate
    "INDPRO",      # Industrial production index
    "UNRATE",      # Unemployment rate
    "GDPC1"        # Real GDP (quarterly)
  )

  all_data <- list()
  for (i in seq_along(series_list)) {
    sid <- series_list[i]
    message("Fetching FRED series: ", sid)
    all_data[[sid]] <- fetch_fred_series(sid, api_key)
    Sys.sleep(FRED_RATE_LIMIT_DELAY)
  }

  # Convert GDPC1 to monthly (interpolate / repeat quarterly)
  gdp <- all_data[["GDPC1"]]
  if (nrow(gdp) > 0) {
    gdp_monthly <- quarterly_to_monthly(gdp, "date", "value")
    gdp_monthly$series_id <- "GDPC1"
    all_data[["GDPC1"]] <- gdp_monthly
  }

  # Long to wide: one column per series
  long <- bind_rows(all_data)
  wide <- long %>%
    tidyr::pivot_wider(names_from = series_id, values_from = value)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, "fred_raw.csv")
  readr::write_csv(wide, out_path)
  message("Wrote ", out_path, " (", nrow(wide), " rows).")
  invisible(wide)
}

# Allow sourcing then calling, or direct execution
if (!interactive() && length(commandArgs(trailingOnly = TRUE)) > 0) {
  setwd(dirname(normalizePath(file.path(Sys.getenv("R_SCRIPT_DIR", "."), ".."))))
  run_fetch_fred()
}
