# 02_fetch_eia.R
# Energy–Macro Shock Propagation Simulator — EIA data ingestion
# Fetches: monthly electricity retail sales (demand proxy), generation by source
# Computes: renewable share, demand growth rate
# Output: data/eia_raw.csv

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Load .env
if (!exists("load_dotenv")) {
  load_dotenv <- function(path) {
    if (!file.exists(path)) return(invisible(NULL))
    lines <- readLines(path, warn = FALSE)
    for (line in lines) {
      line <- trimws(line); if (line == "") next
      if (substr(line, 1L, 1L) == "#") line <- trimws(substr(line, 2L, nchar(line)))
      idx <- regexpr("=", line, fixed = TRUE); if (idx < 1) next
      key <- trimws(substr(line, 1L, idx - 1L)); val <- trimws(substr(line, idx + 1L, nchar(line)))
      if (nzchar(key) && nzchar(val)) do.call(Sys.setenv, setNames(list(val), key))
    }
    invisible(NULL)
  }
}
for (env in c(".env", "../.env", "energy_macro_system/.env")) if (file.exists(env)) { load_dotenv(env); break }

EIA_BASE <- "https://api.eia.gov/v2"

#' Fetch EIA v2 route with pagination and error handling
eia_get <- function(route, params = list(), api_key = Sys.getenv("EIA_API_KEY")) {
  if (is.null(api_key) || !nzchar(api_key)) stop("EIA_API_KEY not set.")
  params$api_key <- api_key
  qstr <- paste(names(params), vapply(params, function(p) URLencode(as.character(p), reserved = TRUE), character(1)), sep = "=", collapse = "&")
  url <- paste0(EIA_BASE, "/", route, "?", qstr)
  resp <- tryCatch(
    httr::GET(url, httr::timeout(30), httr::add_headers(Accept = "application/json")),
    error = function(e) stop("EIA request failed: ", conditionMessage(e))
  )
  if (httr::status_code(resp) == 429) {
    message("EIA rate limit; waiting 60s."); Sys.sleep(60); return(eia_get(route, params, api_key))
  }
  if (httr::status_code(resp) != 200) {
    stop("EIA API error ", httr::status_code(resp), ": ", substr(httr::content(resp, "text", encoding = "UTF-8"), 1L, 300))
  }
  httr::content(resp, "parsed")
}

#' Fetch electricity retail sales (monthly) — full US historical, aligned with Python
#' Uses: frequency=monthly, facets stateid=US, sectorid=ALL, start=2000-01, end=2024-12, length=5000
fetch_retail_sales <- function(api_key, length = 5000) {
  params <- list(
    frequency = "monthly",
    "data[0]" = "sales",
    "facets[stateid][]" = "US",
    "facets[sectorid][]" = "ALL",
    start = "2000-01",
    end = "2024-12",
    "sort[0][column]" = "period",
    "sort[0][direction]" = "asc",
    length = length
  )
  body <- eia_get("electricity/retail-sales/data", params, api_key)
  data <- body$response$data %||% list()
  if (length(data) == 0) return(tibble(period = character(), sales = numeric()))
  # Aggregate to US total if multiple series (e.g. by state/sector)
  periods <- vapply(data, function(x) x$period %||% NA_character_, character(1))
  sales   <- vapply(data, function(x) as.numeric(x$sales %||% NA_real_), numeric(1))
  tibble(period = periods, sales = sales) %>%
    group_by(period) %>%
    summarise(retail_sales = sum(sales, na.rm = TRUE), .groups = "drop")
}

#' Fetch electricity net generation (monthly) — total and renewable
#' EIA v2: electricity/electric-power-operational-data or net-generation
fetch_net_generation <- function(api_key, length = 500) {
  # Try electric-power-operational-data with generation by fuel
  routes_to_try <- c(
    "electricity/electric-power-operational-data/data",
    "electricity/net-generation/data"
  )
  body <- NULL
  for (route in routes_to_try) {
    body <- tryCatch(
      eia_get(route, list(frequency = "monthly", length = length), api_key),
      error = function(e) NULL
    )
    if (!is.null(body) && length(body$response$data %||% list()) > 0) break
  }
  if (is.null(body)) {
    message("EIA generation route not available; using retail sales only for demand. Renewable share will be NA.")
    return(tibble(period = character(), total_generation = numeric(), renewable_generation = numeric()))
  }
  data <- body$response$data %||% list()
  # Structure varies; try to get generation and fuel type
  # Common: period, value, type/fuel. Aggregate total and sum wind/solar/hydro/geothermal as renewable
  out <- tibble(
    period = vapply(data, function(x) x$period %||% NA_character_, character(1)),
    generation = as.numeric(vapply(data, function(x) as.numeric(x$generation %||% x$value %||% NA_real_), numeric(1))),
    type = vapply(data, function(x) tolower(x$type %||% x$fuel %||% ""), character(1))
  )
  renewable_types <- c("wind", "solar", "hydro", "geothermal", "hydroelectric")
  out %>%
    group_by(period) %>%
    summarise(
      total_generation = sum(generation, na.rm = TRUE),
      renewable_generation = sum(generation[tolower(type) %in% renewable_types], na.rm = TRUE),
      .groups = "drop"
    )
}

#' Run EIA fetch: retail sales + generation, compute renewable share and demand growth
run_fetch_eia <- function(out_dir = "data", env_path = NULL) {
  if (!is.null(env_path)) load_dotenv(env_path)
  api_key <- Sys.getenv("EIA_API_KEY")
  if (is.null(api_key) || !nzchar(api_key)) stop("EIA_API_KEY not set.")

  message("Fetching EIA retail sales (demand proxy)...")
  sales_df <- fetch_retail_sales(api_key)
  message("Fetching EIA net generation...")
  gen_df <- fetch_net_generation(api_key)

  # Merge on period (YYYY-MM)
  eia <- sales_df
  eia$period <- as.character(eia$period)
  if (nrow(gen_df) > 0) {
    gen_df$period <- as.character(gen_df$period)
    eia <- eia %>%
      left_join(gen_df, by = "period") %>%
      mutate(
        renewable_share = ifelse(total_generation > 0, renewable_generation / total_generation, NA_real_)
      )
  } else {
    eia$total_generation <- NA_real_
    eia$renewable_generation <- NA_real_
    eia$renewable_share <- NA_real_
  }

  # Demand growth rate (month-over-month % change)
  eia <- eia %>% arrange(period)
  eia$demand_growth_rate <- c(NA, diff(eia$retail_sales) / head(eia$retail_sales, -1)) * 100

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, "eia_raw.csv")
  readr::write_csv(eia, out_path)
  message("Wrote ", out_path, " (", nrow(eia), " rows).")
  invisible(eia)
}
