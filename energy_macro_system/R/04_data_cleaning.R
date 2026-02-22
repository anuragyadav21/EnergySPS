# 04_data_cleaning.R
# Energy–Macro Shock Propagation Simulator — merge and transform to modeling series
# Loads: fred_raw.csv, eia_raw.csv
# Output: data/system_timeseries.csv
# Columns: date, oil_return, inflation_change, rate_change, industrial_return, gdp_growth,
#          demand_change, renewable_share

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

#' Log return: log(x_t / x_{t-1})
log_return <- function(x) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  c(NA, log(x[-1] / x[-n]))
}

#' Percent change: (x_t - x_{t-1}) / x_{t-1} * 100
pct_change <- function(x) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  c(NA, (x[-1] - x[-n]) / x[-n] * 100)
}

#' Load FRED raw and ensure date column
load_fred_raw <- function(path = "data/fred_raw.csv") {
  if (!file.exists(path)) stop("Missing ", path, " — run 01_fetch_fred.R first.")
  d <- readr::read_csv(path, show_col_types = FALSE)
  # FRED date column may be 'date' or first column
  if (!"date" %in% names(d)) {
    nc <- names(d)[1]
    if (tolower(nc) %in% c("date", "observation_date")) d <- d %>% rename(date = !!sym(nc))
    else d <- d %>% rename(date = 1)
  }
  d$date <- as.Date(d$date)
  dplyr::arrange(d, date)
}

#' Load EIA raw and ensure period -> date
load_eia_raw <- function(path = "data/eia_raw.csv") {
  if (!file.exists(path)) stop("Missing ", path, " — run 02_fetch_eia.R first.")
  d <- readr::read_csv(path, show_col_types = FALSE)
  # EIA period is typically YYYY-MM; convert to first of month
  if ("period" %in% names(d)) {
    d$date <- as.Date(paste0(d$period, "-01"), optional = TRUE)
  } else if (!"date" %in% names(d)) {
    stop("EIA file must have 'period' or 'date'.")
  }
  d$date <- as.Date(d$date)
  dplyr::arrange(d, date)
}

#' Merge FRED and EIA on date, compute transformed series, save system_timeseries.csv
run_data_cleaning <- function(data_dir = "data", out_file = "system_timeseries.csv") {
  fred <- load_fred_raw(file.path(data_dir, "fred_raw.csv"))
  eia <- load_eia_raw(file.path(data_dir, "eia_raw.csv"))

  # Common date range
  fred <- fred %>% dplyr::filter(!is.na(date))
  eia <- eia %>% dplyr::filter(!is.na(date))
  dates <- intersect(fred$date, eia$date)
  if (length(dates) == 0) stop("No overlapping dates between FRED and EIA.")

  # Merge
  fred_sub <- fred %>% dplyr::filter(date %in% dates) %>% arrange(date)
  eia_sub <- eia %>% dplyr::filter(date %in% dates) %>% arrange(date)
  # Ensure 1:1 on date (take first match if EIA has multiple per period)
  eia_sub <- eia_sub %>% group_by(date) %>% slice(1) %>% ungroup()

  merged <- fred_sub %>%
    dplyr::select(date, DCOILWTICO, CPIAUCSL, FEDFUNDS, INDPRO, UNRATE, GDPC1) %>%
    left_join(
      eia_sub %>% dplyr::select(date, retail_sales, renewable_share, demand_growth_rate),
      by = "date"
    )

  # Build transformed series (stationarity-oriented)
  # oil_return: log return on oil price
  merged$oil_return <- log_return(merged$DCOILWTICO)
  # inflation_change: % change in CPI
  merged$inflation_change <- pct_change(merged$CPIAUCSL)
  # rate_change: first difference of Fed funds (or % change; spec says "rate_change")
  merged$rate_change <- c(NA, diff(merged$FEDFUNDS))
  # industrial_return: log return on industrial production
  merged$industrial_return <- log_return(merged$INDPRO)
  # gdp_growth: % change in real GDP
  merged$gdp_growth <- pct_change(merged$GDPC1)
  # demand_change: log return on retail sales (demand proxy), or use demand_growth_rate from EIA
  if ("retail_sales" %in% names(merged)) {
    merged$demand_change <- log_return(merged$retail_sales)
  } else {
    merged$demand_change <- merged$demand_growth_rate  # already % change
  }
  # renewable_share: level (keep as is)
  if (!"renewable_share" %in% names(merged)) merged$renewable_share <- NA_real_

  out_df <- merged %>%
    dplyr::select(
      date,
      oil_return,
      inflation_change,
      rate_change,
      industrial_return,
      gdp_growth,
      demand_change,
      renewable_share
    )
  # Remove rows where any core series is NA (renewable_share may be NA if EIA gen not available)
  core_cols <- c("oil_return", "inflation_change", "rate_change", "industrial_return", "gdp_growth", "demand_change")
  out_df <- out_df %>% dplyr::filter(complete.cases(across(all_of(core_cols))))
  if (nrow(out_df) < 100) stop("Merged dataset has ", nrow(out_df), " rows; need > 100 for VAR estimation. Check FRED/EIA date overlap and data quality.")
  # Fill renewable_share NAs: locf then 0 for leading NAs
  out_df <- out_df %>% tidyr::fill(renewable_share, .direction = "down")
  out_df$renewable_share[is.na(out_df$renewable_share)] <- 0

  out_path <- file.path(data_dir, out_file)
  readr::write_csv(out_df, out_path)
  message("Wrote ", out_path, " (", nrow(out_df), " rows).")
  invisible(out_df)
}
