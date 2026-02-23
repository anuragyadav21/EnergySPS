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

#' Log return: log(x_t / x_{t-1}). Returns NA where values are non-positive or NA (avoids NaN).
log_return <- function(x) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  prev <- x[-n]
  curr <- x[-1]
  ratio <- curr / prev
  out <- rep(NA_real_, n)
  ok <- is.finite(ratio) & ratio > 0 & is.finite(prev) & is.finite(curr)
  out[-1][ok] <- log(ratio[ok])
  out
}

#' Percent change: (x_t - x_{t-1}) / x_{t-1} * 100. Returns NA where denominator is zero or NA.
pct_change <- function(x) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  prev <- x[-n]
  curr <- x[-1]
  out <- rep(NA_real_, n)
  denom_ok <- is.finite(prev) & abs(prev) > 1e-10
  ok <- denom_ok & is.finite(curr)
  out[-1][ok] <- (curr[ok] - prev[ok]) / prev[ok] * 100
  out
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
#' FRED is primary (base); EIA is left_joined. Pipeline continues if EIA is empty; stops only if FRED has < 100 rows.
run_data_cleaning <- function(data_dir = "data", out_file = "system_timeseries.csv") {
  # ---- Primary: FRED ----
  fred <- load_fred_raw(file.path(data_dir, "fred_raw.csv"))
  fred <- fred %>% dplyr::filter(!is.na(date))
  message("FRED raw rows: ", nrow(fred))

  # ---- Secondary: EIA (optional) ----
  eia <- tryCatch({
    load_eia_raw(file.path(data_dir, "eia_raw.csv"))
  }, error = function(e) {
    warning("EIA load failed or missing: ", conditionMessage(e), " — continuing with FRED only.")
    NULL
  })
  if (!is.null(eia)) {
    eia <- eia %>% dplyr::filter(!is.na(date))
    if (nrow(eia) == 0) {
      warning("EIA dataset is empty — continuing with FRED only.")
      eia <- NULL
    }
  }
  if (is.null(eia)) {
    eia <- tibble(date = as.Date(character()), retail_sales = numeric(), renewable_share = numeric(), demand_growth_rate = numeric())
  }
  message("EIA rows: ", nrow(eia))

  # ---- Frequency harmonization: convert all series to monthly ----
  # Add year_month (YYYY-MM) and first-of-month date for merging
  fred_sub <- fred %>%
    dplyr::filter(!is.na(date)) %>%
    mutate(year_month = format(date, "%Y-%m"), date_month = as.Date(paste0(format(date, "%Y-%m"), "-01"))) %>%
    arrange(date)
  # Aggregate FRED to one row per month: daily -> monthly mean; monthly/quarterly -> mean (one value per month)
  fred_monthly <- fred_sub %>%
    group_by(year_month, date_month) %>%
    summarise(
      DCOILWTICO = mean(DCOILWTICO, na.rm = TRUE),
      CPIAUCSL = mean(CPIAUCSL, na.rm = TRUE),
      FEDFUNDS = mean(FEDFUNDS, na.rm = TRUE),
      INDPRO = mean(INDPRO, na.rm = TRUE),
      UNRATE = mean(UNRATE, na.rm = TRUE),
      GDPC1 = mean(GDPC1, na.rm = TRUE),
      .groups = "drop"
    )
  # Replace NaN from all-NA months with NA
  for (col in c("DCOILWTICO", "CPIAUCSL", "FEDFUNDS", "INDPRO", "UNRATE", "GDPC1")) {
    if (col %in% names(fred_monthly)) fred_monthly[[col]][!is.finite(fred_monthly[[col]])] <- NA
  }
  # Quarterly GDP: fill so each month in quarter has the same value (expand quarter to 3 months)
  if ("GDPC1" %in% names(fred_monthly)) {
    fred_monthly <- fred_monthly %>% arrange(date_month) %>% tidyr::fill(GDPC1, .direction = "down")
  }
  message("FRED monthly rows: ", nrow(fred_monthly))
  if (nrow(fred_monthly) < 100) {
    stop("Primary (FRED) has fewer than 100 months after aggregation; need >= 100 for VAR estimation.")
  }

  eia_sub <- eia %>% dplyr::filter(!is.na(date)) %>% arrange(date)
  if (nrow(eia_sub) > 0) {
    eia_sub <- eia_sub %>%
      mutate(year_month = format(date, "%Y-%m"), date_month = as.Date(paste0(format(date, "%Y-%m"), "-01"))) %>%
      group_by(year_month, date_month) %>%
      summarise(
        retail_sales = mean(retail_sales, na.rm = TRUE),
        renewable_share = mean(renewable_share, na.rm = TRUE),
        demand_growth_rate = mean(demand_growth_rate, na.rm = TRUE),
        .groups = "drop"
      )
    eia_sub$retail_sales[!is.finite(eia_sub$retail_sales)] <- NA
    eia_sub$renewable_share[!is.finite(eia_sub$renewable_share)] <- NA
    eia_sub$demand_growth_rate[!is.finite(eia_sub$demand_growth_rate)] <- NA
    message("EIA monthly rows: ", nrow(eia_sub))
  } else {
    eia_sub <- tibble(year_month = character(), date_month = as.Date(character()), retail_sales = numeric(), renewable_share = numeric(), demand_growth_rate = numeric())
  }

  # Merge on year_month (and date_month)
  merged <- fred_monthly %>%
    left_join(eia_sub, by = c("year_month", "date_month")) %>%
    rename(date = date_month)
  message("Merged (monthly) rows: ", nrow(merged))

  # Before log transforms: set non-positive and non-finite to NA so no NaN from log()
  if ("DCOILWTICO" %in% names(merged)) {
    merged$DCOILWTICO[merged$DCOILWTICO <= 0 | !is.finite(merged$DCOILWTICO)] <- NA
  }
  if ("INDPRO" %in% names(merged)) {
    merged$INDPRO[merged$INDPRO <= 0 | !is.finite(merged$INDPRO)] <- NA
  }
  if ("CPIAUCSL" %in% names(merged)) {
    merged$CPIAUCSL[merged$CPIAUCSL <= 0 | !is.finite(merged$CPIAUCSL)] <- NA
  }
  if ("GDPC1" %in% names(merged)) {
    merged$GDPC1[merged$GDPC1 <= 0 | !is.finite(merged$GDPC1)] <- NA
  }
  if ("retail_sales" %in% names(merged)) {
    merged$retail_sales[merged$retail_sales <= 0 | !is.finite(merged$retail_sales)] <- NA
  }

  # Build transformed series (stationarity-oriented); log_return and pct_change are NA-safe
  merged$oil_return <- log_return(merged$DCOILWTICO)
  message("After oil_return: rows with non-NA = ", sum(!is.na(merged$oil_return)))

  merged$inflation_change <- pct_change(merged$CPIAUCSL)
  message("After inflation_change: rows with non-NA = ", sum(!is.na(merged$inflation_change)))

  # rate_change: first difference (safe: diff() gives NA where input is NA)
  merged$rate_change <- c(NA, diff(merged$FEDFUNDS))
  merged$rate_change[!is.finite(merged$rate_change)] <- NA
  message("After rate_change: rows with non-NA = ", sum(!is.na(merged$rate_change)))

  merged$industrial_return <- log_return(merged$INDPRO)
  message("After industrial_return: rows with non-NA = ", sum(!is.na(merged$industrial_return)))

  merged$gdp_growth <- pct_change(merged$GDPC1)
  message("After gdp_growth: rows with non-NA = ", sum(!is.na(merged$gdp_growth)))

  # demand_change: log return on retail sales, or EIA growth rate, or 0 if missing
  if ("retail_sales" %in% names(merged) && !all(is.na(merged$retail_sales))) {
    merged$demand_change <- log_return(merged$retail_sales)
  } else if ("demand_growth_rate" %in% names(merged) && !all(is.na(merged$demand_growth_rate))) {
    merged$demand_change <- merged$demand_growth_rate
    merged$demand_change[!is.finite(merged$demand_change)] <- NA
  } else {
    merged$demand_change <- NA_real_
  }
  if (all(is.na(merged$demand_change))) {
    merged$demand_change <- 0
  }
  message("After demand_change: rows with non-NA = ", sum(!is.na(merged$demand_change)))

  # renewable_share: structural (do not require complete)
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

  # Only require essential macro variables (FRED-derived). Structural (demand_change, renewable_share) may be NA.
  essential_cols <- c("oil_return", "inflation_change", "rate_change", "industrial_return", "gdp_growth")
  out_df <- out_df %>% dplyr::filter(complete.cases(across(all_of(essential_cols))))
  message("After complete.cases(essential_cols) rows: ", nrow(out_df))

  # Fill structural variables so VAR has no NA
  out_df$demand_change[is.na(out_df$demand_change)] <- 0
  out_df <- out_df %>% tidyr::fill(renewable_share, .direction = "down")
  out_df$renewable_share[is.na(out_df$renewable_share)] <- 0

  if (nrow(out_df) < 100) {
    stop("Primary (FRED) dataset has fewer than 100 usable rows after merge (got ", nrow(out_df), "). Need >= 100 for VAR estimation.")
  }

  message("Final row count: ", nrow(out_df))
  out_path <- file.path(data_dir, out_file)
  readr::write_csv(out_df, out_path)
  message("Wrote ", out_path, " (", nrow(out_df), " rows).")
  invisible(out_df)
}
