# 03_fetch_worldbank.R
# Energy–Macro Shock Propagation Simulator — World Bank structural indicators
# Fetches: Energy import dependency (% of consumption), Energy intensity of GDP
# No API key required.
# Output: data/worldbank_structural.csv

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

WB_BASE <- "https://api.worldbank.org/v2"

#' Fetch one World Bank indicator for a country
#' @param indicator e.g. EG.IMP.CONS.ZS (energy imports %), EG.USE.COMM.GD.PP.KD (energy intensity)
#' @param country ISO2 country code (default USA)
fetch_wb_indicator <- function(indicator, country = "USA") {
  url <- sprintf(
    "%s/country/%s/indicator/%s?format=json&per_page=500",
    WB_BASE, country, indicator
  )
  resp <- tryCatch(
    httr::GET(url, httr::timeout(30)),
    error = function(e) stop("World Bank request failed: ", conditionMessage(e))
  )
  if (httr::status_code(resp) != 200) {
    stop("World Bank API error ", httr::status_code(resp))
  }
  body <- httr::content(resp, "parsed")
  if (length(body) < 2 || length(body[[2]]) == 0) {
    return(tibble(year = integer(), value = numeric(), indicator = character()))
  }
  data <- body[[2]]
  tibble(
    year = as.integer(vapply(data, function(x) x$date %||% NA_character_, character(1))),
    value = as.numeric(vapply(data, function(x) x$value %||% NA_real_, numeric(1))),
    indicator = indicator
  )
}

#' Run World Bank fetch: energy import dependency, energy intensity
run_fetch_worldbank <- function(out_dir = "data") {
  message("Fetching World Bank: energy import dependency (% of energy use)...")
  imp <- fetch_wb_indicator("EG.IMP.CONS.ZS", "USA")  # Energy imports, net (% of energy use)
  message("Fetching World Bank: energy intensity (energy use per GDP)...")
  intensity <- fetch_wb_indicator("EG.USE.COMM.GD.PP.KD", "USA")  # kg oil equiv per $1000 GDP (PPP)

  # Structural: one row per year, wide format (dplyr::select in case MASS is loaded)
  imp_wide <- imp %>% dplyr::select(year, value) %>% rename(energy_import_pct = value)
  int_wide <- intensity %>% dplyr::select(year, value) %>% rename(energy_intensity = value)

  structural <- full_join(imp_wide, int_wide, by = "year") %>%
    arrange(year) %>%
    dplyr::filter(!is.na(energy_import_pct) | !is.na(energy_intensity))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, "worldbank_structural.csv")
  readr::write_csv(structural, out_path)
  message("Wrote ", out_path, " (", nrow(structural), " rows).")
  invisible(structural)
}
