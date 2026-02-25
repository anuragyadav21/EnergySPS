# AI-Powered Shiny Application — Documentation

This document describes the Energy–Macro Shock Propagation Simulator Shiny app. All content is derived from the actual project code.

---

## 1. Data Summary

### Final cleaned dataset for modeling

- **File:** `data/system_timeseries.csv`
- **Produced by:** `R/04_data_cleaning.R` (`run_data_cleaning()`). Built from FRED (primary) and EIA (optional) after frequency harmonization to **monthly** and left-join on `year_month` / `date_month`; transforms applied after merge.

### VAR model columns

From `R/05_parameter_estimation.R` (`var_columns()`) and `R/04_data_cleaning.R` output:

| Column name | Data type | Description / derivation |
|-------------|-----------|---------------------------|
| `date` | Date | First-of-month date (monthly frequency). Used for alignment only; not in VAR matrix. |
| `oil_return` | numeric | Log return on WTI oil price: `log(x_t / x_{t-1})` from **DCOILWTICO** (FRED). NA where ratio non-positive or missing. |
| `inflation_change` | numeric | Percent change in CPI: `(x_t - x_{t-1}) / x_{t-1} * 100` from **CPIAUCSL** (FRED). NA where denominator zero or missing. |
| `rate_change` | numeric | First difference of Fed funds rate: `diff(FEDFUNDS)` from **FEDFUNDS** (FRED). Non-finite set to NA. |
| `industrial_return` | numeric | Log return on industrial production index: `log(x_t / x_{t-1})` from **INDPRO** (FRED). NA-safe as above. |
| `gdp_growth` | numeric | Percent change in real GDP: `pct_change(GDPC1)` from **GDPC1** (FRED). Quarterly GDP expanded to monthly then transformed. |
| `demand_change` | numeric | Log return on electricity retail sales from EIA (**retail_sales**), or EIA **demand_growth_rate**; if EIA missing/empty, filled with 0. |
| `renewable_share` | numeric | Level (0–1). From EIA **renewable_share**. Filled (locf then 0) if missing. |

**Note:** Rows are filtered to `complete.cases()` on essential columns only (`oil_return`, `inflation_change`, `rate_change`, `industrial_return`, `gdp_growth`). Structural variables `demand_change` and `renewable_share` may be filled; pipeline requires at least 100 rows after filtering.

---

## 2. Technical Details

### External APIs

- **FRED (Federal Reserve Economic Data):** `R/01_fetch_fred.R`. Base URL: `https://api.stlouisfed.org/fred/series/observations`. Series: DCOILWTICO, CPIAUCSL, FEDFUNDS, INDPRO, UNRATE, GDPC1. **Requires API key.**
- **EIA (U.S. Energy Information Administration):** `R/02_fetch_eia.R`. Base URL: `https://api.eia.gov/v2`. **Requires API key.**
- **World Bank:** `R/03_fetch_worldbank.R`. Base URL: `https://api.worldbank.org/v2`. Indicators: EG.IMP.CONS.ZS, EG.USE.COMM.GD.PP.KD (USA). **No API key.**
- **Ollama (local LLM):** `R/08_llm_layer.R`. Default `http://localhost:11434`; configurable via env.
- **OpenAI (LLM fallback):** Same file. `https://api.openai.com/v1/chat/completions`. **Requires API key for fallback.**

### Required environment variables (from code)

- **FRED_API_KEY** — Required for fetching FRED data (`R/01_fetch_fred.R`).
- **EIA_API_KEY** — Required for fetching EIA data (`R/02_fetch_eia.R`).
- **OLLAMA_BASE_URL** — Optional; default `http://localhost:11434` (`R/08_llm_layer.R`).
- **OLLAMA_MODEL** — Optional; default `gemma3:12b` (`R/08_llm_layer.R`).
- **OPENAI_API_KEY** — Optional; for LLM fallback when Ollama unavailable (`R/08_llm_layer.R`).
- **PORT** — Optional; default `8080` in Docker (`Dockerfile`).

**.env loading:** `R/00_load_env.R` and fetch scripts look for `.env` in `energy_macro_system/` or parent; keys loaded via `load_dotenv()` (KEY=value, no quotes).

### R packages

From `install_packages.R` and library/require/:: usage:

- **Explicit in install_packages.R:** cpp11, shiny, httr, jsonlite, dplyr, ggplot2, tidyr, readr, bslib, DT, lubridate, vars, MASS, Matrix.
- **Used via namespace (no extra install):** stats (e.g. quantile in `R/irf_engine.R`), parallel (e.g. detectCores in `install_packages.R`).

### Project file structure (energy_macro_system/)

- **Entry point:** `app.R` — Shiny UI/server; ends with `shinyApp(ui = ui, server = server)`.
- **R pipeline:** `R/00_load_env.R`, `01_fetch_fred.R`, `02_fetch_eia.R`, `03_fetch_worldbank.R`, `04_data_cleaning.R`, `05_parameter_estimation.R`, `06_monte_carlo_engine.R`, `07_stability_metrics.R`, `08_llm_layer.R`, `irf_engine.R`, `run_all.R`.
- **Data:** `data/` — fred_raw.csv, eia_raw.csv, worldbank_structural.csv, system_timeseries.csv, A_matrix.csv, covariance_matrix.csv.
- **Config / deploy:** `.env`, `install_packages.R`, `Dockerfile`, `DEPLOY.md`, `www/custom.css`.
- **Other:** `scripts/` (test_api_keys.R, test_ollama_openai.py, llm_report.py), `python/` (optional pipeline).

### Main entry point

- **Shiny app:** `energy_macro_system/app.R`. Run via `shiny::runApp()` with working directory `energy_macro_system`, or via `run_shiny.command` from repo root (cd into `energy_macro_system`, then `R -e "shiny::runApp(port = 3838, launch.browser = TRUE)"`).

---

## 3. Usage Instructions

### Install dependencies

- **R:** From `install_packages.R`: run `Rscript install_packages.R` from `energy_macro_system/` (or install the listed packages with `install.packages(..., repos = "https://cloud.r-project.org")`). Requires R (e.g. 4.3.x; rocker/shiny:4.3.2 in Docker).
- **System libs (Docker):** In `Dockerfile`: libssl-dev, libcurl4-openssl-dev, libxml2-dev, build-essential (handled in image build).

### Set required environment variables

- Create `energy_macro_system/.env` (or `.env` in parent) with at least:
  - `FRED_API_KEY=<your_key>`
  - `EIA_API_KEY=<your_key>`
- Optional: `OPENAI_API_KEY=...`, `OLLAMA_BASE_URL=http://...`, `OLLAMA_MODEL=...`
- Load by sourcing `R/00_load_env.R` or running scripts that call `load_dotenv()` (fetch scripts do this). Verify with `scripts/test_api_keys.R`.

### Run the app locally

- Set working directory to `energy_macro_system`, then run:
  - `R -e "shiny::runApp(port = 3838, launch.browser = TRUE)"`
  - Or from R: `setwd("energy_macro_system"); shiny::runApp(".")`
- Alternatively, from repo root run the script `run_shiny.command` (bash: cd into `energy_macro_system`, then `R -e "shiny::runApp(port = 3838, launch.browser = TRUE)"`). Open http://localhost:3838.
- If `data/system_timeseries.csv` is missing, the app may run the full R pipeline once or you can run `source("R/run_all.R")` from `energy_macro_system` to pre-populate data.

### Run with Docker

- From repo root:
  - `docker build -f energy_macro_system/Dockerfile -t energy-macro-shiny energy_macro_system`
  - `docker run -p 8080:8080 -e OLLAMA_BASE_URL=http://<your-ollama-host>:11434 energy-macro-shiny`
- Or from app directory:
  - `cd energy_macro_system && docker build -t energy-macro-shiny .`
  - `docker run -p 8080:8080 -e OLLAMA_BASE_URL=http://<your-ollama-host>:11434 energy-macro-shiny`
- Open http://localhost:8080. See `DEPLOY.md` for env vars and DigitalOcean source directory (`energy_macro_system`).
