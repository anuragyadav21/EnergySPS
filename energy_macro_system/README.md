# Energy–Macro Shock Propagation Simulator

Stochastic systems modeling: energy–macro linkages, VAR-based dynamics, Monte Carlo simulation, and stability analysis. **Core data + modeling layer.** Implemented in **Python** (and R retained for reference).

---

## Structure

```
energy_macro_system/
├── data/                    # Outputs (git-ignored except .gitkeep)
│   ├── fred_raw.csv
│   ├── eia_raw.csv
│   ├── worldbank_structural.csv
│   ├── system_timeseries.csv
│   ├── A_matrix.csv
│   └── covariance_matrix.csv
├── python/                  # Python pipeline (primary)
│   ├── load_env.py          # .env + SSL
│   ├── fetch_fred.py        # 01 FRED ingestion
│   ├── fetch_eia.py         # 02 EIA ingestion
│   ├── fetch_worldbank.py   # 03 World Bank structural
│   ├── data_cleaning.py     # 04 Merge + transforms
│   ├── parameter_estimation.py  # 05 VAR, A, Σ
│   ├── monte_carlo_engine.py     # 06 simulate_system()
│   └── stability_metrics.py      # 07 Eigenvalues, amplification
├── R/                       # R pipeline (optional)
│   ├── 01_fetch_fred.R … 07_stability_metrics.R, run_all.R
├── scripts/                 # API key tests (Python + R)
├── run_all.py               # Run full Python pipeline
├── app.R                    # Shiny dashboard (reads data/*.csv)
├── requirements.txt
├── .env                     # FRED_API_KEY, EIA_API_KEY (not committed)
└── README.md
```

---

## 1. Module overview (Python)

| Module | Role |
|--------|------|
| **fetch_fred** | FRED: DCOILWTICO, CPIAUCSL, FEDFUNDS, INDPRO, UNRATE, GDPC1. Quarterly GDP → monthly. Writes `data/fred_raw.csv`. |
| **fetch_eia** | EIA retail sales (demand proxy) + net generation. Renewable share, demand growth. Writes `data/eia_raw.csv`. |
| **fetch_worldbank** | World Bank EG.IMP.CONS.ZS, EG.USE.COMM.GD.PP.KD. Writes `data/worldbank_structural.csv`. |
| **data_cleaning** | Merge FRED + EIA, build oil_return, inflation_change, rate_change, industrial_return, gdp_growth, demand_change, renewable_share. Writes `data/system_timeseries.csv`. |
| **parameter_estimation** | VAR (AIC lag), μ, Σ, A. Saves `A_matrix.csv`, `covariance_matrix.csv`. Prints eigenvalues, spectral radius. |
| **monte_carlo_engine** | `simulate_system(A, Sigma, shock_vector, horizon, n_sim)`. Returns paths, recession prob, inflation stress, demand stress. `oil_shock()`, `demand_shock()`. |
| **stability_metrics** | Eigenvalues, spectral radius, stability, shock amplification, sensitivity ranking. |

---

## 2. Python setup and run

**Install dependencies:**

```bash
cd energy_macro_system
pip install -r requirements.txt
```

**Required:** `pandas`, `numpy`, `statsmodels`, `scipy`. Optional: `certifi` (for SSL on macOS).

**Full pipeline:**

```bash
cd energy_macro_system
python run_all.py
```

**Step by step (from `energy_macro_system`):**

```bash
cd energy_macro_system
python python/fetch_fred.py
python python/fetch_eia.py
python python/fetch_worldbank.py
python python/data_cleaning.py
python python/parameter_estimation.py
# Then in Python:
# from python.monte_carlo_engine import simulate_system; from python.parameter_estimation import run_parameter_estimation
# est = run_parameter_estimation("data"); mc = simulate_system(est["A"], est["Sigma"], horizon=24, n_sim=1000)
python python/stability_metrics.py  # if run after 05
```

**Env:** Put `FRED_API_KEY` and `EIA_API_KEY` in `energy_macro_system/.env`. Scripts load `.env` from current or parent directory.

---

## 3. Shiny dashboard

After running the pipeline (so `data/*.csv` exist), start the dashboard:

```r
setwd("energy_macro_system")
shiny::runApp(".")
```

**R packages for dashboard:** `shiny`, `dplyr`, `tidyr`, `readr`, `ggplot2`, `DT`, `MASS`, `lubridate`.

The app shows: **Overview** (FRED + system series plots, summary stats), **FRED** (per-series plot + table), **EIA** (retail sales, renewable share, table), **World Bank** (structural indicators), **System series** (transformed series), **Model** (A and Σ tables), **Stability** (spectral radius, eigenvalues plot), **Monte Carlo** (run MC from A/Σ, recession/inflation stress, mean GDP path).

## 4. R pipeline (optional)

From `energy_macro_system`:

```r
source("R/run_all.R")
```

Packages: `httr`, `jsonlite`, `dplyr`, `tidyr`, `lubridate`, `readr`, `vars`, `MASS`.

---

## 5. Validation checklist

- [ ] **01** `data/fred_raw.csv`: columns include date, DCOILWTICO, CPIAUCSL, FEDFUNDS, INDPRO, UNRATE, GDPC1.
- [ ] **02** `data/eia_raw.csv`: period, retail_sales, (renewable_share, demand_growth_rate if available).
- [ ] **03** `data/worldbank_structural.csv`: year, energy_import_pct, energy_intensity.
- [ ] **04** `data/system_timeseries.csv`: date, oil_return, inflation_change, rate_change, industrial_return, gdp_growth, demand_change, renewable_share.
- [ ] **05** `data/A_matrix.csv`, `data/covariance_matrix.csv`; eigenvalues and spectral radius printed.
- [ ] **06** `simulate_system()` returns paths, recession_prob_end, inflation_stress_prob, demand_stress_metric.
- [ ] **07** Stability: spectral_radius, is_stable, shock_amplification_factor, sensitivity_ranking.
- [ ] Spectral radius < 1 ⇒ stable; ≥ 1 ⇒ unstable.

---

## 6. Notes

- **EIA generation:** If the generation route is unavailable, renewable share may be NA; 04 fills/zeros it.
- **VAR:** A is the first-lag coefficient matrix; X(t+1) = A X(t) + ε.
- **Shocks:** `oil_shock(magnitude)`, `demand_shock(magnitude)` in `monte_carlo_engine.py`.
