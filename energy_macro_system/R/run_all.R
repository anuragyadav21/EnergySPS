# run_all.R
# Energy–Macro Shock Propagation Simulator — run full pipeline (data + modeling)
# Set working directory to energy_macro_system before sourcing.

# Execution order: 01 -> 02 -> 03 -> 04 -> 05 (06 and 07 are called explicitly or from 05)
message("=== Energy–Macro Shock Propagation Simulator — Pipeline ===\n")

# Ensure we are in energy_macro_system (project root for this app)
if (basename(getwd()) != "energy_macro_system") {
  if (dir.exists("energy_macro_system")) setwd("energy_macro_system")
  else if (dir.exists("../energy_macro_system")) setwd("../energy_macro_system")
}
ROOT <- getwd()
R_DIR <- file.path(ROOT, "R")
DATA_DIR <- file.path(ROOT, "data")

# Phase 1: Data ingestion
message("--- Phase 1: Data ingestion ---")
source(file.path(R_DIR, "01_fetch_fred.R"), local = TRUE)
run_fetch_fred(DATA_DIR)
source(file.path(R_DIR, "02_fetch_eia.R"), local = TRUE)
run_fetch_eia(DATA_DIR)
source(file.path(R_DIR, "03_fetch_worldbank.R"), local = TRUE)
run_fetch_worldbank(DATA_DIR)
source(file.path(R_DIR, "04_data_cleaning.R"), local = TRUE)
run_data_cleaning(DATA_DIR, "system_timeseries.csv")

# Phase 2: Parameter estimation
message("\n--- Phase 2: Parameter estimation ---")
source(file.path(R_DIR, "05_parameter_estimation.R"), local = TRUE)
est <- run_parameter_estimation(DATA_DIR)

# Phase 3 & 4: Monte Carlo and stability
message("\n--- Phase 3: Monte Carlo (example) ---")
source(file.path(R_DIR, "06_monte_carlo_engine.R"), local = TRUE)
A <- est$A
Sigma <- est$Sigma
mc <- simulate_system(A, Sigma, shock_vector = NULL, horizon = 24L, n_sim = 1000L)
message("Recession prob (end): ", round(mc$recession_prob_end, 3))
message("Inflation > 4% prob: ", round(mc$inflation_stress_prob, 3))
message("Demand stress metric: ", round(mc$demand_stress_metric, 3))

message("\n--- Phase 4: Stability metrics ---")
source(file.path(R_DIR, "07_stability_metrics.R"), local = TRUE)
stab <- run_stability_metrics(DATA_DIR)
message(stab$stability_condition)
message("Shock amplification factor: ", round(stab$shock_amplification_factor, 4))

# Optional: LLM interpretation of VAR stability (eigenvalues, spectral radius, shock amplification)
tryCatch({
  source(file.path(R_DIR, "08_llm_layer.R"), local = TRUE)
  llm_out <- llm_interpret_var_stability(stab, use_ollama = TRUE, use_openai = TRUE)
  if (llm_out$success && nzchar(llm_out$narrative)) {
    message("\n--- LLM VAR stability interpretation ---")
    message(llm_out$narrative)
  }
}, error = function(e) message("LLM interpretation skipped: ", conditionMessage(e)))

message("\n=== Pipeline complete. ===")

