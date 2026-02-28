#!/usr/bin/env python3
# run_all.py — Run full Energy–Macro Shock Propagation Simulator pipeline (Python)

import os
import subprocess
import sys
from pathlib import Path

# Run from energy_macro_system or project root
ROOT = Path(__file__).resolve().parent
if ROOT.name != "energy_macro_system" and (ROOT / "energy_macro_system").exists():
    ROOT = ROOT / "energy_macro_system"
os.chdir(ROOT)
sys.path.insert(0, str(ROOT / "python"))

DATA_DIR = ROOT / "data"
DATA_DIR.mkdir(parents=True, exist_ok=True)


def main():
    print("=== Energy–Macro Shock Propagation Simulator — Pipeline (Python) ===\n")

    # Phase 1: Data ingestion
    print("--- Phase 1: Data ingestion ---")
    import load_env
    load_env.load_dotenv(load_env.find_env_path())

    import fetch_fred
    fetch_fred.run_fetch_fred(str(DATA_DIR))

    import fetch_eia
    fetch_eia.run_fetch_eia(str(DATA_DIR))

    import fetch_worldbank
    fetch_worldbank.run_fetch_worldbank(str(DATA_DIR))

    import data_cleaning
    data_cleaning.run_data_cleaning(str(DATA_DIR))

    # Phase 2: Parameter estimation
    print("\n--- Phase 2: Parameter estimation ---")
    import parameter_estimation
    est = parameter_estimation.run_parameter_estimation(str(DATA_DIR))

    # Phase 3: Monte Carlo
    print("\n--- Phase 3: Monte Carlo (example) ---")
    import monte_carlo_engine
    mc = monte_carlo_engine.simulate_system(est["A"], est["Sigma"], horizon=24, n_sim=1000)
    print(f"Recession prob (end): {mc['recession_prob_end']:.3f}")
    print(f"Inflation > 4% prob: {mc['inflation_stress_prob']:.3f}")
    print(f"Demand stress metric: {mc['demand_stress_metric']:.3f}")

    # Phase 4: Stability
    print("\n--- Phase 4: Stability metrics ---")
    import stability_metrics
    stab = stability_metrics.run_stability_metrics(str(DATA_DIR), est.get("columns"))
    print(stab["stability_condition"])
    print(f"Shock amplification factor: {stab['shock_amplification_factor']:.4f}")

    print("\n=== Pipeline complete. ===")
    print("\n--- Starting Shiny app ---")
    print("Dashboard will open at http://localhost:3838 (press Ctrl+C to stop).\n")
    subprocess.run(
        ["R", "-e", "shiny::runApp('.', port = 3838, launch.browser = TRUE)"],
        cwd=str(ROOT),
    )


if __name__ == "__main__":
    main()
