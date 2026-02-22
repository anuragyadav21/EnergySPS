# 06_monte_carlo_engine.py — Monte Carlo: X(t+1) = A X(t) + ε

from pathlib import Path
from typing import List, Optional, Union

import numpy as np

VAR_NAMES = [
    "oil_return", "inflation_change", "rate_change", "industrial_return",
    "gdp_growth", "demand_change", "renewable_share",
]
OIL_IDX = 0
INFLATION_IDX = 1
GDP_IDX = 4
DEMAND_IDX = 5


def simulate_system(
    A: np.ndarray,
    Sigma: np.ndarray,
    shock_vector: Optional[np.ndarray] = None,
    horizon: int = 12,
    n_sim: int = 1000,
    x0: Optional[np.ndarray] = None,
    seed: Optional[int] = None,
) -> dict:
    """
    Simulate X(t+1) = A X(t) + ε, ε ~ N(0, Σ).
    Returns paths, GDP distribution, recession prob, inflation stress prob, demand stress.
    """
    if seed is not None:
        np.random.seed(seed)
    K = A.shape[0]
    if A.shape != (K, K) or Sigma.shape != (K, K):
        raise ValueError("A and Sigma must be square and same dimension.")
    horizon = int(horizon)
    n_sim = int(n_sim)

    # Draw all innovations: (horizon, n_sim, K)
    eps = np.random.multivariate_normal(np.zeros(K), Sigma, size=(horizon, n_sim))

    paths = np.zeros((n_sim, horizon, K))
    x = np.tile(x0 if x0 is not None else np.zeros(K), (n_sim, 1))
    for t in range(horizon):
        x = x @ A.T + eps[t]
        if t == 0 and shock_vector is not None and len(shock_vector) == K:
            x = x + shock_vector
        paths[:, t, :] = x

    gdp_paths = paths[:, :, GDP_IDX]
    inflation_paths = paths[:, :, INFLATION_IDX]
    demand_paths = paths[:, :, DEMAND_IDX]

    recession_prob_end = np.mean(gdp_paths[:, -1] < 0)
    recession_prob_any = np.mean(np.any(gdp_paths < 0, axis=1))
    inflation_stress_prob = np.mean(inflation_paths > 4)
    demand_stress_metric = np.mean(demand_paths < -2)

    return {
        "paths": paths,
        "horizon": horizon,
        "n_sim": n_sim,
        "var_names": VAR_NAMES[:K],
        "gdp_distribution": gdp_paths.ravel(),
        "gdp_mean_path": gdp_paths.mean(axis=0),
        "gdp_q05": np.percentile(gdp_paths, 5, axis=0),
        "gdp_q95": np.percentile(gdp_paths, 95, axis=0),
        "recession_prob_end": recession_prob_end,
        "recession_prob_any": recession_prob_any,
        "inflation_stress_prob": inflation_stress_prob,
        "demand_stress_metric": demand_stress_metric,
    }


def oil_shock(magnitude: float, var_names: Optional[List[str]] = None) -> np.ndarray:
    """Shock vector for oil (oil_return = magnitude)."""
    names = var_names or VAR_NAMES
    v = np.zeros(len(names))
    if "oil_return" in names:
        v[names.index("oil_return")] = magnitude
    return v


def demand_shock(magnitude: float, var_names: Optional[List[str]] = None) -> np.ndarray:
    """Shock vector for demand (demand_change = magnitude)."""
    names = var_names or VAR_NAMES
    v = np.zeros(len(names))
    if "demand_change" in names:
        v[names.index("demand_change")] = magnitude
    return v
