# 05_parameter_estimation.py — VAR estimation, A and Sigma

import sys
from pathlib import Path
from typing import List, Optional, Tuple, Any

import numpy as np
import pandas as pd

_USE_STATSMODELS = False
_var_model = None

try:
    from statsmodels.tsa.api import VAR
    _USE_STATSMODELS = True
    _var_model = VAR
except ImportError:
    pass


def _fit_var1_ols(Y: np.ndarray) -> Tuple[np.ndarray, np.ndarray, Any]:
    """Fallback: fit VAR(1) via OLS when statsmodels is not installed. Returns (A, Sigma, fake_res)."""
    # Y_t = c + A @ Y_{t-1} + eps.  X = [1, Y_{t-1}], fit Y_t = X @ B.
    T, K = Y.shape
    Y_lag = Y[:-1]   # (T-1, K)
    Y_cur = Y[1:]    # (T-1, K)
    X = np.column_stack([np.ones(T - 1), Y_lag])  # (T-1, 1+K)
    B, _, _, _ = np.linalg.lstsq(X, Y_cur, rcond=None)
    c = B[0]
    A = B[1:].T   # (K, K)
    resid = Y_cur - X @ B
    Sigma = np.cov(resid.T, bias=False)
    # Fake result object so get_A_matrix and sigma_u work
    class FakeVARRes:
        neqs = K
        params = np.column_stack([c, A])  # (K, 1+K)
        sigma_u = Sigma
    return A, Sigma, FakeVARRes()

VAR_COLUMNS = [
    "oil_return", "inflation_change", "rate_change", "industrial_return",
    "gdp_growth", "demand_change", "renewable_share",
]


def load_system_timeseries(path: str) -> pd.DataFrame:
    """Load system_timeseries.csv."""
    p = Path(path)
    if not p.exists():
        raise FileNotFoundError(f"Missing {path} — run 04_data_cleaning first.")
    d = pd.read_csv(p)
    d["date"] = pd.to_datetime(d["date"])
    return d


def estimate_moments(Y: np.ndarray) -> dict:
    """Estimate μ, Σ, correlation."""
    mask = ~np.any(np.isnan(Y), axis=1)
    Yc = Y[mask]
    mu = np.nanmean(Yc, axis=0)
    Sigma = np.cov(Yc.T, bias=False)
    corr = np.corrcoef(Yc.T)
    return {"mu": mu, "Sigma": Sigma, "correlation": corr}


def fit_var(Y: np.ndarray, max_lag: int = 8) -> Tuple[object, int]:
    """Fit VAR (statsmodels if available, else VAR(1) OLS fallback). Returns (fitted result, p)."""
    if _USE_STATSMODELS and _var_model is not None:
        model = _var_model(Y)
        sel = model.select_order(maxlags=max_lag)
        p = int(sel.selected_orders.get("aic", 1))
        p = max(1, p)
        print(f"VAR lag order (AIC): {p}")
        return model.fit(p), p
    print("statsmodels not installed; using VAR(1) OLS fallback.")
    A, Sigma, res = _fit_var1_ols(Y)
    return res, 1


def get_A_matrix(var_res) -> np.ndarray:
    """Extract lag-1 coefficient matrix A (K x K) from VAR results (statsmodels or OLS fallback)."""
    params = var_res.params
    K = params.shape[0]
    A = params[:, 1 : 1 + K].copy()
    return A


def run_parameter_estimation(data_dir: str = "data") -> dict:
    """Load system_timeseries, estimate μ/Σ, fit VAR, save A and Sigma. Return dict with A, Sigma, etc."""
    ts_path = Path(data_dir) / "system_timeseries.csv"
    d = load_system_timeseries(str(ts_path))
    cols = [c for c in VAR_COLUMNS if c in d.columns]
    if len(cols) < 2:
        raise ValueError("Need at least 2 series in system_timeseries for VAR.")
    # Ensure float and drop rows with any missing
    df_y = d[cols].apply(pd.to_numeric, errors="coerce")
    df_y = df_y.dropna(how="any")
    Y = df_y.values.astype(np.float64)
    if len(Y) < 30:
        raise ValueError("Insufficient observations for VAR (need at least 30).")

    print("Estimating moments (μ, Σ, correlation)...")
    moments = estimate_moments(Y)
    print("Fitting VAR (lag selection by AIC if statsmodels available)...")
    var_res, _ = fit_var(Y)
    A = get_A_matrix(var_res)
    Sigma = getattr(var_res, "sigma_u", None)
    if Sigma is None:
        Sigma = moments["Sigma"]

    # Ensure symmetric and valid
    Sigma = (Sigma + Sigma.T) / 2
    ev = np.linalg.eigvalsh(Sigma)
    if np.any(ev <= 0):
        try:
            from scipy.linalg import sqrtm
            # Nearest PD: add small diagonal then project
            Sigma = Sigma + np.eye(len(Sigma)) * (1e-6 - min(0, ev.min()))
            ev = np.linalg.eigvalsh(Sigma)
            if np.any(ev <= 0):
                Sigma = Sigma + np.eye(len(Sigma)) * (1e-5 - ev.min())
        except ImportError:
            Sigma = Sigma + np.eye(len(Sigma)) * 1e-6

    Path(data_dir).mkdir(parents=True, exist_ok=True)
    pd.DataFrame(A, index=cols, columns=cols).to_csv(Path(data_dir) / "A_matrix.csv")
    pd.DataFrame(Sigma, index=cols, columns=cols).to_csv(Path(data_dir) / "covariance_matrix.csv")

    eigs = np.linalg.eigvals(A)
    rho = np.max(np.abs(eigs))
    print(f"Eigenvalues of A (modulus): {np.abs(eigs).round(4).tolist()}")
    print(f"Spectral radius ρ(A) = {rho:.4f} — system is {'UNSTABLE' if rho >= 1 else 'stable'}.")

    return {
        "mu": moments["mu"],
        "Sigma": Sigma,
        "correlation": moments["correlation"],
        "A": A,
        "var_res": var_res,
        "eigenvalues": eigs,
        "spectral_radius": rho,
        "columns": cols,
    }


if __name__ == "__main__":
    root = Path(__file__).resolve().parent.parent
    import os
    os.chdir(root)
    run_parameter_estimation("data")
