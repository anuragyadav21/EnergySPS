# 07_stability_metrics.py — Eigenvalues, spectral radius, amplification, sensitivity

from pathlib import Path
from typing import List, Optional, Union

import numpy as np
import pandas as pd


def compute_stability_metrics(
    A: np.ndarray,
    var_names: Optional[List[str]] = None,
) -> dict:
    """
    Compute eigenvalues, spectral radius, stability, shock amplification, sensitivity ranking.
    """
    K = A.shape[0]
    if var_names is None:
        var_names = [f"V{i}" for i in range(K)]
    if len(var_names) != K:
        var_names = [f"V{i}" for i in range(K)]

    eigs = np.linalg.eigvals(A)
    mod = np.abs(eigs)
    rho = np.max(mod)
    is_stable = rho < 1
    condition = "|λ|_max < 1: stable" if is_stable else "|λ|_max >= 1: UNSTABLE"

    amp_one = np.linalg.norm(A, "fro")
    shock_amplification_factor = np.sqrt(np.sum(A ** 2))

    row_impact = np.sum(np.abs(A), axis=1)
    col_impact = np.sum(np.abs(A), axis=0)
    sensitivity = pd.DataFrame({
        "variable": var_names,
        "influenced_by": row_impact,
        "influences": col_impact,
        "total_sensitivity": row_impact + col_impact,
    }).sort_values("total_sensitivity", ascending=False)

    return {
        "eigenvalues": eigs,
        "eigenvalue_modulus": mod,
        "spectral_radius": rho,
        "is_stable": is_stable,
        "stability_condition": condition,
        "shock_amplification_factor": shock_amplification_factor,
        "norm_Frobenius": amp_one,
        "sensitivity_ranking": sensitivity,
    }


def run_stability_metrics(
    data_dir: str = "data",
    var_names: Optional[List[str]] = None,
) -> dict:
    """Load A from data dir and compute stability metrics."""
    A_path = Path(data_dir) / "A_matrix.csv"
    if not A_path.exists():
        raise FileNotFoundError(f"Missing {A_path} — run 05_parameter_estimation first.")
    df = pd.read_csv(A_path, index_col=0)
    A = df.values
    names = var_names or list(df.index)
    return compute_stability_metrics(A, names)
