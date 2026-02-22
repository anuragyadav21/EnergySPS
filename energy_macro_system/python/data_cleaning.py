# 04_data_cleaning.py — Merge FRED + EIA, build system_timeseries.csv

import sys
from pathlib import Path
from typing import List

import numpy as np
import pandas as pd

DATA_COLUMNS = [
    "date", "oil_return", "inflation_change", "rate_change",
    "industrial_return", "gdp_growth", "demand_change", "renewable_share",
]


def log_return(x: pd.Series) -> pd.Series:
    """Log return: log(x_t / x_{t-1})."""
    return np.log(x / x.shift(1))


def pct_change(x: pd.Series) -> pd.Series:
    """Percent change."""
    return x.pct_change(fill_method=None) * 100


def load_fred_raw(path: str) -> pd.DataFrame:
    """Load FRED raw CSV. Ensure date column."""
    p = Path(path)
    if not p.exists():
        raise FileNotFoundError(f"Missing {path} — run 01_fetch_fred first.")
    d = pd.read_csv(p)
    if "date" not in d.columns:
        d = d.rename(columns={d.columns[0]: "date"})
    d["date"] = pd.to_datetime(d["date"]).dt.date
    return d.sort_values("date").reset_index(drop=True)


def load_eia_raw(path: str) -> pd.DataFrame:
    """Load EIA raw CSV. period -> date."""
    p = Path(path)
    if not p.exists():
        raise FileNotFoundError(f"Missing {path} — run 02_fetch_eia first.")
    d = pd.read_csv(p)
    if "period" in d.columns:
        d["date"] = pd.to_datetime(d["period"].astype(str) + "-01").dt.date
    elif "date" not in d.columns:
        raise ValueError("EIA file must have 'period' or 'date'.")
    return d.sort_values("date").reset_index(drop=True)


def run_data_cleaning(data_dir: str = "data", out_file: str = "system_timeseries.csv") -> pd.DataFrame:
    """Merge FRED + EIA, compute transformed series, write system_timeseries.csv."""
    fred = load_fred_raw(Path(data_dir) / "fred_raw.csv")
    eia = load_eia_raw(Path(data_dir) / "eia_raw.csv")

    fred = fred.dropna(subset=["date"])
    eia = eia.dropna(subset=["date"])
    dates = np.intersect1d(fred["date"].values, eia["date"].values)
    if len(dates) == 0:
        raise ValueError("No overlapping dates between FRED and EIA.")

    fred_sub = fred[fred["date"].isin(dates)].sort_values("date").drop_duplicates("date")
    eia_sub = eia[eia["date"].isin(dates)].sort_values("date").groupby("date").first().reset_index()

    merged = fred_sub[
        ["date", "DCOILWTICO", "CPIAUCSL", "FEDFUNDS", "INDPRO", "UNRATE", "GDPC1"]
    ].merge(
        eia_sub[["date", "retail_sales", "renewable_share", "demand_growth_rate"]],
        on="date",
        how="left",
    )

    merged["oil_return"] = log_return(merged["DCOILWTICO"])
    merged["inflation_change"] = pct_change(merged["CPIAUCSL"])
    merged["rate_change"] = merged["FEDFUNDS"].diff()
    merged["industrial_return"] = log_return(merged["INDPRO"])
    merged["gdp_growth"] = pct_change(merged["GDPC1"])
    if "retail_sales" in merged.columns:
        merged["demand_change"] = log_return(merged["retail_sales"])
    else:
        merged["demand_change"] = merged["demand_growth_rate"]
    if "renewable_share" not in merged.columns:
        merged["renewable_share"] = np.nan

    out_df = merged[DATA_COLUMNS].copy()
    core_cols = [c for c in DATA_COLUMNS if c != "date" and c != "renewable_share"]
    out_df = out_df.dropna(subset=core_cols)
    out_df["renewable_share"] = out_df["renewable_share"].ffill().fillna(0)

    out_path = Path(data_dir) / out_file
    out_df.to_csv(out_path, index=False)
    print(f"Wrote {out_path} ({len(out_df)} rows).")
    return out_df


if __name__ == "__main__":
    root = Path(__file__).resolve().parent.parent
    import os
    os.chdir(root)
    run_data_cleaning("data")
