# 01_fetch_fred.py
# Energy–Macro Shock Propagation Simulator — FRED data ingestion
# Fetches: DCOILWTICO, CPIAUCSL, FEDFUNDS, INDPRO, UNRATE, GDPC1
# Output: data/fred_raw.csv

import json
import os
import sys
import time
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Optional

import pandas as pd

# Allow run as script or as module
_SCRIPT_DIR = Path(__file__).resolve().parent
if str(_SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(_SCRIPT_DIR))
from load_env import SSL_CONTEXT, find_env_path, load_dotenv

FRED_BASE = "https://api.stlouisfed.org/fred/series/observations"
FRED_RATE_LIMIT_DELAY = 0.5


def _ensure_env():
    load_dotenv(find_env_path())


def fetch_fred_series(series_id: str, api_key: str) -> pd.DataFrame:
    """Fetch one FRED series. Returns DataFrame with date, value, series_id."""
    if not api_key:
        raise ValueError("FRED_API_KEY not set. Add to .env or set environment.")
    url = (
        f"{FRED_BASE}?series_id={urllib.parse.quote(series_id, safe='')}"
        f"&api_key={urllib.parse.quote(api_key, safe='')}&file_type=json"
    )
    try:
        req = urllib.request.Request(url)
        with urllib.request.urlopen(req, timeout=30, context=SSL_CONTEXT) as resp:
            if resp.status == 429:
                print("FRED rate limit; waiting 60s.")
                time.sleep(60)
                return fetch_fred_series(series_id, api_key)
            if resp.status != 200:
                raise RuntimeError(f"FRED API error {resp.status}: {resp.read(200).decode()}")
            body = json.load(resp)
    except urllib.error.URLError as e:
        raise RuntimeError(f"FRED request failed: {e}") from e
    obs = body.get("observations") or []
    if not obs:
        return pd.DataFrame(columns=["date", "value", "series_id"])

    def _to_float(v):
        if v is None or v == "" or v == ".":
            return float("nan")
        try:
            return float(v)
        except (ValueError, TypeError):
            return float("nan")

    df = pd.DataFrame([
        {"date": o.get("date"), "value": _to_float(o.get("value")), "series_id": series_id}
        for o in obs
    ])
    df["date"] = pd.to_datetime(df["date"]).dt.date
    return df


def quarterly_to_monthly(df: pd.DataFrame, date_col: str = "date", value_col: str = "value") -> pd.DataFrame:
    """Convert quarterly series to monthly (forward-fill)."""
    df = df.sort_values(date_col).reset_index(drop=True)
    dates = pd.date_range(str(df[date_col].min()), str(df[date_col].max()), freq="MS")
    out = pd.DataFrame({date_col: dates.date, value_col: float("nan")})
    for _, row in df.iterrows():
        mask = pd.to_datetime(out[date_col]) >= pd.Timestamp(row[date_col])
        out.loc[mask, value_col] = row[value_col]
    out[value_col] = out[value_col].ffill()
    return out


def run_fetch_fred(out_dir: str = "data", env_path: Optional[str] = None) -> pd.DataFrame:
    """Fetch all FRED series, convert GDP to monthly, write data/fred_raw.csv."""
    _ensure_env()
    if env_path:
        load_dotenv(env_path)
    api_key = os.environ.get("FRED_API_KEY", "")
    if not api_key:
        raise ValueError("FRED_API_KEY not set.")

    series_list = [
        "DCOILWTICO", "CPIAUCSL", "FEDFUNDS", "INDPRO", "UNRATE", "GDPC1"
    ]
    all_data = []
    for sid in series_list:
        print(f"Fetching FRED series: {sid}")
        df = fetch_fred_series(sid, api_key)
        all_data.append(df)
        time.sleep(FRED_RATE_LIMIT_DELAY)

    # GDP to monthly
    gdp = all_data[series_list.index("GDPC1")]
    if len(gdp) > 0:
        gdp_monthly = quarterly_to_monthly(gdp, "date", "value")
        gdp_monthly["series_id"] = "GDPC1"
        all_data[series_list.index("GDPC1")] = gdp_monthly

    long = pd.concat(all_data, ignore_index=True)
    wide = long.pivot_table(index="date", columns="series_id", values="value").reset_index()

    Path(out_dir).mkdir(parents=True, exist_ok=True)
    out_path = Path(out_dir) / "fred_raw.csv"
    wide.to_csv(out_path, index=False)
    print(f"Wrote {out_path} ({len(wide)} rows).")
    return wide


if __name__ == "__main__":
    # Run from energy_macro_system or project root
    root = Path(__file__).resolve().parent.parent
    os.chdir(root)
    run_fetch_fred("data")
