# 03_fetch_worldbank.py — World Bank structural indicators (no API key)

import json
import sys
import urllib.request
from pathlib import Path
from typing import Optional

import pandas as pd

_SCRIPT_DIR = Path(__file__).resolve().parent
if str(_SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(_SCRIPT_DIR))
from load_env import SSL_CONTEXT

WB_BASE = "https://api.worldbank.org/v2"


def fetch_wb_indicator(indicator: str, country: str = "USA") -> pd.DataFrame:
    """Fetch one World Bank indicator. Returns DataFrame with year, value, indicator."""
    url = f"{WB_BASE}/country/{country}/indicator/{indicator}?format=json&per_page=500"
    req = urllib.request.Request(url)
    try:
        with urllib.request.urlopen(req, timeout=30, context=SSL_CONTEXT) as resp:
            if resp.status != 200:
                raise RuntimeError(f"World Bank API error {resp.status}")
            body = json.load(resp)
    except urllib.error.URLError as e:
        raise RuntimeError(f"World Bank request failed: {e}") from e
    if len(body) < 2 or not body[1]:
        return pd.DataFrame(columns=["year", "value", "indicator"])
    data = body[1]
    df = pd.DataFrame([
        {"year": int(d.get("date", 0)), "value": float(d.get("value", float("nan"))) if d.get("value") is not None else float("nan")}
        for d in data
    ])
    return df


def run_fetch_worldbank(out_dir: str = "data") -> pd.DataFrame:
    """Fetch energy import dependency and energy intensity. Write data/worldbank_structural.csv."""
    print("Fetching World Bank: energy import dependency (% of energy use)...")
    imp = fetch_wb_indicator("EG.IMP.CONS.ZS", "USA")
    imp = imp.rename(columns={"value": "energy_import_pct"})[["year", "energy_import_pct"]]
    print("Fetching World Bank: energy intensity...")
    intensity = fetch_wb_indicator("EG.USE.COMM.GD.PP.KD", "USA")
    intensity = intensity.rename(columns={"value": "energy_intensity"})[["year", "energy_intensity"]]

    structural = imp.merge(intensity, on="year", how="outer").sort_values("year")
    structural = structural.dropna(how="all", subset=["energy_import_pct", "energy_intensity"])

    Path(out_dir).mkdir(parents=True, exist_ok=True)
    out_path = Path(out_dir) / "worldbank_structural.csv"
    structural.to_csv(out_path, index=False)
    print(f"Wrote {out_path} ({len(structural)} rows).")
    return structural


if __name__ == "__main__":
    root = Path(__file__).resolve().parent.parent
    import os
    os.chdir(root)
    run_fetch_worldbank("data")
