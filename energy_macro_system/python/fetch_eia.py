# fetch_eia.py — EIA Retail Sales (Full Historical Monthly)

import json
import os
import sys
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Dict, Any, Optional

import pandas as pd

_SCRIPT_DIR = Path(__file__).resolve().parent
if str(_SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(_SCRIPT_DIR))

from load_env import SSL_CONTEXT, find_env_path, load_dotenv

EIA_BASE = "https://api.eia.gov/v2"


# ---------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------

def _ensure_env():
    load_dotenv(find_env_path())


def eia_get(route: str, params: Dict[str, Any], api_key: str) -> Dict:
    if not api_key:
        raise ValueError("EIA_API_KEY not set.")

    params = {**params, "api_key": api_key}

    query = "&".join(
        f"{k}={urllib.parse.quote(str(v), safe='')}"
        for k, v in params.items()
    )

    url = f"{EIA_BASE}/{route}?{query}"
    req = urllib.request.Request(url, headers={"Accept": "application/json"})

    with urllib.request.urlopen(req, timeout=30, context=SSL_CONTEXT) as resp:
        if resp.status != 200:
            raise RuntimeError(f"EIA API error {resp.status}")
        return json.load(resp)


def _to_float(v):
    try:
        if v in (None, "", "."):
            return float("nan")
        return float(v)
    except Exception:
        return float("nan")


# ---------------------------------------------------------------------
# Retail Sales (Demand Proxy)
# ---------------------------------------------------------------------

def fetch_retail_sales(api_key: str) -> pd.DataFrame:

    print("Fetching full historical US retail electricity sales...")

    body = eia_get(
        "electricity/retail-sales/data",
        {
            "frequency": "monthly",
            "data[0]": "sales",
            "facets[stateid][]": "US",
            "facets[sectorid][]": "ALL",
            "start": "2000-01",
            "end": "2024-12",
            "sort[0][column]": "period",
            "sort[0][direction]": "asc",
            "length": 5000,
        },
        api_key,
    )

    data = (body.get("response") or {}).get("data") or []

    if not data:
        raise RuntimeError("No retail sales data returned from EIA.")

    rows = [
        {
            "period": d.get("period"),
            "retail_sales": _to_float(d.get("sales")),
        }
        for d in data
    ]

    df = pd.DataFrame(rows)

    return df



# ---------------------------------------------------------------------
# Main Runner
# ---------------------------------------------------------------------

def run_fetch_eia(out_dir: str = "data", env_path: Optional[str] = None):

    _ensure_env()

    if env_path:
        load_dotenv(env_path)

    api_key = os.environ.get("EIA_API_KEY")
    if not api_key:
        raise ValueError("EIA_API_KEY not set in .env")

    sales = fetch_retail_sales(api_key)

    # Normalize to month-end
    sales["date"] = pd.to_datetime(sales["period"]) + pd.offsets.MonthEnd(0)
    sales = sales.sort_values("date").reset_index(drop=True)

    # Demand growth
    sales["demand_growth_rate"] = sales["retail_sales"].pct_change() * 100

    # Temporary placeholder for compatibility
    sales["renewable_share"] = float("nan")

    Path(out_dir).mkdir(parents=True, exist_ok=True)
    out_path = Path(out_dir) / "eia_raw.csv"

    sales.to_csv(out_path, index=False)

    print(f"Wrote {out_path} ({len(sales)} rows).")

    return sales


if __name__ == "__main__":
    root = Path(__file__).resolve().parent.parent
    os.chdir(root)
    run_fetch_eia("data")
