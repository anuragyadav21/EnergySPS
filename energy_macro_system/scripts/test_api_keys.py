#!/usr/bin/env python3
"""
test_api_keys.py — Verify FRED, EIA, and OpenAI API keys with minimal requests.
Run from project root: python energy_macro_system/scripts/test_api_keys.py
Or from energy_macro_system: python scripts/test_api_keys.py

Uses stdlib + optional certifi for SSL on macOS. Install certifi if you see SSL errors:
  pip install certifi
"""

import json
import os
import ssl
import sys
import urllib.parse
import urllib.request
from pathlib import Path

# SSL context: use certifi's CA bundle if available (fixes macOS certificate verify failed)
def _make_ssl_context():
    try:
        import certifi
        return ssl.create_default_context(cafile=certifi.where())
    except ImportError:
        pass
    # On macOS, Python from python.org often has no certs; use unverified so script runs.
    # For verified connections run: pip install certifi
    if sys.platform == "darwin":
        ctx = ssl.create_default_context()
        ctx.check_hostname = False
        ctx.verify_mode = ssl.CERT_NONE
        return ctx
    return ssl.create_default_context()

_SSL_CONTEXT = _make_ssl_context()
_SSL_UNVERIFIED = _SSL_CONTEXT.verify_mode == ssl.CERT_NONE


def load_dotenv(path: str = ".env") -> None:
    """Load KEY=value from file into os.environ. Also handles '# KEY=value' lines."""
    path = Path(path)
    if not path.exists():
        return
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line:
            continue
        if line.startswith("#"):
            line = line.lstrip("#").strip()
        if "=" not in line:
            continue
        key, _, val = line.partition("=")
        key, val = key.strip(), val.strip()
        if key and val:
            os.environ[key] = val


def find_env_path() -> str:
    """Locate .env next to script (energy_macro_system/.env) or in cwd."""
    script_dir = Path(__file__).resolve().parent
    env_path = script_dir.parent / ".env"
    if env_path.exists():
        return str(env_path)
    if (Path.cwd() / ".env").exists():
        return ".env"
    if (Path.cwd() / "energy_macro_system" / ".env").exists():
        return "energy_macro_system/.env"
    return ".env"


def test_fred(key: str) -> tuple[bool, str]:
    """Test FRED API with GDPC1 series."""
    if not key:
        return False, "FRED_API_KEY not set"
    url = (
        "https://api.stlouisfed.org/fred/series/observations"
        f"?series_id=GDPC1&api_key={urllib.parse.quote(key, safe='')}&file_type=json&limit=2"
    )
    try:
        req = urllib.request.Request(url)
        with urllib.request.urlopen(req, timeout=10, context=_SSL_CONTEXT) as resp:
            if resp.status != 200:
                return False, f"HTTP {resp.status}: {resp.read(200).decode()}"
            data = json.load(resp)
            obs = data.get("observations") or []
            return True, f"OK — got {len(obs)} observation(s) for GDPC1"
    except Exception as e:
        return False, str(e)


def test_eia(key: str) -> tuple[bool, str]:
    """Test EIA API v2 electricity retail-sales."""
    if not key:
        return False, "EIA_API_KEY not set"
    url = (
        "https://api.eia.gov/v2/electricity/retail-sales/data"
        f"?api_key={urllib.parse.quote(key, safe='')}&frequency=monthly&data[0]=revenue&length=1"
    )
    try:
        req = urllib.request.Request(url, headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=10, context=_SSL_CONTEXT) as resp:
            if resp.status != 200:
                return False, f"HTTP {resp.status}: {resp.read(200).decode()}"
            data = json.load(resp)
            recs = (data.get("response") or {}).get("data") or []
            return True, f"OK — got {len(recs)} record(s) from electricity/retail-sales"
    except Exception as e:
        return False, str(e)


def test_openai(key: str) -> tuple[bool, str]:
    """Test OpenAI API (list models)."""
    if not key:
        return False, "OPENAI_API_KEY not set"
    try:
        req = urllib.request.Request(
            "https://api.openai.com/v1/models",
            headers={"Authorization": f"Bearer {key}"},
        )
        with urllib.request.urlopen(req, timeout=15, context=_SSL_CONTEXT) as resp:
            if resp.status != 200:
                return False, f"HTTP {resp.status}: {resp.read(200).decode()}"
            data = json.load(resp)
            models = data.get("data") or []
            return True, f"OK — list models returned {len(models)} model(s)"
    except Exception as e:
        return False, str(e)


def main() -> int:
    if _SSL_UNVERIFIED:
        print("(Using SSL fallback for macOS. For verified HTTPS: pip install certifi)\n")
    load_dotenv(find_env_path())

    fred_key = os.environ.get("FRED_API_KEY", "")
    eia_key = os.environ.get("EIA_API_KEY", "")
    openai_key = os.environ.get("OPENAI_API_KEY", "")

    print("--- API key presence ---")
    print("FRED_API_KEY:   ", "set" if fred_key else "NOT SET")
    print("EIA_API_KEY:    ", "set" if eia_key else "NOT SET")
    print("OPENAI_API_KEY: ", "set" if openai_key else "NOT SET")
    print()

    print("--- Test queries ---")
    r_fred, msg_fred = test_fred(fred_key)
    r_eia, msg_eia = test_eia(eia_key)
    r_openai, msg_openai = test_openai(openai_key)
    print("FRED:   ", msg_fred)
    print("EIA:    ", msg_eia)
    print("OpenAI: ", msg_openai)
    print()

    all_ok = r_fred and r_eia and r_openai
    print("--- Result ---")
    if all_ok:
        print("All API keys working.")
    else:
        print("One or more checks failed. Fix keys or .env (uncomment KEY=value lines).")

    # Sample query: show a few rows from FRED and EIA
    if all_ok and r_fred:
        url = (
            "https://api.stlouisfed.org/fred/series/observations"
            f"?series_id=GDPC1&api_key={urllib.parse.quote(fred_key, safe='')}&file_type=json&limit=5"
        )
        with urllib.request.urlopen(urllib.request.Request(url), timeout=10, context=_SSL_CONTEXT) as resp:
            data = json.load(resp)
        print("\n--- Sample FRED (GDPC1, real GDP) ---")
        for o in data.get("observations") or []:
            print("  ", o.get("date", ""), " ", o.get("value", ""))

    if all_ok and r_eia:
        url = (
            "https://api.eia.gov/v2/electricity/retail-sales/data"
            f"?api_key={urllib.parse.quote(eia_key, safe='')}&frequency=monthly&data[0]=revenue&length=3"
        )
        req = urllib.request.Request(url, headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=10, context=_SSL_CONTEXT) as resp:
            data = json.load(resp)
        recs = (data.get("response") or {}).get("data") or []
        print("\n--- Sample EIA (retail sales revenue) ---")
        for d in recs:
            print("  ", d.get("period", ""), " ", d.get("revenue", ""), " ", d.get("type", ""))

    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
