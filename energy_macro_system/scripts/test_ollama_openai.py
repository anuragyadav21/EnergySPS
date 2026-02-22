#!/usr/bin/env python3
"""
test_ollama_openai.py — Verify Ollama (local) and OpenAI API are working.

- Ollama: no API key; uses OLLAMA_BASE_URL and OLLAMA_MODEL (e.g. gemma3:12b).
  Ensure Ollama is running (ollama serve) and model is available (e.g. ollama pull gemma3:12b).
- OpenAI: uses OPENAI_API_KEY from .env.

Run from project root:
  python energy_macro_system/scripts/test_ollama_openai.py
Or from energy_macro_system:
  python scripts/test_ollama_openai.py
"""

import json
import os
import ssl
import sys
import urllib.request
from pathlib import Path


def _make_ssl_context():
    try:
        import certifi
        return ssl.create_default_context(cafile=certifi.where())
    except ImportError:
        pass
    if sys.platform == "darwin":
        ctx = ssl.create_default_context()
        ctx.check_hostname = False
        ctx.verify_mode = ssl.CERT_NONE
        return ctx
    return ssl.create_default_context()


_SSL = _make_ssl_context()


def load_dotenv(path: str = ".env") -> None:
    p = Path(path)
    if not p.exists():
        return
    for line in p.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or "=" not in line:
            continue
        if line.startswith("#"):
            line = line.lstrip("#").strip()
        key, _, val = line.partition("=")
        key, val = key.strip(), val.strip()
        if key and val:
            os.environ[key] = val


def find_env_path() -> str:
    script_dir = Path(__file__).resolve().parent
    for candidate in (script_dir.parent / ".env", Path.cwd() / ".env", Path.cwd() / "energy_macro_system" / ".env"):
        if candidate.exists():
            return str(candidate)
    return ".env"


def test_ollama(base_url: str, model: str) -> tuple[bool, str]:
    """Test Ollama: list models or a minimal generate. No API key."""
    base_url = (base_url or "http://localhost:11434").rstrip("/")
    if not base_url.startswith("http"):
        base_url = "http://" + base_url

    # 1) Health / tags
    try:
        req = urllib.request.Request(f"{base_url}/api/tags", method="GET")
        with urllib.request.urlopen(req, timeout=5) as resp:
            if resp.status != 200:
                return False, f"Ollama /api/tags returned HTTP {resp.status}"
            data = json.load(resp)
            models = data.get("models") or []
            if not models and not model:
                return True, "OK — Ollama reachable; no models listed (run: ollama pull gemma3:12b)"
            if not models:
                return False, f"Ollama reachable but no models; requested model '{model}' not found"
    except urllib.error.URLError as e:
        return False, f"Ollama unreachable: {e.reason}. Is 'ollama serve' running?"
    except Exception as e:
        return False, f"Ollama error: {e}"

    # 2) Optional: minimal generate with requested or first available model
    use_model = model or (models[0].get("name") if models else "gemma3:12b")
    try:
        body = json.dumps({"model": use_model, "prompt": "Say OK", "stream": False}).encode("utf-8")
        req = urllib.request.Request(
            f"{base_url}/api/generate",
            data=body,
            headers={"Content-Type": "application/json"},
            method="POST",
        )
        with urllib.request.urlopen(req, timeout=30) as resp:
            if resp.status != 200:
                return False, f"Ollama /api/generate returned HTTP {resp.status}"
            out = json.load(resp)
            if "response" not in out:
                return False, "Ollama generate: no 'response' in body"
            return True, f"OK — Ollama respondered (model: {use_model})"
    except urllib.error.HTTPError as e:
        return False, f"Ollama generate HTTP {e.code}: {e.read(200).decode()}"
    except Exception as e:
        return False, f"Ollama generate: {e}"


def test_openai(key: str) -> tuple[bool, str]:
    """Test OpenAI: list models (no charge) or minimal chat completion."""
    if not key or key.strip() == "":
        return False, "OPENAI_API_KEY not set in .env"
    try:
        req = urllib.request.Request(
            "https://api.openai.com/v1/models",
            headers={"Authorization": f"Bearer {key.strip()}"},
        )
        with urllib.request.urlopen(req, timeout=15, context=_SSL) as resp:
            if resp.status != 200:
                return False, f"OpenAI API returned HTTP {resp.status}"
            data = json.load(resp)
            models = data.get("data") or []
            return True, f"OK — OpenAI API working ({len(models)} models listed)"
    except urllib.error.HTTPError as e:
        body = e.read(500).decode()
        if e.code == 401:
            return False, "OpenAI API key invalid or expired (401). Check OPENAI_API_KEY in .env"
        return False, f"OpenAI HTTP {e.code}: {body}"
    except Exception as e:
        return False, f"OpenAI error: {e}"


def main() -> int:
    load_dotenv(find_env_path())

    base_url = os.environ.get("OLLAMA_BASE_URL", "http://localhost:11434")
    model = os.environ.get("OLLAMA_MODEL", "")
    openai_key = os.environ.get("OPENAI_API_KEY", "")

    print("--- Config (from .env) ---")
    print("OLLAMA_BASE_URL: ", base_url or "(not set)")
    print("OLLAMA_MODEL:    ", model or "(default/first available)")
    print("OPENAI_API_KEY:  ", "set" if openai_key else "NOT SET")
    print()

    print("--- Tests ---")
    ok_ollama, msg_ollama = test_ollama(base_url, model)
    ok_openai, msg_openai = test_openai(openai_key)
    print("Ollama: ", msg_ollama)
    print("OpenAI: ", msg_openai)
    print()

    print("--- Result ---")
    if ok_ollama and ok_openai:
        print("Both Ollama and OpenAI are working.")
    elif ok_ollama:
        print("Ollama is working. OpenAI check failed — fix OPENAI_API_KEY in .env.")
    elif ok_openai:
        print("OpenAI is working. Ollama check failed — ensure 'ollama serve' is running and a model is pulled.")
    else:
        print("Both checks failed. See messages above.")

    return 0 if (ok_ollama and ok_openai) else 1


if __name__ == "__main__":
    sys.exit(main())
