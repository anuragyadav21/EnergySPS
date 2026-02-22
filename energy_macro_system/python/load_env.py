# load_env.py — Load .env and SSL context for API calls

import os
import ssl
import sys
from pathlib import Path


def load_dotenv(path: str = ".env") -> None:
    """Load KEY=value from file into os.environ. Handles '# KEY=value' lines."""
    p = Path(path)
    if not p.exists():
        return
    for line in p.read_text(encoding="utf-8").splitlines():
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
    """Locate .env next to project root."""
    for name in (".env", "../.env", "energy_macro_system/.env"):
        if Path(name).exists():
            return name
    return ".env"


def make_ssl_context():
    """SSL context for HTTPS (certifi on macOS)."""
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


SSL_CONTEXT = make_ssl_context()
