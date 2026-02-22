# Shiny App — Deployment (DigitalOcean App Platform)

## Environment variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `OLLAMA_BASE_URL` | Yes (when using remote Ollama) | `http://localhost:11434` | Base URL of the Ollama server (no trailing slash). Set to your Ollama server, e.g. `http://<host>:11434`. |
| `OLLAMA_MODEL` | No | `gemma3:12b` | Ollama model name for LLM interpretation. |
| `PORT` | No | `8080` | Port the app listens on. DigitalOcean sets this automatically. |
| `OPENAI_API_KEY` | No | — | Optional; used as LLM fallback when Ollama is unavailable. |

## Docker build and run (local)

From the **repository root**:

```bash
docker build -f energy_macro_system/Dockerfile -t energy-macro-shiny energy_macro_system
docker run -p 8080:8080 -e OLLAMA_BASE_URL=http://<your-ollama-host>:11434 energy-macro-shiny
```

Or from the app directory:

```bash
cd energy_macro_system
docker build -t energy-macro-shiny .
docker run -p 8080:8080 -e OLLAMA_BASE_URL=http://<your-ollama-host>:11434 energy-macro-shiny
```

Open http://localhost:8080

## Setting OLLAMA_BASE_URL in DigitalOcean

1. In **DigitalOcean App Platform**, open your app.
2. Go to **Settings** (or the component that runs the Shiny app).
3. Under **App-Level Environment Variables** (or **Component → Environment Variables**), add:
   - **Key:** `OLLAMA_BASE_URL`
   - **Value:** `http://<your-ollama-server>:11434` (replace with your Ollama server host; no trailing slash).
4. Optionally add `OLLAMA_MODEL` (e.g. `gemma3:12b`) and `OPENAI_API_KEY` if you use OpenAI fallback.
5. Save and **restart** the app so the new variables are applied.

## Source directory (DigitalOcean)

Set the **Source Directory** to **`energy_macro_system`** so the Dockerfile in that folder is used and the build context is correct.
