# 08_llm_layer.R
# Clean, concise LLM narrative layer for Energy–Macro dashboard

library(httr)
library(jsonlite)

OLLAMA_BASE_URL_DEFAULT <- "http://localhost:11434"
OLLAMA_MODEL_DEFAULT    <- "gemma3:12b"

# -------------------------------
# Generic Ollama Call
# -------------------------------
ollama_generate <- function(prompt,
                            base_url = Sys.getenv("OLLAMA_BASE_URL", OLLAMA_BASE_URL_DEFAULT),
                            model    = Sys.getenv("OLLAMA_MODEL", OLLAMA_MODEL_DEFAULT),
                            timeout  = 90) {

  # If base URL or model is missing, fail fast
  if (!nzchar(base_url)) base_url <- OLLAMA_BASE_URL_DEFAULT
  if (!nzchar(model))    model    <- OLLAMA_MODEL_DEFAULT
  if (!nzchar(base_url) || !nzchar(model)) {
    message("Ollama base URL or model not set; skipping Ollama call.")
    return(NULL)
  }

  base_url <- sub("/$", "", base_url)
  url <- paste0(base_url, "/api/generate")

  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(temperature = 0.2)  # more stable, concise output
  )

  res <- tryCatch({
    httr::POST(url, body = body, encode = "json", httr::timeout(timeout))
  }, error = function(e) {
    message("Ollama request failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(res) && httr::status_code(res) == 200) {
    out <- jsonlite::fromJSON(httr::content(res, as = "text"), simplifyDataFrame = FALSE)
    return(out$response)
  }

  message("Ollama did not return a successful response (status ",
          if (!is.null(res)) httr::status_code(res) else "NULL", ").")
  return(NULL)
}

# -------------------------------
# Generic OpenAI Call (fallback)
# -------------------------------
openai_chat <- function(prompt,
                        api_key = Sys.getenv("OPENAI_API_KEY"),
                        model = "gpt-4o-mini",
                        timeout = 30) {

  if (!nzchar(api_key)) return(NULL)

  body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt))
  )

  res <- tryCatch({
    httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        Authorization = paste0("Bearer ", api_key),
        "Content-Type" = "application/json"
      ),
      body = body,
      encode = "json",
      httr::timeout(timeout)
    )
  }, error = function(e) return(NULL))

  if (!is.null(res) && httr::status_code(res) == 200) {
    out <- jsonlite::fromJSON(httr::content(res, as = "text"), simplifyDataFrame = FALSE)
    return(out$choices[[1]]$message$content)
  }

  return(NULL)
}

# -------------------------------
# Clean Narrative Generator
# -------------------------------
llm_narrative <- function(metrics_list,
                          use_ollama = TRUE,
                          use_openai = TRUE) {

  data_block <- paste(capture.output(print(metrics_list)), collapse = "\n")

  prompt <- paste0(
    "You are a macroeconomic analyst.\n\n",
    "Explain the following model results in plain English for a senior policymaker.\n\n",
    data_block, "\n\n",
    "Requirements:\n",
    "- One short paragraph (4-6 sentences).\n",
    "- Clear and non-technical.\n",
    "- Highlight the main risk.\n",
    "- Mention whether the system appears stable.\n",
    "- Do not invent numbers.\n"
  )

  narrative <- NULL

  # Try Ollama first
  if (use_ollama) {
    narrative <- ollama_generate(prompt)
    if (!is.null(narrative) && nzchar(narrative)) {
      message("LLM narrative from Ollama.")
    }
  }

  # Fallback to OpenAI
  if (is.null(narrative) && use_openai) {
    narrative <- openai_chat(prompt)
    if (!is.null(narrative) && nzchar(narrative)) {
      message("LLM narrative from OpenAI.")
    }
  }

  if (is.null(narrative)) {
    message("No LLM available. Returning metrics only.")
    return(metrics_list)
  }

  metrics_list$llm_narrative <- narrative
  return(metrics_list)
}

# -------------------------------
# VAR Stability Interpretation
# -------------------------------

#' Interpret VAR stability metrics via LLM (eigenvalues, spectral radius, shock amplification).
#' @param stability_metrics List from compute_stability_metrics(A) or similar:
#'   eigenvalues, eigenvalue_modulus (or derived from eigenvalues), spectral_radius,
#'   is_stable, stability_condition, shock_amplification_factor, norm_Frobenius.
#' @param use_ollama Use local Ollama if TRUE
#' @param use_openai Use OpenAI if TRUE
#' @param ollama_model Optional Ollama model name; if NULL, uses OLLAMA_MODEL env or default.
#' @return List with fields narrative (character) and success (logical).
llm_interpret_var_stability <- function(stability_metrics,
                                        use_ollama = TRUE,
                                        use_openai = TRUE,
                                        ollama_model = NULL) {
  if (is.null(stability_metrics)) {
    return(list(narrative = NULL, success = FALSE))
  }
  sm <- stability_metrics

  # Ensure derived fields exist
  if (is.null(sm$eigenvalue_modulus) && !is.null(sm$eigenvalues)) {
    sm$eigenvalue_modulus <- Mod(sm$eigenvalues)
  }
  if (is.null(sm$shock_amplification_factor)) sm$shock_amplification_factor <- NA_real_
  if (is.null(sm$norm_Frobenius)) sm$norm_Frobenius <- NA_real_

  mod_str <- if (!is.null(sm$eigenvalue_modulus)) {
    paste(round(sm$eigenvalue_modulus, 4), collapse = ", ")
  } else {
    "NA"
  }
  rho    <- sm$spectral_radius
  stable <- isTRUE(sm$is_stable)
  amp    <- sm$shock_amplification_factor
  nf     <- sm$norm_Frobenius

  data_block <- paste0(
    "VAR stability metrics:\n",
    "spectral_radius = ", round(rho, 4), "; is_stable = ", stable,
    "; stability_condition = ",
    if (!is.null(sm$stability_condition)) sm$stability_condition else if (stable) "stable" else "UNSTABLE",
    ";\neigenvalue moduli = ", mod_str,
    "; shock_amplification_factor = ", if (is.na(amp)) "NA" else round(amp, 4),
    if (!is.na(nf)) paste0("; norm_Frobenius = ", round(nf, 4)) else "",
    ".\nConsider eigenvalue moduli > 0.95 as near-unit roots."
  )

  instruction <- paste0(
    "You are an energy–macro analyst. Interpret the following VAR stability metrics in exactly three short sections.\n\n",
    data_block, "\n\n",
    "In exactly three short sections (2–4 sentences each), interpret for a policymaker:\n",
    "1. Eigenvalues: Is the system stable? Is there strong persistence? Are there near-unit roots?\n",
    "2. Spectral radius: Comment on long-run stability and mean reversion strength.\n",
    "3. Shock amplification: Comment on short-run vulnerability and transmission intensity.\n\n",
    "Be concise and professional."
  )

  narrative <- NULL

  if (use_ollama) {
    base_url <- Sys.getenv("OLLAMA_BASE_URL", OLLAMA_BASE_URL_DEFAULT)
    model    <- if (is.null(ollama_model) || !nzchar(ollama_model)) {
      Sys.getenv("OLLAMA_MODEL", OLLAMA_MODEL_DEFAULT)
    } else {
      ollama_model
    }
    if (nzchar(base_url) && nzchar(model)) {
      narrative <- ollama_generate(instruction, base_url = base_url, model = model)
      if (!is.null(narrative) && nzchar(narrative)) {
        message("LLM VAR stability narrative from Ollama (", model, ")")
      }
    }
  }

  if (is.null(narrative) && use_openai) {
    narrative <- openai_chat(instruction)
    if (!is.null(narrative) && nzchar(narrative)) {
      message("LLM VAR stability narrative from OpenAI")
    }
  }

  list(narrative = narrative, success = !is.null(narrative) && nzchar(narrative))
}

# -------------------------------
# IRF Interpretation
# -------------------------------

#' Interpret impulse response (IRF) summary via LLM for the dashboard.
#' @param irf_summary_list List with: shock_var (character), horizon (integer), shock_sd (numeric),
#'   peak, time_to_peak, half_life, cumulative (numeric, for shocked variable), and variable_summary
#'   (data.frame with columns: variable, impact_0, peak_abs, time_to_peak, end_response) or a
#'   character text_summary for a pre-formatted block.
#' @param use_ollama Use Ollama if TRUE
#' @param use_openai Use OpenAI if TRUE
#' @param ollama_model Optional Ollama model name; if NULL, uses OLLAMA_MODEL env or default.
#' @return List with narrative (character) and success (logical).
llm_interpret_irf <- function(irf_summary_list,
                              use_ollama = TRUE,
                              use_openai = TRUE,
                              ollama_model = NULL) {
  if (is.null(irf_summary_list)) {
    return(list(narrative = NULL, success = FALSE))
  }
  s <- irf_summary_list

  head_str <- paste0(
    "Impulse response setup: shock to variable \"", s$shock_var, "\"; ",
    "horizon = ", s$horizon, " months; ",
    "shock size = ", s$shock_sd, " SD (structural).\n",
    "Summary for the shocked variable: ",
    "peak (max absolute response) = ", if (is.null(s$peak) || is.na(s$peak)) "NA" else round(s$peak, 4),
    "; time to peak = ", if (is.null(s$time_to_peak) || is.na(s$time_to_peak)) "NA" else paste0(s$time_to_peak, " mo"),
    "; half-life = ", if (is.null(s$half_life) || is.na(s$half_life)) "NA" else paste0(s$half_life, " mo"),
    "; cumulative impact = ", if (is.null(s$cumulative) || is.na(s$cumulative)) "NA" else round(s$cumulative, 4), ".\n"
  )

  if (!is.null(s$text_summary) && is.character(s$text_summary) && nzchar(s$text_summary)) {
    data_block <- paste0(head_str, s$text_summary)
  } else if (is.data.frame(s$variable_summary) && nrow(s$variable_summary) > 0) {
    vs <- s$variable_summary
    lines <- paste0(
      "  ", vs$variable, ": impact at h=0 = ", round(vs$impact_0, 4),
      "; peak |response| = ", round(vs$peak_abs, 4), " at h = ", vs$time_to_peak,
      "; response at end horizon = ", round(vs$end_response, 4)
    )
    data_block <- paste0(head_str, "Per-variable responses:\n", paste(lines, collapse = "\n"))
  } else {
    data_block <- head_str
  }

  instruction <- paste0(
    "Interpret the following impulse response (IRF) summary for a normal person.\n\n",
    data_block, "\n\n",
    "Write a short narrative (about one short paragraph, 3-5 sentences) that:\n",
    "0. Tell the reader what he is looking at and why is this important in plain english.\n",
    "1. States what shock is being applied and over what horizon. Explain what is shock does in plain english.\n",
    "2. Describes which variables respond most (on impact and over time), the sign of key effects (e.g. oil shock raises or lowers inflation, GDP), and how persistent the effects are (time to peak, half-life).\n",
    "3. Gives a brief takeaway for policy or risk (e.g. transmission channels, which sectors or variables are most exposed).\n",
    "Be concise and professional; use the numbers above to ground your interpretation."
  )

  narrative <- NULL

  if (use_ollama) {
    base_url <- Sys.getenv("OLLAMA_BASE_URL", OLLAMA_BASE_URL_DEFAULT)
    model    <- if (is.null(ollama_model) || !nzchar(ollama_model)) {
      Sys.getenv("OLLAMA_MODEL", OLLAMA_MODEL_DEFAULT)
    } else {
      ollama_model
    }
    if (nzchar(base_url) && nzchar(model)) {
      narrative <- ollama_generate(instruction, base_url = base_url, model = model)
      if (!is.null(narrative) && nzchar(narrative)) {
        message("LLM IRF narrative from Ollama (", model, ")")
      }
    }
  }

  if (is.null(narrative) && use_openai) {
    narrative <- openai_chat(instruction)
    if (!is.null(narrative) && nzchar(narrative)) {
      message("LLM IRF narrative from OpenAI")
    }
  }

  list(narrative = narrative, success = !is.null(narrative) && nzchar(narrative))
}

# -------------------------------
# Overview Statistics Interpretation
# -------------------------------

#' Interpret overview statistics (Mean, SD, Min, Max) for system variables via LLM.
#' @param overview_stats Data frame with columns: Variable, Mean, SD, Min, Max (or compatible structure)
#' @param use_ollama Use Ollama if TRUE
#' @param use_openai Use OpenAI if TRUE
#' @param ollama_model Optional Ollama model name (e.g. gemma3:12b, deepseek-r1:8b); if NULL, uses OLLAMA_MODEL env or default
#' @return List with narrative (character) and success (logical)
llm_interpret_overview <- function(overview_stats,
                                   use_ollama = TRUE,
                                   use_openai = TRUE,
                                   ollama_model = NULL) {
  if (is.null(overview_stats) || !is.data.frame(overview_stats) || nrow(overview_stats) == 0) {
    return(list(narrative = NULL, success = FALSE))
  }

  var_lines <- paste0(
    "  ", overview_stats$Variable, ": Mean = ", overview_stats$Mean,
    ", SD = ", overview_stats$SD,
    ", Min = ", overview_stats$Min,
    ", Max = ", overview_stats$Max
  )

  data_block <- paste0(
    "System series summary statistics (from VAR model inputs):\n",
    paste(var_lines, collapse = "\n"),
    "\n\n",
    "These variables represent: oil returns, inflation change, rate change, industrial return, GDP growth, demand change, and renewable share."
  )

  instruction <- paste0(
    "You are an energy–macro analyst. Interpret the following system series summary statistics for a policymaker.\n\n",
    data_block, "\n\n",
    "Write one short paragraph (5–7 sentences) that:\n",
    "- Identifies the most economically dangerous variable (not just most volatile).\n",
    "- Explains what combination of variables could create systemic risk.\n",
    "- Notes any asymmetry (large upside vs downside range).\n",
    "- States one thing that would genuinely worry a policymaker.\n",
    "- Avoid generic phrases like 'suggests volatility' or 'indicates uncertainty'.\n",
    "Be direct and insightful."
  )

  narrative <- NULL

  if (use_ollama) {
    base_url <- Sys.getenv("OLLAMA_BASE_URL", OLLAMA_BASE_URL_DEFAULT)
    model    <- if (is.null(ollama_model) || !nzchar(ollama_model)) {
      Sys.getenv("OLLAMA_MODEL", OLLAMA_MODEL_DEFAULT)
    } else {
      ollama_model
    }
    if (nzchar(base_url) && nzchar(model)) {
      narrative <- ollama_generate(instruction, base_url = base_url, model = model)
      if (!is.null(narrative) && nzchar(narrative)) {
        message("LLM overview narrative from Ollama (", model, ")")
      }
    }
  }

  if (is.null(narrative) && use_openai) {
    narrative <- openai_chat(instruction)
    if (!is.null(narrative) && nzchar(narrative)) {
      message("LLM overview narrative from OpenAI")
    }
  }

  list(narrative = narrative, success = !is.null(narrative) && nzchar(narrative))
}

