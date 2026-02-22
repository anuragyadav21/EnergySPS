# Energy–Macro Shock Propagation Simulator — Shiny Dashboard
# Run: setwd("energy_macro_system"); shiny::runApp(".")
# If data/system_timeseries.csv exists: loads cached data. Else: runs R pipeline once.

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(DT)
# MASS not loaded globally to avoid masking dplyr::select; use MASS::mvrnorm() where needed.

# IRF engine (impulse response: IRF(h) = A^h · e_j)
source(file.path("R", "irf_engine.R"), local = TRUE)
source(file.path("R", "07_stability_metrics.R"), local = TRUE)
source(file.path("R", "08_llm_layer.R"), local = TRUE)
source(file.path("R", "05_parameter_estimation.R"), local = TRUE)

# Data directory (relative to app)
DATA_DIR <- "data"

# ---- Data loading ----
load_pipeline_data <- function() {
  out <- list(
    fred = NULL, eia = NULL, worldbank = NULL,
    system_ts = NULL, A_matrix = NULL, Sigma = NULL,
    error = NULL
  )
  tryCatch({
    if (file.exists(file.path(DATA_DIR, "fred_raw.csv"))) {
      out$fred <- read_csv(file.path(DATA_DIR, "fred_raw.csv"), show_col_types = FALSE)
      out$fred$date <- as.Date(out$fred$date)
    }
    if (file.exists(file.path(DATA_DIR, "eia_raw.csv"))) {
      out$eia <- read_csv(file.path(DATA_DIR, "eia_raw.csv"), show_col_types = FALSE)
      if ("period" %in% names(out$eia)) out$eia$date <- as.Date(paste0(out$eia$period, "-01"), optional = TRUE)
    }
    if (file.exists(file.path(DATA_DIR, "worldbank_structural.csv"))) {
      out$worldbank <- read_csv(file.path(DATA_DIR, "worldbank_structural.csv"), show_col_types = FALSE)
    }
    if (file.exists(file.path(DATA_DIR, "system_timeseries.csv"))) {
      out$system_ts <- read_csv(file.path(DATA_DIR, "system_timeseries.csv"), show_col_types = FALSE)
      out$system_ts$date <- as.Date(out$system_ts$date)
    }
    if (file.exists(file.path(DATA_DIR, "A_matrix.csv"))) {
      out$A_matrix <- as.matrix(read.csv(file.path(DATA_DIR, "A_matrix.csv"), row.names = 1))
      storage.mode(out$A_matrix) <- "numeric"
    }
    if (file.exists(file.path(DATA_DIR, "covariance_matrix.csv"))) {
      out$Sigma <- as.matrix(read.csv(file.path(DATA_DIR, "covariance_matrix.csv"), row.names = 1))
      storage.mode(out$Sigma) <- "numeric"
    }
  }, error = function(e) out$error <- conditionMessage(e))
  out
}

# ---- Stability from A ----
stability_from_A <- function(A) {
  if (is.null(A) || !is.matrix(A)) return(list(spectral_radius = NA, is_stable = NA, eigs = NULL))
  eigs <- eigen(A, only.values = TRUE)$values
  rho <- max(Mod(eigs))
  list(spectral_radius = rho, is_stable = rho < 1, eigenvalues = eigs)
}

# ---- System series Y-axis labels (what the numbers represent) ----
SYSTEM_SERIES_YLAB <- c(
  oil_return = "Log return (oil price)",
  inflation_change = "Inflation % change (CPI)",
  rate_change = "Rate change (pp, Fed funds)",
  industrial_return = "Log return (industrial production)",
  gdp_growth = "GDP growth (%)",
  demand_change = "Demand % change (electricity)",
  renewable_share = "Renewable share (0–1)"
)

# ---- Simple MC in R (X(t+1) = A X(t) + eps) ----
run_mc <- function(A, Sigma, horizon = 24, n_sim = 500, seed = 42) {
  if (is.null(A) || is.null(Sigma)) return(NULL)
  set.seed(seed)
  K <- nrow(A)
  eps <- MASS::mvrnorm(n_sim * horizon, mu = rep(0, K), Sigma = Sigma)
  dim(eps) <- c(horizon, n_sim, K)
  paths <- array(0, dim = c(n_sim, horizon, K))
  x <- matrix(0, n_sim, K)
  for (t in seq_len(horizon)) {
    x <- x %*% t(A) + eps[t, , ]
    paths[, t, ] <- x
  }
  gdp_idx <- min(5, K)
  inf_idx <- min(2, K)
  list(
    paths = paths,
    recession_prob = mean(paths[, horizon, gdp_idx] < 0),
    inflation_stress_prob = mean(paths[, , inf_idx] > 4),
    gdp_mean_path = colMeans(paths[, , gdp_idx])
  )
}

# ---- UI ----
# Title bar with collapsible project summary (hidden until user toggles).
ui <- page_sidebar(
  title = tagList(
    div(class = "dashboard-title-row",
        span("Energy\u2013Macro Simulator", class = "dashboard-title-text"),
        tags$details(class = "title-summary-details", open = FALSE,
          tags$summary("What is this?", class = "title-summary-toggle"),
          tags$div(class = "title-summary-content",
            tags$p("This dashboard is an Energy\u2013Macro Shock Propagation Simulator. It links energy and electricity data with macro variables (oil, inflation, rates, GDP, demand), estimates a VAR model, and checks stability, runs Monte Carlo for recession and inflation risk, and impulse responses. It gives a single view of system stability, key risks, and how shocks propagate\u2014so analysts and policymakers can see if the system is stable, what could go wrong, and how energy and macro factors interact.")
          )
        )
    )
  ),
  window_title = "Energy-Macro Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$link(href = "custom.css", rel = "stylesheet")),
  # System Status Summary at top so executives see key metrics immediately (decision-focused).
  div(class = "executive-status-section",
      h4("System Status Summary", class = "status-summary-title"),
      uiOutput("system_status_cards_ui"),
      hr(class = "status-hr")
  ),
  sidebar = sidebar(
    width = 320,
    actionButton("refresh", "Refresh data", icon = icon("refresh"), class = "btn-primary"),
    hr(),
    selectInput("ollama_model", "Ollama model (LLM)",
                choices = c("gemma3:12b", "deepseek-r1:8b", "gemma3:4b"),
                selected = "gemma3:12b"),
    downloadButton("download_full_llm_report", "Save full LLM report", class = "btn-outline-secondary"),
    hr(),
    p("Data: ", strong(DATA_DIR), ". Run ", code("python run_all.py"), " to update."),
    hr(),
    uiOutput("startup_message_ui"),
    uiOutput("data_status_ui")
  ),
  tabsetPanel(
        id = "tabs",
        tabPanel("Overview", value = "overview",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("FRED and system series anchor the model. Use the table and plots to spot gaps or outliers.")
                 ),
                 # One chart at a time to reduce density and improve scanability (executive-friendly).
                 selectInput("overview_chart", "Show chart", choices = c("FRED levels" = "fred", "System series" = "system"), selected = "fred", width = "220px"),
                 conditionalPanel(condition = "input.overview_chart == 'fred'",
                   plotOutput("overview_fred_plot", height = "380px"),
                   uiOutput("overview_fred_links_ui")),
                 conditionalPanel(condition = "input.overview_chart == 'system'",
                   plotOutput("overview_system_plot", height = "380px")),
                 h5("System series summary", class = "section-heading"),
                 div(DTOutput("overview_stats_table"), style = "font-size: 0.85em; max-height: 280px;"),
                 tags$p(tags$small("Mean = long-run avg; SD = volatility."), class = "text-muted"),
                 hr(),
                 actionButton("get_llm_overview_btn", "Get LLM interpretation", class = "btn-primary"),
                 downloadButton("download_overview_llm", "Save report", class = "btn-outline-secondary"),
                 uiOutput("overview_llm_ui")),
        tabPanel("FRED", value = "fred",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("U.S. macro series from FRED. Select a series to inspect levels.")
                 ),
                 selectInput("fred_series", "Series", choices = c("DCOILWTICO", "CPIAUCSL", "FEDFUNDS", "INDPRO", "UNRATE", "GDPC1"), selected = "DCOILWTICO"),
                 uiOutput("fred_link_ui"),
                 plotOutput("fred_plot", height = "400px"),
                 uiOutput("fred_table_links_ui"),
                 DTOutput("fred_table")),
        tabPanel("EIA", value = "eia",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("Electricity sales and renewable share. Demand and generation mix.")
                 ),
                 plotOutput("eia_sales_plot", height = "340px"),
                 plotOutput("eia_renewable_plot", height = "340px"),
                 DTOutput("eia_table")),
        tabPanel("World Bank", value = "worldbank",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("Structural indicators. Context for the VAR; model uses system series.")
                 ),
                 plotOutput("wb_plot", height = "400px"),
                 DTOutput("wb_table")),
        tabPanel("System series", value = "system",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("Model variables (returns and growth rates). Inputs to VAR, IRF, and MC.")
                 ),
                 selectInput("system_series", "Series", choices = c("oil_return", "inflation_change", "rate_change", "industrial_return", "gdp_growth", "demand_change", "renewable_share"), selected = "oil_return"),
                 plotOutput("system_plot", height = "400px"),
                 DTOutput("system_table")),
        tabPanel("Model (A & \u03A3)", value = "model",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("A = one-step transition; \u03A3 = innovation covariance. Both from VAR.")
                 ),
                 h4("Transition matrix A", class = "section-heading"),
                 DTOutput("A_table"),
                 h4("Residual covariance \u03A3", class = "section-heading"),
                 DTOutput("Sigma_table")),
        tabPanel("Stability", value = "stability",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("\u03C1(A) < 1 means stable; eigenvalues inside unit circle.")
                 ),
                 conditionalPanel(
                   condition = "input.tabs === 'stability'",
                   uiOutput("stability_value_boxes_ui"),
                   verbatimTextOutput("stability_text"),
                   plotOutput("stability_eigs_plot", height = "400px"),
                   hr(),
                   actionButton("get_llm_stability_btn", "Get LLM interpretation", class = "btn-primary"),
                   downloadButton("download_stability_llm", "Save report", class = "btn-outline-secondary"),
                   uiOutput("stability_llm_ui")
                 )),
        tabPanel("Monte Carlo", value = "mc",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("Paths from A and \u03A3. Probabilities = share of paths meeting the condition.")
                 ),
                 conditionalPanel(
                   condition = "input.tabs === 'mc'",
                   fluidRow(
                     column(3, numericInput("mc_horizon", "Horizon", value = 24, min = 6, max = 60)),
                     column(3, numericInput("mc_nsim", "Simulations", value = 500, min = 100, max = 2000)),
                     column(3, actionButton("run_mc_btn", "Run MC", class = "btn-primary"))
                   ),
                   uiOutput("mc_value_boxes_ui"),
                   verbatimTextOutput("mc_summary"),
                   plotOutput("mc_gdp_path", height = "380px")
                 )),
        tabPanel("Impulse Response", value = "irf",
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("About"), tags$p("Response of each variable to a one-time shock. Decay = stability.")
                 ),
                 tags$details(class = "dashboard-details", open = FALSE,
                   tags$summary("Interpretation"), tags$p("Structural shock from Cholesky of \u03A3. Bands = 90% bootstrap.")
                 ),
                 conditionalPanel(
                   condition = "input.tabs === 'irf'",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("shock_var", "Select Shock Variable:", choices = character(0)),
                       numericInput("shock_sd", "Shock size (SD units)", value = 1, min = 0, step = 0.1),
                       sliderInput("irf_horizon", "Horizon (months):", min = 6, max = 60, value = 24),
                       checkboxInput("irf_show_bands", "Show confidence bands", value = FALSE),
                       actionButton("irf_bootstrap_btn", "Compute bootstrap bands (500)", class = "btn-secondary")
                     ),
                     mainPanel(
                       fluidRow(
                         column(3, div(class = "value-card", h5("Peak response", style = "margin-top: 0;"), div(class = "value", textOutput("irf_peak_response")))),
                         column(3, div(class = "value-card", h5("Time to peak", style = "margin-top: 0;"), div(class = "value", textOutput("irf_time_to_peak")))),
                         column(3, div(class = "value-card", h5("Half-life", style = "margin-top: 0;"), div(class = "value", textOutput("irf_half_life")))),
                         column(3, div(class = "value-card", h5("Cumulative impact", style = "margin-top: 0;"), div(class = "value", textOutput("irf_cumulative"))))
                       ),
                       plotOutput("irf_plot", height = "400px"),
                       hr(),
                       actionButton("get_llm_irf_btn", "Get LLM interpretation", class = "btn-primary"),
                       downloadButton("download_irf_llm", "Save report", class = "btn-outline-secondary"),
                       uiOutput("irf_llm_ui")
                     )
                   )
                 )
  )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  startup_message <- reactiveVal("")
  A_matrix <- reactiveVal(NULL)
  Sigma_matrix <- reactiveVal(NULL)

  # Helper function to load or estimate A and Sigma
  load_or_estimate_var <- function() {
    A_path <- file.path(DATA_DIR, "A_matrix.csv")
    Sigma_path <- file.path(DATA_DIR, "covariance_matrix.csv")
    system_ts_path <- file.path(DATA_DIR, "system_timeseries.csv")
    
    # If CSV files exist, load them
    if (file.exists(A_path) && file.exists(Sigma_path)) {
      A <- as.matrix(read.csv(A_path, row.names = 1))
      storage.mode(A) <- "numeric"
      Sigma <- as.matrix(read.csv(Sigma_path, row.names = 1))
      storage.mode(Sigma) <- "numeric"
      return(list(A = A, Sigma = Sigma, source = "loaded"))
    }
    
    # If system_timeseries exists but A/Sigma don't, estimate VAR
    if (file.exists(system_ts_path)) {
      tryCatch({
        est <- run_parameter_estimation(DATA_DIR)
        return(list(A = est$A, Sigma = est$Sigma, source = "estimated"))
      }, error = function(e) {
        return(list(A = NULL, Sigma = NULL, source = paste0("estimation failed: ", conditionMessage(e))))
      })
    }
    
    # No data available
    return(list(A = NULL, Sigma = NULL, source = "no_data"))
  }

  # One-time init: load data and VAR matrices
  if (!file.exists(file.path(DATA_DIR, "system_timeseries.csv"))) {
    tryCatch({
      source(file.path("R", "run_all.R"), local = FALSE)
      startup_message("Data freshly ingested")
    }, error = function(e) {
      startup_message(paste0("Ingestion failed: ", conditionMessage(e)))
    })
  } else {
    startup_message("Loaded cached data")
  }
  data(load_pipeline_data())
  
  # Load or estimate A and Sigma once at startup
  var_result <- load_or_estimate_var()
  if (!is.null(var_result$A) && !is.null(var_result$Sigma)) {
    A_matrix(var_result$A)
    Sigma_matrix(var_result$Sigma)
    if (var_result$source == "estimated") {
      startup_message(paste0(startup_message(), " (VAR estimated)"))
    }
  }

  observeEvent(input$refresh, {
    data(load_pipeline_data())
    # Reload or re-estimate A and Sigma on refresh
    var_result <- load_or_estimate_var()
    if (!is.null(var_result$A) && !is.null(var_result$Sigma)) {
      A_matrix(var_result$A)
      Sigma_matrix(var_result$Sigma)
    }
  })

  output$startup_message_ui <- renderUI({
    msg <- startup_message()
    if (nzchar(msg)) tags$p(tags$strong(msg), style = "color: #1976d2;") else NULL
  })

  output$data_status_ui <- renderUI({
    d <- data()
    if (is.null(d)) return(tags$p("Click Refresh to load data."))
    if (!is.null(d$error)) return(tags$p(tags$span(d$error, style = "color: red;")))
    n_fred <- if (is.null(d$fred)) 0 else nrow(d$fred)
    n_eia <- if (is.null(d$eia)) 0 else nrow(d$eia)
    n_sys <- if (is.null(d$system_ts)) 0 else nrow(d$system_ts)
    tags$p(
      "FRED: ", n_fred, " rows | ",
      "EIA: ", n_eia, " | ",
      "System: ", n_sys, " rows"
    )
  })

  # Overview: FRED links
  output$overview_fred_links_ui <- renderUI({
    d <- data()$fred
    if (is.null(d) || nrow(d) == 0) return(NULL)
    fred_series_codes <- c("DCOILWTICO", "CPIAUCSL", "FEDFUNDS", "INDPRO", "UNRATE", "GDPC1")
    present_codes <- fred_series_codes[fred_series_codes %in% names(d)]
    if (length(present_codes) == 0) return(NULL)
    links <- lapply(present_codes, function(code) {
      tags$a(href = paste0("https://fred.stlouisfed.org/series/", code), 
             target = "_blank", code, 
             style = "color: #1976d2; text-decoration: underline; margin-right: 8px; font-size: 0.85em;")
    })
    tags$p(
      tags$strong("FRED links: "),
      tags$span(links),
      style = "margin-top: 5px; margin-bottom: 5px; font-size: 0.85em;"
    )
  })

  # Overview
  output$overview_fred_plot <- renderPlot({
    d <- data()$fred
    if (is.null(d) || nrow(d) == 0) return(plot(0, type = "n", main = "No FRED data"))
    d <- d %>% dplyr::filter(date >= as.Date("2000-01-01"))
    if (nrow(d) == 0) return(plot(0, type = "n", main = "No FRED data from 2000 onwards"))
    dlong <- d %>% pivot_longer(-date, names_to = "series", values_to = "value") %>% dplyr::filter(!is.na(value))
    if (nrow(dlong) == 0) return(plot(0, type = "n", main = "No FRED data"))
    ggplot(dlong, aes(date, value, color = series)) + geom_line(na.rm = TRUE, linewidth = 0.6) +
      theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(title = "FRED series (levels, from 2000)", x = NULL, y = NULL) + facet_wrap(~series, scales = "free_y", ncol = 2)
  })

  output$overview_system_plot <- renderPlot({
    d <- data()$system_ts
    if (is.null(d) || nrow(d) == 0) return(plot(0, type = "n", main = "No system series"))
    dlong <- d %>% pivot_longer(-date, names_to = "series", values_to = "value") %>% dplyr::filter(!is.na(value))
    if (nrow(dlong) == 0) return(plot(0, type = "n", main = "No system series"))
    dlong <- dlong %>% mutate(
      series_label = ifelse(series %in% names(SYSTEM_SERIES_YLAB),
                            paste0(series, "\n(", as.character(SYSTEM_SERIES_YLAB[series]), ")"),
                            series)
    )
    ggplot(dlong, aes(date, value, color = series)) + geom_line(na.rm = TRUE, linewidth = 0.5) +
      theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(title = "System series (returns/changes)", x = NULL, y = "Value (see facet for units)") +
      facet_wrap(~series_label, scales = "free_y", ncol = 2)
  })

  output$overview_stats_table <- renderDT({
    d <- data()
    if (is.null(d$system_ts) || nrow(d$system_ts) == 0) return(NULL)
    ds <- d$system_ts
    ds <- ds[, setdiff(names(ds), "date"), drop = FALSE]
    summ <- data.frame(
      Variable = names(ds),
      Mean = round(vapply(ds, function(x) mean(x, na.rm = TRUE), numeric(1)), 4),
      SD = round(vapply(ds, function(x) sd(x, na.rm = TRUE), numeric(1)), 4),
      Min = round(vapply(ds, function(x) min(x, na.rm = TRUE), numeric(1)), 4),
      Max = round(vapply(ds, function(x) max(x, na.rm = TRUE), numeric(1)), 4)
    )
    datatable(summ, options = list(dom = "t", paging = FALSE, scrollX = TRUE, scrollY = "260px", deferRender = TRUE), rownames = FALSE)
  })

  # LLM interpretation for Overview tab (on button click)
  overview_llm_display <- reactiveVal("")
  observeEvent(input$get_llm_overview_btn, {
    d <- data()
    if (is.null(d$system_ts) || nrow(d$system_ts) == 0) {
      overview_llm_display("Load system series data first.")
      return()
    }
    withProgress(message = "LLM interpreting overview statistics...", value = 0, {
      incProgress(0.1, detail = "Preparing data...")
      overview_llm_display("Interpreting…")
      ds <- d$system_ts
      ds <- ds[, setdiff(names(ds), "date"), drop = FALSE]
      overview_stats <- data.frame(
        Variable = names(ds),
        Mean = round(vapply(ds, function(x) mean(x, na.rm = TRUE), numeric(1)), 4),
        SD = round(vapply(ds, function(x) sd(x, na.rm = TRUE), numeric(1)), 4),
        Min = round(vapply(ds, function(x) min(x, na.rm = TRUE), numeric(1)), 4),
        Max = round(vapply(ds, function(x) max(x, na.rm = TRUE), numeric(1)), 4)
      )
      incProgress(0.2, detail = "Calling LLM...")
      llm_out <- llm_interpret_overview(overview_stats, use_ollama = TRUE, use_openai = TRUE, ollama_model = input$ollama_model)
      incProgress(0.7, detail = "Processing response...")
      if (llm_out$success && nzchar(llm_out$narrative)) {
        overview_llm_display(llm_out$narrative)
      } else {
        overview_llm_display("LLM unavailable; check Ollama/OpenAI.")
      }
    })
  })
  output$overview_llm_ui <- renderUI({
    txt <- overview_llm_display()
    if (!nzchar(txt)) return(tags$p("Click \"Get LLM interpretation\" to run (requires Ollama).", class = "text-muted"))
    tags$div(class = "llm-summary-card",
             tags$h5("AI Economic Summary", class = "llm-summary-card-title"),
             tags$div(class = "llm-narrative", tags$pre(txt, style = "white-space: pre-wrap; margin: 0; font-family: inherit;")))
  })
  output$download_overview_llm <- downloadHandler(
    filename = function() paste0("overview_llm_report_", format(Sys.Date(), "%Y-%m-%d"), ".txt"),
    content = function(file) {
      txt <- overview_llm_display()
      header <- paste0(
        "Energy–Macro Dashboard — Overview LLM Interpretation\n",
        "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
      )
      writeLines(paste0(header, if (nzchar(txt)) txt else "(No interpretation generated yet.)"), file)
    }
  )

  # System Status Summary (top of dashboard): 12-month MC and stability for executive metrics.
  status_summary_12m <- reactive({
    A <- A_matrix()
    S <- Sigma_matrix()
    if (is.null(A) || is.null(S)) return(NULL)
    run_mc(A, S, horizon = 12L, n_sim = 300L)
  }) %>% bindCache(A_matrix(), Sigma_matrix())

  output$system_status_cards_ui <- renderUI({
    sm <- stability_metrics_for_ui()
    mc12 <- status_summary_12m()
    if (is.null(sm)) {
      return(tags$p("Load pipeline data (run pipeline) to see system status.", class = "text-muted status-placeholder"))
    }
    stable <- isTRUE(sm$is_stable)
    rho <- round(sm$spectral_radius, 3)
    amp <- if (!is.null(sm$shock_amplification_factor) && !is.na(sm$shock_amplification_factor)) round(sm$shock_amplification_factor, 2) else NA
    reces_pct <- if (!is.null(mc12)) sprintf("%.1f%%", 100 * mc12$recession_prob) else "\u2014"
    infl_pct <- if (!is.null(mc12)) sprintf("%.1f%%", 100 * mc12$inflation_stress_prob) else "\u2014"
    fluidRow(
      column(3, tags$div(class = "status-metric-card",
        value_box(title = "System Stability", value = if (stable) "Stable" else "Unstable", theme = "secondary"),
        tags$details(class = "status-card-details",
          tags$summary("What's this?"),
          tags$p("Whether the VAR is stable: spectral radius \u03C1(A) < 1. Derived from the transition matrix A (eigenvalues).")
        )
      )),
      column(3, tags$div(class = "status-metric-card",
        value_box(title = "1-Year Recession Risk", value = reces_pct, theme = "secondary"),
        tags$details(class = "status-card-details",
          tags$summary("What's this?"),
          tags$p("Probability that GDP growth is negative at 12 months. From Monte Carlo simulation using A and \u03A3 (300 paths, 12-month horizon).")
        )
      )),
      column(3, tags$div(class = "status-metric-card",
        value_box(title = "Inflation Risk", value = infl_pct, theme = "secondary"),
        tags$details(class = "status-card-details",
          tags$summary("What's this?"),
          tags$p("Probability that inflation (CPI) exceeds 4% over the 12-month path. From the same Monte Carlo simulation.")
        )
      )),
      column(3, tags$div(class = "status-metric-card",
        value_box(title = "Shock Sensitivity", value = if (is.na(amp)) "\u2014" else as.character(amp), theme = "secondary"),
        tags$details(class = "status-card-details",
          tags$summary("What's this?"),
          tags$p("Maximum amplification of a one-time shock (norm of powers of A). From stability metrics computed from A.")
        )
      ))
    )
  })

  # FRED tab: link to FRED website
  output$fred_link_ui <- renderUI({
    s <- input$fred_series
    if (is.null(s) || s == "") return(NULL)
    fred_url <- paste0("https://fred.stlouisfed.org/series/", s)
    tags$p(
      tags$a(href = fred_url, target = "_blank", 
              paste0("View ", s, " on FRED"), 
              style = "color: #1976d2; text-decoration: underline;"),
      style = "margin-top: -10px; margin-bottom: 10px;"
    )
  })

  # FRED tab
  output$fred_plot <- renderPlot({
    d <- data()$fred
    s <- input$fred_series
    if (is.null(d) || !s %in% names(d)) return(plot(0, type = "n", main = "No data"))
    d <- d %>% dplyr::filter(date >= as.Date("2000-01-01"))
    dd <- d %>% dplyr::filter(!is.na(.data[[s]]))
    if (nrow(dd) == 0) return(plot(0, type = "n", main = "No data from 2000 onwards"))
    ggplot(dd, aes(date, .data[[s]])) + geom_line(color = "steelblue", linewidth = 0.8) +
      theme_minimal() + labs(title = paste("FRED:", s, "(from 2000)"), x = NULL, y = s)
  })

  output$fred_table_links_ui <- renderUI({
    d <- data()$fred
    if (is.null(d)) return(NULL)
    fred_series_codes <- c("DCOILWTICO", "CPIAUCSL", "FEDFUNDS", "INDPRO", "UNRATE", "GDPC1")
    present_codes <- fred_series_codes[fred_series_codes %in% names(d)]
    if (length(present_codes) == 0) return(NULL)
    links <- lapply(present_codes, function(code) {
      tags$a(href = paste0("https://fred.stlouisfed.org/series/", code), 
             target = "_blank", code, 
             style = "color: #1976d2; text-decoration: underline; margin-right: 10px;")
    })
    tags$p(
      tags$strong("FRED series links: "),
      tags$span(links),
      style = "margin-top: 10px; margin-bottom: 5px; font-size: 0.9em;"
    )
  })

  output$fred_table <- renderDT({
    d <- data()$fred
    if (is.null(d)) return(NULL)
    d <- d %>% dplyr::filter(date >= as.Date("2000-01-01"))
    datatable(d, options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE), rownames = FALSE)
  })

  # EIA tab
  output$eia_sales_plot <- renderPlot({
    d <- data()$eia
    if (is.null(d) || !"retail_sales" %in% names(d)) return(plot(0, type = "n", main = "No EIA data"))
    dd <- d %>% dplyr::filter(!is.na(retail_sales))
    if (nrow(dd) == 0) return(plot(0, type = "n", main = "No EIA data"))
    ggplot(dd, aes(date, retail_sales)) + geom_line(color = "darkgreen", linewidth = 0.8) +
      theme_minimal() + labs(title = "Electricity retail sales (demand proxy)", x = NULL, y = "Sales")
  })

  output$eia_renewable_plot <- renderPlot({
    d <- data()$eia
    if (is.null(d) || !"renewable_share" %in% names(d)) return(plot(0, type = "n", main = "No renewable share"))
    dd <- d %>% dplyr::filter(!is.na(renewable_share))
    if (nrow(dd) == 0) return(plot(0, type = "n", main = "No renewable share"))
    ggplot(dd, aes(date, renewable_share)) + geom_line(color = "forestgreen", linewidth = 0.8) +
      theme_minimal() + labs(title = "Renewable share of generation", x = NULL, y = "Share") + ylim(0, NA)
  })

  output$eia_table <- renderDT({
    d <- data()$eia
    if (is.null(d)) return(NULL)
    datatable(d, options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE), rownames = FALSE)
  })

  # World Bank tab
  output$wb_plot <- renderPlot({
    d <- data()$worldbank
    if (is.null(d) || nrow(d) == 0) return(plot(0, type = "n", main = "No World Bank data"))
    d <- d %>% dplyr::filter(year >= 2000)
    if (nrow(d) == 0) return(plot(0, type = "n", main = "No World Bank data from 2000 onwards"))
    dlong <- d %>% pivot_longer(-year, names_to = "indicator", values_to = "value") %>% dplyr::filter(!is.na(value))
    if (nrow(dlong) == 0) return(plot(0, type = "n", main = "No World Bank data"))
    ggplot(dlong, aes(year, value, color = indicator)) + geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
      theme_minimal() + theme(legend.position = "bottom") + labs(title = "World Bank structural indicators (from 2000)", x = "Year", y = NULL)
  })

  output$wb_table <- renderDT({
    d <- data()$worldbank
    if (is.null(d)) return(NULL)
    d <- d %>% dplyr::filter(year >= 2000)
    datatable(d, options = list(pageLength = 12, deferRender = TRUE), rownames = FALSE)
  })

  # System series tab
  output$system_plot <- renderPlot({
    d <- data()$system_ts
    s <- input$system_series
    if (is.null(d) || !s %in% names(d)) return(plot(0, type = "n", main = "No data"))
    dd <- d %>% dplyr::filter(!is.na(.data[[s]]))
    if (nrow(dd) == 0) return(plot(0, type = "n", main = "No data"))
    ylab <- if (s %in% names(SYSTEM_SERIES_YLAB)) as.character(SYSTEM_SERIES_YLAB[s]) else s
    ggplot(dd, aes(date, .data[[s]])) + geom_line(color = "purple", linewidth = 0.7) +
      theme_minimal() + labs(title = paste("System series:", s), x = NULL, y = ylab)
  })

  output$system_table <- renderDT({
    d <- data()$system_ts
    if (is.null(d)) return(NULL)
    datatable(d, options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE), rownames = FALSE)
  })

  # Model tab
  output$A_table <- renderDT({
    A <- A_matrix()
    if (is.null(A)) return(NULL)
    datatable(round(A, 4), options = list(dom = "t", scrollX = TRUE, deferRender = TRUE))
  })

  output$Sigma_table <- renderDT({
    S <- Sigma_matrix()
    if (is.null(S)) return(NULL)
    datatable(round(S, 6), options = list(dom = "t", scrollX = TRUE, deferRender = TRUE))
  })

  # Stability tab: value boxes + text
  stability_metrics_for_ui <- reactive({
    A <- A_matrix()
    if (is.null(A) || !is.matrix(A)) return(NULL)
    compute_stability_metrics(A)
  })
  output$stability_value_boxes_ui <- renderUI({
    sm <- stability_metrics_for_ui()
    if (is.null(sm)) return(tags$p("Load A matrix first (run pipeline).", class = "text-muted"))
    rho <- round(sm$spectral_radius, 4)
    stable <- isTRUE(sm$is_stable)
    amp <- if (!is.null(sm$shock_amplification_factor) && !is.na(sm$shock_amplification_factor)) round(sm$shock_amplification_factor, 4) else NA
    fluidRow(
      column(4, value_box(
        title = "Spectral radius \u03C1(A)",
        value = rho,
        theme = if (stable) "success" else "danger"
      )),
      column(4, value_box(
        title = "Stability",
        value = if (stable) "Stable" else "Unstable",
        theme = if (stable) "success" else "danger"
      )),
      if (!is.na(amp)) column(4, value_box(
        title = "Shock amplification",
        value = amp
      )) else NULL
    )
  })
  output$stability_text <- renderPrint({
    A <- A_matrix()
    if (is.null(A)) { cat("Load A matrix first (run pipeline).\n"); return(invisible(NULL)) }
    s <- stability_from_A(A)
    cat("Spectral radius \u03C1(A): ", round(s$spectral_radius, 4), "\n")
    cat("Stability: ", if (s$is_stable) "stable (\u03C1 < 1)" else "UNSTABLE (\u03C1 \u2265 1)", "\n")
    cat("Eigenvalues (modulus): ", paste(round(Mod(s$eigenvalues), 4), collapse = ", "), "\n")
  })

  output$stability_eigs_plot <- renderPlot({
    A <- A_matrix()
    if (is.null(A)) return(plot(0, type = "n", main = "No A matrix"))
    eigs <- eigen(A, only.values = TRUE)$values
    df <- data.frame(Re = Re(eigs), Im = Im(eigs), Mod = Mod(eigs))
    ggplot(df, aes(Re, Im, size = Mod)) + geom_point(alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed") + geom_hline(yintercept = 0, linetype = "dashed") +
      coord_fixed() + theme_minimal() + labs(title = "Eigenvalues of A (unit circle = stability boundary)", x = "Real", y = "Imaginary")
  })

  # LLM interpretation for Stability tab (on button click)
  stability_llm_display <- reactiveVal("")
  observeEvent(input$get_llm_stability_btn, {
    A <- A_matrix()
    if (is.null(A) || !is.matrix(A)) {
      stability_llm_display("Load A matrix first.")
      return()
    }
    withProgress(message = "LLM interpreting VAR stability...", value = 0, {
      incProgress(0.1, detail = "Computing stability metrics...")
      stability_llm_display("Interpreting…")
      stab <- compute_stability_metrics(A)
      incProgress(0.2, detail = "Calling LLM...")
      llm_out <- llm_interpret_var_stability(stab, use_ollama = TRUE, use_openai = TRUE, ollama_model = input$ollama_model)
      incProgress(0.7, detail = "Processing response...")
      if (llm_out$success && nzchar(llm_out$narrative)) {
        stability_llm_display(llm_out$narrative)
      } else {
        stability_llm_display("LLM unavailable; check Ollama/OpenAI.")
      }
    })
  })
  output$stability_llm_ui <- renderUI({
    txt <- stability_llm_display()
    if (!nzchar(txt)) return(NULL)
    tags$div(class = "llm-summary-card",
             tags$h5("AI Economic Summary", class = "llm-summary-card-title"),
             tags$div(class = "llm-narrative", tags$pre(txt, style = "white-space: pre-wrap; margin: 0; font-family: inherit;")))
  })
  output$download_stability_llm <- downloadHandler(
    filename = function() paste0("stability_llm_report_", format(Sys.Date(), "%Y-%m-%d"), ".txt"),
    content = function(file) {
      txt <- stability_llm_display()
      header <- paste0(
        "Energy–Macro Dashboard — VAR Stability LLM Interpretation\n",
        "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
      )
      writeLines(paste0(header, if (nzchar(txt)) txt else "(No interpretation generated yet.)"), file)
    }
  )

  # Impulse Response tab: sync shock variable choices with A columns (adaptive to VAR dimension)
  observe({
    A <- A_matrix()
    if (!is.null(A) && ncol(A) > 0) {
      cols <- colnames(A)
      if (is.null(cols)) cols <- rownames(A)
      if (is.null(cols)) cols <- paste0("V", seq_len(ncol(A)))
      updateSelectInput(session, "shock_var", choices = cols, selected = cols[1])
    }
  })

  # IRF computation: optimized to only recompute when necessary inputs change
  # Dependencies: A_matrix(), Sigma_matrix(), input$shock_var, input$irf_horizon, input$shock_sd
  # Note: Horizon changes require recomputation (different recursion length), which is mathematically necessary
  irf_data <- reactive({
    A <- A_matrix()
    Sigma <- Sigma_matrix()
    if (is.null(A) || is.null(Sigma) || is.null(input$shock_var) || input$shock_var == "") return(NULL)
    cols <- colnames(A)
    if (is.null(cols)) cols <- rownames(A)
    if (is.null(cols)) cols <- paste0("V", seq_len(ncol(A)))
    if (!input$shock_var %in% cols) return(NULL)
    shock_sd <- input$shock_sd
    if (is.null(shock_sd) || !is.numeric(shock_sd)) shock_sd <- 1
    compute_irf(
      A = A,
      Sigma = Sigma,
      shock_var = input$shock_var,
      horizon = as.integer(input$irf_horizon),
      columns = cols,
      shock_sd_multiplier = shock_sd
    )
  }) %>% bindCache(A_matrix(), Sigma_matrix(), input$shock_var, input$irf_horizon, input$shock_sd)

  irf_bootstrap_result <- reactiveVal(NULL)
  observeEvent(input$irf_bootstrap_btn, {
    A <- A_matrix()
    Sigma <- Sigma_matrix()
    system_ts <- data()$system_ts
    if (is.null(A) || is.null(Sigma) || is.null(system_ts) || is.null(input$shock_var) || input$shock_var == "") {
      irf_bootstrap_result(NULL)
      return()
    }
    cols <- colnames(A)
    if (is.null(cols)) cols <- rownames(A)
    if (is.null(cols)) cols <- paste0("V", seq_len(ncol(A)))
    if (!input$shock_var %in% cols) { irf_bootstrap_result(NULL); return() }
    shock_sd <- input$shock_sd
    if (is.null(shock_sd) || !is.numeric(shock_sd)) shock_sd <- 1
    withProgress(message = "Bootstrap IRF (500 replications)...", value = 0, {
      boot_out <- tryCatch(
        bootstrap_irf(
          system_ts = system_ts,
          A = A,
          Sigma = Sigma,
          shock_var = input$shock_var,
          horizon = as.integer(input$irf_horizon),
          columns = cols,
          shock_sd_multiplier = shock_sd,
          n_boot = 500L
        ),
        error = function(e) { message(conditionMessage(e)); NULL }
      )
      irf_bootstrap_result(boot_out)
    })
  })
  # Clear bootstrap when shock/horizon/size change so bands don't apply to old settings
  observe({
    input$shock_var
    input$irf_horizon
    input$shock_sd
    irf_bootstrap_result(NULL)
  })

  # IRF summary metrics: depends only on irf_data() (which already includes shock_var dependency)
  # This ensures summary only recomputes when IRF data changes, not on other input changes
  irf_summary <- reactive({
    df <- irf_data()
    if (is.null(df) || nrow(df) == 0) {
      return(list(peak = NA, time_to_peak = NA, half_life = NA, cumulative = NA))
    }
    # Extract shock_var from input (needed to identify which column to analyze)
    # Note: irf_data() already depends on input$shock_var, so this doesn't cause extra reactivity
    shock_var <- input$shock_var
    if (is.null(shock_var) || !shock_var %in% names(df)) {
      return(list(peak = NA, time_to_peak = NA, half_life = NA, cumulative = NA))
    }
    H <- nrow(df) - 1
    resp_shock <- as.numeric(df[[shock_var]])
    peak_abs <- max(abs(resp_shock), na.rm = TRUE)
    if (peak_abs <= 0) {
      return(list(peak = 0, time_to_peak = 0, half_life = NA, cumulative = sum(resp_shock, na.rm = TRUE)))
    }
    time_to_peak <- which.max(abs(resp_shock)) - 1
    half_peak <- 0.5 * peak_abs
    # Half-life: first horizon after peak where |response| <= half of peak
    after_peak <- seq(time_to_peak + 1, length(resp_shock))
    half_life_idx <- after_peak[abs(resp_shock[after_peak]) <= half_peak]
    half_life <- if (length(half_life_idx) > 0) half_life_idx[1] - 1 else NA
    list(
      peak = peak_abs,
      time_to_peak = time_to_peak,
      half_life = half_life,
      cumulative = sum(resp_shock, na.rm = TRUE)
    )
  })

  output$irf_peak_response <- renderText({
    s <- irf_summary()
    if (is.na(s$peak)) return("—") else sprintf("%.4f", s$peak)
  })
  output$irf_time_to_peak <- renderText({
    s <- irf_summary()
    if (is.na(s$time_to_peak)) return("—") else paste0(s$time_to_peak, " mo")
  })
  output$irf_half_life <- renderText({
    s <- irf_summary()
    if (is.na(s$half_life)) return("—") else paste0(s$half_life, " mo")
  })
  output$irf_cumulative <- renderText({
    s <- irf_summary()
    if (is.na(s$cumulative)) return("—") else sprintf("%.4f", s$cumulative)
  })

  # LLM interpretation for Impulse Response tab (on button click)
  irf_llm_display <- reactiveVal("")
  observeEvent(input$get_llm_irf_btn, {
    df <- irf_data()
    s <- irf_summary()
    if (is.null(df) || nrow(df) == 0 || is.null(input$shock_var) || !input$shock_var %in% names(df)) {
      irf_llm_display("Load data and select a shock variable first.")
      return()
    }
    withProgress(message = "LLM interpreting impulse response...", value = 0, {
      incProgress(0.1, detail = "Preparing IRF summary...")
      irf_llm_display("Interpreting…")
      H <- nrow(df) - 1
      variable_summary <- data.frame(
        variable = names(df),
        impact_0 = as.numeric(df[1, ]),
        peak_abs = vapply(df, function(x) max(abs(x), na.rm = TRUE), 0),
        time_to_peak = vapply(df, function(x) which.max(abs(x))[1L] - 1L, 0L),
        end_response = as.numeric(df[nrow(df), ]),
        stringsAsFactors = FALSE
      )
      irf_summary_list <- list(
        shock_var = input$shock_var,
        horizon = as.integer(input$irf_horizon),
        shock_sd = input$shock_sd,
        peak = s$peak,
        time_to_peak = s$time_to_peak,
        half_life = s$half_life,
        cumulative = s$cumulative,
        variable_summary = variable_summary
      )
      incProgress(0.2, detail = "Calling LLM...")
      llm_out <- llm_interpret_irf(irf_summary_list, use_ollama = TRUE, use_openai = TRUE, ollama_model = input$ollama_model)
      incProgress(0.7, detail = "Processing response...")
      if (llm_out$success && nzchar(llm_out$narrative)) {
        irf_llm_display(llm_out$narrative)
      } else {
        irf_llm_display("LLM unavailable; check Ollama/OpenAI.")
      }
    })
  })
  output$irf_llm_ui <- renderUI({
    txt <- irf_llm_display()
    if (!nzchar(txt)) return(NULL)
    tags$div(class = "llm-summary-card",
             tags$h5("AI Economic Summary", class = "llm-summary-card-title"),
             tags$div(class = "llm-narrative", tags$pre(txt, style = "white-space: pre-wrap; margin: 0; font-family: inherit;")))
  })
  output$download_irf_llm <- downloadHandler(
    filename = function() paste0("irf_llm_report_", format(Sys.Date(), "%Y-%m-%d"), ".txt"),
    content = function(file) {
      txt <- irf_llm_display()
      header <- paste0(
        "Energy–Macro Dashboard — Impulse Response LLM Interpretation\n",
        "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
      )
      writeLines(paste0(header, if (nzchar(txt)) txt else "(No interpretation generated yet.)"), file)
    }
  )

  output$download_full_llm_report <- downloadHandler(
    filename = function() paste0("energy_macro_llm_report_", format(Sys.Date(), "%Y-%m-%d"), ".txt"),
    content = function(file) {
      o <- overview_llm_display()
      s <- stability_llm_display()
      i <- irf_llm_display()
      header <- paste0(
        "Energy–Macro Dashboard — Full LLM Report\n",
        "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
      )
      sec1 <- paste0(
        "1. Overview\n", "------------\n",
        if (nzchar(o)) o else "(No interpretation generated yet for this section.)", "\n\n"
      )
      sec2 <- paste0(
        "2. VAR Stability\n", "----------------\n",
        if (nzchar(s)) s else "(No interpretation generated yet for this section.)", "\n\n"
      )
      sec3 <- paste0(
        "3. Impulse Response\n", "--------------------\n",
        if (nzchar(i)) i else "(No interpretation generated yet for this section.)", "\n"
      )
      writeLines(paste0(header, sec1, sec2, sec3), file)
    }
  )

  output$irf_plot <- renderPlot({
    df <- irf_data()
    if (is.null(df) || nrow(df) == 0) {
      return(plot(0, type = "n", main = "Load A matrix (run pipeline) and select shock variable."))
    }
    H <- as.integer(input$irf_horizon)
    df$h <- 0:H
    df_long <- tidyr::pivot_longer(df,
                                   cols = -h,
                                   names_to = "variable",
                                   values_to = "response")
    boot <- irf_bootstrap_result()
    show_bands <- isTRUE(input$irf_show_bands) && !is.null(boot) && nrow(boot$irf_main) == nrow(df)
    if (show_bands) {
      # Long format for bands: h, variable, lower, upper
      band_long <- tidyr::pivot_longer(
        data.frame(h = 0:H, boot$irf_lower),
        cols = -h, names_to = "variable", values_to = "lower"
      )
      upper_long <- tidyr::pivot_longer(
        data.frame(h = 0:H, boot$irf_upper),
        cols = -h, names_to = "variable", values_to = "upper"
      )
      band_long$upper <- upper_long$upper
      df_long <- dplyr::left_join(df_long, band_long, by = c("h", "variable"))
      p <- ggplot(df_long, aes(x = h, y = response, color = variable)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.25, colour = NA) +
        geom_line(linewidth = 1) +
        theme_minimal() +
        labs(title = paste("Impulse Response to", input$shock_var, "Shock"),
             subtitle = paste("Shock magnitude:", input$shock_sd, "SD. Bands: 90% bootstrap (5th–95th)."),
             x = "Horizon (months)",
             y = "Response")
    } else {
      p <- ggplot(df_long, aes(x = h, y = response, color = variable)) +
        geom_line(linewidth = 1) +
        theme_minimal() +
        labs(title = paste("Impulse Response to", input$shock_var, "Shock"),
             subtitle = paste("Shock magnitude:", input$shock_sd, "SD"),
             x = "Horizon (months)",
             y = "Response")
    }
    p
  })

  # Monte Carlo tab
  mc_result <- reactiveVal(NULL)
  observeEvent(input$run_mc_btn, {
    A <- A_matrix()
    S <- Sigma_matrix()
    if (is.null(A) || is.null(S)) { mc_result(NULL); return() }
    res <- run_mc(A, S, horizon = input$mc_horizon, n_sim = input$mc_nsim)
    mc_result(res)
  })

  output$mc_value_boxes_ui <- renderUI({
    r <- mc_result()
    if (is.null(r)) return(NULL)
    fluidRow(
      column(4, value_box(
        title = "Recession prob (GDP < 0 at horizon)",
        value = sprintf("%.1f%%", 100 * r$recession_prob),
        theme = if (r$recession_prob > 0.2) "warning" else "primary"
      )),
      column(4, value_box(
        title = "Inflation stress prob (> 4%)",
        value = sprintf("%.1f%%", 100 * r$inflation_stress_prob),
        theme = if (r$inflation_stress_prob > 0.2) "warning" else "primary"
      )),
      column(4, value_box(
        title = "Simulations",
        value = dim(r$paths)[1]
      ))
    )
  })

  output$mc_summary <- renderPrint({
    r <- mc_result()
    if (is.null(r)) { cat("Click 'Run MC' to simulate (requires A and Σ loaded).\n"); return(invisible(NULL)) }
    cat("Horizon:", ncol(r$paths), "| Simulations:", dim(r$paths)[1], "\n")
    cat("Recession prob (GDP growth < 0 at horizon):", round(r$recession_prob, 3), "\n")
    cat("Inflation stress prob (> 4%):", round(r$inflation_stress_prob, 3), "\n")
  })

  output$mc_gdp_path <- renderPlot({
    r <- mc_result()
    if (is.null(r)) return(plot(0, type = "n", main = "Run MC first"))
    df <- data.frame(month = seq_along(r$gdp_mean_path), gdp_mean = r$gdp_mean_path)
    ggplot(df, aes(month, gdp_mean)) + geom_line(color = "steelblue", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_minimal() + labs(title = "Mean GDP growth path across simulations", x = "Month", y = "GDP growth (%)")
  })
}

shinyApp(ui = ui, server = server)

