# 06_monte_carlo_engine.R
# Energy–Macro Shock Propagation Simulator — Monte Carlo engine
# Model: X(t+1) = A X(t) + ε,  ε ~ N(0, Σ)
# Supports oil and demand shock injection.
# Dimension K is derived from A/Sigma (adaptive to VAR constant-variable removal).
# Returns: paths, GDP distribution, recession prob, inflation > 4% prob, demand stress

# MASS not loaded; use MASS::mvrnorm() to avoid masking dplyr::select
library(dplyr)

# Default variable names (used only when rownames(A) are missing or for shock helpers)
VAR_NAMES_DEFAULT <- c("oil_return", "inflation_change", "rate_change", "industrial_return",
                       "gdp_growth", "demand_change", "renewable_share")

#' Simulate the linear system X(t+1) = A X(t) + ε with optional shock injection
#' System dimension K is taken from nrow(A); variable names from rownames(A).
#' @param A State transition matrix (K x K)
#' @param Sigma Innovation covariance (K x K)
#' @param shock_vector Optional length-K shock at t=1 (e.g. oil or demand shock); NULL = no shock
#' @param horizon Number of time steps
#' @param n_sim Number of simulation paths
#' @param x0 Optional initial state (length K); if NULL, uses zero
#' @return List with: paths (n_sim x horizon x K array), summary stats, recession_prob, inflation_stress_prob, demand_stress_metric
simulate_system <- function(A,
                            Sigma,
                            shock_vector = NULL,
                            horizon = 12L,
                            n_sim = 1000L,
                            x0 = NULL) {
  # Derive dimension from VAR output (adaptive to constant-variable removal)
  K <- nrow(A)
  if (is.null(K) || K == 0) K <- ncol(Sigma)
  if (ncol(A) != K || nrow(Sigma) != K || ncol(Sigma) != K) {
    stop("A and Sigma must be square and same dimension.")
  }
  message("Monte Carlo: system dimension K = ", K)

  var_names <- rownames(A)
  if (is.null(var_names)) var_names <- colnames(A)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(K))

  horizon <- as.integer(horizon)
  n_sim <- as.integer(n_sim)

  # Innovations: (T, N, K) with T=horizon, N=n_sim, K=nrow(A)
  eps <- MASS::mvrnorm(n_sim * horizon, mu = rep(0, K), Sigma = Sigma)
  dim(eps) <- c(horizon, n_sim, K)

  # paths: [sim, time, var] = (n_sim, horizon, K)
  paths <- array(0, dim = c(n_sim, horizon, K))
  if (is.null(x0)) x0 <- rep(0, K)
  x <- matrix(x0, n_sim, K, byrow = TRUE)

  for (t in seq_len(horizon)) {
    # X(t+1) = A %*% X(t) + eps(t); eps[t, , ] is (n_sim x K)
    x <- x %*% t(A) + eps[t, , ]
    if (t == 1L && length(shock_vector) == K) {
      x <- x + matrix(shock_vector, n_sim, K, byrow = TRUE)
    }
    paths[, t, ] <- x
  }

  # Indices by name (missing if variable was dropped in VAR)
  gdp_idx     <- match("gdp_growth",     var_names)
  inflation_idx <- match("inflation_change", var_names)
  demand_idx  <- match("demand_change",  var_names)

  gdp_paths <- if (!is.na(gdp_idx)) paths[, , gdp_idx, drop = TRUE] else NULL
  inflation_paths <- if (!is.na(inflation_idx)) paths[, , inflation_idx, drop = TRUE] else NULL
  demand_paths <- if (!is.na(demand_idx)) paths[, , demand_idx, drop = TRUE] else NULL

  recession_prob_end <- if (!is.null(gdp_paths)) mean(gdp_paths[, horizon] < 0) else NA_real_
  recession_prob_any <- if (!is.null(gdp_paths)) mean(apply(gdp_paths < 0, 1, any)) else NA_real_
  inflation_stress_prob <- if (!is.null(inflation_paths)) mean(inflation_paths > 4) else NA_real_
  demand_stress_metric <- if (!is.null(demand_paths)) mean(demand_paths < -2) else NA_real_

  gdp_distribution <- if (!is.null(gdp_paths)) as.vector(gdp_paths) else numeric(0)
  gdp_mean_path <- if (!is.null(gdp_paths)) colMeans(gdp_paths) else rep(NA_real_, horizon)
  gdp_q05 <- if (!is.null(gdp_paths)) apply(gdp_paths, 2, quantile, 0.05) else rep(NA_real_, horizon)
  gdp_q95 <- if (!is.null(gdp_paths)) apply(gdp_paths, 2, quantile, 0.95) else rep(NA_real_, horizon)

  list(
    paths = paths,
    horizon = horizon,
    n_sim = n_sim,
    K = K,
    var_names = var_names,
    gdp_distribution = gdp_distribution,
    gdp_mean_path = gdp_mean_path,
    gdp_q05 = gdp_q05,
    gdp_q95 = gdp_q95,
    recession_prob_end = recession_prob_end,
    recession_prob_any = recession_prob_any,
    inflation_stress_prob = inflation_stress_prob,
    demand_stress_metric = demand_stress_metric
  )
}

#' Build shock vector for oil price shock (all other components 0).
#' var_names must match current system (e.g. rownames(A)); length defines dimension.
oil_shock <- function(magnitude, var_names = VAR_NAMES_DEFAULT) {
  K <- length(var_names)
  v <- setNames(rep(0, K), var_names)
  if ("oil_return" %in% var_names) v["oil_return"] <- magnitude
  unname(v)
}

#' Build shock vector for demand shock.
#' var_names must match current system (e.g. rownames(A)); length defines dimension.
demand_shock <- function(magnitude, var_names = VAR_NAMES_DEFAULT) {
  K <- length(var_names)
  v <- setNames(rep(0, K), var_names)
  if ("demand_change" %in% var_names) v["demand_change"] <- magnitude
  unname(v)
}
