# 05_parameter_estimation.R
# Energy–Macro Shock Propagation Simulator — parameter estimation
# Loads: data/system_timeseries.csv
# Estimates: μ, Σ, correlation, VAR (lag order selection), interaction matrix A
# Outputs: data/A_matrix.csv, data/covariance_matrix.csv
# Prints: eigenvalues of A, spectral radius

library(dplyr)
library(readr)
library(vars)
# MASS not loaded to avoid masking dplyr::select

#' Load system timeseries (no date column for VAR — use as matrix)
load_system_timeseries <- function(path = "data/system_timeseries.csv") {
  if (!file.exists(path)) stop("Missing ", path, " — run 04_data_cleaning.R first.")
  d <- readr::read_csv(path, show_col_types = FALSE)
  d$date <- as.Date(d$date)
  d
}

#' Select endogenous variables for VAR (all numeric except date)
var_columns <- function() {
  c("oil_return", "inflation_change", "rate_change", "industrial_return",
    "gdp_growth", "demand_change", "renewable_share")
}

#' Estimate mean vector μ and covariance matrix Σ from system_timeseries
estimate_moments <- function(Y) {
  list(
    mu = colMeans(Y, na.rm = TRUE),
    Sigma = cov(Y, use = "complete.obs"),
    correlation = cor(Y, use = "complete.obs")
  )
}

#' Fit VAR with lag order selection (AIC), return VAR object and chosen p
fit_var <- function(Y, max_lag = 8) {
  Y <- as.matrix(Y)
  sel <- vars::VARselect(Y, lag.max = max_lag, type = "const")
  # Use AIC
  p <- as.integer(sel$selection["AIC(n)"])
  if (is.na(p) || p < 1) p <- 1
  message("VAR lag order (AIC): ", p)
  vars::VAR(Y, p = p, type = "const")
}

#' Extract interaction matrix A from VAR
#' For VAR(1): A = A1. For VAR(p): use companion form so state is (y_t, y_{t-1}, ..., y_{t-p+1})
#' Spec uses X(t+1) = A X(t) + ε, so we return the lag-1 coefficient matrix (K x K).
get_A_matrix <- function(var_obj) {
  a_list <- vars::Acoef(var_obj)
  # First lag matrix (K x K) — governs one-step dynamics
  A1 <- a_list[[1]]
  A1
}

#' Remove constant/near-constant variables (sd < threshold) to keep covariance non-singular.
#' Returns list with: Y (matrix), cols_retained (character), cols_removed (character).
drop_constant_variables <- function(Y, threshold = 1e-8) {
  cols <- colnames(Y)
  if (is.null(cols)) cols <- paste0("V", seq_len(ncol(Y)))
  sds <- apply(Y, 2, function(x) sd(x, na.rm = TRUE))
  names(sds) <- cols
  constant <- !is.finite(sds) | sds < threshold
  cols_removed <- cols[constant]
  cols_retained <- cols[!constant]
  Y_retained <- Y[, cols_retained, drop = FALSE]
  colnames(Y_retained) <- cols_retained
  list(Y = Y_retained, cols_retained = cols_retained, cols_removed = cols_removed, sds = sds)
}

#' Run parameter estimation: load data, estimate μ/Σ, fit VAR, save A and Sigma
run_parameter_estimation <- function(data_dir = "data") {
  ts_path <- file.path(data_dir, "system_timeseries.csv")
  d <- load_system_timeseries(ts_path)
  cols <- var_columns()
  cols <- intersect(cols, names(d))
  if (length(cols) < 2) stop("Need at least 2 series in system_timeseries for VAR.")
  Y <- as.matrix(d[, cols])
  colnames(Y) <- cols

  # Remove any remaining NA rows for VAR
  Y <- Y[complete.cases(Y), , drop = FALSE]
  if (nrow(Y) < 100) stop("Insufficient observations for VAR (need at least 100; got ", nrow(Y), "). Ensure merged system_timeseries has > 100 rows.")

  # Drop constant/near-constant variables (sd < 1e-8) to avoid singular covariance
  dropped <- drop_constant_variables(Y, threshold = 1e-8)
  if (length(dropped$cols_removed) > 0) {
    message("Variables removed (sd < 1e-8): ", paste(dropped$cols_removed, collapse = ", "))
  }
  message("Variables retained for VAR: ", paste(dropped$cols_retained, collapse = ", "))
  if (length(dropped$cols_retained) < 3) {
    stop("Insufficient non-constant variables for VAR estimation. Need at least 3; got ", length(dropped$cols_retained), ".")
  }
  Y <- dropped$Y
  cols <- dropped$cols_retained

  message("Estimating moments (μ, Σ, correlation)...")
  moments <- estimate_moments(Y)
  message("Fitting VAR (lag selection by AIC)...")
  var_obj <- fit_var(Y)
  A <- get_A_matrix(var_obj)
  # Residual covariance (Σ for innovations)
  Sigma <- summary(var_obj)$covres

  # Ensure symmetric and valid covariance
  Sigma <- (Sigma + t(Sigma)) / 2
  ev <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
  if (any(ev <= 0)) {
    if (requireNamespace("Matrix", quietly = TRUE)) {
      message("Sigma not positive definite; projecting to nearest PD matrix.")
      Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)
    } else {
      message("Sigma not positive definite; add small diagonal for numerical stability.")
      Sigma <- Sigma + diag(1e-6, nrow(Sigma))
    }
  }

  rownames(A) <- cols
  colnames(A) <- cols
  rownames(Sigma) <- cols
  colnames(Sigma) <- cols

  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(A, file.path(data_dir, "A_matrix.csv"), row.names = TRUE)
  write.csv(Sigma, file.path(data_dir, "covariance_matrix.csv"), row.names = TRUE)

  # Print eigenvalues and spectral radius
  eigs <- eigen(A, only.values = TRUE)$values
  rho <- max(Mod(eigs))
  message("Eigenvalues of A (modulus): ", paste(round(Mod(eigs), 4), collapse = ", "))
  message("Spectral radius ρ(A) = ", round(rho, 4), " — system is ", if (rho >= 1) "UNSTABLE" else "stable", ".")

  invisible(list(
    mu = moments$mu,
    Sigma = Sigma,
    correlation = moments$correlation,
    A = A,
    var_obj = var_obj,
    eigenvalues = eigs,
    spectral_radius = rho,
    columns = cols
  ))
}
