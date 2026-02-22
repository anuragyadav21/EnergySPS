# irf_engine.R
# Impulse Response: structural shocks via Cholesky decomposition of Sigma.
# VAR(1): x_{t+1} = A x_t + ε_t, ε_t = P η_t, Σ = P P', η ~ N(0,I).
# Structural shock j => impulse = P[, j]. IRF: x[0] = shock, x[h] = A %*% x[h-1].

#' Compute impulse response using structural shocks (Cholesky of Sigma).
#' @param A State transition matrix (K x K)
#' @param Sigma Innovation covariance matrix (K x K), must be positive definite
#' @param shock_var Name of variable to shock (must be in columns)
#' @param horizon Number of periods ahead (0 to horizon)
#' @param columns Variable names (e.g. colnames(A)), length K
#' @param shock_sd_multiplier Scale factor applied to structural shock vector (e.g. 1 = one unit structural shock)
#' @return data.frame with horizon+1 rows and K columns (response of each variable over time)
compute_irf <- function(A, Sigma, shock_var, horizon = 24, columns, shock_sd_multiplier = 1) {
  K <- ncol(A)
  if (length(columns) != K) stop("length(columns) must equal ncol(A).")
  if (nrow(Sigma) != K || ncol(Sigma) != K) stop("Sigma must be K x K with K = ncol(A).")
  shock_index <- which(columns == shock_var)
  if (length(shock_index) == 0) {
    stop("Shock variable not found in system.")
  }

  # Cholesky: Sigma = U' U in R, so P = t(U) gives Sigma = P P' (lower triangular P)
  U <- tryCatch(chol(Sigma), error = function(e) NULL)
  if (is.null(U)) {
    if (requireNamespace("Matrix", quietly = TRUE)) {
      Sigma_pd <- as.matrix(Matrix::nearPD(Sigma)$mat)
      U <- chol(Sigma_pd)
    } else {
      stop("Sigma is not positive definite; cannot compute Cholesky. Run parameter estimation or use Matrix::nearPD.")
    }
  }
  P <- t(U)  # lower triangular: Sigma = P %*% t(P)

  # Structural shock vector = j-th column of P (one unit of structural shock j)
  shock <- as.vector(P[, shock_index])
  shock <- as.numeric(shock_sd_multiplier) * shock

  # IRF matrix: row = time h, column = variable
  responses <- matrix(0, nrow = horizon + 1, ncol = K)
  colnames(responses) <- columns

  # h = 0: initial structural shock
  responses[1, ] <- shock

  # Recursive: x[h] = A %*% x[h-1]
  x <- shock
  for (h in seq_len(horizon)) {
    x <- as.vector(A %*% x)
    responses[h + 1, ] <- x
  }

  as.data.frame(responses)
}

#' Bootstrap IRF: resample residuals, simulate data, re-estimate VAR, recompute IRF; return median and 5th/95th percentiles.
#' @param system_ts Data frame with columns matching \code{columns} (and optionally \code{date})
#' @param A Initial VAR coefficient matrix (K x K), used only to get columns; bootstrap refits VAR
#' @param Sigma Initial covariance (K x K)
#' @param shock_var, horizon, columns, shock_sd_multiplier As in \code{compute_irf}
#' @param n_boot Number of bootstrap replications (default 500)
#' @return List with \code{irf_main} (data.frame, main IRF), \code{irf_median}, \code{irf_lower} (5th), \code{irf_upper} (95th) as matrices (horizon+1) x K
bootstrap_irf <- function(system_ts, A, Sigma, shock_var, horizon = 24, columns,
                          shock_sd_multiplier = 1, n_boot = 500L) {
  if (!requireNamespace("vars", quietly = TRUE)) stop("Package 'vars' required for bootstrap IRF.")
  K <- ncol(A)
  if (!all(columns %in% names(system_ts))) stop("system_ts must contain all 'columns'.")
  Y <- as.matrix(system_ts[, columns, drop = FALSE])
  Y <- Y[complete.cases(Y), , drop = FALSE]
  n <- nrow(Y)
  if (n < 50) stop("Insufficient rows in system_ts for bootstrap (need at least 50).")

  # Fit VAR to get residuals and lag order
  sel <- vars::VARselect(Y, lag.max = 8, type = "const")
  p <- as.integer(sel$selection["AIC(n)"])
  if (is.na(p) || p < 1) p <- 1
  var_obj <- vars::VAR(Y, p = p, type = "const")
  res <- as.matrix(residuals(var_obj))  # (n - p) x K
  fitted_mat <- as.matrix(fitted(var_obj))  # (n - p) x K
  T_len <- nrow(res)

  # Main IRF (from supplied A, Sigma)
  irf_main <- as.matrix(compute_irf(A, Sigma, shock_var, horizon, columns, shock_sd_multiplier))

  # Bootstrap replications
  H <- horizon + 1
  irf_stack <- array(0, dim = c(H, K, n_boot))
  for (b in seq_len(n_boot)) {
    ind <- sample.int(T_len, T_len, replace = TRUE)
    res_b <- res[ind, , drop = FALSE]
    # Synthetic data: first p rows = original Y; then fitted + resampled residual
    Y_b <- matrix(0, n, K)
    Y_b[seq_len(p), ] <- Y[seq_len(p), , drop = FALSE]
    for (i in seq_len(T_len)) {
      Y_b[p + i, ] <- fitted_mat[i, ] + res_b[i, ]
    }
    var_b <- tryCatch(vars::VAR(Y_b, p = p, type = "const"), error = function(e) NULL)
    if (is.null(var_b)) next
    A_b <- vars::Acoef(var_b)[[1]]
    Sigma_b <- summary(var_b)$covres
    if (nrow(A_b) != K || ncol(A_b) != K) next
    cols_b <- colnames(Y_b)
    if (is.null(cols_b)) cols_b <- columns
    irf_b <- tryCatch(
      as.matrix(compute_irf(A_b, Sigma_b, shock_var, horizon, cols_b, shock_sd_multiplier)),
      error = function(e) NULL
    )
    if (!is.null(irf_b) && nrow(irf_b) == H && ncol(irf_b) == K) irf_stack[, , b] <- irf_b
  }

  # Median, 5th, 95th percentile (across replications)
  irf_median <- matrix(0, H, K)
  irf_lower <- matrix(0, H, K)
  irf_upper <- matrix(0, H, K)
  for (h in seq_len(H)) {
    for (k in seq_len(K)) {
      vals <- irf_stack[h, k, ]
      irf_median[h, k] <- stats::quantile(vals, 0.5, na.rm = TRUE)
      irf_lower[h, k] <- stats::quantile(vals, 0.05, na.rm = TRUE)
      irf_upper[h, k] <- stats::quantile(vals, 0.95, na.rm = TRUE)
    }
  }
  colnames(irf_main) <- colnames(irf_median) <- colnames(irf_lower) <- colnames(irf_upper) <- columns

  list(
    irf_main = as.data.frame(irf_main),
    irf_median = irf_median,
    irf_lower = irf_lower,
    irf_upper = irf_upper
  )
}
