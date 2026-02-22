# 07_stability_metrics.R
# Energy–Macro Shock Propagation Simulator — stability and sensitivity metrics
# Computes: eigenvalues of A, stability condition, shock amplification, sensitivity ranking

#' Compute stability metrics for the transition matrix A
#' @param A State transition matrix (K x K)
#' @param var_names Optional names for rows/columns
#' @return List with eigenvalues, spectral_radius, is_stable, amplification_factor, sensitivity_ranking
compute_stability_metrics <- function(A, var_names = NULL) {
  K <- nrow(A)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(K))
  e <- eigen(A)
  evals <- e$values
  evecs <- e$vectors
  mod <- Mod(evals)
  rho <- max(mod)
  is_stable <- rho < 1

  # Shock amplification: max norm of A^k as k increases (for stable system, converges)
  # Amplification over one step: ||A|| (e.g. Frobenius or spectral norm)
  amp_one_step <- norm(A, "F")  # Frobenius; or "2" for spectral
  # Multi-step: for stable system, max transient amplification can be computed via power iteration
  # Simple metric: sum of squared coefficients (total effect of unit shock)
  shock_amplification_factor <- sqrt(sum(A^2))

  # Sensitivity ranking: which variable is most "driven" by others (row sum of |A|)
  # or which variable "drives" others most (column sum of |A|)
  row_impact <- rowSums(abs(A))   # how much this variable is influenced by all others
  col_impact <- colSums(abs(A))   # how much this variable influences others
  sensitivity_ranking <- data.frame(
    variable = var_names,
    influenced_by = row_impact,
    influences = col_impact,
    total_sensitivity = row_impact + col_impact
  )
  sensitivity_ranking <- sensitivity_ranking[order(-sensitivity_ranking$total_sensitivity), ]

  list(
    eigenvalues = evals,
    eigenvalue_modulus = mod,
    spectral_radius = rho,
    is_stable = is_stable,
    stability_condition = if (is_stable) "|λ|_max < 1: stable" else "|λ|_max >= 1: UNSTABLE",
    shock_amplification_factor = shock_amplification_factor,
    norm_Frobenius = amp_one_step,
    sensitivity_ranking = sensitivity_ranking
  )
}

#' Load A from data dir and run stability metrics
run_stability_metrics <- function(data_dir = "data", var_names = NULL) {
  A_path <- file.path(data_dir, "A_matrix.csv")
  if (!file.exists(A_path)) stop("Missing ", A_path, " — run 05_parameter_estimation.R first.")
  A_df <- read.csv(A_path, row.names = 1)
  A <- as.matrix(A_df)
  if (is.null(var_names)) var_names <- rownames(A)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(nrow(A)))
  compute_stability_metrics(A, var_names)
}
