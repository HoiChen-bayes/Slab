# helper functions for stein -------------------------------------
# (1) M_0_star                      : the mean square error of of estimated OLS
# (2) a_star                        : the shrinkage factor of stein regression
# (3) main helper function of stein : the shrinkage estimation from OLS

# (1) M_0_star ------------------------------------------------------------
#' @keywords internal
#' @noRd
Calculate_M0 <- function(sigma_square, sigma_lambda_inv) {
  M0 <- sigma_square * sum(diag(sigma_lambda_inv))
  return(M0)
}

# (2) a_star ------------------------------------------------------------
#' @keywords internal
#' @noRd
Calculate_A_star <- function(est_OLS, M0) {
  est_OLS_square <- sum(est_OLS^2)
  a_star <- est_OLS_square / (est_OLS_square + M0)
  return(a_star)
}

# (3) main helper function of stein -------------------------------------
#' @keywords internal
#' @noRd
st_est <- function(theta_vector, sigma_square, sigma_lambda_inv) {

  M0 <- Calculate_M0(sigma_square, sigma_lambda_inv)
  a_star <- Calculate_A_star(theta_vector, M0)

  est_St <- a_star * theta_vector

  return(as.vector(est_St))
}


