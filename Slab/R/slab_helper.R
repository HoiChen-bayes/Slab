# helper functions for slab -------------------------------------
# (1) Al_u                         :
# (2) Delta_u                      : the denominator of the global minimum
# (3) theta_SR                     : the algorithm to calculate the slab theta from OLS
# (4) main helper function of slab : the shrinkage estimation from OLS

# (1) Al_u ------------------------------------------------------------
#' @keywords internal
#' @import expm
#' @noRd
Al_u <- function(u, l, sigma_lambda_inv) {
  al_u <- t(u) %*% (sigma_lambda_inv %^% l) %*% u
  return(al_u)
}

# (2) Delta_u ------------------------------------------------------------
#' @keywords internal
#' @noRd
Calculate_Delta_u <- function(sigma_square, a0_u, a1_u, a2_u, a3_u, theta_vector, u) {
  delta_u <- sigma_square * (a0_u * a3_u - a1_u * a2_u) + a3_u * (theta_vector %*% u)^2
  return(delta_u)
}

# (3) theta_SR  -------------------------------------
Calculate_Theta_SR <- function(delta_u, sigma_square, a1_u, a2_u, sigma_lambda_inv, theta_vector, p_plus_1) {
  J <- matrix(1, p_plus_1, p_plus_1)
  if (delta_u > 0) {
    mu_u_gm <- (sigma_square * a2_u) / delta_u
    scalar_value <- as.numeric(mu_u_gm / (1 + mu_u_gm * a1_u))
    theta_sr <- (diag(1, p_plus_1) - scalar_value * sigma_lambda_inv %*% J) %*% theta_vector
  } else {
    warning("delta_u is not greater than zero. This may lead to unexpected results.")
    theta_sr <- (diag(1, p_plus_1) - sigma_lambda_inv %*% J) %*% theta_vector
  }
  return(theta_sr)
}

# (4) main helper function of slab -------------------------------------
#' @keywords internal
#' @noRd
sr_est <- function(u, theta_vector, sigma_square, sigma_lambda_inv, p_plus_1, I_p_plus_1, lambda) {
  if (lambda == 0){
    a0_u <- sum(u * u)
  }
  if (lambda > 0){
    a0_u <- Al_u(u, 0, sigma_lambda_inv)
  }
  a1_u <- Al_u(u, 1, sigma_lambda_inv)
  a2_u <- Al_u(u, 2, sigma_lambda_inv)
  a3_u <- Al_u(u, 3, sigma_lambda_inv)
  delta_u <- Calculate_Delta_u(sigma_square, a0_u, a1_u, a2_u, a3_u, theta_vector, u)

  theta_sr <- Calculate_Theta_SR(delta_u, sigma_square, a1_u, a2_u, sigma_lambda_inv, theta_vector, p_plus_1)

  return(as.vector(theta_sr))
}
