# helper functions for stein -------------------------------------
# (1) Sh_est : main function of Sh regression

# (1) sh_est ------------------------------------------------------------
#' @keywords internal
#' @noRd
sh_est <- function(theta_vector, sigma_square, sigma_lambda_inv, p_plus_1){
  theta_matrix <- matrix(theta_vector, ncol = 1)

  A <- sigma_lambda_inv
  B <- theta_matrix %*% t(theta_matrix)
  C <- B

  C_star <- sylvester(A, B, C)
  est_Sh <- as.vector(C_star %*% theta_vector)

  return(est_Sh)
}

