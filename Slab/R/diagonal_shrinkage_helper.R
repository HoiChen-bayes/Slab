# helper functions for stein -------------------------------------
# (1) b_star_k : the shrinkage factor of DSh regression
# (2) dsh_est  : main function

# (1) b_star_k ------------------------------------------------------------
#' @keywords internal
#' @noRd
b_star_k <- function(k, sigma_square, sigma_lambda_inv, theta_vector){
  theta_vector_k_square = theta_vector[k]^2
  theta_vector_k_square/(theta_vector_k_square + sigma_square * sigma_lambda_inv[k, k])
}

# (2) dsh_est ------------------------------------------------------------
#' @keywords internal
#' @noRd
dsh_est <- function(theta_vector, sigma_square, sigma_lambda_inv, p_plus_1){

  b_star_k <- sapply(1:p_plus_1, function(k) b_star_k(k, sigma_square, sigma_lambda_inv, theta_vector))

  est_DSh <- b_star_k * theta_vector

  return(as.vector(est_DSh))
}
