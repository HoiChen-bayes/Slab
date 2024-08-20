# helper functions for stein -------------------------------------
# (1) OLS_est      : Ordinary Least Square Estimation
# (2) cvglmnet_est : (when data has multicollinearity and estimation add lambda)
# (3) Sigma_Lambda : ridge penalty

# (1) OLS_est ------------------------------------------------------------
#' @keywords internal
#' @noRd
OLS_est <- function(data_X, data_Y, nlambda = nlambda, ...) {
  rank_X = qr(data_X)$rank
  n_col_X = ncol(data_X)
  if (rank_X < n_col_X) {
    warning("Warning: The input data has multicollinearity, so we will use not cv.glmnet to solve the problem. Therefore, the coefficient is based on cv.glmnet.")
    result_RR = cvglmnet_est(data_X, data_Y, nlambda = nlambda, ...)
    return(result_RR)
  } else {
    lm_fit = lm(data_Y ~ data_X)
    est = as.vector(coef(lm_fit))
    sigma = summary(lm_fit)$sigma
    return(list(est = est, sigma = sigma, lambda_min = 0))
  }
}


# (2) cvglmnet_est (when data has multicollinearity and estimation add lambda) ------------------------------------------------------------
#' @keywords internal
#' @noRd
cvglmnet_est <- function(data_X, data_Y, lambda = NULL, nlambda = nlambda, glmnet_grid = c(0, 10^seq(-6,6,length = nlambda)), alpha = 0, intercept = TRUE, ...) {
  if (is.null(lambda)){ #user did not enter lambda and cv.glmnet find the best lambda
    cvglmnet_fit = cv.glmnet(data_X, data_Y, alpha = alpha, lambda = glmnet_grid, intercept = intercept, ...)
  } else { #user enter lambda vector that more than one value of lambda
    cvglmnet_fit = cv.glmnet(data_X, data_Y, alpha = alpha, lambda = lambda, intercept = intercept, ...)
  }

  lambda = cvglmnet_fit$lambda
  cvm = cvglmnet_fit$cvm
  cvsd = cvglmnet_fit$cvsd
  lambda_min_cvglmnet = cvglmnet_fit$lambda.min
  theta_cvglmnet <- as.vector(coef(cvglmnet_fit, s = "lambda.min"))

  fitted_values = predict(cvglmnet_fit, newx = data_X, s = lambda_min_cvglmnet)
  residuals = data_Y - fitted_values
  sigma_cvglmnet = sqrt(sum(residuals^2) / (length(data_Y) - ncol(data_X) - 1))

  return(list(est = theta_cvglmnet, sigma = sigma_cvglmnet, lambda_min = lambda_min_cvglmnet, lambda = lambda, cvm = cvm, cvsd = cvsd))
}


# (3) Sigma_Lambda ------------------------------------------------------------
#' @keywords internal
#' @import mnormt
#' @noRd
Sigma_Lambda <- function(sl.XXX.temp, lambda_val, I_p_plus_1) {
  sigma_lambda <- t(sl.XXX.temp) %*% sl.XXX.temp + lambda_val * I_p_plus_1
  return(sigma_lambda)
}
