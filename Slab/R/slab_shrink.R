#' Slab and Shrinkage Generalised LASSO Estimator
#'
#' This function implements the Slab and Shrinkage Generalised LASSO Estimation
#' method for regression models.
#'
#' @param x Input matrix of dimension n x p, where n is the number of observations
#' and p is the number of variables. The matrix should not include an intercept
#' term as this will be handled internally if needed.
#' @param y Response variable vector with n observations.
#' @param u A numeric or numeric vector that represents the tuning parameter(s)
#' for regularization strength in the model. Defaults to 1. If a vector is provided,
#' the first element should be the tuning value for the intercept, followed by
#' tuning values for other coefficients.
#' @param lambda A numeric value, vector or NULL.
#' If NULL, the function will automatically determine the optimal lambda value
#' using cross-validation with cv.glmnet(). The optimal lambda, selected based
#' on the given criteria (typically the one minimizing prediction error), will be
#' used for subsequent regression models, and this value will be returned as the
#' best lambda.
#' If a lambda value is provided by the user, this specified value will be used
#' directly in the regression models without further modification, and the same
#' value will be returned.
#' @param nlambda The number of lambda values to be used if lambda is not specified.
#' This is typically used in conjunction with cross-validation to determine the
#' optimal lambda. Note that specifying both `nlambda` and `lambda` simultaneously
#' is not allowed and will result in an error.
#' @param models A character vector specifying which regression models to estimate.
#' Available choices are:
#' \itemize{
#'   \item{'SR': }{Slab Regression}
#'   \item{'ST': }{Stein Regression}
#'   \item{'DSH': }{Diagonal Shrinkage Regression}
#'   \item{'SH': }{Shrinkage Regression}
#' }
#' The specified models will be applied sequentially to the input data. If no models
#' are provided by the user, all four models will be processed by default, and the results
#' for each will be included in the output.

#' @param exclude An optional vector of column indices to be excluded from the
#' input matrix x before the model fitting. This allows for the omission of
#' variables that are not to be considered in the analysis.
#' @param ...
#'
#' @return
#' A list of results with class 'custom_lm' containing the following components:
#' \describe{
#'   \item{coefficients}{A list of coefficient vectors for each specified model (SR, ST, DSH, SH).}
#'   \item{fitted.values}{A list of fitted values for each specified model.}
#'   \item{residuals}{A list of residuals for each specified model.}
#'   \item{est}{The estimated parameter vector \code{theta}.}
#'   \item{cvm}{The cross-validation mean squared error.}
#'   \item{cvsd}{The cross-validation standard deviation.}
#'   \item{lambda_range}{The range of lambda values used in the cross-validation.}
#'   \item{u}{The regularization parameter vector \code{u}.}
#'   \item{lambda}{The best lambda value selected.}
#'   \item{sigma}{The estimated error variance.}
#'   \item{model}{The list of models specified.}
#' }
#'
#' @examples
#' Set a random seed for reproducibility
#' set.seed(123)
#'
#' # Define the number of observations and variables
#' n <- 100 # Number of observations
#' p <- 5   # Number of variables
#'
#' # Generate random data for predictors
#' test_x <- matrix(rnorm(n * p), n, p)
#'
#' # Generate coefficients including an intercept
#' beta <- matrix(rnorm(p + 1), p + 1, 1)
#'
#' # Calculate response variable with added noise
#' test_y <- cbind(1, test_x) %*% beta + rnorm(n, sd = 0.5)
#'
#' # Fit a model using a fictional slab_shrink function
#' result <- slab_shrink(test_x, test_y) # Estimation works with just x and y
#' print(result)
#'
#' # Fit models "ST" and "DSH" using a fictional slab_shrink function
#' result_ST_DSH <- slab_shrink(test_x, test_y, models = c("ST", "DSH"))
#' print(result_ST_DSH)
#'
#' # Fit a model using a fictional slab_shrink function with a specific u value
#' result_u_2 <- slab_shrink(test_x, test_y, u = 2)
#' print(result_u_2)
#' # Fit a model with a vector of u values including tuning for intercept
#' result_u_vector <- slab_shrink(test_x, test_y, u = c(0, 1:5))
#' print(result_u_vector)
#'
#' # Fit a model with a specific lambda value
#' result_lambda_10 <- slab_shrink(test_x, test_y, lambda = 10)
#' print(result_lambda_10)
#' # Fit a model with a vector of randomly generated lambda values
#' result_lambda_vector <- slab_shrink(test_x, test_y, lambda = runif(10, min = 0, max = 10))
#' print(result_lambda_vector)
#'
#' # Fit a model excluding the first and third predictors from x
#' # This might be useful to test the impact of specific variables on the model
#' result_exclude_1_3 <- slab_shrink(test_x, test_y, exclude = c(1,3))
#' print(result_exclude_1_3)
#'
#'
#' @importFrom glmnet glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom maotai sylvester
#' @export
slab_shrink <- function(x, y, u = 1, lambda = NULL, nlambda = 100, models = c("SR", "ST", "DSH", "SH"), exclude = NULL, ...){
  ################ Check Input Data ##################
  # Handle exclusion of specified columns
  if (!is.null(exclude)) {
    # Check if exclude contains NA or is 0
    if (anyNA(exclude) || any(exclude == 0)) {
      stop("Errors: Exclusion must not contain NA or zero values.")
    }

    if (!is.vector(exclude) || is.matrix(exclude) || is.list(exclude)) {
      stop("Errors: Exclusion must be a simple numerical or character vector.")
    }

    # Check the exclude type and handle it accordingly
    if (is.numeric(exclude)) {
      # Check that exclude can only be integers
      if (!is.integer(exclude) && !all(exclude == as.integer(exclude))) {
        stop("Errors: Exclusion must be an integer vector.")
      }
      # Check if the numeric index is within the allowed range
      if (any(exclude > ncol(x))) {
        stop("Errors: Exclusion indices are out of bounds.")
      }
      x <- x[, -exclude, drop = FALSE]
    } else if (is.character(exclude)) {
      # Check if the column name exists in the matrix column names
      if (!all(exclude %in% colnames(x))) {
        stop("Errors: All exclusion names must match column names in the data.")
      }
      # Exclude columns by name
      x <- x[, !colnames(x) %in% exclude, drop = FALSE]
    } else {
      # If exclude is neither a number nor a character
      stop("Errors: Exclusion must be either a numeric vector or a character vector.")
    }
  }

  # check matrix
  check_x_matrix(x) # Function at checker (1)
  # treat one-col matrix as vector
  check_y(y) # Function at checker (2)
  if (NROW(y) != NROW(x)) stop("Errors: 'y' and 'x' have unequal observations.")

  ################ Check Input Parameter ##################
  if (any(is.na(u))) stop("Errors: 'u' has NA values.")


  if (!is.null(lambda) && !missing(nlambda)) {
    warning("Warning: Both 'lambda' and 'nlambda' are provided. Only 'lambda' will be used; 'nlambda' will be ignored.")
  }

  ################ lambda ##################
  # Check lambda
  lambda_type <- check_lambda(lambda) # Function at checker (3)

  # Determine which glmnet function to use based on lambda_type
  if (lambda_type == "vector") {
    fit <- cvglmnet_est(x, y, lambda = lambda, alpha = 0, ...) # Function at global_helper (2)
    lambda_range <- fit$lambda
    cvm <- fit$cvm
    cvsd <- fit$cvsd
    lambda_best <- fit$lambda_min
    theta <- fit$est
    sigma <- fit$sigma
  } else if (lambda_type == "single_value") {
    fit <- glmnet(x, y, lambda = lambda, alpha = 0, family = "gaussian", ...)
    lambda_range <- 0
    cvm <- 0
    cvsd <- 0
    lambda_best <- lambda
    theta <- coef(fit)
    sigma <- sigma(fit)
  } else if (lambda_type == "NULL") {
    fit <- cvglmnet_est(x, y, alpha = 0, nlambda = nlambda, ...) # Function at global_helper (2)
    lambda_range <- fit$lambda
    cvm <- fit$cvm
    cvsd <- fit$cvsd
    lambda_best <- fit$lambda_min
    theta <- fit$est
    sigma <- fit$sigma
  } else if (lambda_type == 0) {
    fit <- OLS_est(x, y, nlambda = nlambda, ...) # Function at global_helper (1)
    lambda_range <- fit$lambda
    cvm <- fit$cvm
    cvsd <- fit$cvsd
    lambda_best <- fit$lambda_min
    theta <- fit$est
    sigma <- fit$sigma
  } else {
    stop("Unexpected lambda type returned.")
  }

  theta_vector <- as.vector(theta)
  sigma_square <- sigma^2
  X_tilde <- cbind(1, x)
  p_plus_1 <- (ncol(x)+1)
  I_p_plus_1 <- diag(1, p_plus_1)

  sigma_lambda <- Sigma_Lambda(X_tilde, lambda_best, I_p_plus_1)
  sigma_lambda_inv <- pd.solve(sigma_lambda)

  results <- list()
  results$call = match.call()
  results$coefficients <- list()

  if ("SR" %in% models) {
    if (!(is.numeric(u) && length(dim(u)) <= 1)) {
      stop("Errors: 'u' must be a numeric scalar or a numeric vector.")
    } else if (is.numeric(u) && length(u) == 1) {
      u <- rep(u, p_plus_1)
    } else if (is.numeric(u) && length(u) > 1) {
      if (length(u) != p_plus_1) {
        stop("Errors: The length of the vector u does not match the dimension of X_tilde.")
      }
      u <- u
    }
    sr_theta_vector <- sr_est(u, theta_vector, sigma_square, sigma_lambda_inv, p_plus_1, I_p_plus_1, lambda_best)
    sr_theta <- set_coefficient_names(sr_theta_vector, x)  # Set names
    results$coefficients$SR <- sr_theta
    sr_fitted.values  <-  X_tilde %*% sr_theta_vector
    results$fitted.values$SR <- sr_fitted.values
    sr_residuals <- y - sr_fitted.values
    results$residuals$SR <- sr_residuals
  }
  if ("ST" %in% models) {
    st_theta_vector <- st_est(theta_vector, sigma_square, sigma_lambda_inv)
    st_theta <- set_coefficient_names(st_theta_vector, x)  # Set names
    results$coefficients$ST <- st_theta
    st_fitted.values  <-  X_tilde %*% st_theta_vector
    results$fitted.values$ST <- st_fitted.values
    st_residuals <- y - st_fitted.values
    results$residuals$ST <- st_residuals
  }
  if ("DSH" %in% models) {
    dsh_theta_vector <- dsh_est(theta_vector, sigma_square, sigma_lambda_inv, p_plus_1)
    dsh_theta <- set_coefficient_names(dsh_theta_vector, x)  # Set names
    results$coefficients$DSH <- dsh_theta
    dsh_fitted.values  <-  X_tilde %*% dsh_theta_vector
    results$fitted.values$DSH <- dsh_fitted.values
    dsh_residuals <- y - dsh_fitted.values
    results$residuals$DSH <- dsh_residuals
  }
  if ("SH" %in% models) {
    sh_theta_vector <- sh_est(theta_vector, sigma_square, sigma_lambda_inv, p_plus_1)
    sh_theta <- set_coefficient_names(sh_theta_vector, x)  # Set names
    results$coefficients$SH <- sh_theta
    sh_fitted.values  <-  X_tilde %*% sh_theta_vector
    results$fitted.values$SH <- sh_fitted.values
    sh_residuals <- y - sh_fitted.values
    results$residuals$SH <- sh_residuals
  }

  results$est <- theta
  results$cvm <- cvm
  results$cvsd <- cvsd
  results$lambda_range <- lambda_range
  results$u <- u
  results$lambda <- lambda_best
  results$sigma <- sigma
  results$model <- models

  class(results) <- 'custom_lm'
  return(results)
}


