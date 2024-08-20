#' @title Plot Cross-Validation Coefficients
#'
#' @description This function plots the cross-validation error against the log of lambda values
#' for a given glmnet model. It visualizes the performance of the model across different levels
#' of regularization.
#'
#' @param result_model A model object returned by cv.glmnet containing cross-validation results.
#' This object should include cvm (cross-validation error mean), cvsd (cross-validation error standard deviation),
#' lambda_range (range of lambda values), and lambda (optimal lambda value).
#'
#' @return This function does not return a value. It generates a plot.
#'
#' @examples
#' # Same data generate process as slab_shrink
#' set.seed(123)
#' n <- 100 # Number of observations
#' p <- 50  # Number of variables
#' test_x <- matrix(rnorm(n * p), n, p)
#' beta <- matrix(rnorm(p + 1), p + 1, 1)
#' test_y <- cbind(1, test_x) %*% beta + rnorm(n, sd = 0.5)
#'
#' # Fit a model using a fictional slab_shrink function
#' result <- slab_shrink(test_x, test_y) # Estimation works with just x and y
#'
#' # Plot the model using default settings
#' plot.cv_model(result)
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal ggtitle xlab ylab
#' @importFrom tidyr pivot_longer
#'
#' @export plot.cv_model
plot.cv_model <- function(result_model) {
  cvm <- result_model$cvm
  cvsd <- result_model$cvsd
  lambda_range <- result_model$lambda_range
  lambda <- result_model$lambda

  log_lambda_range <- log(lambda_range)
  log_lambda <- log(lambda)

  if (all(cvm == 0)) {
    message("Since cvm contains only zero values, it indicates that cross-validation did not yield multiple λ values. Therefore, coefficients are obtained based on a single λ value. We are using plot_model() to visualize the results.")
    plot.model(result_model)
  } else {
    if (any(lambda_range == 0)) {
      message("Since one or more λ values are zero, their log values are -Inf, and they will not be displayed in the plot.")
      log_lambda_range[lambda_range == 0] <- NA  # Replace -Inf with NA to avoid plotting issues
    }

    df <- data.frame(log_lambda_range, cvm, cvsd)
    p <- ggplot(df, aes(x = log_lambda_range, y = cvm)) +
      geom_line() +
      geom_point(color = "blue") +
      geom_errorbar(aes(ymin = cvm - cvsd, ymax = cvm + cvsd), width = 0.005, color = "grey") +
      geom_vline(xintercept = log_lambda, color = "red", linetype = "dashed") +
      geom_text(aes(x = log_lambda + 0.1, y = max(cvm), label = sprintf("%.2f", log_lambda)),
                vjust = -1, hjust = -0.1, color = "red") +
      labs(x = expression(log(lambda)), y = "CV Error", title = "Cross-Validation Error vs. Lambda") +
      theme_minimal()

    return(p)
  }
}
