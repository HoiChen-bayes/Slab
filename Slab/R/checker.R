# CHECKERS ----------------------------------------------------------------
# 01. check_x_matrix : if input x is a matrix that can be processed to the estimators
# 02. check_y        : if input x is a correct format that can be processed to the estimators
# 03. check_lambda   : if input lambda can be processed to the estimators

# 01. check_x_matrix ---------------------------------------------------------
#' @keywords internal
#' @noRd
check_x_matrix <- function(x) {
  # Get the name of the input parameter
  input_name <- deparse(substitute(x))
  # Check if it is a matrix
  cond1 <- is.matrix(x)
  if (!cond1) {
    stop(sprintf("Errors: The input '%s' should be a matrix.", input_name))
  }
  cond2 <- nrow(x) == 0L
  if (cond2) {
    stop(sprintf("Errors: The input '%s' has 0 cases", input_name))
  }
  cond3 <- ncol(x) == 0L
  if (cond3) {
    stop(sprintf("Errors: The input '%s' has 0 variates", input_name))
  }
  # Check if a matrix has fewer columns than rows
  cond4 <- ncol(x) < nrow(x)
  if (!cond4) {
    stop(sprintf("Errors: The input '%s' should have more rows than columns.", input_name))
  }
  # Check if matrix contains infinite or NA values
  cond5 <- !(any(is.infinite(x)) || any(is.na(x)))
  if (!cond5) {
    stop(sprintf("Errors: The input '%s' should not contain infinite or NA values.", input_name))
  }
  # If all conditions are met, returns TRUE
  TRUE
}

# 02. check_y ----------------------------------------------------------
#' @keywords internal
#' @noRd

check_y <- function(x) {
  # Get the name of the input parameter
  input_name <- deparse(substitute(x))
  # Check if it is a matrix
  cond1 <- is.numeric(x) || is.vector(x) || is.matrix(x) || is.array(x)
  if (!cond1) {
    stop(sprintf("Errors: The input '%s' should be a matrix.", input_name))
  }
  cond2 <- NROW(x) == 0L
  if (cond2) {
    stop(sprintf("Errors: The input '%s' has 0 (non-NA) cases", input_name))
  }
  # Check if a matrix has fewer columns than rows
  cond3 <- NCOL(x) < NROW(x)
  if (!cond3) {
    stop(sprintf("Errors: The input '%s' should have more rows than columns.", input_name))
  }
  # Check if matrix contains infinite or NA values
  cond4 <- !(any(is.infinite(x)) || any(is.na(x)))
  if (!cond4) {
    stop(sprintf("Errors: The input '%s' should not contain infinite or NA values.", input_name))
  }
  con5 <- NCOL(x) == 1
  if(!con5) {
    stop(sprintf("Errors: The input '%s' should have only one dimension.", input_name))
  }
  # If all conditions are met, returns TRUE with this message
  message(sprintf("Note: The input '%s' should be in gaussian distribution, so our method can proviod accurate results. Otherwise the results might not be correct.", input_name))
}


# 03. check_lambda ---------------------------------------------------------
#' @keywords internal
#' @noRd
check_lambda <- function(x) {
  # Get the name of the input parameter
  input_name <- deparse(substitute(x))

  # Checks whether user has input x or not
  if (is.null(x)) {
    return("NULL")
  }

  # Checks whether x is a vector or a single number
  if(!is.vector(x, mode = "numeric")) {
    stop(sprintf("Errors: The input '%s' should be a numeric vector or a single value.", input_name))
  }

  # Checks if x is a vector
  if (length(x) > 1) {
    # Checks if x has na value
    if (any(is.na(x))) {
      stop(sprintf("Errors: The input '%s' has NA values.", input_name))
    }
    # Checks if x has negative value
    if (any(x < 0)) {
      stop(sprintf("Errors: The input '%s' has negative values.", input_name))
    } else {
    # If x is a vector with length greater than 1, for cv.glmnet
    message(sprintf("Note: This method will use cv.glmnet and will try to find the best lambda from the vector '%s'.", input_name))
    return("vector")  # Using A to represent multiple lambda values
    }
  }
  # Checks if x is a single value
  else if (length(x) == 1) {
    if (x == 0) {
      message("Note: This method will use lm(), which is OLS as lambda is 0")
      return(0)
    } else {
      # If x is a single numeric value or a vector of length 1
      warning(sprintf("Warning: This method will use glmnet, not cv.glmnet. Therefore, the coefficient might not be optimal as no cross-validation will be conducted to find the best lambda for '%s'.", input_name))
      return("single_value")  # Use B to represent a single lambda value
    }
  } else {
    stop(sprintf("Errors: The input '%s' should be a numeric vector or a single value.", input_name))
  }



}


