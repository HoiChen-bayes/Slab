# output function -------------------------------------
# (1) $.sr_est     :
# (2) print.sr_est : the output to print


# (1) set_coefficient_names ------------------------------------------------------------
#' @keywords internal
#' @noRd
set_coefficient_names <- function(theta, x) {
  if (is.null(colnames(x))) {
    colnames(x) <- paste("x", 1:ncol(x), sep = "")
  }

  col_names <- c("(Intercept)", colnames(x))
  names(theta) <- col_names
  return(theta)
}


# (2) print(results) ------------------------------------------------------------
#' @keywords internal
#' @noRd
print.custom_lm <- function(object) {
 if (length(object$coefficients) > 0) {
    for (model_name in names(object$coefficients)) {
      cat("\nCall:\n")
      print(object$call)

      cat("Coefficient of Model ", model_name, ":\n", sep = "")
      print(object$coefficients[[model_name]], digits = 4)
    }
  } else {
    cat("No coefficients available.\n")
  }

  invisible(object)
}


# (3) summary(results) ------------------------------------------------------------
#' @keywords internal
#' @noRd
summary.custom_lm <- function(object) {
  lapply(object, function(model) {
    list(
      coefficients = model$coefficients
    )
  })
}
