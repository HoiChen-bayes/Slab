#' Plot Model Coefficients
#'
#' This function visualizes the coefficients from the provided model results.
#' It supports selection of specific models to display, grouping of coefficients
#' for cleaner visualization, and focusing on specific coefficients through
#' the `show_only` parameter. Coefficients can be grouped into multiple charts
#' to avoid cluttered plots.
#'
#' @param model_results A list containing the model results, which must include
#'        a named list of coefficients. Each element of the list should correspond
#'        to a different model.
#' @param models Optional vector of model names to display. If NULL, all models
#'        in `model_results` are used. If specified, only coefficients from
#'        these models are plotted.
#' @param group The maximum number of coefficients to display per plot. If the
#'        number of coefficients or specified `show_only` indices exceeds this
#'        number, the coefficients will be split across multiple plots.
#'        Defaults to 20. If `group` is greater than 20, it will be set to 20
#'        to maintain plot readability.
#' @param show_only Optional numeric vector specifying the indices of the
#'        coefficients to display. This allows for focusing on specific
#'        coefficients. If not NULL, only the specified indices are shown.
#'        If `show_only` contains more indices than `group`, coefficients
#'        will be distributed across multiple plots according to `group`.
#'
#' @return A ggplot object displaying the specified or all coefficients.
#'         The plot includes options for interactivity, such as zooming
#'         and panning, if displayed in an interactive R environment.
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
#' plot.model(result)
#'
#' # Plot the model for specific models "SR" and "ST"
#' plot.model(result, models = c("SR", "ST"))
#'
#' # Plot the model with groups of 15 coefficients each
#' plot.model(result, group = 15)
#'
#' # Plot the model showing only specified coefficient indices
#' plot.model(result, show_only = c(1:12, 30:40, 45:50))
#'
#' # Expect an error when out-of-range indices are provided in show_only
#' # This line is meant to show error handling and should cause an error when run
#' tryCatch({
#'   plot.model(result, show_only = c(51:60)),
#'   error = function(e) cat("Expected error for out-of-range show_only: ", e$message, "\n")
#' })
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal ggtitle xlab ylab
#' @importFrom tidyr pivot_longer
#'
#' @export plot.model
plot.model <- function(model_results, models = NULL, group = 20, show_only = NULL) {

  # Extract coefficients
  if (is.null(model_results$coefficients)) {
    stop("Errors: Invalid input. Please provide valid model results.")
  }
  if (length(model_results$coefficients) == 0) {
    stop("Error: 'coefficients' is empty. Please provide valid model coefficients.")
  }

  coefficients <- model_results$coefficients

  # Select the model to draw
  if (!is.null(models)) {
    missing_models <- models[!models %in% names(coefficients)]
    if (length(missing_models) > 0) {
      warning("Warning: The following models are not in the model_results and will be ignored: ",
              paste(missing_models, collapse = ", "))
    }
    coefficients <- coefficients[names(coefficients) %in% models]
  }

  # Create a data frame of coefficients and convert to long format
  if (length(coefficients) == 0) {
    stop("Error: No valid models found in the coefficients.")
  }

  coef_df <- data.frame(coefficients)
  print("Data frame of coefficients:")
  print(coef_df)

  coef_df$Index <- 0:(nrow(coef_df)-1)
  coef_long <- pivot_longer(coef_df, cols = -Index, names_to = "Model", values_to = "Coefficient")
  coef_long$Model <- factor(coef_long$Model, levels = c("SR", "ST", "DSH", "SH"))

  # Apply default grouping if not using show_only
  if (group > 20) {
    group <- 20
    warning("group cannot be greater than 20. It has been set to 20 to avoid messy visualizations.")
  }

  if (!is.null(show_only)) {
    # Make sure show_only is a numeric type
    if (is.numeric(show_only)) {
      # Remove duplicates and check which values are not in coef_df$Index
      show_only <- sort(unique(show_only))
      invalid_indices <- show_only[!show_only %in% coef_df$Index]

      # If there are illegal indexes, warn and remove them
      if (length(invalid_indices) > 0) {
        warning("Warning: The following indices are not in your coefficients and will be ignored: ",
                paste(invalid_indices, collapse = ", "))
        # Preserve valid show_only values
        show_only <- show_only[show_only %in% coef_df$Index]
      }

      # Check if show_only is empty
      if (length(show_only) == 0) {
        stop("Error: No valid indices provided in 'show_only'.")
      }

      # Filter coef_long using a valid show_only index
      coef_long <- coef_long[coef_long$Index %in% show_only, ]

      # Automatically group by group parameter
      if (length(show_only) > group) {
        coef_long$Group <- rep((seq_along(show_only) - 1) %/% group + 1, each = length(unique(coef_long$Model)))
      } else {
        coef_long$Group <- 1 # All coefficients are in one group
      }
    } else {
      stop("Error: 'show_only' must be a numeric vector.")
    }
  } else {
    coef_long$Group <- (coef_long$Index) %/% group + 1
  }

  # Plot coefficients
  p_coef <- ggplot(coef_long, aes(x = Index, y = Coefficient, color = Model, group = interaction(Model, Group))) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    ggtitle("Comparison of Model Coefficients") +
    xlab("Coefficient Index") +
    ylab("Coefficient Value") +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12)
    ) +
    facet_wrap(~ Group, ncol = 1, scales = "free_x")


  if (!is.null(show_only)) {
    # 直接设置自定义的刻度
    p_coef <- p_coef + scale_x_continuous(breaks = show_only, minor_breaks = show_only, expand = expansion(mult = c(0.01, 0.01)))
  } else {
    # 设置默认的刻度
    p_coef <- p_coef + scale_x_continuous(
      breaks = if (length(coef_df$Index) < 5) {
        seq(min(coef_df$Index), max(coef_df$Index), by = 1)
      } else {
        seq(min(coef_df$Index), max(coef_df$Index), by = (group / 4))
      },
      minor_breaks = seq(min(coef_df$Index), max(coef_df$Index), by = 1, ),
      expand = expansion(mult = c(0.01, 0.01))
    )
  }

  # p_coef <- p_coef + list(...)

  return(p_coef)

  # # Check if residual plot is needed
  # if (residuals) {
  #   # Extract residual data
  #   residuals_data <- model_results$residuals
  #   if (!is.null(models)) {
  #     residuals_data <- residuals_data[names(residuals_data) %in% models]
  #   }
  #
  #   # Create a data frame of residuals and convert to long format
  #   res_df <- data.frame(residuals_data)
  #   res_df$Index <- 1:nrow(res_df)
  #   res_long <- pivot_longer(res_df, cols = -Index, names_to = "Model", values_to = "Residuals")
  #
  #   # Draw residual graph
  #   p_res <- ggplot(res_long, aes(x = Index, y = Residuals, color = Model, group = Model, ...)) +
  #     geom_line() +
  #     geom_point() +
  #     theme_minimal() +
  #     labs(x = "Observation Index", y = "Residuals", title = "Residuals by Model")
  #   plots$residuals_plot = p_res
  # }
  #
  # return(plots)
}
