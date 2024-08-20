library(testthat)
library(Slab)
library(ggplot2)

# Generate synthetic data for testing
set.seed(123)
n <- 100 # Number of observations
p <- 50  # Number of variables
test_x <- matrix(rnorm(n * p), n, p)
beta <- matrix(rnorm(p + 1), p + 1, 1)
test_y <- cbind(1, test_x) %*% beta + rnorm(n, sd = 0.5) # Linear combination with noise

# estimation works with just x and y
result <- slab_shrink(test_x, test_y)

test_that("plot works with just estimation", {
  # plot all models wanted
  p <- plot.model(result)
  plot_data <- ggplot_build(p)$data[[1]]
  plot_data_SR_plot_1 <- plot_data[plot_data$group == 1, ]
  plot_data_SR_plot_2 <- plot_data[plot_data$group == 5, ]
  plot_data_SR_plot_3 <- plot_data[plot_data$group == 9, ]
  expect_equal(length(plot_data_SR_plot_1$y), 20)
  expect_equal(length(plot_data_SR_plot_2$y), 20)
  expect_equal(length(plot_data_SR_plot_3$y), 11)
  expect_equal(plot_data_SR_plot_1$y, as.vector(result$coefficients$SR)[1:20])
  expect_equal(plot_data_SR_plot_2$y, as.vector(result$coefficients$SR)[21:40])
  expect_equal(plot_data_SR_plot_3$y, as.vector(result$coefficients$SR)[41:51])

  plot_data_ST_plot_1 <- plot_data[plot_data$group == 2, ]
  plot_data_ST_plot_2 <- plot_data[plot_data$group == 6, ]
  plot_data_ST_plot_3 <- plot_data[plot_data$group == 10, ]
  expect_equal(length(plot_data_ST_plot_1$y), 20)
  expect_equal(length(plot_data_ST_plot_2$y), 20)
  expect_equal(length(plot_data_ST_plot_3$y), 11)
  expect_equal(plot_data_ST_plot_1$y, as.vector(result$coefficients$ST)[1:20])
  expect_equal(plot_data_ST_plot_2$y, as.vector(result$coefficients$ST)[21:40])
  expect_equal(plot_data_ST_plot_3$y, as.vector(result$coefficients$ST)[41:51])

  plot_data_DSH_plot_1 <- plot_data[plot_data$group == 3, ]
  plot_data_DSH_plot_2 <- plot_data[plot_data$group == 7, ]
  plot_data_DSH_plot_3 <- plot_data[plot_data$group == 11, ]
  expect_equal(length(plot_data_DSH_plot_1$y), 20)
  expect_equal(length(plot_data_DSH_plot_2$y), 20)
  expect_equal(length(plot_data_DSH_plot_3$y), 11)
  expect_equal(plot_data_DSH_plot_1$y, as.vector(result$coefficients$DSH)[1:20])
  expect_equal(plot_data_DSH_plot_2$y, as.vector(result$coefficients$DSH)[21:40])
  expect_equal(plot_data_DSH_plot_3$y, as.vector(result$coefficients$DSH)[41:51])

  plot_data_SH_plot_1 <- plot_data[plot_data$group == 4, ]
  plot_data_SH_plot_2 <- plot_data[plot_data$group == 8, ]
  plot_data_SH_plot_3 <- plot_data[plot_data$group == 12, ]
  expect_equal(length(plot_data_SH_plot_1$y), 20)
  expect_equal(length(plot_data_SH_plot_2$y), 20)
  expect_equal(length(plot_data_SH_plot_3$y), 11)
  expect_equal(plot_data_SH_plot_1$y, as.vector(result$coefficients$SH)[1:20])
  expect_equal(plot_data_SH_plot_2$y, as.vector(result$coefficients$SH)[21:40])
  expect_equal(plot_data_SH_plot_3$y, as.vector(result$coefficients$SH)[41:51])

  # plot specific models wanted
  p_SR_DSH <- plot.model(result, models = c("SR", "DSH"))
  expect_equal(nrow(p_SR_DSH$data), 102)
  plot_data <- ggplot_build(p_SR_DSH)$data[[1]]
  plot_data_SR_plot_1 <- plot_data[plot_data$group == 1, ]
  plot_data_SR_plot_2 <- plot_data[plot_data$group == 3, ]
  plot_data_SR_plot_3 <- plot_data[plot_data$group == 5, ]
  expect_equal(length(plot_data_SR_plot_1$y), 20)
  expect_equal(length(plot_data_SR_plot_2$y), 20)
  expect_equal(length(plot_data_SR_plot_3$y), 11)
  expect_equal(plot_data_SR_plot_1$y, as.vector(result$coefficients$SR)[1:20])
  expect_equal(plot_data_SR_plot_2$y, as.vector(result$coefficients$SR)[21:40])
  expect_equal(plot_data_SR_plot_3$y, as.vector(result$coefficients$SR)[41:51])

  plot_data_DSH_plot_1 <- plot_data[plot_data$group == 2, ]
  plot_data_DSH_plot_2 <- plot_data[plot_data$group == 4, ]
  plot_data_DSH_plot_3 <- plot_data[plot_data$group == 6, ]
  expect_equal(length(plot_data_DSH_plot_1$y), 20)
  expect_equal(length(plot_data_DSH_plot_2$y), 20)
  expect_equal(length(plot_data_DSH_plot_3$y), 11)
  expect_equal(plot_data_DSH_plot_1$y, as.vector(result$coefficients$DSH)[1:20])
  expect_equal(plot_data_DSH_plot_2$y, as.vector(result$coefficients$DSH)[21:40])
  expect_equal(plot_data_DSH_plot_3$y, as.vector(result$coefficients$DSH)[41:51])

  # Check that plot layout is correct
  layers <- lapply(p$layers, function(layer) class(layer$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_true("GeomLine" %in% layers)
  expect_equal(p$labels$title, "Comparison of Model Coefficients")
  expect_equal(p$labels$x, "Coefficient Index")
  expect_equal(p$labels$y, "Coefficient Value")

  # Check that plot will be splited to three plot
  expect_equal(length(ggplot_build(p)$layout$panel_params), 3)
  expect_equal(ggplot_build(p)$layout$panel_params[[1]]$x$breaks, c(0,5,10,15,NA,NA,NA,NA,NA,NA,NA))
  expect_equal(ggplot_build(p)$layout$panel_params[[2]]$x$breaks, c(NA,NA,NA,NA,20,25,30,35,NA,NA,NA))
  expect_equal(ggplot_build(p)$layout$panel_params[[3]]$x$breaks, c(NA,NA,NA,NA,NA,NA,NA,NA,40,45,50))

  expect_equal(length(ggplot_build(p)$layout$panel_params[[1]]$x$minor_breaks), 20)
  expect_equal(length(ggplot_build(p)$layout$panel_params[[2]]$x$minor_breaks), 20)
  expect_equal(length(ggplot_build(p)$layout$panel_params[[3]]$x$minor_breaks), 11)

  # Check that errors with wrong input
  expect_error(plot.model(result$coefficient),
               "Errors: Invalid input. Please provide valid model results.")
  empty_result <- list(coefficients = list(), residuals = list())
  expect_error(plot.model(empty_result),
               "Error: 'coefficients' is empty. Please provide valid model coefficients.")
  expect_warning(plot.model(result, models = c("SR", "OLS")))
})

# plot specific models wanted
result_ST_SH <- slab_shrink(test_x, test_y, models = c("ST", "SH"))

test_that("plot works with just estimation", {
  # plot all available models wanted
  p <- plot.model(result_ST_SH)
  expect_s3_class(p, "ggplot")
  plot_data <- ggplot_build(p)$data[[1]]

  expect_null(result_ST_SH$coefficients$SR)
  expect_null(result_ST_SH$coefficients$DSH)

  plot_data_ST_plot_1 <- plot_data[plot_data$group == 1, ]
  plot_data_ST_plot_2 <- plot_data[plot_data$group == 3, ]
  plot_data_ST_plot_3 <- plot_data[plot_data$group == 5, ]
  expect_equal(length(plot_data_ST_plot_1$y), 20)
  expect_equal(length(plot_data_ST_plot_2$y), 20)
  expect_equal(length(plot_data_ST_plot_3$y), 11)
  expect_equal(plot_data_ST_plot_1$y, as.vector(result_ST_SH$coefficients$ST)[1:20])
  expect_equal(plot_data_ST_plot_2$y, as.vector(result_ST_SH$coefficients$ST)[21:40])
  expect_equal(plot_data_ST_plot_3$y, as.vector(result_ST_SH$coefficients$ST)[41:51])

  plot_data_SH_plot_1 <- plot_data[plot_data$group == 2, ]
  plot_data_SH_plot_2 <- plot_data[plot_data$group == 4, ]
  plot_data_SH_plot_3 <- plot_data[plot_data$group == 6, ]
  expect_equal(length(plot_data_SH_plot_1$y), 20)
  expect_equal(length(plot_data_SH_plot_2$y), 20)
  expect_equal(length(plot_data_SH_plot_3$y), 11)
  expect_equal(plot_data_SH_plot_1$y, as.vector(result_ST_SH$coefficients$SH)[1:20])
  expect_equal(plot_data_SH_plot_2$y, as.vector(result_ST_SH$coefficients$SH)[21:40])
  expect_equal(plot_data_SH_plot_3$y, as.vector(result_ST_SH$coefficients$SH)[41:51])
  expect_equal(nrow(p$data), 102)

  # plot specific models wanted
  expect_error(plot.model(result_ST_SH, models = c("SR", "DSH")))
  expect_warning(plot.model(result_ST_SH, models = c("SR", "ST", "DSH")))

  # Check that plot layout is correct
  layers <- lapply(p$layers, function(layer) class(layer$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_true("GeomLine" %in% layers)
  expect_equal(p$labels$title, "Comparison of Model Coefficients")
  expect_equal(p$labels$x, "Coefficient Index")
  expect_equal(p$labels$y, "Coefficient Value")

  # Check that plot will be splited to three plot
  expect_equal(length(ggplot_build(p)$layout$panel_params), 3)
  expect_equal(ggplot_build(p)$layout$panel_params[[1]]$x$breaks, c(0,5,10,15,NA,NA,NA,NA,NA,NA,NA))
  expect_equal(ggplot_build(p)$layout$panel_params[[2]]$x$breaks, c(NA,NA,NA,NA,20,25,30,35,NA,NA,NA))
  expect_equal(ggplot_build(p)$layout$panel_params[[3]]$x$breaks, c(NA,NA,NA,NA,NA,NA,NA,NA,40,45,50))

  expect_equal(length(ggplot_build(p)$layout$panel_params[[1]]$x$minor_breaks), 20)
  expect_equal(length(ggplot_build(p)$layout$panel_params[[2]]$x$minor_breaks), 20)
  expect_equal(length(ggplot_build(p)$layout$panel_params[[3]]$x$minor_breaks), 11)

  # Check that errors with wrong input
  expect_error(plot.model(result$coefficient),
               "Errors: Invalid input. Please provide valid model results.")
  empty_result <- list(coefficients = list(), residuals = list())
  expect_error(plot.model(empty_result),
               "Error: 'coefficients' is empty. Please provide valid model coefficients.")
})

test_that("plot works with 12 coef in one group", {
  # plot all available models wanted
  p_group_12 <- plot.model(result, group = 12)
  expect_s3_class(p_group_12, "ggplot")

  # Check that plot layout is correct
  layers <- lapply(p_group_12$layers, function(layer) class(layer$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_true("GeomLine" %in% layers)
  expect_equal(p_group_12$labels$title, "Comparison of Model Coefficients")
  expect_equal(p_group_12$labels$x, "Coefficient Index")
  expect_equal(p_group_12$labels$y, "Coefficient Value")

  # Check that plot will be splited to three plot
  expect_equal(length(ggplot_build(p_group_12)$layout$panel_params), 5)
  expect_equal(ggplot_build(p_group_12)$layout$panel_params[[1]]$x$breaks, c(0,3,6,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  expect_equal(ggplot_build(p_group_12)$layout$panel_params[[2]]$x$breaks, c(NA,NA,NA,NA,12,15,18,21,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  expect_equal(ggplot_build(p_group_12)$layout$panel_params[[3]]$x$breaks, c(NA,NA,NA,NA,NA,NA,NA,NA,24,27,30,33,NA,NA,NA,NA,NA))
  expect_equal(ggplot_build(p_group_12)$layout$panel_params[[4]]$x$breaks, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,36,39,42,45,NA))
  expect_equal(ggplot_build(p_group_12)$layout$panel_params[[5]]$x$breaks, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,48))

  expect_equal(length(ggplot_build(p_group_12)$layout$panel_params[[1]]$x$minor_breaks), 12)
  expect_equal(length(ggplot_build(p_group_12)$layout$panel_params[[2]]$x$minor_breaks), 12)
  expect_equal(length(ggplot_build(p_group_12)$layout$panel_params[[3]]$x$minor_breaks), 12)
  expect_equal(length(ggplot_build(p_group_12)$layout$panel_params[[4]]$x$minor_breaks), 12)
  expect_equal(length(ggplot_build(p_group_12)$layout$panel_params[[5]]$x$minor_breaks), (51-4*12))

  # Check that errors with wrong input
  expect_error(plot.model(result$coefficient),
               "Errors: Invalid input. Please provide valid model results.")
  empty_result <- list(coefficients = list(), residuals = list())
  expect_error(plot.model(empty_result),
               "Error: 'coefficients' is empty. Please provide valid model coefficients.")
})


test_that("plot works with show_only parameter", {
  # plot all available models wanted
  want_only = c(0:5, 7:20, 30:33, 49:50)
  p_want_only <- plot.model(result, group = 12, show_only = want_only)
  expect_s3_class(p_want_only, "ggplot")

  # Check that plot layout is correct
  layers <- lapply(p_want_only$layers, function(layer) class(layer$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_true("GeomLine" %in% layers)
  expect_equal(p_want_only$labels$title, "Comparison of Model Coefficients")
  expect_equal(p_want_only$labels$x, "Coefficient Index")
  expect_equal(p_want_only$labels$y, "Coefficient Value")

  expect_equal(length(ggplot_build(p_want_only)$layout$panel_params), 3)
  expect_equal(ggplot_build(p_want_only)$layout$panel_params[[1]]$x$breaks, c(0:5, 7:12, rep(NA, length(want_only) - 12)))
  expect_equal(ggplot_build(p_want_only)$layout$panel_params[[2]]$x$breaks, c(rep(NA, 12), 13:20, 30:33,rep(NA, length(want_only) - 24)))
  expect_equal(ggplot_build(p_want_only)$layout$panel_params[[3]]$x$breaks, c(rep(NA, 24), 49:50))

  expect_equal(ggplot_build(p_want_only)$layout$panel_params[[1]]$x$minor_breaks, c(0:5, 7:12))
  expect_equal(ggplot_build(p_want_only)$layout$panel_params[[2]]$x$minor_breaks, c(13:20, 30:33))
  expect_equal(ggplot_build(p_want_only)$layout$panel_params[[3]]$x$minor_breaks, c(49:50))

  # plot all models with duplicated wanted
  want_duplicate = c(0:5, 7:20, 30:33, 49:50, 10:1)
  p_want_duplicate <- plot.model(result, group = 12, show_only = want_duplicate)
  expect_s3_class(p_want_duplicate, "ggplot")

  expect_equal(length(ggplot_build(p_want_duplicate)$layout$panel_params), 3)
  expect_equal(ggplot_build(p_want_duplicate)$layout$panel_params[[1]]$x$breaks, c(0:11, rep(NA, length(unique(want_duplicate)) - 12)))
  expect_equal(ggplot_build(p_want_duplicate)$layout$panel_params[[2]]$x$breaks, c(rep(NA, 12), 12:20, 30:32,rep(NA, length(unique(want_duplicate)) - 24)))
  expect_equal(ggplot_build(p_want_duplicate)$layout$panel_params[[3]]$x$breaks, c(rep(NA, 24), 33, 49:50))

  expect_equal(ggplot_build(p_want_duplicate)$layout$panel_params[[1]]$x$minor_breaks, c(0:11))
  expect_equal(ggplot_build(p_want_duplicate)$layout$panel_params[[2]]$x$minor_breaks, c(12:20, 30:32))
  expect_equal(ggplot_build(p_want_duplicate)$layout$panel_params[[3]]$x$minor_breaks, c(33, 49:50))

  want_out_range = c(0:5, 7:20, 30:33, 49:55)
  p_want_out_range <- plot.model(result, group = 12, show_only = want_out_range)
  expect_warning(plot.model(result, group = 12, show_only = want_out_range),
                 "Warning: The following indices are not in your coefficients and will be ignored: 51, 52, 53, 54, 55")

  expect_equal(length(ggplot_build(p_want_out_range)$layout$panel_params), 3)
  expect_equal(ggplot_build(p_want_out_range)$layout$panel_params[[1]]$x$breaks, c(0:5, 7:12, rep(NA, length(want_only) - 12)))
  expect_equal(ggplot_build(p_want_out_range)$layout$panel_params[[2]]$x$breaks, c(rep(NA, 12), 13:20, 30:33,rep(NA, length(want_only) - 24)))
  expect_equal(ggplot_build(p_want_out_range)$layout$panel_params[[3]]$x$breaks, c(rep(NA, 24), 49:50))

  expect_equal(ggplot_build(p_want_out_range)$layout$panel_params[[1]]$x$minor_breaks, c(0:5, 7:12))
  expect_equal(ggplot_build(p_want_out_range)$layout$panel_params[[2]]$x$minor_breaks, c(13:20, 30:33))
  expect_equal(ggplot_build(p_want_out_range)$layout$panel_params[[3]]$x$minor_breaks, c(49:50))

  want_out_range = c(51:55)
  expect_error(plot.model(result, group = 12, show_only = want_out_range),
               "Error: No valid indices provided in 'show_only'.")
})




