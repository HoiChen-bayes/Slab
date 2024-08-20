library(testthat)
library(Slab)
library(ggplot2)

# Generate synthetic data for testing
set.seed(123)
n <- 100 # Number of observations
p <- 5  # Number of variables
test_x <- matrix(rnorm(n * p), n, p)
beta <- matrix(rnorm(p + 1), p + 1, 1)
test_y <- cbind(1, test_x) %*% beta + rnorm(n, sd = 0.5) # Linear combination with noise

test_that("plot works with just estimation", {
  result <- slab_shrink(test_x, test_y)
  p <- plot.cv_model(result)
  p_build <- ggplot_build(p)

  log_lambda_range <- log(result$lambda_range)
  log_lambda_range[is.infinite(log_lambda_range)] <- NA
  expect_equal(p_build$data[[1]]$x, log_lambda_range[order(log_lambda_range, na.last = TRUE)])

  expect_equal(p_build$data[[1]]$y, result$cvm[order(log_lambda_range, na.last = TRUE)])

  expect_equal(p_build$data[[3]]$ymin, result$cvm - result$cvsd)
  expect_equal(p_build$data[[3]]$ymax, result$cvm + result$cvsd)

  expect_equal(p_build$data[[4]]$xintercept, log(result$lambda))
})

test_that("plot works with just lambda vector", {
  result <- slab_shrink(test_x, test_y, lambda = runif(10, min = 0, max = 20))
  p <- plot.cv_model(result)
  p_build <- ggplot_build(p)

  log_lambda_range <- log(result$lambda_range)
  log_lambda_range[is.infinite(log_lambda_range)] <- NA
  expect_equal(p_build$data[[1]]$x, log_lambda_range[order(log_lambda_range, na.last = TRUE)])

  expect_equal(p_build$data[[1]]$y, result$cvm[order(log_lambda_range, na.last = TRUE)])

  expect_equal(p_build$data[[3]]$ymin, result$cvm - result$cvsd)
  expect_equal(p_build$data[[3]]$ymax, result$cvm + result$cvsd)

  expect_equal(p_build$data[[4]]$xintercept, log(result$lambda))
})

test_that("plot works with single value of lambda", {
  result <- slab_shrink(test_x, test_y, lambda = 0)
  p <- plot.cv_model(result)
  p_build <- ggplot_build(p)

  expect_equal(p_build$data[[1]]$x, ggplot_build(plot.model(result))$data[[1]]$x)
  expect_equal(p_build$data[[1]]$y, ggplot_build(plot.model(result))$data[[1]]$y)

  layers <- lapply(p$layers, function(layer) class(layer$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_true("GeomLine" %in% layers)
  expect_equal(p$labels$title, "Comparison of Model Coefficients")
  expect_equal(p$labels$x, "Coefficient Index")
  expect_equal(p$labels$y, "Coefficient Value")
})

test_that("plot works with nlambda = 1000", {
  result <- slab_shrink(test_x, test_y, nlambda = 1000)
  p <- plot.cv_model(result)
  p_build <- ggplot_build(p)

  log_lambda_range <- log(result$lambda_range)
  log_lambda_range[is.infinite(log_lambda_range)] <- NA
  expect_equal(p_build$data[[1]]$x, log_lambda_range[order(log_lambda_range, na.last = TRUE)])
  expect_equal(length(p_build$data[[1]]$x), 1001)
  expect_equal(p_build$data[[1]]$y, result$cvm[order(log_lambda_range, na.last = TRUE)])

  expect_equal(p_build$data[[3]]$ymin, result$cvm - result$cvsd)
  expect_equal(p_build$data[[3]]$ymax, result$cvm + result$cvsd)

  expect_equal(p_build$data[[4]]$xintercept, log(result$lambda))

  expect_equal(p$labels$x, expression(log(lambda)))
  expect_equal(p$labels$y, "CV Error")
})





