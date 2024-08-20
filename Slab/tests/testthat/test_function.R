library(testthat)
library(Slab)

# Generate synthetic data for testing
set.seed(123)
n <- 100 # Number of observations
p <- 5  # Number of variables
test_x <- matrix(rnorm(n * p), n, p)
beta <- matrix(rnorm(p + 1), p + 1, 1)
test_y <- cbind(1, test_x) %*% beta + rnorm(n, sd = 0.5) # Linear combination with noise

test_that("estimation works with just x and y", {
  result <- slab_shrink(test_x, test_y)
  expect_true(is.list(result))

  # Check that coefficients is within the specified range
  expect_true("coefficients" %in% names(result))
  expect_equal(length(result$coefficients$SR), 6)
  expect_equal(length(result$coefficients$ST), 6)
  expect_equal(length(result$coefficients$DSH), 6)
  expect_equal(length(result$coefficients$SH), 6)

  # Check that lambda is within the specified range
  expect_true("lambda" %in% names(result))
  lambda_values <- c(0, 10^seq(-6, 6, length = 100))
  expect_true(result$lambda %in% lambda_values)

  # Check that sigma is within 0.25 of 0.5
  expect_true("sigma" %in% names(result))
  expect_true(result$sigma >= 0.25 && result$sigma <= 0.75)

  # Check that model is one of the specified types
  expect_true("model" %in% names(result))
  valid_models <- c("SR", "ST", "DSH", "SH")
  expect_true(all(result$model %in% valid_models))

  # Check that u is within the specified range
  expect_true("u" %in% names(result))
  expect_true(all(result$u == 1))
})


test_that("y and x have unequal observations", {
  # Check that y with unequal observations
  test_y_with_101_obs <- matrix(rnorm(101), ncol = 1)
  expect_error(slab_shrink(test_x, test_y_with_101_obs),
               "Errors: 'y' and 'x' have unequal observations.")

  # Check that y with two-dimensional observations
  test_y_with_2_col <- matrix(rnorm(200), ncol = 2)
  expect_error(slab_shrink(test_x, test_y_with_2_col),
               "Errors: The input 'y' should have only one dimension.")

  # Check that x with unequal observations
  test_x_with_101_obs <- matrix(rnorm(101 * 5), 101, 5)
  expect_error(slab_shrink(test_x_with_101_obs, test_y),
               "Errors: 'y' and 'x' have unequal observations.")

  # Check that x with more covariates than observations
  test_x_with_more_covariates <- matrix(rnorm(5 * 5), 5, 5) # 6 covariates including intercept > 5 observations
  expect_error(slab_shrink(test_x_with_more_covariates, test_y),
               "Errors: The input 'x' should have more rows than columns.")
})


test_that("estimation works with u equal to different number or a vector", {
  # Check that u is not 1
  result <- slab_shrink(test_x, test_y, u = 10)
  expect_true("u" %in% names(result))
  expect_true(all(result$u == 10))

  # Check that u is a vector
  result <- slab_shrink(test_x, test_y, u = c(1,2,3,4,5,6))
  expect_true("u" %in% names(result))
  expect_true(is.vector(result$u))
  expect_true(all(result$u == c(1, 2, 3, 4, 5, 6)))

  # Check that u has NA value
  expect_error(slab_shrink(test_x, test_y, u = c(1, NA, 3)),
               "Errors: 'u' has NA values")

  # Check that u is a matrix
  expect_error(slab_shrink(test_x, test_y, u = matrix(1:6, nrow = 2, ncol = 3)),
               "Errors: 'u' must be a numeric scalar or a numeric vector.")

  # Check that u with unequal dimensions
  expect_error(slab_shrink(test_x, test_y, u = c(1, 2, 3)),
               "Errors: The length of the vector u does not match the dimension of X_tilde.")
})


test_that("estimation works with different lambdas", {
  # Check that lambda = 0
  result_lambda_0 <- slab_shrink(test_x, test_y, lambda = 0)
  expect_equal(length(result_lambda_0$coefficients$SR), 6)
  expect_equal(length(result_lambda_0$coefficients$ST), 6)
  expect_equal(length(result_lambda_0$coefficients$DSH), 6)
  expect_equal(length(result_lambda_0$coefficients$SH), 6)
  expect_true(result_lambda_0$lambda == 0)

  # Check that lambda = 1
  result_lambda_1 <- slab_shrink(test_x, test_y, lambda = 1)
  expect_equal(length(result_lambda_1$coefficients$SR), 6)
  expect_equal(length(result_lambda_1$coefficients$ST), 6)
  expect_equal(length(result_lambda_1$coefficients$DSH), 6)
  expect_equal(length(result_lambda_1$coefficients$SH), 6)
  expect_true(result_lambda_1$lambda == 1)

  # Check coefficients are different when lambda = 1 and 0
  expect_false(all(result_lambda_0$coefficients$SR == result_lambda_1$coefficients$SR),
               info = "Coefficients for SR should differ between lambda = 0 and lambda = 1")
  expect_false(all(result_lambda_0$coefficients$ST == result_lambda_1$coefficients$ST),
               info = "Coefficients for ST should differ between lambda = 0 and lambda = 1")
  expect_false(all(result_lambda_0$coefficients$DSH == result_lambda_1$coefficients$DSH),
               info = "Coefficients for DSH should differ between lambda = 0 and lambda = 1")
  expect_false(all(result_lambda_0$coefficients$SH == result_lambda_1$coefficients$SH),
               info = "Coefficients for SH should differ between lambda = 0 and lambda = 1")

  # Check that lambda is a range that user want to find the best lambda from the range
  lambda_range <- c(0, 10^seq(-1,1,length = 100))
  result_lambda_range <- slab_shrink(test_x, test_y, lambda = lambda_range)
  expect_equal(length(result_lambda_range$lambda), 1)
  expect_true(result_lambda_range$lambda %in% lambda_range,
              info = "Lambda should be within the specified range given")

  # Check that lambda is a vector that user want to find the best lambda from the vector
  lambda_vector <- c(0, 0.33, 1, 2.5, 5, 10, 30, 75, 100)
  result_lambda_vector <- slab_shrink(test_x, test_y, lambda = lambda_vector)
  expect_equal(length(result_lambda_vector$lambda), 1)
  expect_true(result_lambda_vector$lambda %in% lambda_vector,
              info = "Lambda should be within the specified vector given")

  # Check that lambda has NA value
  lambda_na <- c(0, 0.33, NA, 2.5, 5, 10, 30, 75, 100)
  expect_error(slab_shrink(test_x, test_y, lambda = lambda_na),
               "Errors: The input 'lambda' has NA values.")

  # Check that lambda has negative value
  lambda_neg <- c(0, 0.33, -1, 2.5, 5, 10, 30, 75, 100)
  expect_error(slab_shrink(test_x, test_y, lambda = lambda_neg),
               "Errors: The input 'lambda' has negative values.")

  # Check that lambda is a matrix
  lambda_matrix <- matrix(rnorm(100), ncol = 1)
  expect_error(slab_shrink(test_x, test_y, lambda = lambda_matrix),
               "Errors: The input 'lambda' should be a numeric vector or a single value.")

  # Check that lambda is empty
  lambda_empty <- c()
  result_lambda_empty <- slab_shrink(test_x, test_y, lambda = lambda_empty)
  expect_true(result_lambda_empty$lambda >= 0)
  expect_equal(length(result_lambda_empty$lambda), 1)
})

test_that("estimation works with different nlambdas", {
  # Check that nlambda = 10
  result_nlambda_10 <- slab_shrink(test_x, test_y, nlambda = 10)
  expect_equal(length(result_nlambda_10$lambda), 1)
  lambda_values <- c(0, 10^seq(-6, 6, length = 100))
  expect_true(result_nlambda_10$lambda %in% lambda_values)

  # Check that lambda and nlambda cannot exist at the same time
  expect_warning(slab_shrink(test_x, test_y, lambda = 1, nlambda = 10),
                 "Warning: Both 'lambda' and 'nlambda' are provided. Only 'lambda' will be used; 'nlambda' will be ignored.")

  # Check that lambda and nlambda are provided will only use lambda
  result_lambda_nlambda <- slab_shrink(test_x, test_y, lambda = 1, nlambda = 10)
  result_lambda_1 <- slab_shrink(test_x, test_y, lambda = 1)
  expect_true(all(result_lambda_nlambda$lambda == result_lambda_1$lambda))
  expect_true(all(result_lambda_nlambda$coefficients$SR == result_lambda_1$coefficients$SR))
  expect_true(all(result_lambda_nlambda$coefficients$ST == result_lambda_1$coefficients$ST))
  expect_true(all(result_lambda_nlambda$coefficients$DSH == result_lambda_1$coefficients$DSH))
  expect_true(all(result_lambda_nlambda$coefficients$SH == result_lambda_1$coefficients$SH))
})


test_that("estimation works with different models", {
  # Check that only SR is wanted
  result_SR <- slab_shrink(test_x, test_y, models = "SR")
  expect_equal(length(result_SR$coefficients$SR), 6)
  expect_equal(length(result_SR$coefficients$ST), 0)
  expect_equal(length(result_SR$coefficients$DSH), 0)
  expect_equal(length(result_SR$coefficients$SH), 0)
  expect_true(!is.null(result_SR$coefficients$SR))
  expect_null(result_SR$coefficients$ST)
  expect_null(result_SR$coefficients$DSH)
  expect_null(result_SR$coefficients$SH)

  # Check that only ST is wanted
  result_ST <- slab_shrink(test_x, test_y, models = "ST")
  expect_equal(length(result_ST$coefficients$SR), 0)
  expect_equal(length(result_ST$coefficients$ST), 6)
  expect_equal(length(result_ST$coefficients$DSH), 0)
  expect_equal(length(result_ST$coefficients$SH), 0)
  expect_null(result_ST$coefficients$SR)
  expect_true(!is.null(result_ST$coefficients$ST))
  expect_null(result_ST$coefficients$DSH)
  expect_null(result_ST$coefficients$SH)

  # Check that only DSH is wanted
  result_DSH <- slab_shrink(test_x, test_y, models = "DSH")
  expect_equal(length(result_DSH$coefficients$SR), 0)
  expect_equal(length(result_DSH$coefficients$ST), 0)
  expect_equal(length(result_DSH$coefficients$DSH), 6)
  expect_equal(length(result_DSH$coefficients$SH), 0)
  expect_null(result_DSH$coefficients$SR)
  expect_null(result_DSH$coefficients$ST)
  expect_true(!is.null(result_DSH$coefficients$DSH))
  expect_null(result_DSH$coefficients$SH)

  # Check that only SH is wanted
  result_SH <- slab_shrink(test_x, test_y, models = "SH")
  expect_equal(length(result_SH$coefficients$SR), 0)
  expect_equal(length(result_SH$coefficients$ST), 0)
  expect_equal(length(result_SH$coefficients$DSH), 0)
  expect_equal(length(result_SH$coefficients$SH), 6)
  expect_null(result_SH$coefficients$SR)
  expect_null(result_SH$coefficients$ST)
  expect_null(result_SH$coefficients$DSH)
  expect_true(!is.null(result_SH$coefficients$SH))

  # Check that two models are wanted
  result_SR_DSH <- slab_shrink(test_x, test_y, models = c("SR","DSH"))
  expect_equal(length(result_SR_DSH$coefficients$SR), 6)
  expect_equal(length(result_SR_DSH$coefficients$ST), 0)
  expect_equal(length(result_SR_DSH$coefficients$DSH), 6)
  expect_equal(length(result_SR_DSH$coefficients$SH), 0)
  expect_true(!is.null(result_SR_DSH$coefficients$SR))
  expect_null(result_SR_DSH$coefficients$ST)
  expect_true(!is.null(result_SR_DSH$coefficients$DSH))
  expect_null(result_SR_DSH$coefficients$SH)

  # Check that three models are wanted
  result_SR_ST_SH <- slab_shrink(test_x, test_y, models = c("SR","ST","SH"))
  expect_equal(length(result_SR_ST_SH$coefficients$SR), 6)
  expect_equal(length(result_SR_ST_SH$coefficients$ST), 6)
  expect_equal(length(result_SR_ST_SH$coefficients$DSH), 0)
  expect_equal(length(result_SR_ST_SH$coefficients$SH), 6)
  expect_true(!is.null(result_SR_ST_SH$coefficients$SR))
  expect_true(!is.null(result_SR_ST_SH$coefficients$ST))
  expect_null(result_SR_ST_SH$coefficients$DSH)
  expect_true(!is.null(result_SR_ST_SH$coefficients$SH))

  # Check that all models are wanted
  result_all <- slab_shrink(test_x, test_y, models = c("SR","ST","DSH","SH"))
  expect_equal(length(result_all$coefficients$SR), 6)
  expect_equal(length(result_all$coefficients$ST), 6)
  expect_equal(length(result_all$coefficients$DSH), 6)
  expect_equal(length(result_all$coefficients$SH), 6)
  expect_true(!is.null(result_all$coefficients$SR))
  expect_true(!is.null(result_all$coefficients$ST))
  expect_true(!is.null(result_all$coefficients$DSH))
  expect_true(!is.null(result_all$coefficients$SH))
})

test_that("estimation works with exclusion of columns correctly", {
  # Give test_x colname
  colnames(test_x) <- c("Alpha", "Beta", "Gamma", "Delta", "Eta")
  # Check that exclude the first column, so Alpha will be removed
  result_exclude_1 <- slab_shrink(test_x, test_y, exclude = 1)
  expect_true("coefficients" %in% names(result_exclude_1))
  expect_equal(length(result_exclude_1$coefficients$SR), 5)
  expect_equal(length(result_exclude_1$coefficients$ST), 5)
  expect_equal(length(result_exclude_1$coefficients$DSH), 5)
  expect_equal(length(result_exclude_1$coefficients$SH), 5)
  expect_false(is.na(result_exclude_1$coefficients$SR["(Intercept)"]))
  expect_true(is.na(result_exclude_1$coefficients$SR["Alpha"]))
  expect_false(is.na(result_exclude_1$coefficients$SR["Beta"]))
  expect_true(is.na(result_exclude_1$coefficients$ST["Alpha"]))
  expect_false(is.na(result_exclude_1$coefficients$ST["Gamma"]))
  expect_true(is.na(result_exclude_1$coefficients$DSH["Alpha"]))
  expect_false(is.na(result_exclude_1$coefficients$DSH["Delta"]))
  expect_true(is.na(result_exclude_1$coefficients$SH["Alpha"]))
  expect_false(is.na(result_exclude_1$coefficients$SH["Eta"]))

  # Check that exclude the first and last column, so Alpha and Eta will be removed
  result_exclude_1_5 <- slab_shrink(test_x, test_y, exclude = c(1, 5))
  expect_true("coefficients" %in% names(result_exclude_1_5))
  expect_equal(length(result_exclude_1_5$coefficients$SR), 4)
  expect_equal(length(result_exclude_1_5$coefficients$ST), 4)
  expect_equal(length(result_exclude_1_5$coefficients$DSH), 4)
  expect_equal(length(result_exclude_1_5$coefficients$SH), 4)
  expect_false(is.na(result_exclude_1_5$coefficients$SR["(Intercept)"]))
  expect_true(is.na(result_exclude_1_5$coefficients$SR["Alpha"]))
  expect_true(is.na(result_exclude_1_5$coefficients$SR["Eta"]))
  expect_false(is.na(result_exclude_1_5$coefficients$SR["Beta"]))
  expect_true(is.na(result_exclude_1_5$coefficients$ST["Alpha"]))
  expect_true(is.na(result_exclude_1_5$coefficients$ST["Eta"]))
  expect_false(is.na(result_exclude_1_5$coefficients$ST["Gamma"]))
  expect_true(is.na(result_exclude_1_5$coefficients$DSH["Alpha"]))
  expect_true(is.na(result_exclude_1_5$coefficients$DSH["Eta"]))
  expect_false(is.na(result_exclude_1_5$coefficients$DSH["Delta"]))
  expect_true(is.na(result_exclude_1_5$coefficients$SH["Alpha"]))

  # Check that exclude the Beta column
  result_exclude_beta <- slab_shrink(test_x, test_y, exclude = "Beta")
  expect_true("coefficients" %in% names(result_exclude_beta))
  expect_equal(length(result_exclude_beta$coefficients$SR), 5)
  expect_equal(length(result_exclude_beta$coefficients$ST), 5)
  expect_equal(length(result_exclude_beta$coefficients$DSH), 5)
  expect_equal(length(result_exclude_beta$coefficients$SH), 5)
  expect_false(is.na(result_exclude_beta$coefficients$SR["(Intercept)"]))
  expect_true(is.na(result_exclude_beta$coefficients$SR["Beta"]))
  expect_false(is.na(result_exclude_beta$coefficients$SR["Alpha"]))
  expect_true(is.na(result_exclude_beta$coefficients$ST["Beta"]))
  expect_false(is.na(result_exclude_beta$coefficients$ST["Gamma"]))
  expect_true(is.na(result_exclude_beta$coefficients$DSH["Beta"]))
  expect_false(is.na(result_exclude_beta$coefficients$DSH["Delta"]))
  expect_true(is.na(result_exclude_beta$coefficients$SH["Beta"]))
  expect_false(is.na(result_exclude_beta$coefficients$SH["Eta"]))

  # Check that exclude the Beta, Gamma, and Delta columns
  result_exclude_beta <- slab_shrink(test_x, test_y, exclude = c("Beta", "Gamma", "Delta"))
  expect_true("coefficients" %in% names(result_exclude_beta))
  expect_equal(length(result_exclude_beta$coefficients$SR), 3)
  expect_equal(length(result_exclude_beta$coefficients$ST), 3)
  expect_equal(length(result_exclude_beta$coefficients$DSH), 3)
  expect_equal(length(result_exclude_beta$coefficients$SH), 3)
  expect_false(is.na(result_exclude_beta$coefficients$SR["(Intercept)"]))
  expect_true(is.na(result_exclude_beta$coefficients$SR["Beta"]))
  expect_true(is.na(result_exclude_beta$coefficients$SR["Gamma"]))
  expect_true(is.na(result_exclude_beta$coefficients$SR["Delta"]))
  expect_false(is.na(result_exclude_beta$coefficients$SR["Alpha"]))
  expect_true(is.na(result_exclude_beta$coefficients$ST["Beta"]))
  expect_true(is.na(result_exclude_beta$coefficients$ST["Gamma"]))
  expect_true(is.na(result_exclude_beta$coefficients$ST["Delta"]))
  expect_true(is.na(result_exclude_beta$coefficients$DSH["Beta"]))
  expect_true(is.na(result_exclude_beta$coefficients$DSH["Gamma"]))
  expect_true(is.na(result_exclude_beta$coefficients$DSH["Delta"]))
  expect_true(is.na(result_exclude_beta$coefficients$SH["Beta"]))
  expect_true(is.na(result_exclude_beta$coefficients$SH["Gamma"]))
  expect_true(is.na(result_exclude_beta$coefficients$SH["Delta"]))
  expect_false(is.na(result_exclude_beta$coefficients$SH["Eta"]))

  # Check that exclude all columns and show errors
  expect_error(slab_shrink(test_x, test_y, exclude = c(1,2,3,4,5)),
               "Errors: The input 'x' has 0 variates")
  expect_error(slab_shrink(test_x, test_y, exclude = c("Alpha", "Beta", "Gamma", "Delta", "Eta")),
               "Errors: The input 'x' has 0 variates")

  # Check to exclude non-existent columns and display errors
  expect_error(slab_shrink(test_x, test_y, exclude = c(2,6)),
               "Errors: Exclusion indices are out of bounds.")
  expect_error(slab_shrink(test_x, test_y, exclude = c("Alpha", "Theta")),
               "Errors: All exclusion names must match column names in the data.")
  expect_error(slab_shrink(test_x, test_y, exclude = 1.2),
               "Errors: Exclusion must be an integer vector.")
  expect_error(slab_shrink(test_x, test_y, exclude = matrix(1:3, 2, 3)),
               "Errors: Exclusion must be a simple numerical or character vector.")
  expect_error(slab_shrink(test_x, test_y, exclude = list(1, 2, 3)),
               "Errors: Exclusion must be a simple numerical or character vector.")

  # Check that mixed of exclusion show errors
  expect_error(slab_shrink(test_x, test_y, exclude = c(1, "Beta", "Delta")),
               "Errors: All exclusion names must match column names in the data.")
})





















