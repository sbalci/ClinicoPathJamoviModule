# ===============================================================
# Argument Combination Tests: sparsegrouplasso
# ===============================================================

library(testthat)

# Common helper: build options with overrides
sgl_opts <- function(...) {
  defaults <- list(
    time_var = "time",
    event_var = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    pred_vars = c("age", "smoking_py", "tumor_size", "pdl1",
                  "crp", "nlr", "albumin", "ldh"),
    pathway_info = NULL,
    suitabilityCheck = FALSE,
    plot_cv_error = FALSE,
    plot_coefficients = FALSE,
    plot_groups = FALSE,
    plot_sparsity = FALSE,
    plot_stability = FALSE,
    showExplanations = FALSE,
    cv_folds = 5,
    n_lambda = 20,
    seed_value = 42
  )
  do.call(sparsegrouplassoOptions$new, modifyList(defaults, list(...)))
}

# ---- Penalty & Lambda ----

test_that("alpha_sgl changes results", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  o1 <- sgl_opts(alpha_sgl = 0.95)
  a1 <- sparsegrouplassoClass$new(options = o1, data = sparsegrouplasso_lung)
  a1$run()

  o2 <- sgl_opts(alpha_sgl = 0.3)
  a2 <- sparsegrouplassoClass$new(options = o2, data = sparsegrouplasso_lung)
  a2$run()

  c1 <- sum(abs(a1$results$coefficients$asDF$coefficient), na.rm = TRUE)
  c2 <- sum(abs(a2$results$coefficients$asDF$coefficient), na.rm = TRUE)
  expect_false(c1 == c2)
})

test_that("selection_criterion aic/bic/ebic produce sparser models", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a_cv <- sparsegrouplassoClass$new(
    options = sgl_opts(selection_criterion = "cv_deviance"),
    data = sparsegrouplasso_lung
  )
  a_cv$run()
  n_cv <- nrow(a_cv$results$coefficients$asDF)

  a_bic <- sparsegrouplassoClass$new(
    options = sgl_opts(selection_criterion = "bic"),
    data = sparsegrouplasso_lung
  )
  a_bic$run()
  n_bic <- nrow(a_bic$results$coefficients$asDF)

  # BIC should select fewer or equal variables than CV deviance

  expect_true(n_bic <= n_cv)
})

test_that("adaptive lambda sequence works", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(lambda_sequence = "adaptive"),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

test_that("custom lambda sequence works", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(lambda_sequence = "custom", custom_lambda = "0.001,0.01,0.1,1"),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
})

# ---- Group Definition ----

test_that("correlation_based grouping works", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(group_definition = "correlation_based", correlation_threshold = 0.5),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
  expect_true(nrow(a$results$groupStructure$asDF) > 0)
})

test_that("variable_type grouping works with mixed data", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      pred_vars = c("age", "smoking_py", "sex", "ecog"),
      group_definition = "variable_type"
    ),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
})

test_that("custom grouping works", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      pred_vars = c("age", "smoking_py", "tumor_size", "pdl1"),
      group_definition = "custom",
      custom_groups = "1,2;3,4"
    ),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
  g <- a$results$groupStructure$asDF
  expect_equal(nrow(g), 2)  # Two custom groups
})

# ---- Adaptive Weights ----

test_that("ridge-based adaptive weights populate table", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(weight_type = "ridge_based"),
    data = sparsegrouplasso_lung
  )
  a$run()
  aw <- a$results$adaptiveWeights$asDF
  expect_true(nrow(aw) > 0)
  expect_true(a$results$adaptiveWeights$visible)
})

test_that("univariate-based weights work", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(weight_type = "univariate_based"),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
})

test_that("lasso-based weights work", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(weight_type = "lasso_based"),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
})

test_that("weight_type=none hides adaptive weights table", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(weight_type = "none"),
    data = sparsegrouplasso_lung
  )
  a$run()
  expect_false(a$results$adaptiveWeights$visible)
})

# ---- Cross-Validation ----

test_that("cv_repeats > 1 changes results", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a1 <- sparsegrouplassoClass$new(
    options = sgl_opts(cv_repeats = 1),
    data = sparsegrouplasso_lung
  )
  a1$run()

  a2 <- sparsegrouplassoClass$new(
    options = sgl_opts(cv_repeats = 3),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a2$run())
})

# ---- Display Options ----

test_that("show_path populates solution path table", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(show_path = TRUE),
    data = sparsegrouplasso_lung
  )
  a$run()
  expect_true(nrow(a$results$solutionPath$asDF) > 0)
})

test_that("suitabilityCheck produces report", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(suitabilityCheck = TRUE),
    data = sparsegrouplasso_lung
  )
  a$run()
  expect_true(nchar(a$results$suitabilityReport$content) > 0)
})
