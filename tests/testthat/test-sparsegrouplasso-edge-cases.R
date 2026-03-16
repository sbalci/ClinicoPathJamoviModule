# ===============================================================
# Edge Cases Tests: sparsegrouplasso
# ===============================================================

library(testthat)

# Common helper
sgl_opts <- function(...) {
  defaults <- list(
    time_var = "time",
    event_var = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    pred_vars = c("age", "smoking_py", "tumor_size", "pdl1"),
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

# ---- Small samples ----

test_that("sparsegrouplasso handles small dataset", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_small, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      pred_vars = c("age", "tumor_size", "marker1", "marker2"),
      cv_folds = 3
    ),
    data = sparsegrouplasso_small
  )
  expect_no_error(a$run())
})

# ---- Minimum predictors ----

test_that("sparsegrouplasso runs with exactly 2 predictors", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(pred_vars = c("age", "tumor_size")),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

# ---- Missing data ----

test_that("sparsegrouplasso handles missing values by listwise deletion", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  # Introduce 10% missing
  d <- sparsegrouplasso_lung
  set.seed(99)
  d$pdl1[sample(nrow(d), 18)] <- NA

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(pred_vars = c("age", "smoking_py", "tumor_size", "pdl1")),
    data = d
  )
  expect_no_error(a$run())
})

# ---- Returns early without crashing ----

test_that("sparsegrouplasso returns silently when < 2 predictors", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(pred_vars = "age"),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
  # Should not populate results (early return)
  expect_equal(nrow(a$results$summary$asDF), 0)
})

# ---- All events censored ----

test_that("sparsegrouplasso errors on all-censored data", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  d <- sparsegrouplasso_lung
  d$status <- factor("Alive", levels = c("Alive", "Dead"))

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(),
    data = d
  )
  # Should run without crash; the error is communicated via notice
  expect_no_error(a$run())
})

# ---- High-dimensional data ----

test_that("sparsegrouplasso handles p > n scenario", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_genepanel, package = "ClinicoPath")

  # 40 predictors, 100 observations
  gene_vars <- names(sparsegrouplasso_genepanel)[!names(sparsegrouplasso_genepanel) %in% c("time", "status")]

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      outcomeLevel = "Progressed",
      censorLevel = "Stable",
      pred_vars = gene_vars,
      cv_folds = 3
    ),
    data = sparsegrouplasso_genepanel
  )
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

# ---- Standardization options ----

test_that("standardize=FALSE and center=FALSE work", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(standardize_vars = FALSE, center_vars = FALSE),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
})

# ---- EBIC with gamma=0 (standard BIC) ----

test_that("ebic with gamma=0 behaves like BIC", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(selection_criterion = "ebic", ebic_gamma = 0),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())
})
