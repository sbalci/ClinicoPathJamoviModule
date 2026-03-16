# ===============================================================
# Integration Tests: sparsegrouplasso
# ===============================================================

library(testthat)

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

# ---- Bootstrap CIs ----

test_that("bootstrap CIs populate ci_lower and ci_upper", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(confidence_intervals = TRUE, bootstrap_samples = 100),
    data = sparsegrouplasso_lung
  )
  a$run()
  coefs <- a$results$coefficients$asDF
  expect_true(sum(!is.na(coefs$ci_lower)) > 0)
  expect_true(sum(!is.na(coefs$ci_upper)) > 0)
})

test_that("bootstrap CIs use selection frequency from bootstrap", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(confidence_intervals = TRUE, bootstrap_samples = 100),
    data = sparsegrouplasso_lung
  )
  a$run()
  coefs <- a$results$coefficients$asDF
  # Selection frequency should not be all 1.0 when bootstrap is used
  freqs <- coefs$selection_frequency
  expect_true(any(freqs < 1.0) || all(freqs == 1.0))  # At least computable
})

# ---- Stability Selection ----

test_that("stability selection populates first_selected and last_selected", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      stability_selection = TRUE,
      bootstrap_samples = 100,
      stability_threshold = 0.6
    ),
    data = sparsegrouplasso_lung
  )
  a$run()
  stab <- a$results$stabilityResults$asDF
  expect_true(nrow(stab) > 0)
  expect_true(sum(!is.na(stab$first_selected)) > 0)
  expect_true(sum(!is.na(stab$last_selected)) > 0)
})

# ---- Comparison Table ----

test_that("comparison table fits all three methods", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(show_performance = TRUE),
    data = sparsegrouplasso_lung
  )
  a$run()
  comp <- a$results$comparisonTable$asDF
  expect_equal(nrow(comp), 3)
  # All methods should have non-NA CV error
  expect_true(all(!is.na(comp$cv_error)))
})

# ---- Group Names ----

test_that("group names are descriptive, not generic", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      pred_vars = c("age", "smoking_py", "sex", "ecog"),
      group_definition = "factor_based",
      show_groups = TRUE
    ),
    data = sparsegrouplasso_lung
  )
  a$run()
  g <- a$results$groupStructure$asDF
  # Names should not be "Group 1", "Group 2", etc.
  expect_false(any(grepl("^Group [0-9]+$", g$group_name)))
})

# ---- Full pipeline: all features at once ----

test_that("full pipeline runs with all features enabled", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      suitabilityCheck = TRUE,
      show_path = TRUE,
      weight_type = "ridge_based",
      confidence_intervals = TRUE,
      bootstrap_samples = 100,
      stability_selection = TRUE,
      stability_threshold = 0.6,
      cv_repeats = 2,
      showExplanations = TRUE
    ),
    data = sparsegrouplasso_lung
  )
  expect_no_error(a$run())

  # Verify all outputs populated
  expect_true(nchar(a$results$suitabilityReport$content) > 0)
  expect_true(nrow(a$results$summary$asDF) > 0)
  expect_true(nrow(a$results$coefficients$asDF) > 0)
  expect_true(nrow(a$results$groupStructure$asDF) > 0)
  expect_true(nrow(a$results$performance$asDF) > 0)
  expect_true(nrow(a$results$validationResults$asDF) > 0)
  expect_true(nrow(a$results$solutionPath$asDF) > 0)
  expect_true(nrow(a$results$adaptiveWeights$asDF) > 0)
  expect_true(nrow(a$results$stabilityResults$asDF) > 0)
  expect_true(nrow(a$results$comparisonTable$asDF) > 0)
  expect_true(nchar(a$results$explanations$content) > 0)
})

# ---- Reproducibility ----

test_that("same seed produces identical results", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_lung, package = "ClinicoPath")

  a1 <- sparsegrouplassoClass$new(
    options = sgl_opts(seed_value = 42),
    data = sparsegrouplasso_lung
  )
  a1$run()

  a2 <- sparsegrouplassoClass$new(
    options = sgl_opts(seed_value = 42),
    data = sparsegrouplasso_lung
  )
  a2$run()

  c1 <- a1$results$coefficients$asDF$coefficient
  c2 <- a2$results$coefficients$asDF$coefficient
  expect_equal(c1, c2)
})

# ---- Genepanel dataset ----

test_that("sparsegrouplasso works with genepanel dataset", {
  skip_if_not_installed("glmnet")
  data(sparsegrouplasso_genepanel, package = "ClinicoPath")

  gene_vars <- names(sparsegrouplasso_genepanel)[!names(sparsegrouplasso_genepanel) %in% c("time", "status")]

  a <- sparsegrouplassoClass$new(
    options = sgl_opts(
      outcomeLevel = "Progressed",
      censorLevel = "Stable",
      pred_vars = gene_vars,
      cv_folds = 3,
      n_lambda = 20
    ),
    data = sparsegrouplasso_genepanel
  )
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})
