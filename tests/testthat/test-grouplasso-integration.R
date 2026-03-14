# ===============================================================
# Integration Tests: grouplasso
# ===============================================================

library(testthat)

test_that("grouplasso stability selection works", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "ki67", "er", "albumin"),
    stability_selection = TRUE,
    bootstrap_samples = 50,
    stability_threshold = 0.6,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    plot_stability = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
  expect_no_error(results$run())
})

test_that("grouplasso nested CV works", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "ki67", "er"),
    nested_cv = TRUE,
    inner_cv_folds = 3,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
  expect_no_error(results$run())
})

test_that("grouplasso permutation test works", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "ki67"),
    permutation_test = TRUE,
    n_permutations = 50,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
  expect_no_error(results$run())
})

test_that("grouplasso clinical output panels work", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "ki67", "er"),
    showSummary = TRUE,
    showExplanations = TRUE,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
  expect_no_error(results$run())
})

test_that("grouplasso genomic with custom pathway grouping works", {
  skip_if_not_installed("grpreg")

  data(grouplasso_genomic, package = "ClinicoPath")

  # Define pathway groups
  pathway_groups <- paste(
    "CCND1:1, CCNE1:1, CDK4:1, CDK6:1, RB1:1",
    "PIK3CA:2, AKT1:2, PTEN:2, MTOR:2, TSC1:2",
    "TP53:3, MDM2:3, ATM:3, CHEK2:3, CDKN2A:3",
    "KRAS:4, BRAF:4, MAP2K1:4, ERK1:4, ERK2:4",
    "BCL2:5, BAX:5, BIRC5:5, CASP3:5, CASP8:5",
    "VEGFA:6, FLT1:6, KDR:6, ANGPT1:6, ANGPT2:6",
    sep = ", "
  )

  gene_vars <- setdiff(names(grouplasso_genomic), c("time", "status"))

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Progressed", censorLevel = "Stable",
    predictors = gene_vars,
    group_definition = "custom",
    group_structure = pathway_groups,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 20, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_genomic)
  expect_no_error(results$run())
})
