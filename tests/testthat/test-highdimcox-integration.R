# ===============================================================
# Integration Tests: highdimcox
# ===============================================================
#
# Tests end-to-end workflows using pre-generated test datasets
# with clinically realistic scenarios.
#
# NOTE: jmvcore::enquo captures symbols, so predictor lists must
# be passed inline (not via pre-assigned variables).

library(testthat)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

skip_highdimcox_deps <- function() {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
}

load_genomic <- function() {
  data_path <- system.file("data", "highdimcox_genomic.rda",
                           package = "ClinicoPath")
  if (data_path == "") {
    data_path <- file.path("../../data", "highdimcox_genomic.rda")
  }
  if (file.exists(data_path)) {
    env <- new.env()
    load(data_path, envir = env)
    return(env$highdimcox_genomic)
  }
  skip("highdimcox_genomic.rda not found")
}

load_proteomic <- function() {
  data_path <- system.file("data", "highdimcox_proteomic.rda",
                           package = "ClinicoPath")
  if (data_path == "") {
    data_path <- file.path("../../data", "highdimcox_proteomic.rda")
  }
  if (file.exists(data_path)) {
    env <- new.env()
    load(data_path, envir = env)
    return(env$highdimcox_proteomic)
  }
  skip("highdimcox_proteomic.rda not found")
}

# ---------------------------------------------------------------
# Full genomic workflow
# ---------------------------------------------------------------

test_that("genomic data: full elastic net workflow with 100 genes", {
  skip_highdimcox_deps()
  d <- load_genomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "survival_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("GENE_", sprintf("%03d", 1:100)),
    regularization_method = "elastic_net",
    alpha_value = 0.5,
    cv_method = "cv_1se",
    cv_folds = 10,
    show_coefficients_table = TRUE,
    show_variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "Group"))
  expect_true(nchar(result$modelSummary$content) > 0)
})

test_that("genomic data: LASSO selects sparse subset of genes", {
  skip_highdimcox_deps()
  d <- load_genomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "survival_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("GENE_", sprintf("%03d", 1:100)),
    regularization_method = "lasso",
    cv_method = "cv_1se",
    show_coefficients_table = TRUE
  )

  expect_true(inherits(result, "Group"))
  # LASSO with 1se should select far fewer than 100
  n_rows <- result$selectedVariables$rowCount
  expect_true(n_rows < 50)
})

# ---------------------------------------------------------------
# Full proteomic workflow
# ---------------------------------------------------------------

test_that("proteomic data: adaptive LASSO with stability selection", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:50)),
    regularization_method = "adaptive_lasso",
    stability_selection = TRUE,
    subsampling_iterations = 100,
    stability_threshold = 0.6,
    show_coefficients_table = TRUE,
    suitabilityCheck = TRUE,
    showSummaries = TRUE
  )

  expect_true(inherits(result, "Group"))
  expect_true(!is.null(result$stabilityResults))
  expect_true(nchar(result$analysisSummary$content) > 0)
})

# ---------------------------------------------------------------
# Comparing regularization methods
# ---------------------------------------------------------------

test_that("all four regularization methods produce valid results", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  methods <- c("lasso", "ridge", "elastic_net", "adaptive_lasso")

  for (m in methods) {
    result <- highdimcox(
      data = d,
      elapsedtime = "follow_up_months",
      outcome = "event_status",
      outcomeLevel = "Dead",
      censorLevel = "Alive",
      predictors = paste0("PROT_", sprintf("%02d", 1:20)),
      regularization_method = m,
      cv_folds = 5,
      show_coefficients_table = TRUE
    )

    expect_true(inherits(result, "Group"),
                info = paste("Failed for method:", m))
    expect_true(result$regularizationMetrics$rowCount > 0,
                info = paste("No metrics for method:", m))
  }
})

# ---------------------------------------------------------------
# Suitability assessment
# ---------------------------------------------------------------

test_that("suitability assessment produces traffic-light HTML", {
  skip_highdimcox_deps()
  d <- load_genomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "survival_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("GENE_", sprintf("%03d", 1:50)),
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "Group"))
  suit_content <- result$suitabilityReport$content
  expect_true(nchar(suit_content) > 0)
  # Should contain traffic-light color codes
  expect_true(grepl("#28a745|#ffc107|#dc3545", suit_content))
})

# ---------------------------------------------------------------
# Mixed clinical + molecular predictors
# ---------------------------------------------------------------

test_that("genomic data: clinical + gene mix with all outputs", {
  skip_highdimcox_deps()
  d <- load_genomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "survival_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("age", "gender", "stage", "grade", "treatment",
                    "GENE_001", "GENE_002", "GENE_003", "GENE_004", "GENE_005",
                    "GENE_006", "GENE_007", "GENE_008", "GENE_009", "GENE_010",
                    "GENE_011", "GENE_012", "GENE_013", "GENE_014", "GENE_015",
                    "GENE_016", "GENE_017", "GENE_018", "GENE_019", "GENE_020",
                    "GENE_021", "GENE_022", "GENE_023", "GENE_024", "GENE_025",
                    "GENE_026", "GENE_027", "GENE_028", "GENE_029", "GENE_030"),
    regularization_method = "elastic_net",
    alpha_value = 0.7,
    show_regularization_path = TRUE,
    show_cv_plot = TRUE,
    show_variable_importance = TRUE,
    show_coefficients_table = TRUE,
    show_model_diagnostics = TRUE,
    showSummaries = TRUE,
    showExplanations = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "Group"))
  expect_true(nchar(result$modelSummary$content) > 0)
  expect_true(nchar(result$analysisSummary$content) > 0)
  expect_true(nchar(result$methodExplanation$content) > 0)
})
