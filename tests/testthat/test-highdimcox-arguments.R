# ===============================================================
# Argument Combination Tests: highdimcox
# ===============================================================
#
# Tests all regularization methods, CV methods, stability
# selection options, and display toggles.
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

# ---------------------------------------------------------------
# Regularization Methods
# ---------------------------------------------------------------

test_that("highdimcox works with LASSO regularization", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    regularization_method = "lasso"
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox works with Ridge regularization", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    regularization_method = "ridge"
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox works with Elastic Net regularization", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    regularization_method = "elastic_net",
    alpha_value = 0.5
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox works with Adaptive LASSO regularization", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    regularization_method = "adaptive_lasso"
  )

  expect_true(inherits(result, "Group"))
})

# ---------------------------------------------------------------
# Cross-Validation Methods
# ---------------------------------------------------------------

test_that("highdimcox works with cv_min lambda selection", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    cv_method = "cv_min"
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox works with custom CV folds", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    cv_folds = 5
  )

  expect_true(inherits(result, "Group"))
})

# ---------------------------------------------------------------
# Alpha Values
# ---------------------------------------------------------------

test_that("highdimcox works with alpha = 0 (ridge-like elastic net)", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    regularization_method = "elastic_net",
    alpha_value = 0.0
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox works with alpha = 1 (lasso-like elastic net)", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    regularization_method = "elastic_net",
    alpha_value = 1.0
  )

  expect_true(inherits(result, "Group"))
})

# ---------------------------------------------------------------
# Stability Selection
# ---------------------------------------------------------------

test_that("highdimcox works with stability selection enabled", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    stability_selection = TRUE,
    subsampling_iterations = 100,
    subsampling_ratio = 0.5,
    stability_threshold = 0.6
  )

  expect_true(inherits(result, "Group"))
  expect_true(!is.null(result$stabilityResults))
})

# ---------------------------------------------------------------
# Display Options
# ---------------------------------------------------------------

test_that("highdimcox works with all display options enabled", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
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
  expect_true(!is.null(result$analysisSummary))
  expect_true(!is.null(result$methodExplanation))
})

test_that("highdimcox works with all display options disabled", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    show_regularization_path = FALSE,
    show_cv_plot = FALSE,
    show_variable_importance = FALSE,
    show_coefficients_table = FALSE,
    show_model_diagnostics = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE,
    suitabilityCheck = FALSE
  )

  expect_true(inherits(result, "Group"))
})

# ---------------------------------------------------------------
# Mixed Predictor Types (factor + continuous)
# ---------------------------------------------------------------

test_that("highdimcox handles mix of factor and continuous predictors", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("PROT_01", "PROT_02", "PROT_03", "PROT_04",
                    "PROT_05", "PROT_06", "PROT_07", "PROT_08",
                    "PROT_09", "PROT_10", "sex")
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox handles clinical + genomic predictors together", {
  skip_highdimcox_deps()
  d <- load_genomic()

  # c() with nested paste0() fails in jmvcore::resolveQuo; spell out names
  result <- highdimcox(
    data = d,
    elapsedtime = "survival_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("age", "gender", "stage", "grade",
                    "GENE_001", "GENE_002", "GENE_003", "GENE_004", "GENE_005",
                    "GENE_006", "GENE_007", "GENE_008", "GENE_009", "GENE_010",
                    "GENE_011", "GENE_012", "GENE_013", "GENE_014", "GENE_015",
                    "GENE_016", "GENE_017", "GENE_018", "GENE_019", "GENE_020")
  )

  expect_true(inherits(result, "Group"))
})
