# ===============================================================
# Basic Functionality Tests: highdimcox (High-Dimensional Cox)
# ===============================================================
#
# Tests that the highdimcox function exists, runs with minimal
# arguments, and produces expected output structure.
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
# Tests
# ---------------------------------------------------------------

test_that("highdimcox function exists in ClinicoPath namespace", {
  skip_highdimcox_deps()
  expect_true(exists("highdimcoxClass"))
})

test_that("highdimcox runs with minimal required arguments (genomic data)", {
  skip_highdimcox_deps()
  d <- load_genomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "survival_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("GENE_", sprintf("%03d", 1:20))
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox runs with proteomic data", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:20))
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox produces expected output items", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15)),
    show_coefficients_table = TRUE,
    suitabilityCheck = TRUE
  )

  # Check expected output items exist
  expect_true(!is.null(result$todo))
  expect_true(!is.null(result$modelSummary))
  expect_true(!is.null(result$selectedVariables))
  expect_true(!is.null(result$regularizationMetrics))
  expect_true(!is.null(result$suitabilityReport))
})

test_that("highdimcox selectedVariables table has correct columns", {
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

  tbl <- result$selectedVariables
  expect_true(inherits(tbl, "Table"))
  col_names <- vapply(tbl$columns, function(c) c$name, character(1))
  expect_true("variable" %in% col_names)
  expect_true("coefficient" %in% col_names)
  expect_true("hazard_ratio" %in% col_names)
  expect_true("importance_score" %in% col_names)
})

test_that("highdimcox regularizationMetrics table has correct columns", {
  skip_highdimcox_deps()
  d <- load_proteomic()

  result <- highdimcox(
    data = d,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("PROT_", sprintf("%02d", 1:15))
  )

  tbl <- result$regularizationMetrics
  expect_true(inherits(tbl, "Table"))
  col_names <- vapply(tbl$columns, function(c) c$name, character(1))
  expect_true("metric" %in% col_names)
  expect_true("value" %in% col_names)
  expect_true("interpretation" %in% col_names)
})
