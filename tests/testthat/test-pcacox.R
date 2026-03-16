# ===============================================================
# Core Tests: pcacox (Principal Component Cox)
# ===============================================================

library(testthat)

# Helper for common options
pca_opts <- function(...) {
  defaults <- list(
    time = "time", status = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "bmi", "albumin", "crp", "ldh",
                   "hemoglobin", "wbc", "platelets", "tumor_size", "ki67"),
    clinical_vars = NULL,
    pca_method = "standard", n_components = 3,
    component_selection = "fixed",
    suitabilityCheck = FALSE,
    plot_scree = FALSE, plot_loadings = FALSE,
    plot_biplot = FALSE, plot_survival = FALSE,
    bootstrap_validation = FALSE, permutation_test = FALSE,
    show_model_comparison = FALSE, pathway_analysis = FALSE
  )
  do.call(pcacoxOptions$new, modifyList(defaults, list(...)))
}

test_that("pcacox class can be instantiated", {
  data(pcacox_clinical, package = "ClinicoPath")
  expect_no_error(pca_opts())
})

test_that("pcacox runs with standard PCA", {
  data(pcacox_clinical, package = "ClinicoPath")

  o <- pca_opts()
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())

  expect_true(nrow(a$results$pcaSummary$asDF) > 0)
  expect_true(nrow(a$results$coxResults$asDF) > 0)
  expect_true(nrow(a$results$modelPerformance$asDF) > 0)
  expect_true(nchar(a$results$technicalDetails$content %||% "") > 0)
  expect_true(nchar(a$results$clinicalInterpretation$content %||% "") > 0)
})

test_that("pcacox runs with supervised PCA", {
  data(pcacox_clinical, package = "ClinicoPath")

  o <- pca_opts(pca_method = "supervised")
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coxResults$asDF) > 0)
})

test_that("pcacox runs with sparse PCA", {
  data(pcacox_clinical, package = "ClinicoPath")

  o <- pca_opts(pca_method = "sparse", sparse_parameter = 0.1)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
})

test_that("pcacox runs with kernel PCA", {
  data(pcacox_clinical, package = "ClinicoPath")

  o <- pca_opts(pca_method = "kernel")
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
})
