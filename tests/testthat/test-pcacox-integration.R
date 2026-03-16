# ===============================================================
# Integration Tests: pcacox
# ===============================================================

library(testthat)

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

test_that("bootstrap validation populates", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(bootstrap_validation = TRUE, n_bootstrap = 50)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  expect_true(nchar(a$results$bootstrapValidation$content %||% "") > 0)
  expect_true(grepl("corrected", a$results$bootstrapValidation$content, ignore.case = TRUE))
})

test_that("permutation test populates", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(permutation_test = TRUE, n_permutations = 50)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  expect_true(nchar(a$results$permutationTest$content %||% "") > 0)
  expect_true(grepl("p-value", a$results$permutationTest$content, ignore.case = TRUE))
})

test_that("full pipeline with all features", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(
    suitabilityCheck = TRUE,
    show_model_comparison = TRUE,
    pathway_analysis = TRUE,
    risk_score = TRUE,
    feature_importance = TRUE,
    bootstrap_validation = TRUE,
    n_bootstrap = 50,
    permutation_test = TRUE,
    n_permutations = 50
  )
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())

  # All outputs populated
  expect_true(nchar(a$results$suitabilityReport$content %||% "") > 0)
  expect_true(nrow(a$results$pcaSummary$asDF) > 0)
  expect_true(nrow(a$results$coxResults$asDF) > 0)
  expect_true(nrow(a$results$modelPerformance$asDF) > 0)
  expect_true(nrow(a$results$featureImportance$asDF) > 0)
  expect_true(nrow(a$results$riskScore$asDF) > 0)
  expect_true(nrow(a$results$modelComparison$asDF) > 0)
  expect_true(nchar(a$results$bootstrapValidation$content %||% "") > 0)
  expect_true(nchar(a$results$permutationTest$content %||% "") > 0)
  expect_true(nchar(a$results$pathwayAnalysis$content %||% "") > 0)
  expect_true(nchar(a$results$technicalDetails$content %||% "") > 0)
  expect_true(nchar(a$results$clinicalInterpretation$content %||% "") > 0)
  expect_true(nchar(a$results$summary$content %||% "") > 0)
})

test_that("reproducibility: same seed gives same results", {
  data(pcacox_clinical, package = "ClinicoPath")

  o1 <- pca_opts()
  a1 <- pcacoxClass$new(options = o1, data = pcacox_clinical)
  a1$run()

  o2 <- pca_opts()
  a2 <- pcacoxClass$new(options = o2, data = pcacox_clinical)
  a2$run()

  c1 <- a1$results$coxResults$asDF$coefficient
  c2 <- a2$results$coxResults$asDF$coefficient
  expect_equal(c1, c2)
})

test_that("genomic dataset works", {
  data(pcacox_genomic, package = "ClinicoPath")
  gene_vars <- names(pcacox_genomic)[!names(pcacox_genomic) %in% c("time", "status")]

  o <- pca_opts(
    predictors = gene_vars,
    n_components = 5,
    component_selection = "variance",
    variance_threshold = 0.8
  )
  a <- pcacoxClass$new(options = o, data = pcacox_genomic)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coxResults$asDF) > 0)
})
