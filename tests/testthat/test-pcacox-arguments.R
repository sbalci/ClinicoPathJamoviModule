# ===============================================================
# Argument Combination Tests: pcacox
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

# ---- Component Selection ----

test_that("CV component selection works", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(component_selection = "cv", cv_folds = 3)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
  expect_true(nchar(a$results$crossValidation$content %||% "") > 0)
})

test_that("variance threshold selection works", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(component_selection = "variance", variance_threshold = 0.6)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
})

test_that("n_components changes output", {
  data(pcacox_clinical, package = "ClinicoPath")

  o1 <- pca_opts(n_components = 1)
  a1 <- pcacoxClass$new(options = o1, data = pcacox_clinical)
  a1$run()

  o2 <- pca_opts(n_components = 5)
  a2 <- pcacoxClass$new(options = o2, data = pcacox_clinical)
  a2$run()

  expect_true(nrow(a1$results$coxResults$asDF) < nrow(a2$results$coxResults$asDF))
})

# ---- Preprocessing ----

test_that("scaling=FALSE changes results", {
  data(pcacox_clinical, package = "ClinicoPath")

  o1 <- pca_opts(scaling = TRUE)
  a1 <- pcacoxClass$new(options = o1, data = pcacox_clinical)
  a1$run()

  o2 <- pca_opts(scaling = FALSE)
  a2 <- pcacoxClass$new(options = o2, data = pcacox_clinical)
  a2$run()

  c1 <- sum(abs(a1$results$coxResults$asDF$coefficient), na.rm = TRUE)
  c2 <- sum(abs(a2$results$coxResults$asDF$coefficient), na.rm = TRUE)
  expect_false(c1 == c2)
})

test_that("centering=FALSE changes results", {
  data(pcacox_clinical, package = "ClinicoPath")

  o1 <- pca_opts(centering = TRUE)
  a1 <- pcacoxClass$new(options = o1, data = pcacox_clinical)
  a1$run()

  o2 <- pca_opts(centering = FALSE)
  a2 <- pcacoxClass$new(options = o2, data = pcacox_clinical)
  a2$run()

  c1 <- sum(abs(a1$results$coxResults$asDF$coefficient), na.rm = TRUE)
  c2 <- sum(abs(a2$results$coxResults$asDF$coefficient), na.rm = TRUE)
  expect_false(c1 == c2)
})

# ---- Supervised PCA weighting ----

test_that("survival_weighting=FALSE falls back to standard PCA", {
  data(pcacox_clinical, package = "ClinicoPath")

  o1 <- pca_opts(pca_method = "supervised", survival_weighting = TRUE)
  a1 <- pcacoxClass$new(options = o1, data = pcacox_clinical)
  a1$run()

  o2 <- pca_opts(pca_method = "supervised", survival_weighting = FALSE)
  a2 <- pcacoxClass$new(options = o2, data = pcacox_clinical)
  a2$run()

  c1 <- sum(abs(a1$results$coxResults$asDF$coefficient), na.rm = TRUE)
  c2 <- sum(abs(a2$results$coxResults$asDF$coefficient), na.rm = TRUE)
  expect_false(c1 == c2)
})

# ---- Display Options ----

test_that("suitability check produces report", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(suitabilityCheck = TRUE)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  expect_true(nchar(a$results$suitabilityReport$content %||% "") > 0)
})

test_that("model comparison populates", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(show_model_comparison = TRUE)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  expect_true(nrow(a$results$modelComparison$asDF) > 0)
})

test_that("feature cluster analysis populates", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(pathway_analysis = TRUE)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  content <- a$results$pathwayAnalysis$content %||% ""
  expect_true(nchar(content) > 0)
  expect_true(grepl("Cluster", content))
})

test_that("risk_score=FALSE hides risk table", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(risk_score = FALSE)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  # Risk table should have 0 or 1 row (empty placeholder)
  expect_true(nrow(a$results$riskScore$asDF) <= 1)
})

test_that("feature_importance=FALSE hides feature table", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(feature_importance = FALSE)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  a$run()
  expect_true(nrow(a$results$featureImportance$asDF) <= 1)
})
