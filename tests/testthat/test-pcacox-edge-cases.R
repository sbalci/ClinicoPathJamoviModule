# ===============================================================
# Edge Cases Tests: pcacox
# ===============================================================

library(testthat)

pca_opts <- function(...) {
  defaults <- list(
    time = "time", status = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "bmi", "albumin", "crp", "ldh"),
    clinical_vars = NULL,
    pca_method = "standard", n_components = 2,
    component_selection = "fixed",
    suitabilityCheck = FALSE,
    plot_scree = FALSE, plot_loadings = FALSE,
    plot_biplot = FALSE, plot_survival = FALSE,
    bootstrap_validation = FALSE, permutation_test = FALSE,
    show_model_comparison = FALSE, pathway_analysis = FALSE
  )
  do.call(pcacoxOptions$new, modifyList(defaults, list(...)))
}

test_that("pcacox handles minimum 2 predictors", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(predictors = c("age", "albumin"), n_components = 1)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coxResults$asDF) > 0)
})

test_that("pcacox returns silently with <2 predictors", {
  data(pcacox_clinical, package = "ClinicoPath")
  o <- pca_opts(predictors = "age", n_components = 1)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
  # Should show welcome/instructions, not crash
})

test_that("pcacox handles missing data by listwise deletion", {
  data(pcacox_clinical, package = "ClinicoPath")
  d <- pcacox_clinical
  set.seed(99)
  d$crp[sample(nrow(d), 8)] <- NA
  o <- pca_opts()
  a <- pcacoxClass$new(options = o, data = d)
  expect_no_error(a$run())
})

test_that("pcacox handles all-censored data gracefully", {
  data(pcacox_clinical, package = "ClinicoPath")
  d <- pcacox_clinical
  d$status <- factor("Alive", levels = c("Alive", "Dead"))
  o <- pca_opts()
  a <- pcacoxClass$new(options = o, data = d)
  # Should not crash -- error communicated via notice
  expect_no_error(a$run())
})

test_that("pcacox handles high-dimensional data (p > n)", {
  data(pcacox_genomic, package = "ClinicoPath")
  gene_vars <- names(pcacox_genomic)[!names(pcacox_genomic) %in% c("time", "status")]
  o <- pca_opts(
    predictors = gene_vars,
    n_components = 5
  )
  a <- pcacoxClass$new(options = o, data = pcacox_genomic)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coxResults$asDF) > 0)
})

test_that("pcacox with n_components > available", {
  data(pcacox_clinical, package = "ClinicoPath")
  # Request 50 components from 10 predictors -- should cap automatically
  o <- pca_opts(n_components = 50)
  a <- pcacoxClass$new(options = o, data = pcacox_clinical)
  expect_no_error(a$run())
})
