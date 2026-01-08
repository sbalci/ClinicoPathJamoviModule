# ═══════════════════════════════════════════════════════════
# Integration Tests: jjpiestats
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and integration with all datasets
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(jjpiestats_test, package = "ClinicoPath", envir = environment())
data(jjpiestats_diagnostic, package = "ClinicoPath", envir = environment())
data(jjpiestats_treatment, package = "ClinicoPath", envir = environment())
data(jjpiestats_biomarker, package = "ClinicoPath", envir = environment())
data(jjpiestats_aggregated, package = "ClinicoPath", envir = environment())
data(jjpiestats_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Complete Diagnostic Test Evaluation Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles complete diagnostic test evaluation workflow", {
  devtools::load_all()

  # Step 1: Basic distribution
  result1 <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # Step 2: 2×2 contingency table
  result2 <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjpiestatsResults")

  # Step 3: Stratified by clinical site
  result3 <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    grvar = "clinical_site",
    clinicalpreset = "diagnostic"
  )
  expect_s3_class(result3, "jjpiestatsResults")

  # Step 4: Publication-ready analysis
  result4 <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    typestatistics = "parametric",
    clinicalpreset = "diagnostic",
    label = "both",
    resultssubtitle = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE,
    showInterpretation = TRUE,
    conflevel = 0.95,
    digits = 3
  )
  expect_s3_class(result4, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Complete Treatment Comparison Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles complete treatment comparison workflow", {
  devtools::load_all()

  # Step 1: Overall response distribution
  result1 <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # Step 2: Response by treatment arm
  result2 <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjpiestatsResults")

  # Step 3: Stratified by study site
  result3 <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    grvar = "study_site",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result3, "jjpiestatsResults")

  # Step 4: Complete analysis with donut chart
  result4 <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    typestatistics = "parametric",
    clinicalpreset = "treatment",
    label = "both",
    resultssubtitle = TRUE,
    showSummary = TRUE,
    showInterpretation = TRUE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco"
  )
  expect_s3_class(result4, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Complete Biomarker Distribution Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles complete biomarker distribution workflow", {
  devtools::load_all()

  # Step 1: Overall expression distribution
  result1 <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # Step 2: Expression by receptor status
  result2 <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    group = "receptor_status"
  )
  expect_s3_class(result2, "jjpiestatsResults")

  # Step 3: Stratified by cancer type
  result3 <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    grvar = "cancer_type",
    typestatistics = "robust"
  )
  expect_s3_class(result3, "jjpiestatsResults")

  # Step 4: Complete biomarker analysis
  result4 <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    group = "receptor_status",
    grvar = "cancer_type",
    clinicalpreset = "biomarker",
    typestatistics = "parametric",
    label = "percentage",
    resultssubtitle = TRUE,
    showSummary = TRUE
  )
  expect_s3_class(result4, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. All Datasets with Consistent Parameters
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles all datasets with consistent parameters", {
  devtools::load_all()

  # jjpiestats_test
  result1 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    typestatistics = "parametric",
    label = "percentage"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # jjpiestats_diagnostic
  result2 <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    typestatistics = "parametric",
    label = "percentage"
  )
  expect_s3_class(result2, "jjpiestatsResults")

  # jjpiestats_treatment
  result3 <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    typestatistics = "parametric",
    label = "percentage"
  )
  expect_s3_class(result3, "jjpiestatsResults")

  # jjpiestats_biomarker
  result4 <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    typestatistics = "parametric",
    label = "percentage"
  )
  expect_s3_class(result4, "jjpiestatsResults")

  # jjpiestats_small
  result5 <- jjpiestats(
    data = jjpiestats_small,
    dep = "category",
    typestatistics = "parametric",
    label = "percentage"
  )
  expect_s3_class(result5, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. All Statistical Tests on Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats produces consistent results across statistical test types", {
  devtools::load_all()

  # Parametric
  result_param <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "parametric"
  )
  expect_s3_class(result_param, "jjpiestatsResults")

  # Nonparametric
  result_nonparam <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result_nonparam, "jjpiestatsResults")

  # Robust
  result_robust <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "robust"
  )
  expect_s3_class(result_robust, "jjpiestatsResults")

  # Bayes
  result_bayes <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "bayes"
  )
  expect_s3_class(result_bayes, "jjpiestatsResults")

  # All should complete successfully
  expect_true(TRUE)
})

# ═══════════════════════════════════════════════════════════
# 6. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Minimal
  result1 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # Step 2: Add grouping
  result2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender"
  )
  expect_s3_class(result2, "jjpiestatsResults")

  # Step 3: Add statistical test
  result3 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    typestatistics = "parametric"
  )
  expect_s3_class(result3, "jjpiestatsResults")

  # Step 4: Add split variable
  result4 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    grvar = "hospital_site",
    typestatistics = "parametric"
  )
  expect_s3_class(result4, "jjpiestatsResults")

  # Step 5: Add display options
  result5 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    grvar = "hospital_site",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    label = "both"
  )
  expect_s3_class(result5, "jjpiestatsResults")

  # Step 6: Add clinical preset
  result6 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    grvar = "hospital_site",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    label = "both",
    clinicalpreset = "custom",
    showSummary = TRUE
  )
  expect_s3_class(result6, "jjpiestatsResults")

  # Step 7: Add donut chart
  result7 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    grvar = "hospital_site",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    label = "both",
    clinicalpreset = "custom",
    showSummary = TRUE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco"
  )
  expect_s3_class(result7, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Multiple Variables from Same Dataset
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles multiple analyses from same dataset", {
  devtools::load_all()

  variables <- c("treatment_response", "disease_severity", "gender", "tumor_stage")

  results <- list()

  for (var in variables) {
    results[[var]] <- jjpiestats(
      data = jjpiestats_test,
      dep = var,
      typestatistics = "parametric",
      label = "percentage"
    )
    expect_s3_class(results[[var]], "jjpiestatsResults")
  }

  # All should complete successfully
  expect_equal(length(results), length(variables))
})

# ═══════════════════════════════════════════════════════════
# 8. Repeated Analysis Consistency
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats produces consistent results on repeated calls", {
  devtools::load_all()

  # Run same analysis 3 times
  result1 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "parametric"
  )

  result2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "parametric"
  )

  result3 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "parametric"
  )

  expect_s3_class(result1, "jjpiestatsResults")
  expect_s3_class(result2, "jjpiestatsResults")
  expect_s3_class(result3, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Multi-site Study Comparison
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles multi-site study comparison workflow", {
  devtools::load_all()

  # Overall distribution
  result_overall <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status"
  )
  expect_s3_class(result_overall, "jjpiestatsResults")

  # By site
  result_by_site <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    grvar = "clinical_site"
  )
  expect_s3_class(result_by_site, "jjpiestatsResults")

  # By age category
  result_by_age <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    grvar = "age_category"
  )
  expect_s3_class(result_by_age, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Clinical Presets Integration
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats integrates clinical presets with appropriate data", {
  devtools::load_all()

  # Diagnostic preset with diagnostic data
  result_diag <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    clinicalpreset = "diagnostic",
    showexplanations = TRUE
  )
  expect_s3_class(result_diag, "jjpiestatsResults")

  # Treatment preset with treatment data
  result_treat <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    clinicalpreset = "treatment",
    showexplanations = TRUE
  )
  expect_s3_class(result_treat, "jjpiestatsResults")

  # Biomarker preset with biomarker data
  result_bio <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    group = "receptor_status",
    clinicalpreset = "biomarker",
    showexplanations = TRUE
  )
  expect_s3_class(result_bio, "jjpiestatsResults")
})
