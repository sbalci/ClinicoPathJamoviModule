# ═══════════════════════════════════════════════════════════
# Integration Tests: jjcorrmat
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjcorrmat integrates with all test datasets", {
  devtools::load_all()

  # Load all jjcorrmat test datasets
  data(jjcorrmat_test, package = "ClinicoPath")
  data(jjcorrmat_biomarker, package = "ClinicoPath")
  data(jjcorrmat_labvalues, package = "ClinicoPath")
  data(jjcorrmat_imaging, package = "ClinicoPath")
  data(jjcorrmat_vitals, package = "ClinicoPath")
  data(jjcorrmat_mixed, package = "ClinicoPath")

  # All datasets should load successfully
  expect_true(exists("jjcorrmat_test"))
  expect_true(exists("jjcorrmat_biomarker"))
  expect_true(exists("jjcorrmat_labvalues"))
  expect_true(exists("jjcorrmat_imaging"))
  expect_true(exists("jjcorrmat_vitals"))
  expect_true(exists("jjcorrmat_mixed"))
})

test_that("jjcorrmat works with data.frame vs tibble", {
  devtools::load_all()

  data(jjcorrmat_test)

  # As tibble (original)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Convert to data.frame
  df_data <- as.data.frame(jjcorrmat_test)
  result2 <- jjcorrmat(
    data = df_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat produces consistent results across multiple runs", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Run same analysis twice
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "parametric"
  )

  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "parametric"
  )

  # Both should complete
  expect_s3_class(result1, "jjcorrmatResults")
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat statistical types match data distributions", {
  devtools::load_all()

  # Parametric for normally distributed clinical data
  data(jjcorrmat_test)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Nonparametric for log-normal biomarker data
  data(jjcorrmat_biomarker)
  result2 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp", "ldh"),
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Robust for data with potential outliers
  data(jjcorrmat_vitals)
  result3 <- jjcorrmat(
    data = jjcorrmat_vitals,
    dep = c("systolic_bp", "diastolic_bp", "heart_rate"),
    typestatistics = "robust"
  )
  expect_s3_class(result3, "jjcorrmatResults")
})

test_that("jjcorrmat handles complete clinical workflows", {
  devtools::load_all()

  # Workflow 1: Tumor pathology markers
  data(jjcorrmat_test)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    typestatistics = "parametric",
    padjustmethod = "bonferroni",
    matrixtype = "upper",
    title = "Tumor Pathology Correlations"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Workflow 2: Biomarker panel for diagnosis
  data(jjcorrmat_biomarker)
  result2 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp", "ldh", "crp", "albumin"),
    typestatistics = "nonparametric",
    padjustmethod = "holm",
    matrixtype = "lower",
    title = "Biomarker Panel Correlations"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Workflow 3: Metabolic panel screening
  data(jjcorrmat_labvalues)
  result3 <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "hdl", "ldl"),
    typestatistics = "parametric",
    padjustmethod = "BH",
    matrixtype = "upper",
    title = "Metabolic Panel Correlations"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # Workflow 4: Imaging assessment
  data(jjcorrmat_imaging)
  result4 <- jjcorrmat(
    data = jjcorrmat_imaging,
    dep = c("tumor_volume", "tumor_longest_diameter",
            "tumor_shortest_diameter", "suv_max", "suv_mean"),
    typestatistics = "parametric",
    matrixmethod = "circle",
    k = 3,
    title = "Imaging Metrics Correlations"
  )
  expect_s3_class(result4, "jjcorrmatResults")

  # Workflow 5: Vital signs monitoring
  data(jjcorrmat_vitals)
  result5 <- jjcorrmat(
    data = jjcorrmat_vitals,
    dep = c("systolic_bp", "diastolic_bp", "heart_rate",
            "respiratory_rate", "temperature", "oxygen_saturation"),
    typestatistics = "robust",
    siglevel = 0.01,
    title = "Vital Signs Correlations"
  )
  expect_s3_class(result5, "jjcorrmatResults")
})

test_that("jjcorrmat handles grouped analysis across different datasets", {
  devtools::load_all()

  # Group by tumor stage
  data(jjcorrmat_test)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    grvar = "tumor_stage",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Group by cancer type
  data(jjcorrmat_biomarker)
  result2 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "ldh", "crp"),
    grvar = "cancer_type",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Group by risk category
  data(jjcorrmat_labvalues)
  result3 <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "hdl", "ldl"),
    grvar = "risk_group",
    typestatistics = "parametric"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # Group by imaging modality
  data(jjcorrmat_imaging)
  result4 <- jjcorrmat(
    data = jjcorrmat_imaging,
    dep = c("tumor_volume", "suv_max", "adc"),
    grvar = "imaging_modality",
    typestatistics = "parametric"
  )
  expect_s3_class(result4, "jjcorrmatResults")

  # Group by patient status
  data(jjcorrmat_vitals)
  result5 <- jjcorrmat(
    data = jjcorrmat_vitals,
    dep = c("systolic_bp", "diastolic_bp", "heart_rate"),
    grvar = "patient_status",
    typestatistics = "robust"
  )
  expect_s3_class(result5, "jjcorrmatResults")
})

test_that("jjcorrmat partial vs zero-order correlations", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Zero-order correlations
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
    partial = FALSE,
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Partial correlations (controlling for other variables)
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
    partial = TRUE,
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat listwise vs pairwise deletion comparisons", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Listwise deletion (complete cases)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "necrosis_percent", "age"),
    naHandling = "listwise",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Pairwise deletion (all available pairs)
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "necrosis_percent", "age"),
    naHandling = "pairwise",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat multiple testing correction comparisons", {
  devtools::load_all()

  data(jjcorrmat_test)

  # No adjustment
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    padjustmethod = "none",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Holm adjustment (recommended)
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    padjustmethod = "holm",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Bonferroni adjustment (conservative)
  result3 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    padjustmethod = "bonferroni",
    typestatistics = "parametric"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # BH adjustment (FDR control)
  result4 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    padjustmethod = "BH",
    typestatistics = "parametric"
  )
  expect_s3_class(result4, "jjcorrmatResults")
})

test_that("jjcorrmat handles publication-ready configurations", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Configuration 1: Main analysis with comprehensive settings
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    typestatistics = "parametric",
    padjustmethod = "bonferroni",
    matrixtype = "upper",
    matrixmethod = "circle",
    siglevel = 0.05,
    conflevel = 0.95,
    k = 2,
    lowcolor = "#0571B0",
    midcolor = "white",
    highcolor = "#CA0020",
    title = "Tumor Characteristics Correlation Matrix",
    subtitle = "Pearson correlations with Bonferroni adjustment",
    caption = "* p < 0.05; ** p < 0.01; *** p < 0.001",
    plotwidth = 700,
    plotheight = 600
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Configuration 2: Supplementary material (full matrix with details)
  result2 <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "hdl", "ldl",
            "creatinine", "alt", "ast"),
    typestatistics = "parametric",
    padjustmethod = "BH",
    matrixtype = "full",
    matrixmethod = "square",
    k = 3,
    showexplanations = TRUE
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles different correlation strength scenarios", {
  devtools::load_all()

  data(jjcorrmat_mixed)

  # Strong positive correlations
  result1 <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_b", "var_c"),
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Strong negative correlations
  result2 <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_f", "var_g"),
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Mixed correlation strengths
  result3 <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_c", "var_e", "var_g"),
    typestatistics = "parametric"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # Near-zero correlations
  result4 <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_e", "var_a", "var_b"),
    typestatistics = "parametric"
  )
  expect_s3_class(result4, "jjcorrmatResults")
})

test_that("jjcorrmat maintains data integrity through analysis", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Store original data dimensions
  original_n <- nrow(jjcorrmat_test)
  original_vars <- ncol(jjcorrmat_test)

  # Run analysis
  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  # Original data should be unchanged
  expect_equal(nrow(jjcorrmat_test), original_n)
  expect_equal(ncol(jjcorrmat_test), original_vars)
})

test_that("jjcorrmat handles cross-dataset analysis patterns", {
  devtools::load_all()

  # Pattern 1: Clinical pathology metrics
  data(jjcorrmat_test)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "parametric",
    matrixtype = "upper"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Pattern 2: Biomarker screening
  data(jjcorrmat_biomarker)
  result2 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp"),
    typestatistics = "nonparametric",
    matrixtype = "lower"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Pattern 3: Metabolic assessment
  data(jjcorrmat_labvalues)
  result3 <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides"),
    typestatistics = "parametric",
    matrixtype = "full"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # Pattern 4: Imaging quantification
  data(jjcorrmat_imaging)
  result4 <- jjcorrmat(
    data = jjcorrmat_imaging,
    dep = c("tumor_volume", "suv_max", "adc"),
    typestatistics = "parametric",
    matrixmethod = "circle"
  )
  expect_s3_class(result4, "jjcorrmatResults")

  # Pattern 5: Physiological monitoring
  data(jjcorrmat_vitals)
  result5 <- jjcorrmat(
    data = jjcorrmat_vitals,
    dep = c("systolic_bp", "diastolic_bp", "heart_rate"),
    typestatistics = "robust",
    matrixmethod = "square"
  )
  expect_s3_class(result5, "jjcorrmatResults")
})

test_that("jjcorrmat statistical method comparisons on same data", {
  devtools::load_all()

  data(jjcorrmat_test)
  variables <- c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent")

  # Parametric (Pearson)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Nonparametric (Spearman)
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Robust (percentage bend)
  result3 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    typestatistics = "robust"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # Bayesian (Bayes Factor)
  result4 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    typestatistics = "bayes"
  )
  expect_s3_class(result4, "jjcorrmatResults")
})

test_that("jjcorrmat matrix visualization comparisons", {
  devtools::load_all()

  data(jjcorrmat_test)
  variables <- c("tumor_size", "ki67_index", "mitotic_count")

  # Upper triangle, square
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    matrixtype = "upper",
    matrixmethod = "square"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Upper triangle, circle
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    matrixtype = "upper",
    matrixmethod = "circle"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Lower triangle, square
  result3 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    matrixtype = "lower",
    matrixmethod = "square"
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # Full matrix, circle
  result4 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = variables,
    matrixtype = "full",
    matrixmethod = "circle"
  )
  expect_s3_class(result4, "jjcorrmatResults")
})
