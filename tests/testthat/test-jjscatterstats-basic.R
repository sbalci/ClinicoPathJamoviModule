# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjscatterstats
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of scatter plots with correlation analysis
# Generated: 2026-01-06

library(testthat)

# Load all test datasets
data(jjscatterstats_test, package = "ClinicoPath", envir = environment())
data(jjscatterstats_clinical, package = "ClinicoPath", envir = environment())
data(jjscatterstats_treatment, package = "ClinicoPath", envir = environment())
data(jjscatterstats_expression, package = "ClinicoPath", envir = environment())
data(jjscatterstats_survival, package = "ClinicoPath", envir = environment())
data(jjscatterstats_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Function Existence and Minimal Execution
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats function exists and runs with minimal arguments", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Different Variable Pairs (Correlation Strengths)
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles strong positive correlation", {

  # Ki67 vs tumor size (r ≈ 0.75)
  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles moderate positive correlation", {

  # Protein expression vs mutation burden (r ≈ 0.5)
  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "protein_expression",
    group = "mutation_burden"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles weak negative correlation", {

  # Immune score vs lymph nodes
  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "immune_score",
    group = "lymph_nodes"
  )

  expect_s3_class(result, "jjscatterstats_Results")
})

test_that("jjscatterstats handles zero correlation", {

  # Age is independent
  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "age",
    group = "response_score"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Statistical Test Types
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles parametric test (Pearson)", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles nonparametric test (Spearman)", {

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    typestatistics = "nonparametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles robust test", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "tumor_size",
    group = "lymph_nodes",
    typestatistics = "robust",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles Bayesian test", {

  result <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    typestatistics = "bayes",
    resultssubtitle = TRUE,
    bfmessage = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Marginal Distribution Types
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles histogram marginals", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "cholesterol",
    group = "triglycerides",
    marginalType = "histogram"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles density marginals", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "protein_expression",
    marginalType = "density"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles boxplot marginals", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "bmi",
    group = "systolic_bp",
    marginalType = "boxplot"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles no marginals", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginalType = "none"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Smooth Methods
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles linear smooth (lm)", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    smoothMethod = "lm"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles LOESS smooth", {

  result <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    smoothMethod = "loess"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles GAM smooth", {

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    smoothMethod = "gam"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Grouped Scatter Plots
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles grouping variable (2 levels)", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles grouping variable (3 levels)", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "protein_expression",
    group = "mutation_burden",
    grvar = "tumor_grade"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles grouping variable (4 levels)", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "tumor_stage"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Customization Options
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles point customization", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    pointsize = 5,
    pointalpha = 0.7
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles smooth line customization", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    smoothlinesize = 2.5,
    smoothlinecolor = "red"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles marginal color customization", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginal = TRUE,
    xsidefill = "#1f77b4",
    ysidefill = "#ff7f0e"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles titles", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    mytitle = "Biomarker Correlation",
    xtitle = "Ki67 Index (%)",
    ytitle = "Tumor Size (mm)"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 8. ggpubr Integration
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles ggpubr plot", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    addGGPubrPlot = TRUE,
    ggpubrPalette = "jco"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles ggpubr with correlation", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "cholesterol",
    group = "triglycerides",
    addGGPubrPlot = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "pearson"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles ggpubr with smooth line", {

  result <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    addGGPubrPlot = TRUE,
    ggpubrAddSmooth = TRUE,
    ggpubrPalette = "npg"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 9. All Test Datasets
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats works with jjscatterstats_test dataset", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats works with jjscatterstats_clinical dataset", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats works with jjscatterstats_treatment dataset", {

  result <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats works with jjscatterstats_expression dataset", {

  result <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats works with jjscatterstats_survival dataset", {

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats works with jjscatterstats_small dataset", {

  result <- jjscatterstats(
    data = jjscatterstats_small,
    dep = "x_var",
    group = "y_var"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Clinical Applications
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles biomarker correlation analysis", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    typestatistics = "parametric",
    marginalType = "histogram",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles laboratory value correlation", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    marginalType = "density",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles dose-response analysis", {

  result <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    smoothMethod = "loess",
    marginalType = "boxplot"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles gene expression correlation", {

  result <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles survival biomarker analysis", {

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    typestatistics = "nonparametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Clinical Presets
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles biomarker_correlation preset", {

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    clinicalPreset = "biomarker_correlation"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles treatment_response_analysis preset", {

  result <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    clinicalPreset = "treatment_response_analysis"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles publication_ready preset", {

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    clinicalPreset = "publication_ready"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})
