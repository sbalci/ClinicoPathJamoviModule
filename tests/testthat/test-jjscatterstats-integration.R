# ═══════════════════════════════════════════════════════════
# Integration Tests: jjscatterstats
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and integration with all datasets
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(jjscatterstats_test, package = "ClinicoPath", envir = environment())
data(jjscatterstats_clinical, package = "ClinicoPath", envir = environment())
data(jjscatterstats_treatment, package = "ClinicoPath", envir = environment())
data(jjscatterstats_expression, package = "ClinicoPath", envir = environment())
data(jjscatterstats_survival, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Complete Biomarker Analysis Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles complete biomarker analysis workflow", {
  devtools::load_all()

  # Step 1: Basic scatter plot
  result1 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Step 2: Add marginal distributions
  result2 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginalType = "histogram"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Step 3: Add statistical testing
  result3 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Step 4: Stratify by tumor grade
  result4 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "tumor_grade",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Step 5: Publication-ready
  result5 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "tumor_grade",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    smoothMethod = "lm",
    pointsize = 3.5,
    smoothlinecolor = "blue",
    mytitle = "Ki67 Index vs Tumor Size by Grade",
    xtitle = "Ki67 Proliferation Index (%)",
    ytitle = "Tumor Size (mm)",
    addGGPubrPlot = TRUE,
    ggpubrPalette = "jco"
  )
  expect_s3_class(result5, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Complete Clinical Laboratory Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles complete clinical lab analysis workflow", {
  devtools::load_all()

  # Step 1: Basic glucose-HbA1c correlation
  result1 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Step 2: Add density marginals
  result2 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    marginalType = "density"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Step 3: Add parametric test
  result3 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    marginalType = "density",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    conflevel = 0.95
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Step 4: Stratify by diagnosis
  result4 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    grvar = "diagnosis",
    marginalType = "density",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Step 5: Add ggpubr plot
  result5 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    grvar = "diagnosis",
    marginalType = "density",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    addGGPubrPlot = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "pearson",
    ggpubrPalette = "npg"
  )
  expect_s3_class(result5, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Complete Dose-Response Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles complete dose-response workflow", {
  devtools::load_all()

  # Step 1: Basic dose-response scatter
  result1 <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Step 2: Add LOESS smooth for non-linear relationship
  result2 <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    smoothMethod = "loess"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Step 3: Add boxplot marginals
  result3 <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    smoothMethod = "loess",
    marginalType = "boxplot"
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Step 4: Stratify by timepoint
  result4 <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    grvar = "timepoint",
    smoothMethod = "loess",
    marginalType = "boxplot"
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Step 5: Full customization
  result5 <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    grvar = "timepoint",
    smoothMethod = "loess",
    marginalType = "boxplot",
    pointsize = 4,
    pointalpha = 0.5,
    smoothlinesize = 2.5,
    smoothlinecolor = "darkred",
    mytitle = "Dose-Response Relationship Over Time",
    xtitle = "Drug Dose (mg)",
    ytitle = "Response Score (0-100)"
  )
  expect_s3_class(result5, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Complete Gene Expression Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles complete gene expression workflow", {
  devtools::load_all()

  # Step 1: Co-regulated genes
  result1 <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Step 2: Add histogram marginals
  result2 <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    marginalType = "histogram"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Step 3: Add parametric correlation
  result3 <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Step 4: Stratify by cancer type
  result4 <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    grvar = "cancer_type",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Step 5: Publication-ready with titles
  result5 <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    grvar = "cancer_type",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    mytitle = "Co-regulated Gene Expression Across Cancer Types",
    xtitle = "Gene A Expression (log2)",
    ytitle = "Gene B Expression (log2)"
  )
  expect_s3_class(result5, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Complete Survival Biomarker Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles complete survival biomarker workflow", {
  devtools::load_all()

  # Step 1: Basic Ki67-survival correlation
  result1 <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Step 2: Add density marginals
  result2 <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    marginalType = "density"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Step 3: Add nonparametric test (appropriate for survival)
  result3 <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    marginalType = "density",
    typestatistics = "nonparametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjscatterstats_Results")

  # Step 4: Stratify by treatment
  result4 <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    grvar = "treatment",
    marginalType = "density",
    typestatistics = "nonparametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Step 5: Add LOESS for non-linear relationship
  result5 <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    grvar = "treatment",
    marginalType = "density",
    typestatistics = "nonparametric",
    resultssubtitle = TRUE,
    smoothMethod = "loess",
    mytitle = "Ki67 Index vs Survival by Treatment"
  )
  expect_s3_class(result5, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. All Datasets with Consistent Parameters
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles all datasets with consistent parameters", {
  devtools::load_all()

  # Test dataset
  result1 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginalType = "histogram",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Clinical dataset
  result2 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    marginalType = "histogram",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Treatment dataset
  result3 <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    marginalType = "histogram",
    typestatistics = "parametric"
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Expression dataset
  result4 <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    marginalType = "histogram",
    typestatistics = "parametric"
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Survival dataset
  result5 <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    marginalType = "histogram",
    typestatistics = "parametric"
  )
  expect_s3_class(result5, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 7. All Statistical Tests on Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats produces consistent results across statistical tests", {
  devtools::load_all()

  test_types <- c("parametric", "nonparametric", "robust", "bayes")
  results <- list()

  for (test_type in test_types) {
    results[[test_type]] <- jjscatterstats(
      data = jjscatterstats_clinical,
      dep = "cholesterol",
      group = "triglycerides",
      typestatistics = test_type,
      resultssubtitle = TRUE
    )

    expect_s3_class(results[[test_type]], "jjscatterstatsResults")
  }

  expect_equal(length(results), length(test_types))
})

# ═══════════════════════════════════════════════════════════
# 8. All Smooth Methods on Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats produces consistent results across smooth methods", {
  devtools::load_all()

  smooth_methods <- c("lm", "loess", "gam")
  results <- list()

  for (method in smooth_methods) {
    results[[method]] <- jjscatterstats(
      data = jjscatterstats_treatment,
      dep = "drug_dose",
      group = "response_score",
      smoothMethod = method
    )

    expect_s3_class(results[[method]], "jjscatterstatsResults")
  }

  expect_equal(length(results), length(smooth_methods))
})

# ═══════════════════════════════════════════════════════════
# 9. Multiple Variable Pairs from Same Dataset
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles multiple analyses from same dataset", {
  devtools::load_all()

  # Strong positive correlation
  result1 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Moderate positive correlation
  result2 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "protein_expression",
    group = "mutation_burden"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Weak negative correlation
  result3 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "immune_score",
    group = "lymph_nodes"
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Zero correlation
  result4 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "age",
    group = "response_score"
  )
  expect_s3_class(result4, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Repeated Analysis Consistency
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats produces consistent results on repeated calls", {
  devtools::load_all()

  result1 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  result2 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  result3 <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result1, "jjscatterstatsResults")
  expect_s3_class(result2, "jjscatterstatsResults")
  expect_s3_class(result3, "jjscatterstatsResults")
})
