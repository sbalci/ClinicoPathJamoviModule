# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjscatterstats
# ═══════════════════════════════════════════════════════════
#
# Tests all parameter combinations and interactions
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jjscatterstats_test, package = "ClinicoPath", envir = environment())
data(jjscatterstats_clinical, package = "ClinicoPath", envir = environment())
data(jjscatterstats_treatment, package = "ClinicoPath", envir = environment())
data(jjscatterstats_expression, package = "ClinicoPath", envir = environment())
data(jjscatterstats_survival, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Statistical Test + Marginal Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles parametric + histogram marginals", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    marginalType = "histogram",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles nonparametric + density marginals", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    typestatistics = "nonparametric",
    marginalType = "density",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles robust + boxplot marginals", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "tumor_size",
    group = "lymph_nodes",
    typestatistics = "robust",
    marginalType = "boxplot",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles bayes + histogram marginals", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    typestatistics = "bayes",
    marginalType = "histogram",
    resultssubtitle = TRUE,
    bfmessage = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Smooth Method + Marginal Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles lm smooth + histogram", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    smoothMethod = "lm",
    marginalType = "histogram"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles loess smooth + density", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_treatment,
    dep = "drug_dose",
    group = "response_score",
    smoothMethod = "loess",
    marginalType = "density"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles gam smooth + boxplot", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "bmi",
    group = "systolic_bp",
    smoothMethod = "gam",
    marginalType = "boxplot"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Grouping + Statistical Test Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles grouping + parametric test", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles grouping + nonparametric test", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    grvar = "disease_stage",
    typestatistics = "nonparametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles grouping + bayes test", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "cholesterol",
    group = "triglycerides",
    grvar = "diagnosis",
    typestatistics = "bayes",
    resultssubtitle = TRUE,
    bfmessage = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Grouping + Marginal + Statistical Test
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles grouping + marginals + statistics", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "protein_expression",
    group = "mutation_burden",
    grvar = "tumor_grade",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Customization Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles full customization", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    typestatistics = "parametric",
    marginalType = "histogram",
    smoothMethod = "lm",
    pointsize = 4,
    pointalpha = 0.6,
    smoothlinesize = 2,
    smoothlinecolor = "red",
    xsidefill = "#1f77b4",
    ysidefill = "#ff7f0e",
    mytitle = "Glucose vs HbA1c",
    xtitle = "Glucose (mg/dL)",
    ytitle = "HbA1c (%)",
    resultssubtitle = TRUE,
    conflevel = 0.99,
    k = 3
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. ggpubr Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles ggpubr + correlation + smooth", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    addGGPubrPlot = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "pearson",
    ggpubrAddSmooth = TRUE,
    ggpubrPalette = "jco"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles ggpubr with different palettes", {
  devtools::load_all()

  palettes <- c("jco", "npg", "aaas", "lancet", "jama", "nejm")

  for (palette in palettes) {
    result <- jjscatterstats(
      data = jjscatterstats_clinical,
      dep = "cholesterol",
      group = "triglycerides",
      addGGPubrPlot = TRUE,
      ggpubrPalette = palette
    )

    expect_s3_class(result, "jjscatterstatsResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 7. Complete Clinical Workflows
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles complete biomarker analysis", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "tumor_grade",
    typestatistics = "parametric",
    marginalType = "histogram",
    smoothMethod = "lm",
    resultssubtitle = TRUE,
    conflevel = 0.95,
    pointsize = 3.5,
    smoothlinecolor = "blue",
    mytitle = "Ki67 vs Tumor Size by Grade"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles complete lab value analysis", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_clinical,
    dep = "glucose",
    group = "hemoglobin_a1c",
    grvar = "diagnosis",
    typestatistics = "parametric",
    marginalType = "density",
    resultssubtitle = TRUE,
    conflevel = 0.95,
    addGGPubrPlot = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrPalette = "npg"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles complete dose-response analysis", {
  devtools::load_all()

  result <- jjscatterstats(
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
    mytitle = "Dose-Response Relationship"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles complete gene expression analysis", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_expression,
    dep = "gene_a_expression",
    group = "gene_b_expression",
    grvar = "cancer_type",
    typestatistics = "parametric",
    marginalType = "histogram",
    resultssubtitle = TRUE,
    mytitle = "Co-regulated Gene Expression",
    xtitle = "Gene A (log2)",
    ytitle = "Gene B (log2)"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles complete survival biomarker analysis", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_survival,
    dep = "ki67_index",
    group = "survival_months",
    grvar = "treatment",
    typestatistics = "nonparametric",
    marginalType = "density",
    resultssubtitle = TRUE,
    smoothMethod = "loess",
    mytitle = "Ki67 vs Survival by Treatment"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles minimal configuration", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles maximum configuration", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "tumor_stage",
    typestatistics = "parametric",
    marginalType = "histogram",
    smoothMethod = "lm",
    resultssubtitle = TRUE,
    conflevel = 0.99,
    k = 3,
    marginal = TRUE,
    xsidefill = "#009E73",
    ysidefill = "#D55E00",
    pointsize = 4,
    pointalpha = 0.6,
    smoothlinesize = 2,
    smoothlinecolor = "blue",
    mytitle = "Comprehensive Biomarker Analysis",
    xtitle = "Ki67 Proliferation Index (%)",
    ytitle = "Tumor Size (mm)",
    addGGPubrPlot = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "pearson",
    ggpubrAddSmooth = TRUE,
    ggpubrPalette = "jco"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 9. All Statistical Tests Consistently
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats produces consistent results across test types", {
  devtools::load_all()

  test_types <- c("parametric", "nonparametric", "robust", "bayes")
  results <- list()

  for (test_type in test_types) {
    results[[test_type]] <- jjscatterstats(
      data = jjscatterstats_clinical,
      dep = "glucose",
      group = "hemoglobin_a1c",
      typestatistics = test_type,
      resultssubtitle = TRUE
    )

    expect_s3_class(results[[test_type]], "jjscatterstatsResults")
  }

  expect_equal(length(results), length(test_types))
})

# ═══════════════════════════════════════════════════════════
# 10. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Basic scatter
  result1 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size"
  )
  expect_s3_class(result1, "jjscatterstatsResults")

  # Step 2: Add marginals
  result2 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginalType = "histogram"
  )
  expect_s3_class(result2, "jjscatterstatsResults")

  # Step 3: Add statistics
  result3 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjscatterstatsResults")

  # Step 4: Add grouping
  result4 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result4, "jjscatterstatsResults")

  # Step 5: Add customization
  result5 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    pointsize = 4,
    smoothlinecolor = "red",
    mytitle = "Biomarker Correlation"
  )
  expect_s3_class(result5, "jjscatterstatsResults")

  # Step 6: Add ggpubr
  result6 <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status",
    marginalType = "histogram",
    typestatistics = "parametric",
    resultssubtitle = TRUE,
    pointsize = 4,
    smoothlinecolor = "red",
    mytitle = "Biomarker Correlation",
    addGGPubrPlot = TRUE,
    ggpubrPalette = "jco"
  )
  expect_s3_class(result6, "jjscatterstatsResults")
})
