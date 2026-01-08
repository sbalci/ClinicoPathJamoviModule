# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjpiestats
# ═══════════════════════════════════════════════════════════
#
# Tests all possible argument combinations for the jjpiestats function
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test data
data(jjpiestats_test, package = "ClinicoPath", envir = environment())
data(jjpiestats_diagnostic, package = "ClinicoPath", envir = environment())
data(jjpiestats_treatment, package = "ClinicoPath", envir = environment())
data(jjpiestats_biomarker, package = "ClinicoPath", envir = environment())
data(jjpiestats_aggregated, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Statistical Test + Group Variable
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines statistical tests with grouping", {
  devtools::load_all()

  # Parametric + group
  result_param <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    typestatistics = "parametric"
  )
  expect_s3_class(result_param, "jjpiestatsResults")

  # Nonparametric + group
  result_nonparam <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result_nonparam, "jjpiestatsResults")

  # Bayes + group
  result_bayes <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    typestatistics = "bayes"
  )
  expect_s3_class(result_bayes, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Statistical Test + Split Variable
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines statistical tests with split variable", {
  devtools::load_all()

  # Parametric + split
  result1 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    grvar = "hospital_site",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # Robust + split
  result2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    grvar = "age_group",
    typestatistics = "robust"
  )
  expect_s3_class(result2, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Group + Split Variable Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles group and split variables together", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "disease_severity",
    grvar = "hospital_site",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Label + Display Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines labels with display options", {
  devtools::load_all()

  # Percentage labels + subtitle
  result1 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    label = "percentage",
    resultssubtitle = TRUE
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # Count labels + Bayes factor message
  result2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "gender",
    label = "counts",
    typestatistics = "parametric",
    bfmessage = TRUE
  )
  expect_s3_class(result2, "jjpiestatsResults")

  # Both labels + all displays
  result3 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "treatment_arm",
    label = "both",
    resultssubtitle = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE
  )
  expect_s3_class(result3, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Clinical Preset + Additional Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines clinical presets with other options", {
  devtools::load_all()

  # Diagnostic preset + results subtitle
  result_diag <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    clinicalpreset = "diagnostic",
    resultssubtitle = TRUE,
    showexplanations = TRUE
  )
  expect_s3_class(result_diag, "jjpiestatsResults")

  # Treatment preset + summary
  result_treat <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    clinicalpreset = "treatment",
    showSummary = TRUE,
    showInterpretation = TRUE
  )
  expect_s3_class(result_treat, "jjpiestatsResults")

  # Biomarker preset + split
  result_bio <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    grvar = "cancer_type",
    clinicalpreset = "biomarker",
    showAssumptions = TRUE
  )
  expect_s3_class(result_bio, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Theme + Donut Chart Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines theme options with donut chart", {
  devtools::load_all()

  # Original theme + donut
  result1 <- jjpiestats(
    data = jjpiestats_test,
    dep = "tumor_stage",
    originaltheme = TRUE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco"
  )
  expect_s3_class(result1, "jjpiestatsResults")

  # jamovi theme + donut (different palette)
  result2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    originaltheme = FALSE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "lancet"
  )
  expect_s3_class(result2, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 7. All Statistical Tests with All Display Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines all statistical tests with all display options", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjpiestats(
      data = jjpiestats_test,
      dep = "treatment_response",
      group = "gender",
      typestatistics = stat_type,
      resultssubtitle = TRUE,
      showSummary = TRUE,
      showAssumptions = TRUE,
      showInterpretation = TRUE
    )

    expect_s3_class(result, "jjpiestatsResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 8. Proportion Test with Ratio
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles proportion test with expected ratios", {
  devtools::load_all()

  # Equal proportions (2 levels)
  result_equal2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "gender",
    proportiontest = TRUE,
    ratio = "0.5,0.5"
  )
  expect_s3_class(result_equal2, "jjpiestatsResults")

  # Unequal proportions (4 levels)
  result_unequal <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    proportiontest = TRUE,
    ratio = "0.25,0.35,0.25,0.15"
  )
  expect_s3_class(result_unequal, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Paired Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles paired/repeated measures analysis", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    paired = TRUE
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Confidence Level + Digits Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats combines confidence levels with decimal digits", {
  devtools::load_all()

  # High confidence + high precision
  result_high <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    conflevel = 0.99,
    digits = 5
  )
  expect_s3_class(result_high, "jjpiestatsResults")

  # Low confidence + low precision
  result_low <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    group = "gender",
    conflevel = 0.90,
    digits = 0
  )
  expect_s3_class(result_low, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Complete Feature Combination - Diagnostic Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles complete diagnostic test workflow", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    grvar = "clinical_site",
    typestatistics = "parametric",
    clinicalpreset = "diagnostic",
    label = "both",
    resultssubtitle = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE,
    showInterpretation = TRUE,
    showexplanations = TRUE,
    conflevel = 0.95,
    digits = 3
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Complete Feature Combination - Treatment Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles complete treatment comparison workflow", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    grvar = "study_site",
    typestatistics = "nonparametric",
    clinicalpreset = "treatment",
    label = "both",
    resultssubtitle = TRUE,
    showSummary = TRUE,
    showInterpretation = TRUE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco",
    conflevel = 0.95,
    digits = 2
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Complete Feature Combination - Biomarker Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles complete biomarker distribution workflow", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    group = "receptor_status",
    grvar = "cancer_type",
    typestatistics = "robust",
    clinicalpreset = "biomarker",
    label = "percentage",
    resultssubtitle = TRUE,
    showSummary = TRUE,
    originaltheme = FALSE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "npg",
    conflevel = 0.95,
    digits = 2
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles minimal configuration", {
  devtools::load_all()

  result_min <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity"
  )

  expect_s3_class(result_min, "jjpiestatsResults")
})

test_that("jjpiestats handles maximum configuration", {
  devtools::load_all()

  result_max <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    grvar = "hospital_site",
    typestatistics = "parametric",
    clinicalpreset = "custom",
    label = "both",
    digits = 3,
    conflevel = 0.99,
    proportiontest = TRUE,
    paired = FALSE,
    bfmessage = TRUE,
    resultssubtitle = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE,
    showInterpretation = TRUE,
    showexplanations = TRUE,
    originaltheme = FALSE,
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco"
  )

  expect_s3_class(result_max, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 15. Different Categorical Variable Levels
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles different numbers of categorical levels", {
  devtools::load_all()

  # 2 levels
  result_2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "gender"
  )
  expect_s3_class(result_2, "jjpiestatsResults")

  # 3 levels
  result_3 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity"
  )
  expect_s3_class(result_3, "jjpiestatsResults")

  # 4 levels
  result_4 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response"
  )
  expect_s3_class(result_4, "jjpiestatsResults")

  # 4 levels (different variable)
  result_4b <- jjpiestats(
    data = jjpiestats_test,
    dep = "tumor_stage"
  )
  expect_s3_class(result_4b, "jjpiestatsResults")
})
