# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjpiestats
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of the jjpiestats function
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jjpiestats_test, package = "ClinicoPath", envir = environment())
data(jjpiestats_diagnostic, package = "ClinicoPath", envir = environment())
data(jjpiestats_treatment, package = "ClinicoPath", envir = environment())
data(jjpiestats_biomarker, package = "ClinicoPath", envir = environment())
data(jjpiestats_aggregated, package = "ClinicoPath", envir = environment())
data(jjpiestats_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Function Existence and Basic Execution
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats function exists and runs with minimal arguments", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Basic Pie Chart
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats creates basic pie chart with single categorical variable", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Statistical Test Types
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles parametric statistics", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles nonparametric statistics", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles robust statistics", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles Bayesian statistics", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "gender",
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Grouping Variable
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles grouping variable for contingency table", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles 2x2 contingency table", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Split/Grouped Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles split variable for separate pie charts", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    grvar = "hospital_site"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles both group and split variables", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "gender",
    grvar = "hospital_site"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Label Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles percentage labels", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    label = "percentage"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles count labels", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    label = "counts"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles both percentage and count labels", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "tumor_stage",
    label = "both"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Clinical Presets
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles diagnostic clinical preset", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_diagnostic,
    dep = "test_result",
    group = "disease_status",
    clinicalpreset = "diagnostic"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles treatment clinical preset", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_treatment,
    dep = "outcome",
    group = "treatment",
    clinicalpreset = "treatment"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles biomarker clinical preset", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_biomarker,
    dep = "expression_level",
    clinicalpreset = "biomarker"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Display Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles statistical results subtitle", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles Bayes factor message", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "gender",
    typestatistics = "parametric",
    bfmessage = TRUE
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Theme Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles original ggstatsplot theme", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    originaltheme = TRUE
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles jamovi default theme", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    originaltheme = FALSE
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Donut Chart Variant
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles donut chart with ggpubr", {
  devtools::load_all()

  result <- jjpiestats(
    data = jjpiestats_test,
    dep = "tumor_stage",
    addGGPubrDonut = TRUE
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles donut chart palettes", {
  devtools::load_all()

  # JCO palette
  result_jco <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco"
  )
  expect_s3_class(result_jco, "jjpiestatsResults")

  # NPG palette
  result_npg <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "npg"
  )
  expect_s3_class(result_npg, "jjpiestatsResults")

  # Lancet palette
  result_lancet <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "lancet"
  )
  expect_s3_class(result_lancet, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 11. All Test Datasets
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats works with all test datasets", {
  devtools::load_all()

  # jjpiestats_test
  result1 <- jjpiestats(data = jjpiestats_test, dep = "treatment_response")
  expect_s3_class(result1, "jjpiestatsResults")

  # jjpiestats_diagnostic
  result2 <- jjpiestats(data = jjpiestats_diagnostic, dep = "test_result")
  expect_s3_class(result2, "jjpiestatsResults")

  # jjpiestats_treatment
  result3 <- jjpiestats(data = jjpiestats_treatment, dep = "outcome")
  expect_s3_class(result3, "jjpiestatsResults")

  # jjpiestats_biomarker
  result4 <- jjpiestats(data = jjpiestats_biomarker, dep = "expression_level")
  expect_s3_class(result4, "jjpiestatsResults")

  # jjpiestats_small
  result5 <- jjpiestats(data = jjpiestats_small, dep = "category")
  expect_s3_class(result5, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Confidence Level Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles different confidence levels", {
  devtools::load_all()

  # 90% confidence
  result_90 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    conflevel = 0.90
  )
  expect_s3_class(result_90, "jjpiestatsResults")

  # 95% confidence (default)
  result_95 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    conflevel = 0.95
  )
  expect_s3_class(result_95, "jjpiestatsResults")

  # 99% confidence
  result_99 <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    conflevel = 0.99
  )
  expect_s3_class(result_99, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Decimal Digits Options
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles different decimal digits", {
  devtools::load_all()

  # 0 digits
  result_0 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    digits = 0
  )
  expect_s3_class(result_0, "jjpiestatsResults")

  # 2 digits (default)
  result_2 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    digits = 2
  )
  expect_s3_class(result_2, "jjpiestatsResults")

  # 5 digits
  result_5 <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    digits = 5
  )
  expect_s3_class(result_5, "jjpiestatsResults")
})
