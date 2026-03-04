# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjbarstats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjbarstats function exists and loads", {

  expect_true(exists("jjbarstats"))
})

test_that("jjbarstats runs with minimal required arguments", {

  data(jjbarstats_test, package = "ClinicoPath")

  # Minimal required arguments (dep and group)
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats produces expected output structure", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))
})

test_that("jjbarstats handles 3×3 contingency table", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles 2×2 contingency table", {

  data(jjbarstats_diagnostic)

  result <- jjbarstats(
    data = jjbarstats_diagnostic,
    dep = "diagnosis",
    group = "test_result",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles parametric statistics", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles nonparametric statistics", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles robust statistics", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles Bayesian statistics", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles paired/repeated measures", {

  data(jjbarstats_paired)

  result <- jjbarstats(
    data = jjbarstats_paired,
    dep = "baseline_status",
    group = "followup_status",
    paired = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles independent samples", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    paired = FALSE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles split-by variable", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    grvar = "sex"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles aggregated data with counts", {

  data(jjbarstats_aggregated)

  result <- jjbarstats(
    data = jjbarstats_aggregated,
    dep = "response_category",
    group = "treatment_group",
    counts = "count"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles pairwise comparisons", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    pairwisecomparisons = TRUE,
    padjustmethod = "holm"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles label display modes", {

  data(jjbarstats_test)

  # Percentage labels
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    label = "percentage"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Count labels
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    label = "counts"
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # Both labels
  result3 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    label = "both"
  )
  expect_s3_class(result3, "jjbarstatsResults")
})

test_that("jjbarstats handles proportion test", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    proportiontest = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles clinical presets", {

  data(jjbarstats_diagnostic)

  # Diagnostic preset
  result1 <- jjbarstats(
    data = jjbarstats_diagnostic,
    dep = "diagnosis",
    group = "test_result",
    clinicalpreset = "diagnostic"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Treatment preset
  data(jjbarstats_test)
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    clinicalpreset = "treatment"
  )
  expect_s3_class(result2, "jjbarstatsResults")
})

test_that("jjbarstats handles two-level factors", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "disease_status",
    group = "sex"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles three-level factors", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles four-level factors", {

  data(jjbarstats_test)

  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "tumor_stage"
  )

  expect_s3_class(result, "jjbarstatsResults")
})
