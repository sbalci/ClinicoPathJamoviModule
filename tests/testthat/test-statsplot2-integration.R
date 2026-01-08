# ═══════════════════════════════════════════════════════════
# Integration Tests: statsplot2
# ═══════════════════════════════════════════════════════════
#
# Tests for integration with test datasets, data workflows,
# and realistic clinical scenarios.

library(testthat)

test_that("statsplot2 integrates with test datasets", {
  devtools::load_all()

  # Test comprehensive dataset loads
  data(statsplot2_test, package = "ClinicoPath")
  expect_true(exists("statsplot2_test"))
  expect_s3_class(statsplot2_test, "data.frame")

  # Test clinical trial data loads
  data(statsplot2_clinical)
  expect_true(exists("statsplot2_clinical"))

  # Test repeated measures data loads
  data(statsplot2_repeated)
  expect_true(exists("statsplot2_repeated"))

  # Test outliers data loads
  data(statsplot2_outliers)
  expect_true(exists("statsplot2_outliers"))

  # Test skewed data loads
  data(statsplot2_skewed)
  expect_true(exists("statsplot2_skewed"))
})

test_that("statsplot2 test datasets have proper structure", {
  devtools::load_all()

  data(statsplot2_test)

  # Check required columns for clinical trial
  expect_true("tumor_reduction" %in% names(statsplot2_test))
  expect_true("treatment" %in% names(statsplot2_test))
  expect_true("pain_score" %in% names(statsplot2_test))
  expect_true("response_status" %in% names(statsplot2_test))

  # Check data types
  expect_true(is.numeric(statsplot2_test$tumor_reduction))
  expect_true(is.factor(statsplot2_test$treatment) ||
              is.character(statsplot2_test$treatment))
  expect_true(is.factor(statsplot2_test$response_status))
})

test_that("statsplot2 scenario: Clinical trial primary outcome", {
  devtools::load_all()

  data(statsplot2_clinical)

  # Primary endpoint: Tumor reduction by treatment
  result <- statsplot2(
    data = statsplot2_clinical,
    dep = "tumor_reduction",
    group = "treatment",
    direction = "independent",
    distribution = "p",
    plotTitle = "Primary Outcome: Tumor Response by Treatment Arm",
    xlab = "Treatment Group",
    ylab = "Tumor Size Reduction (mm)"
  )

  expect_s3_class(result, "statsplot2Results")
  expect_true(!is.null(result$plot))
})

test_that("statsplot2 scenario: Secondary outcomes analysis", {
  devtools::load_all()

  data(statsplot2_test)

  # Pain scores (nonparametric)
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "pain_score",
    group = "treatment",
    distribution = "np",
    plotTitle = "Secondary Outcome: Pain Reduction"
  )
  expect_s3_class(result1, "statsplot2Results")

  # Quality of life (parametric)
  result2 <- statsplot2(
    data = statsplot2_test,
    dep = "qol_score",
    group = "treatment",
    distribution = "p",
    plotTitle = "Secondary Outcome: Quality of Life"
  )
  expect_s3_class(result2, "statsplot2Results")
})

test_that("statsplot2 scenario: Subgroup analysis by disease severity", {
  devtools::load_all()

  data(statsplot2_test)

  # Treatment effect stratified by tumor stage
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "tumor_stage",
    direction = "independent",
    distribution = "p",
    plotTitle = "Treatment Effect by Disease Severity"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Demographic subgroup analysis", {
  devtools::load_all()

  data(statsplot2_test)

  # By sex
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "sex",
    plotTitle = "Treatment Effect by Sex"
  )
  expect_s3_class(result1, "statsplot2Results")

  # By age group
  result2 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "age_group",
    plotTitle = "Treatment Effect by Age Group"
  )
  expect_s3_class(result2, "statsplot2Results")
})

test_that("statsplot2 scenario: Longitudinal symptom tracking", {
  devtools::load_all()

  data(statsplot2_repeated)

  # Symptom severity over time
  result <- statsplot2(
    data = statsplot2_repeated,
    dep = "symptom_severity",
    group = "timepoint",
    direction = "repeated",
    distribution = "p",
    plotTitle = "Symptom Trajectory Over 12 Weeks",
    xlab = "Assessment Timepoint",
    ylab = "Symptom Severity Score"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Treatment comparison over time", {
  devtools::load_all()

  data(statsplot2_repeated)

  # Compare treatment arms longitudinally
  result <- statsplot2(
    data = statsplot2_repeated,
    dep = "symptom_severity",
    group = "timepoint",
    grvar = "treatment_arm",
    direction = "repeated",
    distribution = "p",
    plotTitle = "Treatment Arm Comparison: Symptom Change"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Biomarker correlation analysis", {
  devtools::load_all()

  data(statsplot2_test)

  # Biomarker vs tumor reduction (scatter plot)
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "biomarker_level",
    direction = "independent",
    plotTitle = "Biomarker-Response Correlation",
    xlab = "Biomarker Level",
    ylab = "Tumor Size Reduction (mm)"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Response rate comparison", {
  devtools::load_all()

  data(statsplot2_test)

  # Categorical response by treatment
  result <- statsplot2(
    data = statsplot2_test,
    dep = "response_status",
    group = "treatment",
    direction = "independent",
    plotTitle = "Response Rates by Treatment Arm"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Robust analysis with outliers", {
  devtools::load_all()

  data(statsplot2_outliers)

  # Use robust statistics for outlier-contaminated data
  result <- statsplot2(
    data = statsplot2_outliers,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "r",
    plotTitle = "Robust Analysis: Treatment Effect with Outliers"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Nonparametric analysis for skewed data", {
  devtools::load_all()

  data(statsplot2_skewed)

  # Use nonparametric tests for non-normal distribution
  result <- statsplot2(
    data = statsplot2_skewed,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "np",
    plotTitle = "Nonparametric Analysis: Skewed Outcome Distribution"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 scenario: Bayesian evidence assessment", {
  devtools::load_all()

  data(statsplot2_test)

  # Bayesian analysis with Bayes Factor
  result <- statsplot2(
    data = statsplot2_test,
    dep = "qol_score",
    group = "treatment",
    distribution = "bf",
    plotTitle = "Bayesian Analysis: Treatment Effect Evidence"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 works with filtered data", {
  devtools::load_all()

  data(statsplot2_test)

  # Filter to high-stage patients only
  high_stage <- subset(statsplot2_test, tumor_stage == "Stage III-IV")

  result <- statsplot2(
    data = high_stage,
    dep = "tumor_reduction",
    group = "treatment",
    plotTitle = "Treatment Effect in Advanced Disease"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 works with subsetted data", {
  devtools::load_all()

  data(statsplot2_test)

  # Subset to specific treatment arms
  subset_data <- subset(statsplot2_test,
                        treatment %in% c("Placebo", "High Dose"))

  result <- statsplot2(
    data = subset_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 works with CSV imported data", {
  skip_if_not(file.exists("data/statsplot2_test.csv"),
              "CSV test file not available")

  devtools::load_all()

  # Read from CSV
  csv_data <- read.csv("data/statsplot2_test.csv", stringsAsFactors = TRUE)

  result <- statsplot2(
    data = csv_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 works with OMV imported data", {
  skip_if_not(file.exists("data/statsplot2_test.omv"),
              "OMV test file not available")
  skip_if_not(requireNamespace("jmvReadWrite", quietly = TRUE),
              "jmvReadWrite package not available")

  devtools::load_all()

  # Read from OMV
  omv_data <- jmvReadWrite::read_omv("data/statsplot2_test.omv")

  result <- statsplot2(
    data = omv_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 preserves data integrity", {
  devtools::load_all()

  data(statsplot2_test)
  original_data <- statsplot2_test

  # Run analysis
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Original data should be unchanged
  expect_equal(statsplot2_test, original_data)
})

test_that("statsplot2 handles multiple consecutive analyses", {
  devtools::load_all()

  data(statsplot2_test)

  # Run multiple analyses in sequence
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  result2 <- statsplot2(
    data = statsplot2_test,
    dep = "pain_score",
    group = "treatment",
    distribution = "np"
  )

  result3 <- statsplot2(
    data = statsplot2_test,
    dep = "response_status",
    group = "treatment"
  )

  # All should succeed
  expect_s3_class(result1, "statsplot2Results")
  expect_s3_class(result2, "statsplot2Results")
  expect_s3_class(result3, "statsplot2Results")
})

test_that("statsplot2 handles comprehensive multi-outcome report", {
  devtools::load_all()

  data(statsplot2_test)

  # Primary outcome
  primary <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "tumor_stage",
    distribution = "p",
    plotTitle = "Primary Outcome"
  )
  expect_s3_class(primary, "statsplot2Results")

  # Secondary continuous outcomes
  pain <- statsplot2(
    data = statsplot2_test,
    dep = "pain_score",
    group = "treatment",
    distribution = "np",
    plotTitle = "Pain Scores"
  )
  expect_s3_class(pain, "statsplot2Results")

  qol <- statsplot2(
    data = statsplot2_test,
    dep = "qol_score",
    group = "treatment",
    distribution = "p",
    plotTitle = "Quality of Life"
  )
  expect_s3_class(qol, "statsplot2Results")

  # Categorical outcome
  response <- statsplot2(
    data = statsplot2_test,
    dep = "response_status",
    group = "treatment",
    plotTitle = "Response Rates"
  )
  expect_s3_class(response, "statsplot2Results")
})
