# ═══════════════════════════════════════════════════════════
# Integration Tests: jjbarstats
# ═══════════════════════════════════════════════════════════
#
# Tests for integration with test datasets, data workflows,
# and realistic clinical scenarios.

library(testthat)

test_that("jjbarstats integrates with test datasets", {
  devtools::load_all()

  # Test comprehensive dataset loads
  data(jjbarstats_test, package = "ClinicoPath")
  expect_true(exists("jjbarstats_test"))
  expect_s3_class(jjbarstats_test, "data.frame")

  # Test diagnostic data loads
  data(jjbarstats_diagnostic)
  expect_true(exists("jjbarstats_diagnostic"))

  # Test paired data loads
  data(jjbarstats_paired)
  expect_true(exists("jjbarstats_paired"))

  # Test aggregated data loads
  data(jjbarstats_aggregated)
  expect_true(exists("jjbarstats_aggregated"))

  # Test biomarker data loads
  data(jjbarstats_biomarker)
  expect_true(exists("jjbarstats_biomarker"))
})

test_that("jjbarstats test datasets have proper structure", {
  devtools::load_all()

  data(jjbarstats_test)

  # Check required columns
  expect_true("response" %in% names(jjbarstats_test))
  expect_true("treatment" %in% names(jjbarstats_test))
  expect_true("disease_status" %in% names(jjbarstats_test))

  # Check data types
  expect_true(is.factor(jjbarstats_test$response))
  expect_true(is.factor(jjbarstats_test$treatment))

  # Check factor levels
  expect_equal(nlevels(jjbarstats_test$response), 3)
  expect_equal(nlevels(jjbarstats_test$treatment), 3)
})

test_that("jjbarstats scenario: Clinical trial response analysis", {
  devtools::load_all()

  data(jjbarstats_test)

  # Primary endpoint: Response rates by treatment
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    clinicalpreset = "treatment",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    padjustmethod = "bonferroni",
    showSummary = TRUE,
    showAssumptions = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
  expect_true(!is.null(result$plot))
})

test_that("jjbarstats scenario: Diagnostic test validation", {
  devtools::load_all()

  data(jjbarstats_diagnostic)

  # 2×2 diagnostic table analysis
  result <- jjbarstats(
    data = jjbarstats_diagnostic,
    dep = "diagnosis",
    group = "test_result",
    clinicalpreset = "diagnostic",
    typestatistics = "parametric",
    label = "both",
    showSummary = TRUE,
    showInterpretation = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Treatment effect over time (paired)", {
  devtools::load_all()

  data(jjbarstats_paired)

  # McNemar's test for before/after comparison
  result <- jjbarstats(
    data = jjbarstats_paired,
    dep = "baseline_status",
    group = "followup_status",
    paired = TRUE,
    typestatistics = "parametric",
    label = "both"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Biomarker expression by subtype", {
  devtools::load_all()

  data(jjbarstats_biomarker)

  # HER2 expression across cancer subtypes
  result <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "her2_status",
    group = "subtype",
    clinicalpreset = "biomarker",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    showInterpretation = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Risk factor association", {
  devtools::load_all()

  data(jjbarstats_test)

  # Smoking status vs disease recurrence
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "disease_status",
    group = "smoking_status",
    clinicalpreset = "riskfactor",
    typestatistics = "parametric",
    proportiontest = TRUE,
    showSummary = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Subgroup analysis by demographics", {
  devtools::load_all()

  data(jjbarstats_test)

  # Response by treatment, stratified by sex
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    grvar = "sex",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Response by treatment, stratified by age
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    grvar = "age_group",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Disease severity stratification", {
  devtools::load_all()

  data(jjbarstats_test)

  # Response by tumor stage
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "tumor_stage",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    padjustmethod = "holm"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Comorbidity impact analysis", {
  devtools::load_all()

  data(jjbarstats_test)

  # Disease recurrence by comorbidity status
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "disease_status",
    group = "comorbidity",
    typestatistics = "parametric",
    label = "both",
    showSummary = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Multiple biomarker analysis", {
  devtools::load_all()

  data(jjbarstats_biomarker)

  # ER status by subtype
  result1 <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "er_status",
    group = "subtype"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # PR status by subtype
  result2 <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "pr_status",
    group = "subtype"
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # HER2 status by subtype
  result3 <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "her2_status",
    group = "subtype"
  )
  expect_s3_class(result3, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Treatment comparison by subgroups", {
  devtools::load_all()

  data(jjbarstats_paired)

  # Baseline to follow-up change by treatment arm
  result <- jjbarstats(
    data = jjbarstats_paired,
    dep = "followup_status",
    group = "treatment_arm",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Aggregated clinical trial data", {
  devtools::load_all()

  data(jjbarstats_aggregated)

  # Pre-summarized response rates
  result <- jjbarstats(
    data = jjbarstats_aggregated,
    dep = "response_category",
    group = "treatment_group",
    counts = "count",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Bayesian evidence assessment", {
  devtools::load_all()

  data(jjbarstats_test)

  # Bayesian analysis for treatment effect
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "bayes",
    bfmessage = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Fisher's exact test for small samples", {
  devtools::load_all()

  data(jjbarstats_test)
  small_sample <- jjbarstats_test[1:25, ]

  # Nonparametric for small sample
  result <- jjbarstats(
    data = small_sample,
    dep = "response",
    group = "treatment",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats scenario: Proportion test with expected ratios", {
  devtools::load_all()

  data(jjbarstats_test)

  # Test against unequal expected proportions
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    proportiontest = TRUE,
    ratio = "0.5,0.3,0.2",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats works with filtered data", {
  devtools::load_all()

  data(jjbarstats_test)

  # Filter to advanced stage only
  advanced_stage <- subset(jjbarstats_test,
                           tumor_stage %in% c("Stage III", "Stage IV"))

  result <- jjbarstats(
    data = advanced_stage,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats works with subsetted data", {
  devtools::load_all()

  data(jjbarstats_test)

  # Subset to specific treatment arms
  subset_data <- subset(jjbarstats_test,
                        treatment %in% c("Placebo", "High Dose"))

  result <- jjbarstats(
    data = subset_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats works with CSV imported data", {
  skip_if_not(file.exists("data/jjbarstats_test.csv"),
              "CSV test file not available")

  devtools::load_all()

  # Read from CSV
  csv_data <- read.csv("data/jjbarstats_test.csv", stringsAsFactors = TRUE)

  result <- jjbarstats(
    data = csv_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats works with OMV imported data", {
  skip_if_not(file.exists("data/jjbarstats_test.omv"),
              "OMV test file not available")
  skip_if_not(requireNamespace("jmvReadWrite", quietly = TRUE),
              "jmvReadWrite package not available")

  devtools::load_all()

  # Read from OMV
  omv_data <- jmvReadWrite::read_omv("data/jjbarstats_test.omv")

  result <- jjbarstats(
    data = omv_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats preserves data integrity", {
  devtools::load_all()

  data(jjbarstats_test)
  original_data <- jjbarstats_test

  # Run analysis
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )

  # Original data should be unchanged
  expect_equal(jjbarstats_test, original_data)
})

test_that("jjbarstats handles multiple consecutive analyses", {
  devtools::load_all()

  data(jjbarstats_test)

  # Run multiple analyses in sequence
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )

  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "disease_status",
    group = "smoking_status"
  )

  result3 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "tumor_stage"
  )

  # All should succeed
  expect_s3_class(result1, "jjbarstatsResults")
  expect_s3_class(result2, "jjbarstatsResults")
  expect_s3_class(result3, "jjbarstatsResults")
})

test_that("jjbarstats handles comprehensive multi-outcome report", {
  devtools::load_all()

  data(jjbarstats_test)

  # Primary outcome
  primary <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    grvar = "tumor_stage",
    clinicalpreset = "treatment",
    pairwisecomparisons = TRUE,
    showSummary = TRUE
  )
  expect_s3_class(primary, "jjbarstatsResults")

  # Risk factor analysis
  risk <- jjbarstats(
    data = jjbarstats_test,
    dep = "disease_status",
    group = "smoking_status",
    clinicalpreset = "riskfactor",
    proportiontest = TRUE
  )
  expect_s3_class(risk, "jjbarstatsResults")

  # Biomarker analysis
  data(jjbarstats_biomarker)
  biomarker <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "her2_status",
    group = "subtype",
    clinicalpreset = "biomarker"
  )
  expect_s3_class(biomarker, "jjbarstatsResults")

  # Paired analysis
  data(jjbarstats_paired)
  paired <- jjbarstats(
    data = jjbarstats_paired,
    dep = "baseline_status",
    group = "followup_status",
    paired = TRUE
  )
  expect_s3_class(paired, "jjbarstatsResults")
})
