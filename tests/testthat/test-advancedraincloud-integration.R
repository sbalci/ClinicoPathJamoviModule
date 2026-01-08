# ═══════════════════════════════════════════════════════════
# Integration Tests: advancedraincloud
# ═══════════════════════════════════════════════════════════
#
# Tests for integration with other functions, data workflows,
# and realistic clinical scenarios.

library(testthat)

test_that("advancedraincloud integrates with test dataset", {
  devtools::load_all()

  # Test data should load correctly
  data(advancedraincloud_test, package = "ClinicoPath")

  expect_true(exists("advancedraincloud_test"))
  expect_s3_class(advancedraincloud_test, "data.frame")
  expect_equal(nrow(advancedraincloud_test), 450)
  expect_equal(ncol(advancedraincloud_test), 13)
})

test_that("advancedraincloud test data has proper structure", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Check required variables exist
  expect_true("pain_score" %in% names(advancedraincloud_test))
  expect_true("treatment" %in% names(advancedraincloud_test))
  expect_true("timepoint" %in% names(advancedraincloud_test))
  expect_true("patient_id" %in% names(advancedraincloud_test))

  # Check data types
  expect_true(is.numeric(advancedraincloud_test$pain_score))
  expect_true(is.factor(advancedraincloud_test$treatment))
  expect_true(is.factor(advancedraincloud_test$timepoint))
})

test_that("advancedraincloud works with data subsets", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Subset by timepoint
  baseline <- subset(advancedraincloud_test, timepoint == "Baseline")
  result1 <- advancedraincloud(
    data = baseline,
    y_var = "pain_score",
    x_var = "treatment"
  )
  expect_s3_class(result1, "advancedraincloudResults")

  # Subset by treatment
  high_dose <- subset(advancedraincloud_test, treatment == "High Dose")
  result2 <- advancedraincloud(
    data = high_dose,
    y_var = "pain_score",
    x_var = "timepoint"
  )
  expect_s3_class(result2, "advancedraincloudResults")

  # Subset by multiple conditions
  high_dose_week12 <- subset(
    advancedraincloud_test,
    treatment == "High Dose" & timepoint == "Week 12"
  )
  result3 <- advancedraincloud(
    data = high_dose_week12,
    y_var = "pain_score",
    x_var = "disease_severity"
  )
  expect_s3_class(result3, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Basic group comparison", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: Compare treatment groups at baseline
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    rain_side = "l",
    color_palette = "clinical",
    show_statistics = TRUE,
    show_comparisons = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
  expect_true(!is.null(result$plot))
})

test_that("advancedraincloud scenario: Longitudinal analysis", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: Track patient trajectories over time
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    id_var = "patient_id",
    show_longitudinal = TRUE,
    rain_side = "f",
    show_statistics = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Likert scale survey", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: Analyze patient satisfaction (Likert 1-7)
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "satisfaction",
    x_var = "treatment",
    likert_mode = TRUE,
    show_comparisons = TRUE,
    color_palette = "pastel"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Clinical trial with effect sizes", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: Comprehensive clinical trial analysis
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    show_effect_size = TRUE,
    effect_size_type = "cohens_d",
    show_change_scores = TRUE,
    baseline_group = "Baseline",
    clinical_cutoff = 50,
    show_mcid = TRUE,
    mcid_value = 10,
    responder_threshold = 20,
    show_sample_size = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Biomarker analysis", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: CRP biomarker at end of treatment
  week12_data <- subset(advancedraincloud_test, timepoint == "Week 12")

  result <- advancedraincloud(
    data = week12_data,
    y_var = "crp_level",
    x_var = "treatment",
    log_transform = TRUE,
    clinical_cutoff = 3,
    reference_range_min = 0,
    reference_range_max = 3,
    show_cv_bands = TRUE,
    cv_band_1 = 15,
    cv_band_2 = 20,
    color_palette = "set1"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Subgroup analysis", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: Treatment effects by disease severity
  week12_data <- subset(advancedraincloud_test, timepoint == "Week 12")

  result <- advancedraincloud(
    data = week12_data,
    y_var = "pain_score",
    x_var = "treatment",
    fill_var = "disease_severity",
    cov_var = "age",
    rain_side = "f2x2",
    show_statistics = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Paired comparison", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: Before-after for single treatment arm
  high_dose_data <- subset(
    advancedraincloud_test,
    treatment == "High Dose" & (timepoint == "Baseline" | timepoint == "Week 12")
  )

  result <- advancedraincloud(
    data = high_dose_data,
    y_var = "pain_score",
    x_var = "timepoint",
    id_var = "patient_id",
    show_longitudinal = TRUE,
    rain_side = "f1x1",
    show_comparisons = TRUE,
    show_effect_size = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud scenario: Publication-ready figure", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Scenario: NEJM-style publication figure
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    journal_style = "nejm",
    p_value_position = "above",
    show_sample_size = TRUE,
    show_comparisons = TRUE,
    trial_arms = "Placebo,Low Dose,High Dose",
    time_labels = "Baseline,4 Weeks,12 Weeks",
    population_type = "itt",
    generate_report = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles multiple continuous outcomes", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  # Test with pain_score
  result1 <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment"
  )
  expect_s3_class(result1, "advancedraincloudResults")

  # Test with function_score
  result2 <- advancedraincloud(
    data = baseline_data,
    y_var = "function_score",
    x_var = "treatment"
  )
  expect_s3_class(result2, "advancedraincloudResults")

  # Test with crp_level
  result3 <- advancedraincloud(
    data = baseline_data,
    y_var = "crp_level",
    x_var = "treatment"
  )
  expect_s3_class(result3, "advancedraincloudResults")
})

test_that("advancedraincloud handles multiple grouping variables", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Group by treatment
  result1 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment"
  )
  expect_s3_class(result1, "advancedraincloudResults")

  # Group by timepoint
  result2 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint"
  )
  expect_s3_class(result2, "advancedraincloudResults")

  # Group by disease_severity
  result3 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "disease_severity"
  )
  expect_s3_class(result3, "advancedraincloudResults")

  # Group by responder
  result4 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "responder"
  )
  expect_s3_class(result4, "advancedraincloudResults")
})

test_that("advancedraincloud works with transformed data", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Create derived variables
  derived_data <- advancedraincloud_test
  derived_data$change_from_baseline <- derived_data$pain_score - 65
  derived_data$percent_change <- (derived_data$change_from_baseline / 65) * 100

  result1 <- advancedraincloud(
    data = derived_data,
    y_var = "change_from_baseline",
    x_var = "timepoint",
    fill_var = "treatment"
  )
  expect_s3_class(result1, "advancedraincloudResults")

  result2 <- advancedraincloud(
    data = derived_data,
    y_var = "percent_change",
    x_var = "treatment"
  )
  expect_s3_class(result2, "advancedraincloudResults")
})

test_that("advancedraincloud preserves data integrity", {
  devtools::load_all()

  data(advancedraincloud_test)
  original_data <- advancedraincloud_test

  # Run analysis
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment"
  )

  # Original data should be unchanged
  expect_equal(advancedraincloud_test, original_data)
})

test_that("advancedraincloud handles CSV imported data", {
  skip_if_not(file.exists("data/advancedraincloud_test.csv"),
              "CSV test file not available")

  devtools::load_all()

  # Read from CSV (simulates user import)
  csv_data <- read.csv("data/advancedraincloud_test.csv", stringsAsFactors = TRUE)

  result <- advancedraincloud(
    data = csv_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles OMV imported data", {
  skip_if_not(file.exists("data/advancedraincloud_test.omv"),
              "OMV test file not available")
  skip_if_not(requireNamespace("jmvReadWrite", quietly = TRUE),
              "jmvReadWrite package not available")

  devtools::load_all()

  # Read from OMV (simulates jamovi import)
  omv_data <- jmvReadWrite::read_omv("data/advancedraincloud_test.omv")

  result <- advancedraincloud(
    data = omv_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})
