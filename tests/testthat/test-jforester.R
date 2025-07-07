# Comprehensive tests for jforester function
library(ClinicoPath)

# Load test datasets
data(jforester_meta_analysis_data, package = "ClinicoPath")
data(jforester_cardio_rr_data, package = "ClinicoPath")
data(jforester_survival_hr_data, package = "ClinicoPath")
data(jforester_mean_diff_data, package = "ClinicoPath")
data(jforester_smd_data, package = "ClinicoPath")
data(jforester_complex_meta_data, package = "ClinicoPath")

test_that("jforester module loads correctly", {
  expect_true(exists("jforesterClass"))
  expect_true(is.function(jforester))
})

test_that("jforester handles basic input validation", {
  # Test with insufficient variables
  expect_error(
    jforester(data = jforester_meta_analysis_data, study_labels = NULL),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with no data
  expect_error(
    jforester(data = NULL, study_labels = "study_name"),
    NA  # Should not error during initialization
  )
})

test_that("jforester works with meta-analysis odds ratio data", {
  # Test basic functionality with odds ratio meta-analysis
  result <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    effect_type = "or"
  )
  
  expect_s3_class(result, "jforesterClass")
  expect_true("study_name" %in% names(jforester_meta_analysis_data))
  expect_true("odds_ratio" %in% names(jforester_meta_analysis_data))
})

test_that("jforester handles different effect types", {
  # Test odds ratios
  result_or <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    effect_type = "or"
  )
  
  expect_s3_class(result_or, "jforesterClass")
  
  # Test risk ratios
  result_rr <- jforester(
    data = jforester_cardio_rr_data,
    study_labels = "trial_name",
    estimates = "risk_ratio",
    ci_lower = "rr_lower",
    ci_upper = "rr_upper",
    effect_type = "rr"
  )
  
  expect_s3_class(result_rr, "jforesterClass")
  
  # Test hazard ratios
  result_hr <- jforester(
    data = jforester_survival_hr_data,
    study_labels = "study_id",
    estimates = "hazard_ratio",
    ci_lower = "hr_lower",
    ci_upper = "hr_upper",
    effect_type = "hr"
  )
  
  expect_s3_class(result_hr, "jforesterClass")
  
  # Test mean differences
  result_md <- jforester(
    data = jforester_mean_diff_data,
    study_labels = "study_ref",
    estimates = "mean_difference",
    ci_lower = "md_lower",
    ci_upper = "md_upper",
    effect_type = "md"
  )
  
  expect_s3_class(result_md, "jforesterClass")
  
  # Test standardized mean differences
  result_smd <- jforester(
    data = jforester_smd_data,
    study_labels = "paper_citation",
    estimates = "cohens_d",
    ci_lower = "smd_lower",
    ci_upper = "smd_upper",
    effect_type = "smd"
  )
  
  expect_s3_class(result_smd, "jforesterClass")
})

test_that("jforester handles sample sizes and events", {
  # Test with sample sizes
  result_with_n <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    sample_sizes = "sample_size",
    effect_type = "or"
  )
  
  expect_s3_class(result_with_n, "jforesterClass")
  
  # Test with events
  result_with_events <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    sample_sizes = "sample_size",
    events = "events",
    effect_type = "or"
  )
  
  expect_s3_class(result_with_events, "jforesterClass")
})

test_that("jforester handles different confidence levels", {
  # Test 95% confidence level
  result_95 <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    confidence_level = "95"
  )
  
  expect_s3_class(result_95, "jforesterClass")
  
  # Test 99% confidence level
  result_99 <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    confidence_level = "99"
  )
  
  expect_s3_class(result_99, "jforesterClass")
  
  # Test 90% confidence level
  result_90 <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    confidence_level = "90"
  )
  
  expect_s3_class(result_90, "jforesterClass")
})

test_that("jforester handles reference lines and scaling", {
  # Test custom reference line
  result_ref <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    reference_line = 1.5
  )
  
  expect_s3_class(result_ref, "jforesterClass")
  
  # Test log scale
  result_log <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    log_scale = TRUE
  )
  
  expect_s3_class(result_log, "jforesterClass")
  
  # Test linear scale
  result_linear <- jforester(
    data = jforester_mean_diff_data,
    study_labels = "study_ref",
    estimates = "mean_difference",
    ci_lower = "md_lower",
    ci_upper = "md_upper",
    reference_line = 0,
    log_scale = FALSE,
    effect_type = "md"
  )
  
  expect_s3_class(result_linear, "jforesterClass")
})

test_that("jforester handles color schemes", {
  # Test default color scheme
  result_default <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    color_scheme = "default"
  )
  
  expect_s3_class(result_default, "jforesterClass")
  
  # Test medical color scheme
  result_medical <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    color_scheme = "medical"
  )
  
  expect_s3_class(result_medical, "jforesterClass")
  
  # Test custom colors
  result_custom <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    color_scheme = "custom",
    custom_point_color = "#FF5722",
    custom_ci_color = "#9E9E9E"
  )
  
  expect_s3_class(result_custom, "jforesterClass")
})

test_that("jforester handles point size ranges", {
  # Test small point sizes
  result_small <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    sample_sizes = "sample_size",
    point_size_range = "small"
  )
  
  expect_s3_class(result_small, "jforesterClass")
  
  # Test large point sizes
  result_large <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    sample_sizes = "sample_size",
    point_size_range = "large"
  )
  
  expect_s3_class(result_large, "jforesterClass")
})

test_that("jforester handles summary statistics", {
  # Test with summary effect
  result_summary <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    show_summary = TRUE,
    summary_estimate = 1.45,
    summary_ci_lower = 1.23,
    summary_ci_upper = 1.67
  )
  
  expect_s3_class(result_summary, "jforesterClass")
  
  # Test with heterogeneity statistics
  result_heterogeneity <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    show_heterogeneity = TRUE
  )
  
  expect_s3_class(result_heterogeneity, "jforesterClass")
})

test_that("jforester handles plot customization", {
  # Test custom titles and labels
  result_custom_labels <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    plot_title = "Custom Forest Plot Title",
    x_axis_label = "Custom Odds Ratio",
    effect_type = "or"
  )
  
  expect_s3_class(result_custom_labels, "jforesterClass")
  
  # Test different fonts
  result_font <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    font_family = "Times"
  )
  
  expect_s3_class(result_font, "jforesterClass")
})

test_that("jforester handles table and weight options", {
  # Test with data table
  result_table <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    show_table = TRUE
  )
  
  expect_s3_class(result_table, "jforesterClass")
  
  # Test with weights
  result_weights <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    sample_sizes = "sample_size",
    include_weights = TRUE
  )
  
  expect_s3_class(result_weights, "jforesterClass")
})

test_that("jforester handles arrow labels", {
  # Test with arrow labels
  result_arrows <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    arrow_labels = TRUE,
    left_arrow_label = "Favors Control",
    right_arrow_label = "Favors Treatment"
  )
  
  expect_s3_class(result_arrows, "jforesterClass")
})

test_that("jforester handles edge cases", {
  # Test with small dataset
  small_data <- jforester_meta_analysis_data[1:3, ]
  
  expect_error({
    result_small <- jforester(
      data = small_data,
      study_labels = "study_name",
      estimates = "odds_ratio",
      ci_lower = "or_lower_ci",
      ci_upper = "or_upper_ci"
    )
  }, NA)  # Should not error during initialization
  
  # Test with missing confidence intervals
  missing_ci_data <- jforester_meta_analysis_data
  missing_ci_data$or_lower_ci[1:2] <- NA
  
  expect_error({
    result_missing <- jforester(
      data = missing_ci_data,
      study_labels = "study_name",
      estimates = "odds_ratio",
      ci_lower = "or_lower_ci",
      ci_upper = "or_upper_ci"
    )
  }, NA)  # Should handle missing data gracefully
})

test_that("jforester validates data types", {
  # Test with valid data types
  result_valid <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",  # character
    estimates = "odds_ratio",     # numeric
    ci_lower = "or_lower_ci",     # numeric
    ci_upper = "or_upper_ci"      # numeric
  )
  
  expect_s3_class(result_valid, "jforesterClass")
})

test_that("jforester comprehensive test with complex data", {
  # Test with complex meta-analysis data
  result_complex <- jforester(
    data = jforester_complex_meta_data,
    study_labels = "study_label",
    estimates = "effect_estimate",
    ci_lower = "lower_ci",
    ci_upper = "upper_ci",
    sample_sizes = "total_participants",
    effect_type = "or",
    show_summary = TRUE,
    summary_estimate = 1.25,
    summary_ci_lower = 1.12,
    summary_ci_upper = 1.39,
    show_heterogeneity = TRUE,
    include_weights = TRUE,
    show_table = TRUE,
    color_scheme = "medical",
    plot_title = "Comprehensive Meta-Analysis",
    x_axis_label = "Effect Size (OR)",
    arrow_labels = TRUE
  )
  
  expect_s3_class(result_complex, "jforesterClass")
  
  # Verify the structure of results
  expect_true("instructions" %in% names(result_complex$results))
  expect_true("forest_plot" %in% names(result_complex$results))
  expect_true("data_table" %in% names(result_complex$results))
  expect_true("summary_statistics" %in% names(result_complex$results))
  expect_true("interpretation" %in% names(result_complex$results))
})

test_that("jforester handles different export formats", {
  # Test PNG export format
  result_png <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    export_format = "png"
  )
  
  expect_s3_class(result_png, "jforesterClass")
  
  # Test PDF export format
  result_pdf <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    export_format = "pdf"
  )
  
  expect_s3_class(result_pdf, "jforesterClass")
})

test_that("jforester handles plot dimensions", {
  # Test custom plot dimensions
  result_dimensions <- jforester(
    data = jforester_meta_analysis_data,
    study_labels = "study_name",
    estimates = "odds_ratio",
    ci_lower = "or_lower_ci",
    ci_upper = "or_upper_ci",
    plot_width = 12,
    plot_height = 10,
    dpi = 300
  )
  
  expect_s3_class(result_dimensions, "jforesterClass")
})

test_that("jforester performance with different data sizes", {
  # Test with small dataset
  small_sample <- jforester_meta_analysis_data[1:4, ]
  
  expect_error({
    result_small <- jforester(
      data = small_sample,
      study_labels = "study_name",
      estimates = "odds_ratio",
      ci_lower = "or_lower_ci",
      ci_upper = "or_upper_ci"
    )
  }, NA)
  
  # Test with large dataset
  large_sample <- jforester_complex_meta_data
  
  expect_error({
    result_large <- jforester(
      data = large_sample,
      study_labels = "study_label",
      estimates = "effect_estimate",
      ci_lower = "lower_ci",
      ci_upper = "upper_ci"
    )
  }, NA)
})