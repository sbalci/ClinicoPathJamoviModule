# Load required libraries and data
devtools::load_all()
data(histopathology, package = "ClinicoPath")

test_that("jextractggstats module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("jextractggstatsClass"))
  expect_true(is.function(jextractggstats))
})

test_that("jextractggstats handles basic input validation", {
  # Test with insufficient variables
  expect_error(
    jextractggstats(data = histopathology, dep_var = NULL),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with no data
  expect_error(
    jextractggstats(data = NULL, dep_var = "Age"),
    NA  # Should not error during initialization
  )
})

test_that("jextractggstats works with basic numeric variables", {
  # Test basic functionality with numeric variables
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "histogram"
  )
  
  expect_s3_class(result, "jextractggstatsClass")
  expect_true("Age" %in% names(histopathology))
})

test_that("jextractggstats handles between groups analysis", {
  # Test between groups comparison
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats"
  )
  
  expect_s3_class(result, "jextractggstatsClass")
  
  # Check that results contain expected components
  expect_true("instructions" %in% names(result$results))
  expect_true("extracted_data" %in% names(result$results))
  expect_true("statistical_summary" %in% names(result$results))
  expect_true("interpretation" %in% names(result$results))
})

test_that("jextractggstats handles different statistical tests", {
  # Test parametric analysis
  result_parametric <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    statistical_test = "parametric"
  )
  
  expect_s3_class(result_parametric, "jextractggstatsClass")
  
  # Test non-parametric analysis
  result_nonparametric <- jextractggstats(
    data = histopathology,
    dep_var = "Age", 
    group_var = "Sex",
    analysis_type = "between_stats",
    statistical_test = "nonparametric"
  )
  
  expect_s3_class(result_nonparametric, "jextractggstatsClass")
  
  # Test robust analysis
  result_robust <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex", 
    analysis_type = "between_stats",
    statistical_test = "robust"
  )
  
  expect_s3_class(result_robust, "jextractggstatsClass")
})

test_that("jextractggstats handles different analysis types", {
  # Test histogram analysis
  result_histogram <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "histogram"
  )
  
  expect_s3_class(result_histogram, "jextractggstatsClass")
  
  # Test correlation analysis
  result_correlation <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "OverallTime",
    analysis_type = "correlation"
  )
  
  expect_s3_class(result_correlation, "jextractggstatsClass")
  
  # Test contingency table analysis
  result_contingency <- jextractggstats(
    data = histopathology,
    dep_var = "Sex",
    group_var = "Grade",
    analysis_type = "contingency_stats"
  )
  
  expect_s3_class(result_contingency, "jextractggstatsClass")
  
  # Test one-sample analysis
  result_one_sample <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "one_sample_stats",
    test_value = 50
  )
  
  expect_s3_class(result_one_sample, "jextractggstatsClass")
})

test_that("jextractggstats handles different extraction components", {
  # Test all components extraction
  result_all <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    extract_components = "all"
  )
  
  expect_s3_class(result_all, "jextractggstatsClass")
  
  # Test subtitle data only
  result_subtitle <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    extract_components = "subtitle_data"
  )
  
  expect_s3_class(result_subtitle, "jextractggstatsClass")
  
  # Test descriptive data only
  result_descriptive <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    extract_components = "descriptive_data"
  )
  
  expect_s3_class(result_descriptive, "jextractggstatsClass")
})

test_that("jextractggstats handles different effect size types", {
  # Test eta-squared
  result_eta <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    effect_size_type = "eta"
  )
  
  expect_s3_class(result_eta, "jextractggstatsClass")
  
  # Test Cohen's d
  result_d <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    effect_size_type = "cohens_d"
  )
  
  expect_s3_class(result_d, "jextractggstatsClass")
})

test_that("jextractggstats handles pairwise comparisons", {
  # Test with pairwise comparisons enabled
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Group",  # Group has multiple levels
    analysis_type = "between_stats",
    pairwise_comparisons = TRUE,
    pairwise_correction = "holm"
  )
  
  expect_s3_class(result, "jextractggstatsClass")
  
  # Test different correction methods
  result_bonferroni <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Group",
    analysis_type = "between_stats", 
    pairwise_comparisons = TRUE,
    pairwise_correction = "bonferroni"
  )
  
  expect_s3_class(result_bonferroni, "jextractggstatsClass")
})

test_that("jextractggstats handles confidence levels", {
  # Test with different confidence levels
  result_95 <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    conf_level = 0.95
  )
  
  expect_s3_class(result_95, "jextractggstatsClass")
  
  # Test with 99% confidence level
  result_99 <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    conf_level = 0.99
  )
  
  expect_s3_class(result_99, "jextractggstatsClass")
})

test_that("jextractggstats handles display options", {
  # Test with detailed results enabled
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    detailed_results = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "jextractggstatsClass")
  
  # Test with minimal output
  result_minimal <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    detailed_results = FALSE,
    show_interpretation = FALSE
  )
  
  expect_s3_class(result_minimal, "jextractggstatsClass")
})

test_that("jextractggstats handles plotting options", {
  # Test with centrality plotting
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    centrality_plotting = TRUE,
    outlier_tagging = FALSE
  )
  
  expect_s3_class(result, "jextractggstats")
  
  # Test with outlier tagging
  result_outliers <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex", 
    analysis_type = "between_stats",
    centrality_plotting = TRUE,
    outlier_tagging = TRUE
  )
  
  expect_s3_class(result_outliers, "jextractggstatsClass")
})

test_that("jextractggstats handles data inclusion options", {
  # Test with plot data included
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    include_plot_data = TRUE,
    include_model_data = TRUE
  )
  
  expect_s3_class(result, "jextractggstatsClass")
  
  # Test without additional data
  result_minimal <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    include_plot_data = FALSE,
    include_model_data = FALSE
  )
  
  expect_s3_class(result_minimal, "jextractggstatsClass")
})

test_that("jextractggstats handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:10, ]
  
  expect_error({
    result <- jextractggstats(
      data = small_data,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    )
  }, NA)  # Should not error during initialization
  
  # Test with single group variable level (should handle gracefully)
  single_group_data <- histopathology[histopathology$Sex == "Male", ]
  
  expect_error({
    result <- jextractggstats(
      data = single_group_data,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    )
  }, NA)  # Should not error during initialization
})

test_that("jextractggstats handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology[1:50, ]
  test_data$Age[1:5] <- NA
  test_data$Sex[6:10] <- NA
  
  expect_error({
    result <- jextractggstats(
      data = test_data,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    )
  }, NA)  # Should handle missing data gracefully
})

test_that("jextractggstats validates input types", {
  # Test that non-numeric dependent variables are handled appropriately
  mixed_data <- histopathology
  
  # Should work with numeric dependent variables
  result <- jextractggstats(
    data = mixed_data,
    dep_var = "Age",  # This is numeric
    group_var = "Sex",  # This is categorical
    analysis_type = "between_stats"
  )
  
  expect_s3_class(result, "jextractggstatsClass")
})

test_that("jextractggstats comprehensive test with all options", {
  # Test with all major options enabled
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    extract_components = "all",
    statistical_test = "parametric",
    effect_size_type = "eta",
    pairwise_comparisons = TRUE,
    pairwise_correction = "holm",
    conf_level = 0.95,
    bf_prior = 0.707,
    centrality_plotting = TRUE,
    outlier_tagging = FALSE,
    output_format = "table",
    include_plot_data = TRUE,
    include_model_data = TRUE,
    detailed_results = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "jextractggstatsClass")
  
  # Verify the structure of results
  expect_true("instructions" %in% names(result$results))
  expect_true("extracted_data" %in% names(result$results))
  expect_true("statistical_summary" %in% names(result$results))
  expect_true("interpretation" %in% names(result$results))
})

test_that("jextractggstats handles different output formats", {
  # Test HTML table format
  result_table <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    output_format = "table"
  )
  
  expect_s3_class(result_table, "jextractggstatsClass")
  
  # Test other output formats
  result_dataframe <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    output_format = "dataframe"
  )
  
  expect_s3_class(result_dataframe, "jextractggstatsClass")
})

test_that("jextractggstats handles new analysis types with appropriate parameters", {
  # Test one-sample stats with different test values
  result_one_sample_zero <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "one_sample_stats",
    test_value = 0
  )
  
  expect_s3_class(result_one_sample_zero, "jextractggstatsClass")
  
  # Test one-sample stats with custom test value
  result_one_sample_custom <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "one_sample_stats",
    test_value = 65,
    statistical_test = "parametric"
  )
  
  expect_s3_class(result_one_sample_custom, "jextractggstatsClass")
  
  # Test contingency stats with categorical variables
  result_contingency_detailed <- jextractggstats(
    data = histopathology,
    dep_var = "Sex",
    group_var = "Outcome",
    analysis_type = "contingency_stats",
    extract_components = "all",
    detailed_results = TRUE
  )
  
  expect_s3_class(result_contingency_detailed, "jextractggstatsClass")
})

test_that("jextractggstats performance with different data sizes", {
  # Test with different sample sizes
  small_sample <- histopathology[1:20, ]
  medium_sample <- histopathology[1:100, ]
  large_sample <- histopathology
  
  # Small sample
  expect_error({
    result_small <- jextractggstats(
      data = small_sample,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    )
  }, NA)
  
  # Medium sample
  expect_error({
    result_medium <- jextractggstats(
      data = medium_sample,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    )
  }, NA)
  
  # Large sample
  expect_error({
    result_large <- jextractggstats(
      data = large_sample,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    )
  }, NA)
})
