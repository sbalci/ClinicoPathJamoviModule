# Load required libraries and data
data(histopathology, package = "ClinicoPath")

# NOTE: the two contingency_stats blocks need R/jextractggstats.h.R regenerated
# from the current jamovi/jextractggstats.a.yaml, which now permits a factor
# dependent variable (Contingency Table and Bar Chart cross-tabulate two
# categorical variables, so they could never run while dep_var was
# `permitted: [numeric]`). Until then jmvcore rejects them at the option layer
# with "Argument 'dep_var' requires a numeric variable". Run:
#   Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'

test_that("jextractggstats module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  expect_true(exists("jextractggstatsClass"))
  expect_true(is.function(jextractggstats))
})

test_that("jextractggstats handles basic input validation", {
  # `jextractggstats(data = d, dep_var = NULL)` cannot be called from R at all:
  # jmvcore::select(data, character(0)) builds a zero-column frame and dies in
  # `row.names<-` with "invalid 'row.names' length" BEFORE any module code runs.
  # Likewise `data = NULL` dies inside jmvcore with "attempt to apply
  # non-function". Both are jmvcore-wide, not defects here; the jamovi GUI
  # reaches .init()/.run() by another route and shows the instructions panel.
  # What the module owns is that route, so that is what is asserted.
  res <- jextractggstats(dep_var = NULL)
  expect_s3_class(res, "jextractggstatsResults")
  expect_match(res$instructions$content, "Statistical Data Extraction")
})

test_that("jextractggstats works with basic numeric variables", {
  # Test basic functionality with numeric variables
  result <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "histogram"
  )
  
  expect_s3_class(result, "jextractggstatsResults")
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
  
  expect_s3_class(result, "jextractggstatsResults")

  # The wrapper returns the results Group itself, so the items are reached
  # directly (result$results does not exist and raises "'results' does not
  # exist in this results element").
  expect_false(is.null(result$instructions))
  expect_false(is.null(result$extracted_data))
  expect_false(is.null(result$statistical_summary))
  expect_false(is.null(result$interpretation))
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
  
  expect_s3_class(result_parametric, "jextractggstatsResults")
  
  # Test non-parametric analysis
  result_nonparametric <- jextractggstats(
    data = histopathology,
    dep_var = "Age", 
    group_var = "Sex",
    analysis_type = "between_stats",
    statistical_test = "nonparametric"
  )
  
  expect_s3_class(result_nonparametric, "jextractggstatsResults")
  
  # Test robust analysis
  result_robust <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex", 
    analysis_type = "between_stats",
    statistical_test = "robust"
  )
  
  expect_s3_class(result_robust, "jextractggstatsResults")
})

test_that("jextractggstats handles different analysis types", {
  # Test histogram analysis
  result_histogram <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "histogram"
  )
  
  expect_s3_class(result_histogram, "jextractggstatsResults")
  
  # Test correlation analysis
  result_correlation <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "OverallTime",
    analysis_type = "correlation"
  )
  
  expect_s3_class(result_correlation, "jextractggstatsResults")
  
  # Test contingency table analysis
  result_contingency <- jextractggstats(
    data = histopathology,
    dep_var = "Sex",
    group_var = "Grade",
    analysis_type = "contingency_stats"
  )
  
  expect_s3_class(result_contingency, "jextractggstatsResults")
  
  # Test one-sample analysis
  result_one_sample <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "one_sample_stats",
    test_value = 50
  )
  
  expect_s3_class(result_one_sample, "jextractggstatsResults")
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
  
  expect_s3_class(result_all, "jextractggstatsResults")
  
  # Test subtitle data only
  result_subtitle <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    extract_components = "subtitle_data"
  )
  
  expect_s3_class(result_subtitle, "jextractggstatsResults")
  
  # Test descriptive data only
  result_descriptive <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    extract_components = "descriptive_data"
  )
  
  expect_s3_class(result_descriptive, "jextractggstatsResults")
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
  
  expect_s3_class(result_eta, "jextractggstatsResults")
  
  # Test Cohen's d
  result_d <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    effect_size_type = "cohens_d"
  )
  
  expect_s3_class(result_d, "jextractggstatsResults")
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
  
  expect_s3_class(result, "jextractggstatsResults")
  
  # Test different correction methods
  result_bonferroni <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Group",
    analysis_type = "between_stats", 
    pairwise_comparisons = TRUE,
    pairwise_correction = "bonferroni"
  )
  
  expect_s3_class(result_bonferroni, "jextractggstatsResults")
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
  
  expect_s3_class(result_95, "jextractggstatsResults")
  
  # Test with 99% confidence level
  result_99 <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    conf_level = 0.99
  )
  
  expect_s3_class(result_99, "jextractggstatsResults")
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
  
  expect_s3_class(result, "jextractggstatsResults")
  
  # Test with minimal output
  result_minimal <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    detailed_results = FALSE,
    show_interpretation = FALSE
  )
  
  expect_s3_class(result_minimal, "jextractggstatsResults")
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
  
  expect_s3_class(result, "jextractggstatsResults")

  # Test with outlier tagging
  result_outliers <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex", 
    analysis_type = "between_stats",
    centrality_plotting = TRUE,
    outlier_tagging = TRUE
  )
  
  expect_s3_class(result_outliers, "jextractggstatsResults")
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
  
  expect_s3_class(result, "jextractggstatsResults")
  
  # Test without additional data
  result_minimal <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    include_plot_data = FALSE,
    include_model_data = FALSE
  )
  
  expect_s3_class(result_minimal, "jextractggstatsResults")
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
  
  # A grouping variable with a single level cannot support a between-groups
  # comparison, so the analysis must SAY so rather than accept it. jmvcore's
  # reject() is that message: in jamovi it is shown to the user as the reason
  # the analysis produced nothing.
  single_group_data <- histopathology[histopathology$Sex == "Male", ]

  expect_error(
    jextractggstats(
      data = single_group_data,
      dep_var = "Age",
      group_var = "Sex",
      analysis_type = "between_stats"
    ),
    "at least 2 levels"
  )
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
  
  expect_s3_class(result, "jextractggstatsResults")
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
  
  expect_s3_class(result, "jextractggstatsResults")

  # Verify the structure of results
  expect_false(is.null(result$instructions))
  expect_false(is.null(result$extracted_data))
  expect_false(is.null(result$statistical_summary))
  expect_false(is.null(result$interpretation))
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
  
  expect_s3_class(result_table, "jextractggstatsResults")
  
  # Test other output formats
  result_dataframe <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    group_var = "Sex",
    analysis_type = "between_stats",
    output_format = "dataframe"
  )
  
  expect_s3_class(result_dataframe, "jextractggstatsResults")
})

test_that("jextractggstats handles new analysis types with appropriate parameters", {
  # Test one-sample stats with different test values
  result_one_sample_zero <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "one_sample_stats",
    test_value = 0
  )
  
  expect_s3_class(result_one_sample_zero, "jextractggstatsResults")
  
  # Test one-sample stats with custom test value
  result_one_sample_custom <- jextractggstats(
    data = histopathology,
    dep_var = "Age",
    analysis_type = "one_sample_stats",
    test_value = 65,
    statistical_test = "parametric"
  )
  
  expect_s3_class(result_one_sample_custom, "jextractggstatsResults")
  
  # Test contingency stats with categorical variables
  result_contingency_detailed <- jextractggstats(
    data = histopathology,
    dep_var = "Sex",
    group_var = "Outcome",
    analysis_type = "contingency_stats",
    extract_components = "all",
    detailed_results = TRUE
  )
  
  expect_s3_class(result_contingency_detailed, "jextractggstatsResults")
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

test_that("jextractggstats actually populates the extracted components", {
  # Regression guard: ggstatsplot::extract_stats() takes ONLY the plot object.
  # Passing type = "subtitle" etc. raised "unused argument", which the backend's
  # tryCatch swallowed, so the analysis rendered an empty result.
  skip_if_not_installed("ggstatsplot")
  expect_identical(names(formals(ggstatsplot::extract_stats)), "p")

  set.seed(42)
  df <- data.frame(
    y = c(stats::rnorm(25, 10, 2), stats::rnorm(25, 12, 2), stats::rnorm(25, 15, 2)),
    x = factor(rep(c("GroupA", "GroupB", "GroupC"), each = 25))
  )

  result <- jextractggstats(
    data = df,
    dep_var = "y",
    group_var = "x",
    analysis_type = "between_stats",
    statistical_test = "parametric",
    extract_components = "all",
    pairwise_comparisons = TRUE
  )

  html <- result$extracted_data$content

  # A genuine extraction failure must be reported, not silently swallowed
  expect_false(grepl("Could not extract", html, fixed = TRUE))

  # Components must actually be present
  expect_true(grepl("Main Statistical Results", html, fixed = TRUE))
  expect_true(grepl("Pairwise Comparisons", html, fixed = TRUE))
  expect_gt(length(gregexpr("<table", html, fixed = TRUE)[[1]]), 1)
})
