context("test-desctools")

# Load required library
library(ClinicoPath)

test_that("desctools function exists and is callable", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic functionality - instructions mode (no analysis selected)
  result <- desctools(
    data = histopathology,
    effect_size_analysis = FALSE,
    group_var = "Group",           # Required parameter, will be ignored when analysis is FALSE
    continuous_var = "Age",        # Required parameter, will be ignored when analysis is FALSE  
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = FALSE,
    fitted_probs = "Group",        # Required parameter, will be ignored when analysis is FALSE
    observed_outcomes = "Sex",     # Required parameter, will be ignored when analysis is FALSE
    hl_groups = 10,
    normality_var = "Age",         # Required parameter, will be ignored when analysis is FALSE
    categorical_tests = FALSE,
    cat_var1 = "Group",           # Required parameter, will be ignored when analysis is FALSE
    cat_var2 = "Sex",             # Required parameter, will be ignored when analysis is FALSE
    stratum_var = "Grade_Level",  # Required parameter, will be ignored when analysis is FALSE
    ordered_exposure = "Grade",   # Required parameter, will be ignored when analysis is FALSE
    binary_outcome = "Death",     # Required parameter, will be ignored when analysis is FALSE
    multiple_testing = "none",
    show_effect_sizes = TRUE,
    show_goodness_tests = TRUE,
    show_categorical_tests = TRUE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools effect size analysis works", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test effect size analysis with appropriate variables
  result <- desctools(
    data = histopathology,
    effect_size_analysis = TRUE,
    group_var = "Group",
    continuous_var = "Age",
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = FALSE,
    fitted_probs = "Group",        # Required but unused
    observed_outcomes = "Sex",     # Required but unused
    hl_groups = 10,
    normality_var = "Age",         # Required but unused
    categorical_tests = FALSE,
    cat_var1 = "Group",           # Required but unused
    cat_var2 = "Sex",             # Required but unused
    stratum_var = "Grade_Level",  # Required but unused
    ordered_exposure = "Grade",   # Required but unused
    binary_outcome = "Death",     # Required but unused
    multiple_testing = "none",
    show_effect_sizes = TRUE,
    show_goodness_tests = FALSE,
    show_categorical_tests = FALSE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools goodness of fit tests work", {
  
  # Load test data with fitted probabilities
  data("dca_test_data", package = "ClinicoPath")
  
  # Test Hosmer-Lemeshow test
  result <- desctools(
    data = dca_test_data,
    effect_size_analysis = FALSE,
    group_var = "sex",            # Required but unused
    continuous_var = "age",       # Required but unused
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = TRUE,
    fitted_probs = "basic_model",
    observed_outcomes = "cardiac_event_numeric",
    hl_groups = 10,
    normality_var = "age",
    categorical_tests = FALSE,
    cat_var1 = "sex",             # Required but unused
    cat_var2 = "diabetes",        # Required but unused
    stratum_var = "hospital",     # Required but unused
    ordered_exposure = "age",     # Required but unused
    binary_outcome = "cardiac_event_numeric", # Required but unused
    multiple_testing = "none",
    show_effect_sizes = FALSE,
    show_goodness_tests = TRUE,
    show_categorical_tests = FALSE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools categorical tests work", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test Cochran-Armitage trend test
  result <- desctools(
    data = histopathology,
    effect_size_analysis = FALSE,
    group_var = "Group",          # Required but unused
    continuous_var = "Age",       # Required but unused
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = FALSE,
    fitted_probs = "Group",       # Required but unused
    observed_outcomes = "Sex",    # Required but unused
    hl_groups = 10,
    normality_var = "Age",        # Required but unused
    categorical_tests = TRUE,
    cat_var1 = "Group",
    cat_var2 = "Sex",
    stratum_var = "Grade_Level",
    ordered_exposure = "Grade",
    binary_outcome = "Death",
    multiple_testing = "none",
    show_effect_sizes = FALSE,
    show_goodness_tests = FALSE,
    show_categorical_tests = TRUE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools with BreastCancer dataset", {
  
  # Load test data
  data("BreastCancer", package = "ClinicoPath")
  
  # Test effect size analysis with BreastCancer data
  result <- desctools(
    data = BreastCancer,
    effect_size_analysis = TRUE,
    group_var = "Class",
    continuous_var = "Cl.thickness",
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = FALSE,
    fitted_probs = "Class",       # Required but unused
    observed_outcomes = "Class",  # Required but unused
    hl_groups = 10,
    normality_var = "Cell.size",
    categorical_tests = FALSE,
    cat_var1 = "Class",          # Required but unused
    cat_var2 = "Class",          # Required but unused
    stratum_var = "Class",       # Required but unused
    ordered_exposure = "Cl.thickness", # Required but unused
    binary_outcome = "Class",    # Required but unused
    multiple_testing = "none",
    show_effect_sizes = TRUE,
    show_goodness_tests = FALSE,
    show_categorical_tests = FALSE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools handles multiple testing correction", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test with Benjamini-Hochberg correction
  result <- desctools(
    data = histopathology,
    effect_size_analysis = TRUE,
    group_var = "Group",
    continuous_var = "Age",
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = FALSE,
    fitted_probs = "Group",
    observed_outcomes = "Sex",
    hl_groups = 10,
    normality_var = "MeasurementA",
    categorical_tests = FALSE,
    cat_var1 = "Group",
    cat_var2 = "Sex",
    stratum_var = "Grade_Level",
    ordered_exposure = "Grade",
    binary_outcome = "Death",
    multiple_testing = "BH",
    show_effect_sizes = TRUE,
    show_goodness_tests = FALSE,
    show_categorical_tests = FALSE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools comprehensive analysis", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test all analysis types together
  result <- desctools(
    data = histopathology,
    effect_size_analysis = TRUE,
    group_var = "Group",
    continuous_var = "Age",
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = TRUE,
    fitted_probs = "Group",           # This won't work but function will handle gracefully
    observed_outcomes = "Sex",        # This won't work but function will handle gracefully
    hl_groups = 10,
    normality_var = "MeasurementA",
    categorical_tests = TRUE,
    cat_var1 = "Group",
    cat_var2 = "Sex",
    stratum_var = "Grade_Level",
    ordered_exposure = "Grade",
    binary_outcome = "Death",
    multiple_testing = "BH",
    show_effect_sizes = TRUE,
    show_goodness_tests = TRUE,
    show_categorical_tests = TRUE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  
})

test_that("desctools return structure is correct", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test return structure
  result <- desctools(
    data = histopathology,
    effect_size_analysis = TRUE,
    group_var = "Group",
    continuous_var = "Age",
    pooled_sd = TRUE,
    hedges_correction = FALSE,
    effect_ci_level = 0.95,
    goodness_of_fit = FALSE,
    fitted_probs = "Group",
    observed_outcomes = "Sex",
    hl_groups = 10,
    normality_var = "Age",
    categorical_tests = FALSE,
    cat_var1 = "Group",
    cat_var2 = "Sex",
    stratum_var = "Grade_Level",
    ordered_exposure = "Grade",
    binary_outcome = "Death",
    multiple_testing = "none",
    show_effect_sizes = TRUE,
    show_goodness_tests = FALSE,
    show_categorical_tests = FALSE,
    show_interpretations = TRUE
  )
  
  expect_s3_class(result, "desctoolsResults")
  expect_true("instructions" %in% names(result))
  expect_true("effect_size_results" %in% names(result))
  expect_true("goodness_fit_results" %in% names(result))
  expect_true("categorical_results" %in% names(result))
  
})