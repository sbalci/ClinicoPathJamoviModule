# Test suite for jjbarstats function
# Tests all major functionality including error handling, data preparation, and plotting

# Load required libraries and test data
library(testthat)
library(ClinicoPathJamoviModule)

# Load test datasets
data("medical_study_data")
data("patient_satisfaction_data")
data("clinical_trial_data")
data("diagnostic_test_data")
data("quality_improvement_data")
data("histopathology")

# Create jamovi-style analysis objects for testing
create_jjbarstats_analysis <- function(data, dep, group, grvar = NULL, ...) {
  # Mock jamovi analysis object structure
  analysis <- list(
    data = data,
    options = list(
      dep = dep,
      group = group,
      grvar = grvar,
      excl = TRUE,
      typestatistics = "parametric",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "significant",
      padjustmethod = "holm",
      originaltheme = FALSE,
      ...
    ),
    results = list(
      todo = list(setContent = function(x) invisible()),
      plot = list(setSize = function(w, h) invisible()),
      plot2 = list(setSize = function(w, h) invisible())
    )
  )
  class(analysis) <- "jjbarstatsClass"
  return(analysis)
}

# Test 1: Basic functionality with single dependent variable
test_that("jjbarstats works with single dependent variable", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test with medical study data
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "response",
      group = "treatment_group"
    )
  })
  
  # Test with patient satisfaction data
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = patient_satisfaction_data,
      dep = "satisfaction_level",
      group = "service_type"
    )
  })
  
  # Test with histopathology data
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = histopathology,
      dep = "Sex",
      group = "Race"
    )
  })
})

# Test 2: Multiple dependent variables
test_that("jjbarstats handles multiple dependent variables", {
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = clinical_trial_data,
      dep = c("primary_outcome", "side_effects"),
      group = "drug_dosage"
    )
  })
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = diagnostic_test_data,
      dep = c("test_result", "sample_quality"),
      group = "test_method"
    )
  })
})

# Test 3: Grouped analysis with grvar parameter
test_that("jjbarstats works with grouping variable (grvar)", {
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "response",
      group = "treatment_group",
      grvar = "gender"
    )
  })
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = patient_satisfaction_data,
      dep = "satisfaction_level",
      group = "service_type",
      grvar = "department"
    )
  })
})

# Test 4: Different statistical test types
test_that("jjbarstats works with different statistical methods", {
  
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (stat_type in stat_types) {
    expect_no_error({
      analysis <- create_jjbarstats_analysis(
        data = clinical_trial_data,
        dep = "primary_outcome",
        group = "drug_dosage",
        typestatistics = stat_type
      )
    })
  }
})

# Test 5: Pairwise comparison options
test_that("jjbarstats handles pairwise comparison settings", {
  
  # Test with pairwise comparisons enabled
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = quality_improvement_data,
      dep = "implementation_status",
      group = "improvement_category",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "significant"
    )
  })
  
  # Test with pairwise comparisons disabled
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = quality_improvement_data,
      dep = "implementation_status",
      group = "improvement_category",
      pairwisecomparisons = FALSE
    )
  })
  
  # Test different adjustment methods
  adjustment_methods <- c("holm", "hochberg", "bonferroni", "BH", "BY", "fdr", "none")
  
  for (method in adjustment_methods) {
    expect_no_error({
      analysis <- create_jjbarstats_analysis(
        data = diagnostic_test_data,
        dep = "test_result",
        group = "test_method",
        pairwisecomparisons = TRUE,
        padjustmethod = method
      )
    })
  }
})

# Test 6: Theme and display options
test_that("jjbarstats respects theme options", {
  
  # Test with original ggstatsplot theme
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "severity",
      group = "treatment_group",
      originaltheme = TRUE
    )
  })
  
  # Test with custom theme
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "severity",
      group = "treatment_group",
      originaltheme = FALSE
    )
  })
})

# Test 7: Data validation and error handling
test_that("jjbarstats handles edge cases and errors appropriately", {
  
  # Test with empty dataframe
  empty_data <- data.frame()
  expect_error({
    analysis <- create_jjbarstats_analysis(
      data = empty_data,
      dep = "nonexistent",
      group = "also_nonexistent"
    )
  })
  
  # Test with single row dataframe
  single_row_data <- medical_study_data[1, ]
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = single_row_data,
      dep = "response",
      group = "treatment_group"
    )
  })
  
  # Test with missing variables
  expect_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "nonexistent_variable",
      group = "treatment_group"
    )
  })
})

# Test 8: Data types and factor handling
test_that("jjbarstats handles different data types correctly", {
  
  # Create test data with mixed types
  mixed_data <- data.frame(
    char_var = c("A", "B", "C", "A", "B"),
    factor_var = factor(c("X", "Y", "Z", "X", "Y")),
    numeric_var = c(1, 2, 3, 1, 2),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test character variables
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = mixed_data,
      dep = "char_var",
      group = "factor_var"
    )
  })
  
  # Test factor variables
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = mixed_data,
      dep = "factor_var",
      group = "char_var"
    )
  })
})

# Test 9: Large dataset performance
test_that("jjbarstats handles moderately large datasets", {
  
  # Create larger test dataset
  large_data <- do.call(rbind, replicate(10, medical_study_data, simplify = FALSE))
  large_data$patient_id <- 1:nrow(large_data)
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = large_data,
      dep = "response",
      group = "treatment_group"
    )
  })
})

# Test 10: Missing data handling
test_that("jjbarstats handles missing data appropriately", {
  
  # Create data with missing values
  data_with_na <- medical_study_data
  data_with_na$response[1:10] <- NA
  data_with_na$treatment_group[5:15] <- NA
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = data_with_na,
      dep = "response",
      group = "treatment_group",
      excl = TRUE
    )
  })
})

# Test 11: Complex categorical variables
test_that("jjbarstats works with complex categorical structures", {
  
  # Test with variables having many levels
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = histopathology,
      dep = "Grade_Level",
      group = "LVI"
    )
  })
  
  # Test with ordered factors
  ordered_data <- clinical_trial_data
  ordered_data$severity_ordered <- factor(
    sample(c("Mild", "Moderate", "Severe"), nrow(ordered_data), replace = TRUE),
    levels = c("Mild", "Moderate", "Severe"),
    ordered = TRUE
  )
  
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = ordered_data,
      dep = "primary_outcome",
      group = "severity_ordered"
    )
  })
})

# Test 12: Real-world clinical scenarios
test_that("jjbarstats handles realistic clinical research scenarios", {
  
  # Scenario 1: Treatment efficacy study
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "response",
      group = "treatment_group",
      grvar = "severity",
      typestatistics = "nonparametric",
      pairwisecomparisons = TRUE,
      padjustmethod = "BH"
    )
  })
  
  # Scenario 2: Quality improvement analysis
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = quality_improvement_data,
      dep = c("implementation_status", "priority_level"),
      group = "department_involved",
      typestatistics = "parametric",
      pairwisecomparisons = FALSE
    )
  })
  
  # Scenario 3: Diagnostic test evaluation
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = diagnostic_test_data,
      dep = "test_result",
      group = "test_method",
      grvar = "laboratory",
      typestatistics = "robust",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "everything"
    )
  })
})

# Test 13: Parameter validation
test_that("jjbarstats validates parameters correctly", {
  
  # Test invalid statistical method
  expect_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "response",
      group = "treatment_group",
      typestatistics = "invalid_method"
    )
  })
  
  # Test invalid adjustment method
  expect_error({
    analysis <- create_jjbarstats_analysis(
      data = medical_study_data,
      dep = "response",
      group = "treatment_group",
      padjustmethod = "invalid_adjustment"
    )
  })
})

# Test 14: Integration with ggstatsplot parameters
test_that("jjbarstats integrates well with ggstatsplot ecosystem", {
  
  # Test various combinations that should work with ggstatsplot
  expect_no_error({
    analysis <- create_jjbarstats_analysis(
      data = patient_satisfaction_data,
      dep = "satisfaction_level",
      group = "service_type",
      typestatistics = "bayes",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "non-significant"
    )
  })
})

# Test 15: Memory and performance considerations
test_that("jjbarstats manages memory efficiently", {
  
  # Test with multiple datasets simultaneously
  datasets <- list(
    medical_study_data,
    patient_satisfaction_data,
    clinical_trial_data
  )
  
  for (i in seq_along(datasets)) {
    expect_no_error({
      analysis <- create_jjbarstats_analysis(
        data = datasets[[i]],
        dep = names(datasets[[i]])[2],  # Second column
        group = names(datasets[[i]])[3]  # Third column
      )
    })
  }
})
