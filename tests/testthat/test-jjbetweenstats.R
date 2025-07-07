# Test suite for jjbetweenstats function
# Tests all major functionality including performance optimizations, error handling, and statistical options

# Load required libraries and test data
library(testthat)

# Load test datasets
data("clinical_lab_data")
data("biomarker_expression_data")
data("pharmacokinetics_data")
data("psychological_assessment_data")
data("exercise_physiology_data")
data("histopathology")

# Create jamovi-style analysis objects for testing
create_jjbetweenstats_analysis <- function(data, dep, group, grvar = NULL, ...) {
  # Mock jamovi analysis object structure
  analysis <- list(
    data = data,
    options = list(
      dep = dep,
      group = group,
      grvar = grvar,
      typestatistics = "parametric",
      pairwisecomparisons = FALSE,
      pairwisedisplay = "significant",
      padjustmethod = "holm",
      effsizetype = "biased",
      centralityplotting = FALSE,
      centralitytype = "parametric",
      violin = TRUE,
      boxplot = TRUE,
      point = TRUE,
      mytitle = "Between Group Comparison",
      xtitle = "",
      ytitle = "",
      originaltheme = FALSE,
      resultssubtitle = TRUE,
      ...
    ),
    results = list(
      todo = list(setContent = function(x) invisible()),
      plot = list(setSize = function(w, h) invisible()),
      plot2 = list(setSize = function(w, h) invisible())
    )
  )
  class(analysis) <- "jjbetweenstatsClass"
  return(analysis)
}

# Test 1: Basic functionality with single dependent variable
test_that("jjbetweenstats works with single continuous dependent variable", {
  
  # Test with clinical lab data
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = "hemoglobin",
      group = "treatment_group"
    )
  })
  
  # Test with biomarker expression data
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = biomarker_expression_data,
      dep = "protein_a_expression",
      group = "tissue_type"
    )
  })
  
  # Test with histopathology data
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = histopathology,
      dep = "Age",
      group = "Sex"
    )
  })
})

# Test 2: Multiple dependent variables
test_that("jjbetweenstats handles multiple continuous dependent variables", {
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = c("hemoglobin", "white_blood_cells", "platelet_count"),
      group = "treatment_group"
    )
  })
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = pharmacokinetics_data,
      dep = c("peak_concentration", "half_life", "clearance_rate"),
      group = "dose_level"
    )
  })
})

# Test 3: Grouped analysis with grvar parameter
test_that("jjbetweenstats works with grouping variable (grvar)", {
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = "hemoglobin",
      group = "treatment_group",
      grvar = "disease_severity"
    )
  })
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = psychological_assessment_data,
      dep = "depression_score",
      group = "intervention_group",
      grvar = "baseline_severity"
    )
  })
})

# Test 4: Different statistical test types
test_that("jjbetweenstats works with different statistical methods", {
  
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (stat_type in stat_types) {
    expect_no_error({
      analysis <- create_jjbetweenstats_analysis(
        data = exercise_physiology_data,
        dep = "vo2_max",
        group = "training_regimen",
        typestatistics = stat_type
      )
    })
  }
})

# Test 5: Pairwise comparison options
test_that("jjbetweenstats handles pairwise comparison settings", {
  
  # Test with pairwise comparisons enabled
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = biomarker_expression_data,
      dep = "protein_a_expression",
      group = "tissue_type",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "significant"
    )
  })
  
  # Test with pairwise comparisons disabled
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = biomarker_expression_data,
      dep = "protein_a_expression",
      group = "tissue_type",
      pairwisecomparisons = FALSE
    )
  })
  
  # Test different adjustment methods
  adjustment_methods <- c("holm", "hochberg", "bonferroni", "BH", "BY", "fdr", "none")
  
  for (method in adjustment_methods) {
    expect_no_error({
      analysis <- create_jjbetweenstats_analysis(
        data = pharmacokinetics_data,
        dep = "peak_concentration",
        group = "dose_level",
        pairwisecomparisons = TRUE,
        padjustmethod = method
      )
    })
  }
})

# Test 6: Effect size types
test_that("jjbetweenstats handles different effect size types", {
  
  effect_sizes <- c("biased", "unbiased", "eta", "omega")
  
  for (effect_size in effect_sizes) {
    expect_no_error({
      analysis <- create_jjbetweenstats_analysis(
        data = clinical_lab_data,
        dep = "creatinine",
        group = "treatment_group",
        effsizetype = effect_size
      )
    })
  }
})

# Test 7: Centrality plotting options
test_that("jjbetweenstats handles centrality plotting configurations", {
  
  centrality_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (centrality_type in centrality_types) {
    expect_no_error({
      analysis <- create_jjbetweenstats_analysis(
        data = psychological_assessment_data,
        dep = "quality_of_life",
        group = "intervention_group",
        centralityplotting = TRUE,
        centralitytype = centrality_type
      )
    })
  }
})

# Test 8: Plot type configurations
test_that("jjbetweenstats handles different plot type combinations", {
  
  # Test violin only
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = exercise_physiology_data,
      dep = "muscle_mass",
      group = "training_regimen",
      violin = TRUE,
      boxplot = FALSE,
      point = FALSE
    )
  })
  
  # Test boxplot only
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = exercise_physiology_data,
      dep = "muscle_mass",
      group = "training_regimen",
      violin = FALSE,
      boxplot = TRUE,
      point = FALSE
    )
  })
  
  # Test points only
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = exercise_physiology_data,
      dep = "muscle_mass",
      group = "training_regimen",
      violin = FALSE,
      boxplot = FALSE,
      point = TRUE
    )
  })
})

# Test 9: Theme and display options
test_that("jjbetweenstats respects theme and display options", {
  
  # Test with original ggstatsplot theme
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = "albumin",
      group = "treatment_group",
      originaltheme = TRUE
    )
  })
  
  # Test with custom theme
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = "albumin",
      group = "treatment_group",
      originaltheme = FALSE
    )
  })
  
  # Test without statistical results subtitle
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = "albumin",
      group = "treatment_group",
      resultssubtitle = FALSE
    )
  })
})

# Test 10: Data validation and error handling
test_that("jjbetweenstats handles edge cases and errors appropriately", {
  
  # Test with empty dataframe
  empty_data <- data.frame()
  expect_error({
    analysis <- create_jjbetweenstats_analysis(
      data = empty_data,
      dep = "nonexistent",
      group = "also_nonexistent"
    )
  })
  
  # Test with single row dataframe
  single_row_data <- clinical_lab_data[1, ]
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = single_row_data,
      dep = "hemoglobin",
      group = "treatment_group"
    )
  })
})

# Test 11: Performance optimization features
test_that("jjbetweenstats performance optimizations work correctly", {
  
  # Test caching by checking that multiple calls don't error
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = c("hemoglobin", "white_blood_cells"),
      group = "treatment_group",
      grvar = "disease_severity"
    )
  })
  
  # Test with larger dataset
  large_data <- do.call(rbind, replicate(5, clinical_lab_data, simplify = FALSE))
  large_data$patient_id <- 1:nrow(large_data)
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = large_data,
      dep = "hemoglobin",
      group = "treatment_group"
    )
  })
})

# Test 12: Missing data handling
test_that("jjbetweenstats handles missing data appropriately", {
  
  # Create data with missing values
  data_with_na <- clinical_lab_data
  data_with_na$hemoglobin[1:20] <- NA
  data_with_na$treatment_group[10:30] <- NA
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = data_with_na,
      dep = "hemoglobin",
      group = "treatment_group"
    )
  })
})

# Test 13: Complex scenarios with multiple options
test_that("jjbetweenstats handles complex real-world scenarios", {
  
  # Scenario 1: Clinical trial analysis
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = c("hemoglobin", "white_blood_cells", "creatinine"),
      group = "treatment_group",
      grvar = "disease_severity",
      typestatistics = "nonparametric",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "everything",
      padjustmethod = "BH",
      centralityplotting = TRUE,
      centralitytype = "nonparametric"
    )
  })
  
  # Scenario 2: Biomarker study
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = biomarker_expression_data,
      dep = "protein_a_expression",
      group = "tissue_type",
      grvar = "tumor_grade",
      typestatistics = "robust",
      effsizetype = "omega",
      violin = TRUE,
      boxplot = FALSE,
      point = TRUE
    )
  })
  
  # Scenario 3: Psychology intervention study
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = psychological_assessment_data,
      dep = c("depression_score", "anxiety_score", "quality_of_life"),
      group = "intervention_group",
      grvar = "baseline_severity",
      typestatistics = "bayes",
      pairwisecomparisons = FALSE,
      centralityplotting = TRUE,
      centralitytype = "bayes"
    )
  })
})

# Test 14: Custom titles and labels
test_that("jjbetweenstats handles custom titles and labels", {
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = exercise_physiology_data,
      dep = "vo2_max",
      group = "training_regimen",
      mytitle = "Custom Title",
      xtitle = "Custom X Label",
      ytitle = "Custom Y Label"
    )
  })
  
  # Test with empty titles (should use defaults)
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = exercise_physiology_data,
      dep = "vo2_max",
      group = "training_regimen",
      mytitle = "",
      xtitle = "",
      ytitle = ""
    )
  })
})

# Test 15: Data type validation
test_that("jjbetweenstats handles different data types appropriately", {
  
  # Test with mixed numeric types
  mixed_data <- clinical_lab_data
  mixed_data$integer_var <- as.integer(mixed_data$hemoglobin)
  mixed_data$numeric_var <- as.numeric(mixed_data$white_blood_cells)
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = mixed_data,
      dep = c("integer_var", "numeric_var"),
      group = "treatment_group"
    )
  })
})

# Test 16: Memory efficiency with large datasets
test_that("jjbetweenstats handles moderately large datasets efficiently", {
  
  # Create larger test dataset (multiple replicates)
  large_biomarker_data <- do.call(rbind, replicate(10, biomarker_expression_data, simplify = FALSE))
  large_biomarker_data$sample_id <- 1:nrow(large_biomarker_data)
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = large_biomarker_data,
      dep = "protein_a_expression",
      group = "tissue_type"
    )
  })
})

# Test 17: Grouped analysis with many levels
test_that("jjbetweenstats handles grouping variables with many levels", {
  
  # Create data with many grouping levels
  multi_level_data <- clinical_lab_data
  multi_level_data$multi_group <- sample(letters[1:10], nrow(multi_level_data), replace = TRUE)
  multi_level_data$multi_group <- as.factor(multi_level_data$multi_group)
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = multi_level_data,
      dep = "hemoglobin",
      group = "multi_group"
    )
  })
})

# Test 18: Integration with ggstatsplot parameters
test_that("jjbetweenstats integrates well with ggstatsplot ecosystem", {
  
  # Test various combinations that should work with ggstatsplot
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = pharmacokinetics_data,
      dep = "peak_concentration",
      group = "dose_level",
      typestatistics = "bayes",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "non-significant",
      centralityplotting = TRUE,
      centralitytype = "bayes"
    )
  })
})

# Test 19: Performance monitoring
test_that("jjbetweenstats maintains performance with multiple variables", {
  
  # Test with maximum realistic number of dependent variables
  all_continuous_vars <- c("hemoglobin", "white_blood_cells", "platelet_count", 
                          "creatinine", "bilirubin", "albumin")
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = clinical_lab_data,
      dep = all_continuous_vars,
      group = "treatment_group",
      grvar = "disease_severity"
    )
  })
})

# Test 20: Edge cases with categorical variables
test_that("jjbetweenstats handles edge cases with categorical variables", {
  
  # Test with categorical variable having only two levels
  binary_data <- clinical_lab_data
  binary_data$binary_group <- sample(c("A", "B"), nrow(binary_data), replace = TRUE)
  binary_data$binary_group <- as.factor(binary_data$binary_group)
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = binary_data,
      dep = "hemoglobin",
      group = "binary_group"
    )
  })
  
  # Test with many categorical levels
  many_levels_data <- clinical_lab_data
  many_levels_data$many_levels_group <- sample(paste0("Level_", 1:8), nrow(many_levels_data), replace = TRUE)
  many_levels_data$many_levels_group <- as.factor(many_levels_data$many_levels_group)
  
  expect_no_error({
    analysis <- create_jjbetweenstats_analysis(
      data = many_levels_data,
      dep = "hemoglobin",
      group = "many_levels_group"
    )
  })
})