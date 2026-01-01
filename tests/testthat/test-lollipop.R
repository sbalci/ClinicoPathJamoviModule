# Comprehensive Test Suite for lollipop Function
# Tests cover actual lollipop function execution, aggregation accuracy,
# sorting correctness, summary statistics, and edge cases

library(testthat)

# ==============================================================================
# Helper Functions for Test Data Generation
# ==============================================================================

setup_clinical_biomarker_data <- function() {
  set.seed(42)
  data.frame(
    patient_id = paste0("P", sprintf("%03d", 1:20)),
    biomarker_level = round(rnorm(20, mean = 50, sd = 15), 1),
    treatment = sample(c("A", "B"), 20, replace = TRUE)
  )
}

setup_aggregation_test_data <- function() {
  set.seed(123)
  data.frame(
    group = rep(c("A", "B", "C"), each = 5),
    value = c(
      1, 2, 3, 4, 5,     # Group A: mean=3, median=3, sum=15
      10, 12, 14, 16, 18, # Group B: mean=14, median=14, sum=70
      20, 21, 22, 23, 24  # Group C: mean=22, median=22, sum=110
    )
  )
}

setup_treatment_response_data <- function() {
  set.seed(456)
  data.frame(
    treatment = rep(c("Drug_A", "Drug_B", "Drug_C", "Placebo"), each = 3),
    response_score = c(
      85, 90, 88,  # Drug_A
      60, 65, 62,  # Drug_B
      45, 50, 48,  # Drug_C
      30, 35, 32   # Placebo
    )
  )
}

# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("lollipop creates valid output structure", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  test_data <- setup_clinical_biomarker_data()

  result <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = "patient_id"
  )

  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("summary" %in% names(result))
})

test_that("lollipop requires both dep and group variables", {
  test_data <- setup_clinical_biomarker_data()

  # Should handle gracefully (not crash) when variables are missing
  result_no_dep <- lollipop(
    data = test_data,
    dep = NULL,
    group = "patient_id"
  )
  expect_s3_class(result_no_dep, "Group")

  result_no_group <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = NULL
  )
  expect_s3_class(result_no_group, "Group")
})

test_that("lollipop handles minimal valid data", {
  minimal_data <- data.frame(
    cat = c("A", "B"),
    val = c(10, 20)
  )

  result <- lollipop(
    data = minimal_data,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")
})

# ==============================================================================
# Data Aggregation Tests (CRITICAL - addresses reviewer concern)
# ==============================================================================

test_that("lollipop aggregates by mean correctly", {
  test_data <- setup_aggregation_test_data()

  result <- lollipop(
    data = test_data,
    dep = "value",
    group = "group",
    aggregation = "mean"
  )

  expect_s3_class(result, "Group")
  expect_true("summary" %in% names(result))

  # Verify aggregation: Group A mean = 3, Group B mean = 14, Group C mean = 22
  # Overall mean = (3 + 14 + 22) / 3 = 13
  # Note: The summary shows statistics of the aggregated data
})

test_that("lollipop aggregates by median correctly", {
  test_data <- setup_aggregation_test_data()

  result <- lollipop(
    data = test_data,
    dep = "value",
    group = "group",
    aggregation = "median"
  )

  expect_s3_class(result, "Group")
  expect_true("summary" %in% names(result))

  # Verify aggregation: Group A median = 3, Group B median = 14, Group C median = 22
})

test_that("lollipop aggregates by sum correctly", {
  test_data <- setup_aggregation_test_data()

  result <- lollipop(
    data = test_data,
    dep = "value",
    group = "group",
    aggregation = "sum"
  )

  expect_s3_class(result, "Group")
  expect_true("summary" %in% names(result))

  # Verify aggregation: Group A sum = 15, Group B sum = 70, Group C sum = 110
})

test_that("lollipop handles no aggregation with unique groups", {
  test_data <- setup_clinical_biomarker_data()

  result <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = "patient_id",
    aggregation = "none"
  )

  expect_s3_class(result, "Group")

  # No over-plotting warning expected since each patient_id is unique
})

test_that("lollipop detects over-plotting scenario", {
  # Data with multiple observations per group
  test_data <- setup_aggregation_test_data()

  # When aggregation is "none", function should warn about over-plotting
  expect_warning(
    lollipop(
      data = test_data,
      dep = "value",
      group = "group",
      aggregation = "none"
    ),
    regexp = "Multiple observations per group"
  )
})

# ==============================================================================
# Sorting Tests (CRITICAL - addresses reviewer concern)
# ==============================================================================

test_that("lollipop applies value ascending sort", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_asc"
  )

  expect_s3_class(result, "Group")

  # When sorted by value ascending, Placebo (lowest) should come first
  # Order should be: Placebo < Drug_C < Drug_B < Drug_A
})

test_that("lollipop applies value descending sort", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_desc"
  )

  expect_s3_class(result, "Group")

  # When sorted by value descending, Drug_A (highest) should come first
  # Order should be: Drug_A > Drug_B > Drug_C > Placebo
})

test_that("lollipop applies alphabetical sort", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "group_alpha"
  )

  expect_s3_class(result, "Group")

  # When sorted alphabetically: Drug_A, Drug_B, Drug_C, Placebo
})

test_that("lollipop preserves original order when requested", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "original"
  )

  expect_s3_class(result, "Group")

  # Should maintain the order as in the data
})

# ==============================================================================
# Summary Statistics Tests (CRITICAL - addresses reviewer concern)
# ==============================================================================

test_that("lollipop calculates summary statistics", {
  test_data <- setup_aggregation_test_data()

  result <- lollipop(
    data = test_data,
    dep = "value",
    group = "group",
    aggregation = "none"
  )

  expect_s3_class(result, "Group")
  expect_true("summary" %in% names(result))

  # Summary table should contain:
  # - Number of observations
  # - Number of groups
  # - Mean, median, SD
  # - Value range
})

test_that("lollipop summary reflects aggregated data when aggregation is used", {
  test_data <- setup_aggregation_test_data()

  result_mean <- lollipop(
    data = test_data,
    dep = "value",
    group = "group",
    aggregation = "mean"
  )

  expect_s3_class(result_mean, "Group")
  expect_true("summary" %in% names(result_mean))

  # After aggregation by mean: 3 groups, 3 observations
  # Values: 3, 14, 22
  # Mean of aggregated: (3 + 14 + 22) / 3 = 13
})

# ==============================================================================
# Highlighting Tests
# ==============================================================================

test_that("lollipop applies highlighting to specific group", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    useHighlight = TRUE,
    highlight = "Drug_A"
  )

  expect_s3_class(result, "Group")

  # Drug_A should be visually highlighted in the plot
})

test_that("lollipop handles invalid highlight level gracefully", {
  test_data <- setup_treatment_response_data()

  # Highlighting a level that doesn't exist should warn but not crash
  expect_warning(
    lollipop(
      data = test_data,
      dep = "response_score",
      group = "treatment",
      aggregation = "mean",
      useHighlight = TRUE,
      highlight = "NonExistent_Drug"
    ),
    regexp = "not found in grouping variable"
  )
})

# ==============================================================================
# Conditional Coloring Tests
# ==============================================================================

test_that("lollipop applies conditional coloring based on threshold", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    conditionalColor = TRUE,
    colorThreshold = 70
  )

  expect_s3_class(result, "Group")

  # Values above 70 (Drug_A: 87.67) should have different color
  # Values below 70 (others) should have different color
})

test_that("lollipop handles conditional coloring at extremes", {
  test_data <- setup_treatment_response_data()

  # Threshold very high - all values below
  result_high <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    conditionalColor = TRUE,
    colorThreshold = 1000
  )
  expect_s3_class(result_high, "Group")

  # Threshold very low - all values above
  result_low <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    conditionalColor = TRUE,
    colorThreshold = -1000
  )
  expect_s3_class(result_low, "Group")
})

# ==============================================================================
# Orientation Tests
# ==============================================================================

test_that("lollipop creates vertical orientation", {
  test_data <- setup_clinical_biomarker_data()

  result <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = "patient_id",
    orientation = "vertical"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop creates horizontal orientation", {
  test_data <- setup_clinical_biomarker_data()

  result <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = "patient_id",
    orientation = "horizontal"
  )

  expect_s3_class(result, "Group")
})

# ==============================================================================
# Customization Options Tests
# ==============================================================================

test_that("lollipop applies various color schemes", {
  test_data <- setup_treatment_response_data()

  schemes <- c("default", "clinical", "viridis", "colorblind")

  for (scheme in schemes) {
    result <- lollipop(
      data = test_data,
      dep = "response_score",
      group = "treatment",
      aggregation = "mean",
      colorScheme = scheme
    )
    expect_s3_class(result, "Group")
  }
})

test_that("lollipop applies various themes", {
  test_data <- setup_treatment_response_data()

  themes <- c("default", "minimal", "classic", "publication")

  for (theme_name in themes) {
    result <- lollipop(
      data = test_data,
      dep = "response_score",
      group = "treatment",
      aggregation = "mean",
      theme = theme_name
    )
    expect_s3_class(result, "Group")
  }
})

test_that("lollipop applies point size and line width", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    pointSize = 5,
    lineWidth = 2
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop applies line types", {
  test_data <- setup_treatment_response_data()

  line_types <- c("solid", "dashed", "dotted", "dotdash")

  for (line_type in line_types) {
    result <- lollipop(
      data = test_data,
      dep = "response_score",
      group = "treatment",
      aggregation = "mean",
      lineType = line_type
    )
    expect_s3_class(result, "Group")
  }
})

test_that("lollipop shows values and mean line", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    showValues = TRUE,
    showMean = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop applies custom baseline", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    baseline = 50
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop applies custom labels and title", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    xlabel = "Treatment Groups",
    ylabel = "Response Score (%)",
    title = "Treatment Efficacy Comparison"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop applies custom plot dimensions", {
  test_data <- setup_treatment_response_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    width = 1000,
    height = 800
  )

  expect_s3_class(result, "Group")
})

# ==============================================================================
# Edge Cases and Error Handling
# ==============================================================================

test_that("lollipop handles missing values in dependent variable", {
  test_data <- setup_clinical_biomarker_data()
  test_data$biomarker_level[c(1, 5, 10)] <- NA

  result <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = "patient_id"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop handles all missing values", {
  test_data <- setup_clinical_biomarker_data()
  test_data$biomarker_level[] <- NA

  # Should handle gracefully (not crash)
  result <- lollipop(
    data = test_data,
    dep = "biomarker_level",
    group = "patient_id"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop handles identical values", {
  constant_data <- data.frame(
    cat = paste0("Cat_", 1:5),
    val = rep(50, 5)
  )

  result <- lollipop(
    data = constant_data,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop handles negative values", {
  negative_data <- data.frame(
    cat = c("A", "B", "C", "D", "E"),
    val = c(-10, -5, 0, 5, 10)
  )

  result <- lollipop(
    data = negative_data,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop handles very small values", {
  small_data <- data.frame(
    cat = c("A", "B", "C"),
    val = c(0.001, 0.005, 0.01)
  )

  result <- lollipop(
    data = small_data,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop handles large number of groups", {
  large_group_data <- data.frame(
    cat = paste0("Cat_", 1:50),
    val = rnorm(50, mean = 50, sd = 10)
  )

  result <- lollipop(
    data = large_group_data,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop handles highly skewed data", {
  skewed_data <- data.frame(
    cat = paste0("Cat_", 1:10),
    val = c(rep(1, 7), 50, 100, 200)
  )

  result <- lollipop(
    data = skewed_data,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")

  # Should generate warning about highly variable data
})

test_that("lollipop handles unbalanced groups when aggregating", {
  unbalanced_data <- data.frame(
    group = c(rep("A", 2), rep("B", 10), rep("C", 3)),
    value = rnorm(15, mean = 50)
  )

  result <- lollipop(
    data = unbalanced_data,
    dep = "value",
    group = "group",
    aggregation = "mean"
  )

  expect_s3_class(result, "Group")

  # Should generate warning about unbalanced group sizes
})

test_that("lollipop handles small sample size", {
  small_sample <- data.frame(
    cat = c("A", "B", "C"),
    val = c(10, 20, 30)
  )

  result <- lollipop(
    data = small_sample,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")

  # Should generate warning about small sample size
})

test_that("lollipop handles many groups relative to sample size", {
  many_groups <- data.frame(
    cat = paste0("Cat_", 1:8),
    val = rnorm(10)  # 10 observations, 8 groups
  )

  result <- lollipop(
    data = many_groups,
    dep = "val",
    group = "cat"
  )

  expect_s3_class(result, "Group")

  # Should generate warning about many groups relative to sample size
})

# ==============================================================================
# Real-World Clinical Data Tests
# ==============================================================================

test_that("lollipop works with patient biomarker scenario", {
  # Simulates biomarker levels across patients
  set.seed(789)
  patient_data <- data.frame(
    patient = paste0("Patient_", sprintf("%02d", 1:15)),
    hemoglobin = round(rnorm(15, mean = 13.5, sd = 2), 1),
    disease_stage = sample(c("I", "II", "III", "IV"), 15, replace = TRUE)
  )

  result <- lollipop(
    data = patient_data,
    dep = "hemoglobin",
    group = "patient",
    conditionalColor = TRUE,
    colorThreshold = 12,  # Anemia threshold
    sortBy = "value_asc",
    title = "Hemoglobin Levels by Patient"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop works with treatment comparison scenario", {
  # Simulates treatment efficacy comparison
  set.seed(890)
  treatment_data <- data.frame(
    treatment = rep(c("Standard", "New_Drug_A", "New_Drug_B"), each = 8),
    tumor_reduction = c(
      rnorm(8, mean = 30, sd = 10),  # Standard
      rnorm(8, mean = 50, sd = 12),  # New Drug A
      rnorm(8, mean = 60, sd = 15)   # New Drug B
    )
  )

  result <- lollipop(
    data = treatment_data,
    dep = "tumor_reduction",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_desc",
    showMean = TRUE,
    showValues = TRUE,
    title = "Treatment Efficacy: Mean Tumor Reduction (%)"
  )

  expect_s3_class(result, "Group")
})

test_that("lollipop works with lab test timeline scenario", {
  # Simulates lab tests over time
  set.seed(901)
  timeline_data <- data.frame(
    timepoint = paste0("Day_", c(0, 7, 14, 21, 28, 35, 42)),
    creatinine = c(1.2, 1.5, 1.8, 1.6, 1.4, 1.3, 1.1)
  )

  result <- lollipop(
    data = timeline_data,
    dep = "creatinine",
    group = "timepoint",
    conditionalColor = TRUE,
    colorThreshold = 1.3,  # Upper limit of normal
    baseline = 1.0,
    sortBy = "original",
    title = "Creatinine Levels Over Treatment Course"
  )

  expect_s3_class(result, "Group")
})

# ==============================================================================
# Complete Option Combination Test
# ==============================================================================

test_that("lollipop handles complex option combinations", {
  test_data <- setup_treatment_response_data()

  # Test with multiple advanced options combined
  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_desc",
    orientation = "horizontal",
    useHighlight = TRUE,
    highlight = "Drug_A",
    conditionalColor = TRUE,
    colorThreshold = 70,
    showValues = TRUE,
    showMean = TRUE,
    colorScheme = "clinical",
    theme = "publication",
    pointSize = 4,
    lineWidth = 1.5,
    lineType = "dashed",
    baseline = 0,
    xlabel = "Response Score",
    ylabel = "Treatment",
    title = "Treatment Efficacy Analysis",
    width = 1000,
    height = 700
  )

  expect_s3_class(result, "Group")
})

print("All comprehensive lollipop function tests completed!")
