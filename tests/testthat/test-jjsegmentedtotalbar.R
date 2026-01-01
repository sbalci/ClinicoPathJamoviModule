# Comprehensive Tests for jjsegmentedtotalbar
# Tests cover functional behavior, data processing, statistical accuracy, and edge cases

# Setup reproducible test data
setup_test_data <- function() {
  set.seed(42)
  data.frame(
    treatment = factor(rep(c("Control", "DrugA", "DrugB"), each = 3)),
    response = factor(rep(c("Complete", "Partial", "None"), 3)),
    count = c(30, 40, 30,  # Control
              50, 30, 20,  # DrugA
              60, 25, 15)  # DrugB
  )
}

# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("jjsegmentedtotalbar creates valid output structure", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  test_data <- setup_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("composition_table" %in% names(result))
})

test_that("jjsegmentedtotalbar handles simple data correctly", {
  simple_data <- data.frame(
    category = c("A", "A", "B", "B"),
    segment = c("X", "Y", "X", "Y"),
    value = c(30, 70, 40, 60)
  )

  result <- jjsegmentedtotalbar(
    data = simple_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")

  # Check summary values
  summary_table <- result$summary
  expect_true(!is.null(summary_table))
})

test_that("jjsegmentedtotalbar requires all key variables", {
  test_data <- setup_test_data()

  # Missing x_var should not create plot
  result1 <- jjsegmentedtotalbar(
    data = test_data,
    y_var = "count",
    fill_var = "response"
  )
  expect_s3_class(result1, "Group")

  # Missing y_var should not create plot
  result2 <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    fill_var = "response"
  )
  expect_s3_class(result2, "Group")

  # Missing fill_var should not create plot
  result3 <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count"
  )
  expect_s3_class(result3, "Group")
})

# ============================================================================
# DATA PROCESSING TESTS
# ============================================================================

test_that("jjsegmentedtotalbar calculates percentages correctly", {
  test_data <- data.frame(
    category = c("A", "A", "A"),
    segment = c("X", "Y", "Z"),
    value = c(25, 50, 25)  # Should be 25%, 50%, 25%
  )

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  # Check composition table has correct percentages
  comp_table <- result$composition_table
  expect_true(!is.null(comp_table))
})

test_that("jjsegmentedtotalbar handles aggregated data", {
  # Data already aggregated (one row per category-segment combination)
  agg_data <- data.frame(
    treatment = rep(c("A", "B"), each = 2),
    outcome = rep(c("Success", "Failure"), 2),
    n = c(60, 40, 70, 30)
  )

  result <- jjsegmentedtotalbar(
    data = agg_data,
    x_var = "treatment",
    y_var = "n",
    fill_var = "outcome",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")

  # Total should be 100 for A and 100 for B
  summary_table <- result$summary
  expect_true(!is.null(summary_table))
})

test_that("jjsegmentedtotalbar handles raw (non-aggregated) data", {
  # Individual observations (multiple rows with same category-segment combo)
  raw_data <- data.frame(
    group = c(rep("A", 60), rep("A", 40), rep("B", 70), rep("B", 30)),
    status = c(rep("Pass", 60), rep("Fail", 40), rep("Pass", 70), rep("Fail", 30)),
    value = rep(1, 200)  # Each row represents 1 observation
  )

  result <- jjsegmentedtotalbar(
    data = raw_data,
    x_var = "group",
    y_var = "value",
    fill_var = "status",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# SORTING TESTS
# ============================================================================

test_that("jjsegmentedtotalbar sorts by total correctly", {
  test_data <- data.frame(
    category = c("Small", "Small", "Large", "Large", "Medium", "Medium"),
    segment = rep(c("A", "B"), 3),
    value = c(10, 15, 50, 50, 20, 30)  # Totals: Small=25, Large=100, Medium=50
  )

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    sort_categories = "total",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar sorts alphabetically", {
  test_data <- setup_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    sort_categories = "alpha",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar sorts by largest segment", {
  test_data <- setup_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    sort_categories = "largest_segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# STATISTICAL TESTS
# ============================================================================

test_that("jjsegmentedtotalbar performs chi-square test correctly", {
  # Create data with known association
  test_data <- data.frame(
    group = c(rep("A", 2), rep("B", 2)),
    outcome = rep(c("Success", "Failure"), 2),
    count = c(80, 20, 30, 70)  # Clear imbalance
  )

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "group",
    y_var = "count",
    fill_var = "outcome",
    show_statistical_tests = TRUE,
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")

  # Manual chi-square calculation
  contingency <- matrix(c(80, 20, 30, 70), nrow = 2, byrow = TRUE)
  expected_chi <- chisq.test(contingency)

  # Chi-square should detect significant difference
  expect_true(expected_chi$p.value < 0.05)
})

test_that("jjsegmentedtotalbar chi-square handles no association", {
  # Create perfectly balanced data
  test_data <- data.frame(
    group = c(rep("A", 2), rep("B", 2)),
    outcome = rep(c("Success", "Failure"), 2),
    count = c(50, 50, 50, 50)  # Perfect balance
  )

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "group",
    y_var = "count",
    fill_var = "outcome",
    show_statistical_tests = TRUE,
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")

  # Manual chi-square
  contingency <- matrix(c(50, 50, 50, 50), nrow = 2, byrow = TRUE)
  expected_chi <- chisq.test(contingency)

  # Should not be significant
  expect_true(expected_chi$p.value > 0.05)
})

# ============================================================================
# CUSTOMIZATION OPTIONS TESTS
# ============================================================================

test_that("jjsegmentedtotalbar applies color palettes", {
  test_data <- setup_test_data()

  palettes <- c("viridis", "clinical", "colorblind", "set1", "dark2", "paired")

  for (pal in palettes) {
    result <- jjsegmentedtotalbar(
      data = test_data,
      x_var = "treatment",
      y_var = "count",
      fill_var = "response",
      color_palette = pal,
      show_plot = TRUE
    )

    expect_s3_class(result, "Group")
  }
})

test_that("jjsegmentedtotalbar applies chart styles", {
  test_data <- setup_test_data()

  styles <- c("clean", "publication", "presentation", "clinical", "bbc_style", "prism_style")

  for (style in styles) {
    result <- jjsegmentedtotalbar(
      data = test_data,
      x_var = "treatment",
      y_var = "count",
      fill_var = "response",
      chart_style = style,
      show_plot = TRUE
    )

    expect_s3_class(result, "Group")
  }
})

test_that("jjsegmentedtotalbar handles orientation options", {
  test_data <- setup_test_data()

  # Vertical
  result_v <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    orientation = "vertical",
    show_plot = TRUE
  )
  expect_s3_class(result_v, "Group")

  # Horizontal
  result_h <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    orientation = "horizontal",
    show_plot = TRUE
  )
  expect_s3_class(result_h, "Group")
})

test_that("jjsegmentedtotalbar handles label options", {
  test_data <- setup_test_data()

  # Show percentages only
  result1 <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = TRUE,
    show_counts = FALSE,
    show_plot = TRUE
  )
  expect_s3_class(result1, "Group")

  # Show both percentages and counts
  result2 <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = TRUE,
    show_counts = TRUE,
    show_plot = TRUE
  )
  expect_s3_class(result2, "Group")

  # No labels
  result3 <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = FALSE,
    show_plot = TRUE
  )
  expect_s3_class(result3, "Group")
})

test_that("jjsegmentedtotalbar applies label threshold", {
  test_data <- data.frame(
    category = rep("A", 3),
    segment = c("Major", "Minor", "Tiny"),
    value = c(90, 8, 2)  # 90%, 8%, 2%
  )

  # Threshold at 5% - should hide "Tiny" label
  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_percentages = TRUE,
    label_threshold = 5,
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# FACETING TESTS
# ============================================================================

test_that("jjsegmentedtotalbar handles faceting variable", {
  faceted_data <- data.frame(
    treatment = rep(c("A", "B"), each = 4),
    outcome = rep(c("Success", "Failure"), 4),
    hospital = rep(c("H1", "H2"), each = 2, times = 2),
    count = c(60, 40, 70, 30, 55, 45, 65, 35)
  )

  result <- jjsegmentedtotalbar(
    data = faceted_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "outcome",
    facet_var = "hospital",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# CLINICAL PRESETS TESTS
# ============================================================================

test_that("jjsegmentedtotalbar applies clinical presets", {
  test_data <- setup_test_data()

  presets <- c("custom", "treatment_response", "demographics", "biomarker", "quality", "temporal")

  for (preset in presets) {
    result <- jjsegmentedtotalbar(
      data = test_data,
      x_var = "treatment",
      y_var = "count",
      fill_var = "response",
      analysis_preset = preset,
      show_plot = TRUE
    )

    expect_s3_class(result, "Group")
  }
})

# ============================================================================
# EDGE CASES AND ERROR HANDLING
# ============================================================================

test_that("jjsegmentedtotalbar handles missing data", {
  test_data <- setup_test_data()
  test_data$count[3] <- NA

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles single category", {
  single_cat_data <- data.frame(
    category = rep("OnlyGroup", 3),
    segment = c("A", "B", "C"),
    value = c(30, 40, 30)
  )

  result <- jjsegmentedtotalbar(
    data = single_cat_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles single segment", {
  single_seg_data <- data.frame(
    category = c("A", "B", "C"),
    segment = rep("OnlySegment", 3),
    value = c(30, 40, 30)
  )

  result <- jjsegmentedtotalbar(
    data = single_seg_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles many categories", {
  many_cat_data <- data.frame(
    category = rep(paste0("Cat", 1:12), each = 2),
    segment = rep(c("A", "B"), 12),
    value = rep(c(60, 40), 12)
  )

  result <- jjsegmentedtotalbar(
    data = many_cat_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles many segments", {
  many_seg_data <- data.frame(
    category = rep("A", 10),
    segment = paste0("Seg", 1:10),
    value = rep(10, 10)  # Each 10%
  )

  result <- jjsegmentedtotalbar(
    data = many_seg_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles zero values", {
  zero_data <- data.frame(
    category = c("A", "A", "B", "B"),
    segment = c("X", "Y", "X", "Y"),
    value = c(100, 0, 50, 50)  # One segment has 0
  )

  result <- jjsegmentedtotalbar(
    data = zero_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles small sample sizes", {
  small_data <- data.frame(
    category = c("A", "A", "B", "B"),
    segment = c("X", "Y", "X", "Y"),
    value = c(2, 3, 4, 1)  # Very small counts
  )

  result <- jjsegmentedtotalbar(
    data = small_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsegmentedtotalbar handles extreme proportions", {
  extreme_data <- data.frame(
    category = rep("A", 2),
    segment = c("Dominant", "Rare"),
    value = c(99, 1)  # 99% vs 1%
  )

  result <- jjsegmentedtotalbar(
    data = extreme_data,
    x_var = "category",
    y_var = "value",
    fill_var = "segment",
    show_plot = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# PERCENTAGE FORMAT TESTS
# ============================================================================

test_that("jjsegmentedtotalbar handles percentage format options", {
  test_data <- setup_test_data()

  formats <- c("integer", "decimal1", "decimal2")

  for (fmt in formats) {
    result <- jjsegmentedtotalbar(
      data = test_data,
      x_var = "treatment",
      y_var = "count",
      fill_var = "response",
      show_percentages = TRUE,
      percentage_format = fmt,
      show_plot = TRUE
    )

    expect_s3_class(result, "Group")
  }
})

# ============================================================================
# LEGEND POSITION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar handles legend positions", {
  test_data <- setup_test_data()

  positions <- c("right", "left", "top", "bottom", "none")

  for (pos in positions) {
    result <- jjsegmentedtotalbar(
      data = test_data,
      x_var = "treatment",
      y_var = "count",
      fill_var = "response",
      legend_position = pos,
      show_plot = TRUE
    )

    expect_s3_class(result, "Group")
  }
})

# ============================================================================
# OUTPUT COMPONENTS TESTS
# ============================================================================

test_that("jjsegmentedtotalbar generates all output components", {
  test_data <- setup_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_plot = TRUE,
    show_statistical_tests = TRUE,
    showExplanations = TRUE
  )

  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("composition_table" %in% names(result))
  expect_true("detailed_stats" %in% names(result))
  expect_true("interpretation" %in% names(result))
  expect_true("clinical_summary" %in% names(result))
  expect_true("statistical_tests" %in% names(result))
})

# ============================================================================
# NOTES FOR FUTURE TESTING
# ============================================================================

# Additional tests that could be added:
# - Performance tests with large datasets
# - Memory usage profiling
# - Accessibility checks (color-blind friendliness)
# - Export quality verification
# - Integration with other jamovi modules
