# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjsegmentedtotalbar
# ═══════════════════════════════════════════════════════════
#
# Tests boundary conditions, error cases, and data issues
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jjsegmentedtotalbar_test, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_small, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_demographics, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles small sample (n=30)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_small,
    x_var = "category",
    y_var = "value",
    fill_var = "response"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles very small sample per category", {
  devtools::load_all()

  # Small sample with faceting creates very small groups
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_small,
    x_var = "category",
    y_var = "value",
    fill_var = "response",
    facet_var = "group"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Missing Data
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles missing data in x_var", {
  devtools::load_all()

  test_data_na <- jjsegmentedtotalbar_test
  test_data_na$timepoint[1:30] <- NA

  result <- jjsegmentedtotalbar(
    data = test_data_na,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles missing data in y_var", {
  devtools::load_all()

  test_data_na <- jjsegmentedtotalbar_test
  test_data_na$tumor_response_score[1:25] <- NA

  result <- jjsegmentedtotalbar(
    data = test_data_na,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles missing data in fill_var", {
  devtools::load_all()

  test_data_na <- jjsegmentedtotalbar_test
  test_data_na$response_category[1:40] <- NA

  result <- jjsegmentedtotalbar(
    data = test_data_na,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles missing data in facet_var", {
  devtools::load_all()

  test_data_na <- jjsegmentedtotalbar_test
  test_data_na$treatment[1:35] <- NA

  result <- jjsegmentedtotalbar(
    data = test_data_na,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles missing data in multiple variables", {
  devtools::load_all()

  test_data_na <- jjsegmentedtotalbar_test
  test_data_na$timepoint[1:20] <- NA
  test_data_na$tumor_response_score[21:40] <- NA
  test_data_na$response_category[41:60] <- NA

  result <- jjsegmentedtotalbar(
    data = test_data_na,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar errors on non-existent x_var", {
  devtools::load_all()

  expect_error(
    jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_test,
      x_var = "nonexistent_var",
      y_var = "tumor_response_score",
      fill_var = "response_category"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjsegmentedtotalbar errors on non-existent y_var", {
  devtools::load_all()

  expect_error(
    jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_test,
      x_var = "timepoint",
      y_var = "nonexistent_var",
      fill_var = "response_category"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjsegmentedtotalbar errors on non-existent fill_var", {
  devtools::load_all()

  expect_error(
    jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_test,
      x_var = "timepoint",
      y_var = "tumor_response_score",
      fill_var = "nonexistent_var"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjsegmentedtotalbar errors on numeric x_var", {
  devtools::load_all()

  # x_var should be categorical
  expect_error(
    jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_test,
      x_var = "tumor_response_score",  # Numeric instead of categorical
      y_var = "tumor_response_score",
      fill_var = "response_category"
    ),
    regexp = "categorical|factor|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("jjsegmentedtotalbar errors on numeric fill_var", {
  devtools::load_all()

  # fill_var should be categorical
  expect_error(
    jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_test,
      x_var = "timepoint",
      y_var = "tumor_response_score",
      fill_var = "tumor_response_score"  # Numeric instead of categorical
    ),
    regexp = "categorical|factor|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("jjsegmentedtotalbar errors on empty dataset", {
  devtools::load_all()

  empty_data <- jjsegmentedtotalbar_test[0, ]

  expect_error(
    jjsegmentedtotalbar(
      data = empty_data,
      x_var = "timepoint",
      y_var = "tumor_response_score",
      fill_var = "response_category"
    ),
    regexp = "empty|no.*rows|insufficient",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 4. Boundary Values
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles minimum label threshold (0%)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE,
    label_threshold = 0
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles maximum label threshold (50%)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE,
    label_threshold = 50
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles minimum flerlage label size (2)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage",
    flerlage_show_labels = TRUE,
    flerlage_label_size = 2
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles maximum flerlage label size (12)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage",
    flerlage_show_labels = TRUE,
    flerlage_label_size = 12
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles minimum flerlage alpha (0)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage",
    flerlage_alpha = 0
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles maximum flerlage alpha (1)", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage",
    flerlage_alpha = 1
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Variable Names with Special Characters
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- jjsegmentedtotalbar_test
  names(test_data_spaces)[names(test_data_spaces) == "timepoint"] <- "Time Point"
  names(test_data_spaces)[names(test_data_spaces) == "response_category"] <- "Response Category"

  result <- jjsegmentedtotalbar(
    data = test_data_spaces,
    x_var = "Time Point",
    y_var = "tumor_response_score",
    fill_var = "Response Category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Data Types
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles tibble input", {
  devtools::load_all()

  tibble_data <- tibble::as_tibble(jjsegmentedtotalbar_test)

  result <- jjsegmentedtotalbar(
    data = tibble_data,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles data.frame input", {
  devtools::load_all()

  df_data <- as.data.frame(jjsegmentedtotalbar_test)

  result <- jjsegmentedtotalbar(
    data = df_data,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(jjsegmentedtotalbar_test, jjsegmentedtotalbar_test[1:50, ])

  result <- jjsegmentedtotalbar(
    data = test_data_dup,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Negative and Zero Values
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles zero values in y_var", {
  devtools::load_all()

  test_data_zero <- jjsegmentedtotalbar_test
  test_data_zero$tumor_response_score[1:20] <- 0

  result <- jjsegmentedtotalbar(
    data = test_data_zero,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles all zero values in a category", {
  devtools::load_all()

  test_data_zero <- jjsegmentedtotalbar_test
  # Set all baseline values to zero
  baseline_indices <- which(test_data_zero$timepoint == "Baseline")
  test_data_zero$tumor_response_score[baseline_indices] <- 0

  result <- jjsegmentedtotalbar(
    data = test_data_zero,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Unbalanced Categories
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles unbalanced x_var categories", {
  devtools::load_all()

  # Create highly unbalanced data
  test_data_unbal <- jjsegmentedtotalbar_test
  # Keep only few observations for one timepoint
  indices_to_remove <- which(test_data_unbal$timepoint == "Week 24")[-(1:5)]
  test_data_unbal <- test_data_unbal[-indices_to_remove, ]

  result <- jjsegmentedtotalbar(
    data = test_data_unbal,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles unbalanced fill_var categories", {
  devtools::load_all()

  # Create highly unbalanced fill categories
  test_data_unbal <- jjsegmentedtotalbar_test
  # Keep only few observations for one response category
  indices_to_remove <- which(test_data_unbal$response_category == "Progressive Disease")[-(1:3)]
  test_data_unbal <- test_data_unbal[-indices_to_remove, ]

  result <- jjsegmentedtotalbar(
    data = test_data_unbal,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Single Category Level
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles single x_var level", {
  devtools::load_all()

  # Keep only one timepoint
  test_data_single <- jjsegmentedtotalbar_test[jjsegmentedtotalbar_test$timepoint == "Week 12", ]

  result <- jjsegmentedtotalbar(
    data = test_data_single,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles single fill_var level", {
  devtools::load_all()

  test_data_single <- jjsegmentedtotalbar_test
  test_data_single$response_category <- "Complete Response"  # All same category

  result <- jjsegmentedtotalbar(
    data = test_data_single,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Many Category Levels
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles many fill_var levels", {
  devtools::load_all()

  # Create data with many fill categories
  test_data_many <- jjsegmentedtotalbar_demographics
  # ethnicity has 5 levels which tests handling of many segments

  result <- jjsegmentedtotalbar(
    data = test_data_many,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "ethnicity"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles many fill_var levels with percentages", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "ethnicity",
    show_percentages = TRUE,
    label_threshold = 10  # Higher threshold to avoid clutter
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Extreme Proportions
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles highly skewed proportions", {
  devtools::load_all()

  # Create data where one segment dominates (95%+)
  test_data_skewed <- jjsegmentedtotalbar_test
  # Assign most observations to one category
  indices_to_change <- sample(1:nrow(test_data_skewed), round(nrow(test_data_skewed) * 0.90))
  test_data_skewed$response_category[indices_to_change] <- "Complete Response"

  result <- jjsegmentedtotalbar(
    data = test_data_skewed,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles very small segment proportions", {
  devtools::load_all()

  # Create data where some segments are <1%
  test_data_tiny <- jjsegmentedtotalbar_test
  # Assign very few observations to one category
  indices_to_change <- sample(1:nrow(test_data_tiny), round(nrow(test_data_tiny) * 0.02))
  test_data_tiny$response_category[indices_to_change] <- "Progressive Disease"
  test_data_tiny$response_category[-indices_to_change] <- sample(
    c("Complete Response", "Partial Response", "Stable Disease"),
    length(-indices_to_change), replace = TRUE
  )

  result <- jjsegmentedtotalbar(
    data = test_data_tiny,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE,
    label_threshold = 5  # Won't show tiny segments
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})
