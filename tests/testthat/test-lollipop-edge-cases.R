# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: lollipop
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# Generated: 2026-01-05

library(testthat)
library(ClinicoPath)

# Load test data
data(lollipop_test, package = "ClinicoPath", envir = environment())
data(lollipop_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Missing Data Handling
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles missing values in dep variable", {
  devtools::load_all()

  # lollipop_test already has ~3% missing in hemoglobin
  result <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles all missing values in dep", {
  devtools::load_all()

  test_data_all_na <- lollipop_small
  test_data_all_na$measurement <- NA

  expect_error(
    lollipop(
      data = test_data_all_na,
      dep = "measurement",
      group = "category"
    ),
    regexp = "missing|NA|no.*data|empty",
    ignore.case = TRUE
  )
})

test_that("lollipop handles missing values in group variable", {
  devtools::load_all()

  test_data_na_group <- lollipop_test
  test_data_na_group$treatment_group[1:10] <- NA

  result <- lollipop(
    data = test_data_na_group,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )
  # Should either work (removing NAs) or warn
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles very small datasets", {
  devtools::load_all()

  # 3 observations
  tiny_data <- lollipop_small[1:3, ]

  result <- lollipop(
    data = tiny_data,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles single group", {
  devtools::load_all()

  single_group_data <- lollipop_test %>%
    filter(treatment_group == "Control")

  result <- lollipop(
    data = single_group_data,
    dep = "hemoglobin",
    group = "treatment_group"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Extreme Values
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles very large values", {
  devtools::load_all()

  test_data_large <- lollipop_small
  test_data_large$measurement <- test_data_large$measurement * 10000

  result <- lollipop(
    data = test_data_large,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles very small values", {
  devtools::load_all()

  test_data_small <- lollipop_small
  test_data_small$measurement <- test_data_small$measurement / 10000

  result <- lollipop(
    data = test_data_small,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles negative values", {
  devtools::load_all()

  test_data_neg <- lollipop_small
  test_data_neg$measurement <- -1 * test_data_neg$measurement

  result <- lollipop(
    data = test_data_neg,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles zero values", {
  devtools::load_all()

  test_data_zero <- lollipop_small
  test_data_zero$measurement[1:5] <- 0

  result <- lollipop(
    data = test_data_zero,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Constant Values
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles constant dep variable", {
  devtools::load_all()

  test_data_const <- lollipop_small
  test_data_const$measurement <- 50

  result <- lollipop(
    data = test_data_const,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Special Characters and Spaces in Variables
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- lollipop_small
  names(test_data_spaces)[names(test_data_spaces) == "measurement"] <- "my measurement"
  names(test_data_spaces)[names(test_data_spaces) == "category"] <- "group category"

  result <- lollipop(
    data = test_data_spaces,
    dep = "my measurement",
    group = "group category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles long category names", {
  devtools::load_all()

  test_data_long <- lollipop_small
  test_data_long$category <- paste0("Very Long Category Name Group ", 1:nrow(test_data_long))

  result <- lollipop(
    data = test_data_long,
    dep = "measurement",
    group = "category",
    orientation = "horizontal"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("lollipop errors on non-existent variables", {
  devtools::load_all()

  expect_error(
    lollipop(
      data = lollipop_test,
      dep = "nonexistent_var",
      group = "treatment_group"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("lollipop errors on non-numeric dep variable", {
  devtools::load_all()

  expect_error(
    lollipop(
      data = lollipop_test,
      dep = "treatment_group",  # This is categorical
      group = "disease_severity"
    ),
    regexp = "numeric|continuous|not.*number",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 7. Inf and NaN Handling
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles Inf values", {
  devtools::load_all()

  test_data_inf <- lollipop_small
  test_data_inf$measurement[1:2] <- Inf

  result <- lollipop(
    data = test_data_inf,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles -Inf values", {
  devtools::load_all()

  test_data_neginf <- lollipop_small
  test_data_neginf$measurement[1:2] <- -Inf

  result <- lollipop(
    data = test_data_neginf,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles NaN values", {
  devtools::load_all()

  test_data_nan <- lollipop_small
  test_data_nan$measurement[1:2] <- NaN

  result <- lollipop(
    data = test_data_nan,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Unequal Group Sizes
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles very unequal group sizes", {
  devtools::load_all()

  test_data_unequal <- lollipop_test %>%
    filter(treatment_group %in% c("Control", "Drug A") |
           (treatment_group == "Drug B" & row_number() <= 3))

  result <- lollipop(
    data = test_data_unequal,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Invalid Highlight Level
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles non-existent highlight level", {
  devtools::load_all()

  # Should either error or warn
  expect_condition(
    lollipop(
      data = lollipop_test,
      dep = "hemoglobin",
      group = "treatment_group",
      useHighlight = TRUE,
      highlight = "NonexistentGroup"
    )
  )
})

# ═══════════════════════════════════════════════════════════
# 10. Empty Dataset
# ═══════════════════════════════════════════════════════════

test_that("lollipop errors on empty dataset", {
  devtools::load_all()

  empty_data <- lollipop_small[0, ]

  expect_error(
    lollipop(
      data = empty_data,
      dep = "measurement",
      group = "category"
    ),
    regexp = "empty|no.*data|zero.*rows",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 11. Factor vs Character Grouping Variable
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles factor grouping variable", {
  devtools::load_all()

  test_data_factor <- lollipop_test
  test_data_factor$treatment_group <- as.factor(test_data_factor$treatment_group)

  result <- lollipop(
    data = test_data_factor,
    dep = "hemoglobin",
    group = "treatment_group"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles character grouping variable", {
  devtools::load_all()

  test_data_char <- lollipop_test
  test_data_char$treatment_group <- as.character(test_data_char$treatment_group)

  result <- lollipop(
    data = test_data_char,
    dep = "hemoglobin",
    group = "treatment_group"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(lollipop_small, lollipop_small)

  result <- lollipop(
    data = test_data_dup,
    dep = "measurement",
    group = "category",
    aggregation = "mean"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Threshold at Data Extremes
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles threshold above all values", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    conditionalColor = TRUE,
    colorThreshold = 1000
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles threshold below all values", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    conditionalColor = TRUE,
    colorThreshold = -1000
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Tibble vs data.frame
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles tibble input", {
  devtools::load_all()

  test_tibble <- tibble::as_tibble(lollipop_small)

  result <- lollipop(
    data = test_tibble,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles data.frame input", {
  devtools::load_all()

  test_df <- as.data.frame(lollipop_small)

  result <- lollipop(
    data = test_df,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result, "lollipopResults")
})
