# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: raincloud
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test data
data(raincloud_test, package = "ClinicoPath", envir = environment())
data(raincloud_small, package = "ClinicoPath", envir = environment())
data(raincloud_skewed, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles very small datasets", {
  devtools::load_all()

  # n=30
  result <- raincloud(
    data = raincloud_small,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles single group level", {
  devtools::load_all()

  # Create data with only one level
  single_level_data <- raincloud_small %>%
    filter(group == "Group A")

  result <- raincloud(
    data = single_level_data,
    dep_var = "measurement",
    group_var = "group"
  )

  # Should handle single level (or warn/error appropriately)
  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Unbalanced Groups
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles very unbalanced group sizes", {
  devtools::load_all()

  # Create highly unbalanced data
  unbalanced_data <- raincloud_test %>%
    filter(
      treatment_group == "Control" |
      (treatment_group == "Drug A" & patient_id <= 5)
    )

  result <- raincloud(
    data = unbalanced_data,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Missing Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles missing values in dependent variable", {
  devtools::load_all()

  test_data_na <- raincloud_test
  test_data_na$symptom_score[1:20] <- NA

  result <- raincloud(
    data = test_data_na,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles missing values in grouping variable", {
  devtools::load_all()

  test_data_na_group <- raincloud_test
  test_data_na_group$treatment_group[1:25] <- NA

  result <- raincloud(
    data = test_data_na_group,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles missing values in facet variable", {
  devtools::load_all()

  test_data_na_facet <- raincloud_test
  test_data_na_facet$disease_severity[1:30] <- NA

  result <- raincloud(
    data = test_data_na_facet,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles missing values in color variable", {
  devtools::load_all()

  test_data_na_color <- raincloud_test
  test_data_na_color$gender[1:35] <- NA

  result <- raincloud(
    data = test_data_na_color,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    color_var = "gender"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Extreme Values
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles data with many outliers", {
  devtools::load_all()

  outlier_data <- raincloud_test
  # Add 20% outliers
  outlier_idx <- sample(1:nrow(outlier_data), size = floor(nrow(outlier_data) * 0.2))
  outlier_data$symptom_score[outlier_idx] <- outlier_data$symptom_score[outlier_idx] + rnorm(length(outlier_idx), mean = 40, sd = 15)

  result <- raincloud(
    data = outlier_data,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_outliers = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles constant variable", {
  devtools::load_all()

  constant_data <- raincloud_small
  constant_data$constant_var <- 50  # All same value

  result <- raincloud(
    data = constant_data,
    dep_var = "constant_var",
    group_var = "group"
  )

  # Should handle or warn appropriately
  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles very narrow range", {
  devtools::load_all()

  narrow_data <- raincloud_small
  narrow_data$narrow_var <- rnorm(nrow(narrow_data), mean = 50, sd = 0.001)

  result <- raincloud(
    data = narrow_data,
    dep_var = "narrow_var",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles very wide range", {
  devtools::load_all()

  wide_data <- raincloud_test
  wide_data$wide_var <- rnorm(nrow(wide_data), mean = 50000, sd = 20000)

  result <- raincloud(
    data = wide_data,
    dep_var = "wide_var",
    group_var = "treatment_group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Infinite and NaN Values
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles Inf values", {
  devtools::load_all()

  inf_data <- raincloud_small
  inf_data$measurement[1:3] <- Inf
  inf_data$measurement[4:6] <- -Inf

  result <- raincloud(
    data = inf_data,
    dep_var = "measurement",
    group_var = "group"
  )

  # Should handle or error appropriately
  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles NaN values", {
  devtools::load_all()

  nan_data <- raincloud_small
  nan_data$measurement[1:4] <- NaN

  result <- raincloud(
    data = nan_data,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Variable Name Edge Cases
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- raincloud_small
  names(test_data_spaces)[names(test_data_spaces) == "measurement"] <- "measurement value"

  result <- raincloud(
    data = test_data_spaces,
    dep_var = "measurement value",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles variable names with special characters", {
  devtools::load_all()

  test_data_special <- raincloud_small
  names(test_data_special)[names(test_data_special) == "measurement"] <- "measurement-2024"

  result <- raincloud(
    data = test_data_special,
    dep_var = "measurement-2024",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("raincloud errors on non-existent dependent variable", {
  devtools::load_all()

  expect_error(
    raincloud(
      data = raincloud_test,
      dep_var = "nonexistent_var",
      group_var = "treatment_group"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("raincloud errors on non-existent group variable", {
  devtools::load_all()

  expect_error(
    raincloud(
      data = raincloud_test,
      dep_var = "symptom_score",
      group_var = "nonexistent_group"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("raincloud errors on categorical dependent variable", {
  devtools::load_all()

  expect_error(
    raincloud(
      data = raincloud_test,
      dep_var = "treatment_group",
      group_var = "disease_severity"
    ),
    regexp = "numeric|continuous|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("raincloud errors on numeric grouping variable", {
  devtools::load_all()

  expect_error(
    raincloud(
      data = raincloud_test,
      dep_var = "quality_of_life",
      group_var = "symptom_score"
    ),
    regexp = "factor|categorical|not.*factor",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 8. Empty Dataset
# ═══════════════════════════════════════════════════════════

test_that("raincloud errors on empty dataset", {
  devtools::load_all()

  empty_data <- raincloud_small[0, ]

  expect_error(
    raincloud(
      data = empty_data,
      dep_var = "measurement",
      group_var = "group"
    ),
    regexp = "empty|no.*data|zero.*rows",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 9. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(raincloud_small, raincloud_small)

  result <- raincloud(
    data = test_data_dup,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 10. All Same Group
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles all observations in one group", {
  devtools::load_all()

  single_group_data <- raincloud_small %>%
    mutate(group = factor("Same Group"))

  result <- raincloud(
    data = single_group_data,
    dep_var = "measurement",
    group_var = "group"
  )

  # Should handle or warn appropriately
  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Tibble vs data.frame
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles tibble input", {
  devtools::load_all()

  test_tibble <- tibble::as_tibble(raincloud_small)

  result <- raincloud(
    data = test_tibble,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles data.frame input", {
  devtools::load_all()

  test_df <- as.data.frame(raincloud_small)

  result <- raincloud(
    data = test_df,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Many Category Levels
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles many categorical levels", {
  devtools::load_all()

  # Create data with many levels (10+)
  test_many_levels <- raincloud_test
  test_many_levels$many_groups <- paste0("Group_", sample(1:12, nrow(test_many_levels), replace = TRUE))
  test_many_levels$many_groups <- as.factor(test_many_levels$many_groups)

  result <- raincloud(
    data = test_many_levels,
    dep_var = "symptom_score",
    group_var = "many_groups"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Same Variable for Multiple Roles
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles same variable for group and facet", {
  devtools::load_all()

  # Using same variable for group and facet (should error or warn)
  expect_condition(
    raincloud(
      data = raincloud_test,
      dep_var = "symptom_score",
      group_var = "treatment_group",
      facet_var = "treatment_group"
    )
  )
})

test_that("raincloud handles same variable for group and color", {
  devtools::load_all()

  # Using same variable for group and color
  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    color_var = "treatment_group"
  )

  # May be valid or may warn
  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Negative Values
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles negative values", {
  devtools::load_all()

  negative_data <- raincloud_small
  negative_data$measurement <- negative_data$measurement - 100  # Make negative

  result <- raincloud(
    data = negative_data,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 15. Zero Values
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles zero values", {
  devtools::load_all()

  zero_data <- raincloud_small
  zero_data$measurement[1:10] <- 0

  result <- raincloud(
    data = zero_data,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles all zero values", {
  devtools::load_all()

  all_zero_data <- raincloud_small
  all_zero_data$zero_var <- 0

  result <- raincloud(
    data = all_zero_data,
    dep_var = "zero_var",
    group_var = "group"
  )

  # Should handle or warn appropriately
  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 16. Extreme Skewness
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles extremely skewed distributions", {
  devtools::load_all()

  # Test with right-skewed data
  result_right <- raincloud(
    data = raincloud_skewed,
    dep_var = "right_skewed",
    group_var = "condition",
    normality_test = TRUE
  )
  expect_s3_class(result_right, "raincloudResults")

  # Test with left-skewed data
  result_left <- raincloud(
    data = raincloud_skewed,
    dep_var = "left_skewed",
    group_var = "condition",
    normality_test = TRUE
  )
  expect_s3_class(result_left, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 17. Bimodal Distribution
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles bimodal distributions", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_skewed,
    dep_var = "bimodal",
    group_var = "condition",
    normality_test = TRUE,
    show_violin = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 18. Very Few Points Per Group
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles very few observations per group", {
  devtools::load_all()

  # Create data with only 3 observations per group
  few_points_data <- raincloud_small %>%
    group_by(group) %>%
    slice_head(n = 3) %>%
    ungroup()

  result <- raincloud(
    data = few_points_data,
    dep_var = "measurement",
    group_var = "group",
    show_dots = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 19. Factor Levels with No Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles factor levels with no observations", {
  devtools::load_all()

  # Create factor with levels but some unused
  test_empty_levels <- raincloud_small
  test_empty_levels$group <- factor(
    test_empty_levels$group,
    levels = c("Group A", "Group B", "Group C", "Group D", "Group E")
  )

  result <- raincloud(
    data = test_empty_levels,
    dep_var = "measurement",
    group_var = "group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 20. Invalid Customization Parameters
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles invalid width/size/alpha parameters", {
  devtools::load_all()

  # Zero width (should error or warn)
  expect_condition(
    raincloud(
      data = raincloud_small,
      dep_var = "measurement",
      group_var = "group",
      violin_width = 0
    )
  )

  # Negative size (should error or warn)
  expect_condition(
    raincloud(
      data = raincloud_small,
      dep_var = "measurement",
      group_var = "group",
      dots_size = -1
    )
  )

  # Alpha > 1 (should error or warn)
  expect_condition(
    raincloud(
      data = raincloud_small,
      dep_var = "measurement",
      group_var = "group",
      violin_alpha = 1.5
    )
  )
})
