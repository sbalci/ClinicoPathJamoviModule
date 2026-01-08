# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjpiestats
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test data
data(jjpiestats_test, package = "ClinicoPath", envir = environment())
data(jjpiestats_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles very small datasets", {
  devtools::load_all()

  # n=20
  result <- jjpiestats(
    data = jjpiestats_small,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles single category level", {
  devtools::load_all()

  # Create data with only one level
  single_level_data <- jjpiestats_small %>%
    filter(category == "Group A")

  result <- jjpiestats(
    data = single_level_data,
    dep = "category"
  )

  # Should handle single level (or warn/error appropriately)
  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Unbalanced Categories
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles very unbalanced category distributions", {
  devtools::load_all()

  # Create highly unbalanced data
  unbalanced_data <- jjpiestats_test %>%
    filter(
      treatment_response == "Complete Response" |
      (treatment_response == "Progressive Disease" & row_number() <= 3)
    )

  result <- jjpiestats(
    data = unbalanced_data,
    dep = "treatment_response"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Factor vs Character Variables
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles factor variables", {
  devtools::load_all()

  test_data_factor <- jjpiestats_test
  test_data_factor$treatment_response <- as.factor(test_data_factor$treatment_response)

  result <- jjpiestats(
    data = test_data_factor,
    dep = "treatment_response"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles character variables", {
  devtools::load_all()

  test_data_char <- jjpiestats_test
  test_data_char$disease_severity <- as.character(test_data_char$disease_severity)

  result <- jjpiestats(
    data = test_data_char,
    dep = "disease_severity"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Missing Data
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles missing values in dependent variable", {
  devtools::load_all()

  test_data_na <- jjpiestats_test
  test_data_na$treatment_response[1:10] <- NA

  # Should handle NA appropriately (either error or warn)
  result <- jjpiestats(
    data = test_data_na,
    dep = "treatment_response"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles missing values in grouping variable", {
  devtools::load_all()

  test_data_na_group <- jjpiestats_test
  test_data_na_group$treatment_arm[1:15] <- NA

  result <- jjpiestats(
    data = test_data_na_group,
    dep = "treatment_response",
    group = "treatment_arm"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles missing values in split variable", {
  devtools::load_all()

  test_data_na_split <- jjpiestats_test
  test_data_na_split$hospital_site[1:20] <- NA

  result <- jjpiestats(
    data = test_data_na_split,
    dep = "disease_severity",
    grvar = "hospital_site"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Special Characters and Long Names
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- jjpiestats_small
  names(test_data_spaces)[names(test_data_spaces) == "category"] <- "category variable"

  result <- jjpiestats(
    data = test_data_spaces,
    dep = "category variable"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles long category names", {
  devtools::load_all()

  test_data_long <- jjpiestats_small
  test_data_long$category <- paste0(
    "Very Long Category Name Group ",
    as.character(test_data_long$category),
    " With Extended Description"
  )
  test_data_long$category <- as.factor(test_data_long$category)

  result <- jjpiestats(
    data = test_data_long,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats errors on non-existent variables", {
  devtools::load_all()

  expect_error(
    jjpiestats(
      data = jjpiestats_test,
      dep = "nonexistent_var"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjpiestats errors on numeric dependent variable", {
  devtools::load_all()

  test_data_numeric <- jjpiestats_test
  test_data_numeric$numeric_var <- rnorm(nrow(test_data_numeric))

  expect_error(
    jjpiestats(
      data = test_data_numeric,
      dep = "numeric_var"
    ),
    regexp = "factor|categorical|not.*factor",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 7. Empty Dataset
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats errors on empty dataset", {
  devtools::load_all()

  empty_data <- jjpiestats_small[0, ]

  expect_error(
    jjpiestats(
      data = empty_data,
      dep = "category"
    ),
    regexp = "empty|no.*data|zero.*rows",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 8. Invalid Ratio Values
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles invalid ratio specifications", {
  devtools::load_all()

  # Wrong number of ratios
  expect_condition(
    jjpiestats(
      data = jjpiestats_test,
      dep = "treatment_response",  # 4 levels
      proportiontest = TRUE,
      ratio = "0.5,0.5"  # Only 2 ratios
    )
  )

  # Ratios that don't sum to 1
  expect_condition(
    jjpiestats(
      data = jjpiestats_test,
      dep = "disease_severity",  # 3 levels
      proportiontest = TRUE,
      ratio = "0.4,0.4,0.4"  # Sum = 1.2
    )
  )
})

# ═══════════════════════════════════════════════════════════
# 9. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(jjpiestats_small, jjpiestats_small)

  result <- jjpiestats(
    data = test_data_dup,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Extreme Confidence Levels
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles boundary confidence levels", {
  devtools::load_all()

  # Minimum confidence (0.50)
  result_min <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    conflevel = 0.50
  )
  expect_s3_class(result_min, "jjpiestatsResults")

  # Maximum confidence (0.99)
  result_max <- jjpiestats(
    data = jjpiestats_test,
    dep = "treatment_response",
    group = "treatment_arm",
    conflevel = 0.99
  )
  expect_s3_class(result_max, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Extreme Decimal Digits
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles boundary decimal digits", {
  devtools::load_all()

  # Minimum digits (0)
  result_min <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    digits = 0
  )
  expect_s3_class(result_min, "jjpiestatsResults")

  # Maximum digits (5)
  result_max <- jjpiestats(
    data = jjpiestats_test,
    dep = "disease_severity",
    digits = 5
  )
  expect_s3_class(result_max, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Tibble vs data.frame
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles tibble input", {
  devtools::load_all()

  test_tibble <- tibble::as_tibble(jjpiestats_small)

  result <- jjpiestats(
    data = test_tibble,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

test_that("jjpiestats handles data.frame input", {
  devtools::load_all()

  test_df <- as.data.frame(jjpiestats_small)

  result <- jjpiestats(
    data = test_df,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Many Category Levels
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles many categorical levels", {
  devtools::load_all()

  # Create data with many levels (10+)
  test_many_levels <- jjpiestats_test
  test_many_levels$many_categories <- paste0("Category_", sample(1:15, nrow(test_many_levels), replace = TRUE))
  test_many_levels$many_categories <- as.factor(test_many_levels$many_categories)

  result <- jjpiestats(
    data = test_many_levels,
    dep = "many_categories"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Same Variable for dep and group
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles same variable for dep and group", {
  devtools::load_all()

  # Using same variable for dep and group (should error or warn)
  expect_condition(
    jjpiestats(
      data = jjpiestats_test,
      dep = "treatment_response",
      group = "treatment_response"
    )
  )
})

# ═══════════════════════════════════════════════════════════
# 15. Perfectly Balanced Distribution
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles perfectly balanced categorical distributions", {
  devtools::load_all()

  # Create perfectly balanced data
  n_per_group <- 50
  balanced_data <- tibble(
    id = 1:(n_per_group * 3),
    category = rep(c("A", "B", "C"), each = n_per_group)
  ) %>%
    mutate(category = as.factor(category))

  result <- jjpiestats(
    data = balanced_data,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 16. Category with Zero Counts
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles categories with all levels defined but some with zero counts", {
  devtools::load_all()

  # Create factor with levels but some unused
  test_zero_counts <- jjpiestats_small
  test_zero_counts$category <- factor(
    test_zero_counts$category,
    levels = c("Group A", "Group B", "Group C", "Group D", "Group E")
  )

  result <- jjpiestats(
    data = test_zero_counts,
    dep = "category"
  )

  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 17. All Same Category (Constant Variable)
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles constant categorical variable", {
  devtools::load_all()

  test_constant <- jjpiestats_small
  test_constant$constant_cat <- factor("Same Value")

  result <- jjpiestats(
    data = test_constant,
    dep = "constant_cat"
  )

  # Should handle or warn appropriately
  expect_s3_class(result, "jjpiestatsResults")
})

# ═══════════════════════════════════════════════════════════
# 18. Pre-aggregated Count Data Edge Cases
# ═══════════════════════════════════════════════════════════

test_that("jjpiestats handles aggregated data with zero counts", {
  devtools::load_all()

  data_with_zeros <- tibble(
    category = c("A", "B", "C", "D"),
    counts = c(0, 10, 0, 5)
  ) %>%
    mutate(category = as.factor(category))

  result <- jjpiestats(
    data = data_with_zeros,
    dep = "category",
    counts = "counts"
  )

  expect_s3_class(result, "jjpiestatsResults")
})
