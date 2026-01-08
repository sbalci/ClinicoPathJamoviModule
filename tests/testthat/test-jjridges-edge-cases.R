# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjridges
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test data
data(jjridges_test, package = "ClinicoPath", envir = environment())
data(jjridges_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles very small datasets", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_small,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles single group level", {
  devtools::load_all()

  single_level_data <- jjridges_small %>%
    filter(group == "Group A")

  result <- jjridges(
    data = single_level_data,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Missing Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles missing values in x variable", {
  devtools::load_all()

  test_data_na <- jjridges_test
  test_data_na$ki67_index[1:30] <- NA

  result <- jjridges(
    data = test_data_na,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles missing values in y variable", {
  devtools::load_all()

  test_data_na_y <- jjridges_test
  test_data_na_y$tumor_stage[1:25] <- NA

  result <- jjridges(
    data = test_data_na_y,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles missing values in fill variable", {
  devtools::load_all()

  test_data_na_fill <- jjridges_test
  test_data_na_fill$receptor_status[1:35] <- NA

  result <- jjridges(
    data = test_data_na_fill,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    fill_var = "receptor_status"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Extreme Values
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles outliers", {
  devtools::load_all()

  outlier_data <- jjridges_test
  outlier_data$ki67_index[1:10] <- 200  # Extreme outliers

  result <- jjridges(
    data = outlier_data,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles constant variable", {
  devtools::load_all()

  constant_data <- jjridges_small
  constant_data$constant_var <- 50

  result <- jjridges(
    data = constant_data,
    x_var = "constant_var",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("jjridges errors on non-existent x variable", {
  devtools::load_all()

  expect_error(
    jjridges(
      data = jjridges_test,
      x_var = "nonexistent_var",
      y_var = "tumor_stage"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjridges errors on categorical x variable", {
  devtools::load_all()

  expect_error(
    jjridges(
      data = jjridges_test,
      x_var = "tumor_stage",  # Categorical, not continuous
      y_var = "tumor_grade"
    ),
    regexp = "numeric|continuous|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("jjridges errors on empty dataset", {
  devtools::load_all()

  empty_data <- jjridges_small[0, ]

  expect_error(
    jjridges(
      data = empty_data,
      x_var = "measurement",
      y_var = "group"
    ),
    regexp = "empty|no.*data|zero.*rows",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 5. Boundary Scale Values
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles minimum scale", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    scale = 0.1
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles maximum scale", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    scale = 5.0
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Variable Name Edge Cases
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- jjridges_small
  names(test_data_spaces)[names(test_data_spaces) == "measurement"] <- "measurement value"

  result <- jjridges(
    data = test_data_spaces,
    x_var = "measurement value",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Data Type Handling
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles tibble input", {
  devtools::load_all()

  test_tibble <- tibble::as_tibble(jjridges_small)

  result <- jjridges(
    data = test_tibble,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles data.frame input", {
  devtools::load_all()

  test_df <- as.data.frame(jjridges_small)

  result <- jjridges(
    data = test_df,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Many Group Levels
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles many group levels", {
  devtools::load_all()

  many_levels_data <- jjridges_test
  many_levels_data$many_groups <- paste0("Group_", sample(1:10, nrow(many_levels_data), replace = TRUE))
  many_levels_data$many_groups <- as.factor(many_levels_data$many_groups)

  result <- jjridges(
    data = many_levels_data,
    x_var = "ki67_index",
    y_var = "many_groups"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(jjridges_small, jjridges_small)

  result <- jjridges(
    data = test_data_dup,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Negative and Zero Values
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles negative values", {
  devtools::load_all()

  negative_data <- jjridges_small
  negative_data$measurement <- negative_data$measurement - 100

  result <- jjridges(
    data = negative_data,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles zero values", {
  devtools::load_all()

  zero_data <- jjridges_small
  zero_data$measurement[1:10] <- 0

  result <- jjridges(
    data = zero_data,
    x_var = "measurement",
    y_var = "group"
  )

  expect_s3_class(result, "jjridgesResults")
})
