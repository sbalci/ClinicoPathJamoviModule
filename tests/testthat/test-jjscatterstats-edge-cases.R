# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjscatterstats
# ═══════════════════════════════════════════════════════════
#
# Tests boundary conditions, error cases, and data issues
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jjscatterstats_test, package = "ClinicoPath", envir = environment())
data(jjscatterstats_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles small sample (n=30)", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_small,
    dep = "x_var",
    group = "y_var"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles very small sample per group", {
  devtools::load_all()

  # Group by 3-level variable with n=30 total (n≈10 per group)
  result <- jjscatterstats(
    data = jjscatterstats_small,
    dep = "x_var",
    group = "y_var",
    grvar = "group"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Missing Data
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles missing data in dep variable", {
  devtools::load_all()

  test_data_na <- jjscatterstats_test
  test_data_na$ki67_index[1:30] <- NA

  result <- jjscatterstats(
    data = test_data_na,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles missing data in group variable", {
  devtools::load_all()

  test_data_na <- jjscatterstats_test
  test_data_na$tumor_size[1:25] <- NA

  result <- jjscatterstats(
    data = test_data_na,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles missing data in grouping variable", {
  devtools::load_all()

  test_data_na <- jjscatterstats_test
  test_data_na$receptor_status[1:40] <- NA

  result <- jjscatterstats(
    data = test_data_na,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles missing data in both variables", {
  devtools::load_all()

  test_data_na <- jjscatterstats_test
  test_data_na$ki67_index[1:20] <- NA
  test_data_na$tumor_size[21:40] <- NA

  result <- jjscatterstats(
    data = test_data_na,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Extreme Values and Outliers
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles extreme outliers", {
  devtools::load_all()

  test_data_outliers <- jjscatterstats_test
  # Add extreme outliers
  test_data_outliers$ki67_index[1:5] <- c(150, 200, -50, 180, 160)
  test_data_outliers$tumor_size[6:10] <- c(300, 350, 400, 320, 280)

  result <- jjscatterstats(
    data = test_data_outliers,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles constant dep variable", {
  devtools::load_all()

  test_data_const <- jjscatterstats_test
  test_data_const$ki67_index <- 50  # All values the same

  # Should warn or error about zero variance
  expect_condition(
    jjscatterstats(
      data = test_data_const,
      dep = "ki67_index",
      group = "tumor_size"
    )
  )
})

test_that("jjscatterstats handles constant group variable", {
  devtools::load_all()

  test_data_const <- jjscatterstats_test
  test_data_const$tumor_size <- 50  # All values the same

  # Should warn or error about zero variance
  expect_condition(
    jjscatterstats(
      data = test_data_const,
      dep = "ki67_index",
      group = "tumor_size"
    )
  )
})

# ═══════════════════════════════════════════════════════════
# 4. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats errors on non-existent dep variable", {
  devtools::load_all()

  expect_error(
    jjscatterstats(
      data = jjscatterstats_test,
      dep = "nonexistent_var",
      group = "tumor_size"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjscatterstats errors on non-existent group variable", {
  devtools::load_all()

  expect_error(
    jjscatterstats(
      data = jjscatterstats_test,
      dep = "ki67_index",
      group = "nonexistent_var"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jjscatterstats errors on categorical dep variable", {
  devtools::load_all()

  # Trying to use categorical as continuous
  expect_error(
    jjscatterstats(
      data = jjscatterstats_test,
      dep = "tumor_stage",  # Categorical
      group = "tumor_size"
    ),
    regexp = "numeric|continuous|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("jjscatterstats errors on categorical group variable", {
  devtools::load_all()

  # Trying to use categorical as continuous
  expect_error(
    jjscatterstats(
      data = jjscatterstats_test,
      dep = "ki67_index",
      group = "tumor_grade"  # Categorical
    ),
    regexp = "numeric|continuous|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("jjscatterstats errors on empty dataset", {
  devtools::load_all()

  empty_data <- jjscatterstats_test[0, ]

  expect_error(
    jjscatterstats(
      data = empty_data,
      dep = "ki67_index",
      group = "tumor_size"
    ),
    regexp = "empty|no.*rows|insufficient",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 5. Boundary Values
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles minimum confidence level", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    conflevel = 0.01
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles maximum confidence level", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    conflevel = 0.99
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles minimum point size", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    pointsize = 0.1
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles maximum point size", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    pointsize = 10
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles minimum point alpha", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    pointalpha = 0
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles maximum point alpha", {
  devtools::load_all()

  result <- jjscatterstats(
    data = jjscatterstats_test,
    dep = "ki67_index",
    group = "tumor_size",
    pointalpha = 1
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Variable Names with Special Characters
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- jjscatterstats_test
  names(test_data_spaces)[names(test_data_spaces) == "ki67_index"] <- "Ki67 Index"
  names(test_data_spaces)[names(test_data_spaces) == "tumor_size"] <- "Tumor Size"

  result <- jjscatterstats(
    data = test_data_spaces,
    dep = "Ki67 Index",
    group = "Tumor Size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Data Types
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles tibble input", {
  devtools::load_all()

  tibble_data <- tibble::as_tibble(jjscatterstats_test)

  result <- jjscatterstats(
    data = tibble_data,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles data.frame input", {
  devtools::load_all()

  df_data <- as.data.frame(jjscatterstats_test)

  result <- jjscatterstats(
    data = df_data,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(jjscatterstats_test, jjscatterstats_test[1:50, ])

  result <- jjscatterstats(
    data = test_data_dup,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Negative and Zero Values
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles negative values", {
  devtools::load_all()

  test_data_neg <- jjscatterstats_test
  # Create centered variables with negative values
  test_data_neg$centered_ki67 <- test_data_neg$ki67_index - 50
  test_data_neg$centered_size <- test_data_neg$tumor_size - 50

  result <- jjscatterstats(
    data = test_data_neg,
    dep = "centered_ki67",
    group = "centered_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles zero values", {
  devtools::load_all()

  test_data_zero <- jjscatterstats_test
  test_data_zero$ki67_index[1:20] <- 0
  test_data_zero$tumor_size[21:40] <- 0

  result <- jjscatterstats(
    data = test_data_zero,
    dep = "ki67_index",
    group = "tumor_size"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Single Group Level
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles single group level", {
  devtools::load_all()

  test_data_single <- jjscatterstats_test
  test_data_single$receptor_status <- "Positive"  # All same group

  result <- jjscatterstats(
    data = test_data_single,
    dep = "ki67_index",
    group = "tumor_size",
    grvar = "receptor_status"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Perfect Correlation
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles perfect positive correlation", {
  devtools::load_all()

  test_data_perfect <- jjscatterstats_test
  test_data_perfect$perfect_y <- test_data_perfect$ki67_index * 2  # Perfect correlation

  result <- jjscatterstats(
    data = test_data_perfect,
    dep = "ki67_index",
    group = "perfect_y"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Non-Linear Relationships
# ═══════════════════════════════════════════════════════════

test_that("jjscatterstats handles quadratic relationship", {
  devtools::load_all()

  test_data_quad <- jjscatterstats_test
  test_data_quad$quadratic_y <- test_data_quad$ki67_index^2 + rnorm(nrow(test_data_quad), 0, 100)

  result <- jjscatterstats(
    data = test_data_quad,
    dep = "ki67_index",
    group = "quadratic_y",
    smoothMethod = "loess"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})

test_that("jjscatterstats handles logarithmic relationship", {
  devtools::load_all()

  test_data_log <- jjscatterstats_test
  test_data_log$log_y <- log(test_data_log$ki67_index + 1) * 10 + rnorm(nrow(test_data_log), 0, 2)

  result <- jjscatterstats(
    data = test_data_log,
    dep = "ki67_index",
    group = "log_y",
    smoothMethod = "gam"
  )

  expect_s3_class(result, "jjscatterstatsResults")
})
