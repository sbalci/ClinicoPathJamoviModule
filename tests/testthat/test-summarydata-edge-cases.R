# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: summarydata
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the summarydata jamovi function
#
# Generated: 2026-01-04

# library(testthat)
# library(ClinicoPath)

# Load test data
data(summarydata_test, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# Test 1: Missing Data Handling
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles variables with missing data", {
  # PSA and CA125 have ~8-10% missing data
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "ferritin"),
    distr = TRUE
  )

  # Should complete without error
  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles variables with high missing data", {
  # Create test data with high missingness
  test_data_high_na <- summarydata_test
  test_data_high_na$test_var <- summarydata_test$age_normal
  test_data_high_na$test_var[1:100] <- NA  # 67% missing

  result <- summarydata(
    data = test_data_high_na,
    vars = "test_var"
  )

  # Should complete, statistics computed on available data
  expect_no_error(result)
})

test_that("summarydata handles all missing values", {
  # Create variable with all NA
  # `$all_na_var <- NA` creates a LOGICAL column, which jmvcore rejects as
  # non-numeric before .run() is reached - so this never tested the backend's
  # all-missing branch. summarydata_test already ships a numeric all-NA column.
  test_data_all_na <- summarydata_test
  test_data_all_na$all_na_var <- as.numeric(summarydata_test$all_missing)

  result <- summarydata(
    data = test_data_all_na,
    vars = "all_na_var"
  )

  # Should handle gracefully (likely skip the variable)
  expect_true(inherits(result, "summarydataResults"))
})

# ═══════════════════════════════════════════════════════════
# Test 2: Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles very small datasets", {
  # Very small dataset (n=5)
  small_data <- summarydata_test[1:5, ]

  result <- summarydata(
    data = small_data,
    vars = c("age_normal", "hemoglobin_lab")
  )

  # Should complete but may not run all diagnostics
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles n=3 (minimum for Shapiro-Wilk)", {
  # Exactly 3 observations - minimum for Shapiro-Wilk
  tiny_data <- summarydata_test[1:3, ]

  result <- summarydata(
    data = tiny_data,
    vars = "age_normal",
    distr = TRUE
  )

  # Should work with diagnostics
  expect_no_error(result)
})

test_that("summarydata handles n=2 (below Shapiro-Wilk minimum)", {
  # Only 2 observations - cannot run Shapiro-Wilk
  minimal_data <- summarydata_test[1:2, ]

  result <- summarydata(
    data = minimal_data,
    vars = "age_normal",
    distr = TRUE
  )

  # Should complete but skip normality test
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles outlier detection with small n", {
  # Small sample for IQR outlier detection
  small_data <- summarydata_test[1:10, ]

  result <- summarydata(
    data = small_data,
    vars = "psa_mild_skew",
    outliers = TRUE
  )

  # IQR method needs at least 4 observations
  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 3: Large Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles large datasets", {
  # Shapiro-Wilk maximum is 5000
  # Our test data has 150, let's replicate it
  large_data <- do.call(rbind, replicate(35, summarydata_test, simplify = FALSE))
  # Now ~5250 rows (> 5000)

  result <- summarydata(
    data = large_data,
    vars = c("age_normal", "hemoglobin_lab"),
    distr = TRUE
  )

  # Should complete, but Shapiro-Wilk may not run (n > 5000)
  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 4: Constant and Near-Constant Variables
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles constant variables", {
  # Variable with all same values
  test_data_const <- summarydata_test
  test_data_const$constant_var <- 10

  result <- summarydata(
    data = test_data_const,
    vars = "constant_var",
    distr = TRUE
  )

  # Should complete (SD = 0, may show warnings)
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles near-constant variables", {
  # Variable with very low variance
  test_data_near_const <- summarydata_test
  test_data_near_const$near_const_var <- 10
  test_data_near_const$near_const_var[1] <- 10.01

  result <- summarydata(
    data = test_data_near_const,
    vars = "near_const_var",
    distr = TRUE,
    outliers = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 5: Extreme Values and Outliers
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles extreme outliers", {
  # Test with variables known to have outliers
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "wbc_count", "weight_kg", "ferritin"),
    outliers = TRUE
  )

  # Should detect and report outliers
  expect_no_error(result)
})

test_that("summarydata handles infinite values", {
  # Create data with Inf
  test_data_inf <- summarydata_test
  test_data_inf$test_var <- summarydata_test$age_normal
  test_data_inf$test_var[1] <- Inf
  test_data_inf$test_var[2] <- -Inf

  # jmvcore refuses infinite values at the variable-selection layer, before any
  # analysis code runs, naming the offending column. The old block was a no-op:
  # expect_message(...) || expect_no_error(...) evaluates the || on expectation
  # objects, and the analysis errors rather than messaging in any case.
  expect_error(
    summarydata(data = test_data_inf, vars = "test_var"),
    "infinite values"
  )
})

test_that("summarydata handles very large values", {
  # Extreme but finite values
  test_data_large <- summarydata_test
  test_data_large$large_var <- summarydata_test$age_normal * 1e6

  result <- summarydata(
    data = test_data_large,
    vars = "large_var"
  )

  expect_no_error(result)
})

test_that("summarydata handles very small values", {
  # Very small but non-zero values
  test_data_small <- summarydata_test
  test_data_small$small_var <- summarydata_test$age_normal * 1e-6

  result <- summarydata(
    data = test_data_small,
    vars = "small_var",
    decimal_places = 5
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 6: Non-Numeric Variables
# ═══════════════════════════════════════════════════════════

test_that("summarydata refuses a character variable", {
  # Add character variable
  test_data_char <- summarydata_test
  test_data_char$char_var <- "constant_text"

  # `permitted: [numeric]` is enforced by jmvcore before .run(), so the analysis
  # never sees a non-numeric column. (The backend keeps its own guard as defence
  # in depth; it is unreachable through this path.) The old block ran the
  # analysis and asserted nothing at all.
  expect_error(
    summarydata(data = test_data_char, vars = c("age_normal", "char_var")),
    "numeric variable"
  )
})

test_that("summarydata refuses a factor variable", {
  # Add factor variable
  test_data_factor <- summarydata_test
  test_data_factor$factor_var <- factor(sample(c("A", "B", "C"), nrow(summarydata_test), replace = TRUE))

  # `permitted: [numeric]` is enforced by jmvcore before .run(), so the analysis
  # never sees a non-numeric column. (The backend keeps its own guard as defence
  # in depth; it is unreachable through this path.) The old block ran the
  # analysis and asserted nothing at all.
  expect_error(
    summarydata(data = test_data_factor, vars = c("age_normal", "factor_var")),
    "numeric variable"
  )
})

test_that("summarydata refuses a logical variable", {
  # Add logical variable
  test_data_logical <- summarydata_test
  test_data_logical$logical_var <- sample(c(TRUE, FALSE), nrow(summarydata_test), replace = TRUE)

  # `permitted: [numeric]` is enforced by jmvcore before .run(), so the analysis
  # never sees a non-numeric column. (The backend keeps its own guard as defence
  # in depth; it is unreachable through this path.) The old block ran the
  # analysis and asserted nothing at all.
  expect_error(
    summarydata(data = test_data_logical, vars = c("age_normal", "logical_var")),
    "numeric variable"
  )
})

# ═══════════════════════════════════════════════════════════
# Test 7: Variable Names with Special Characters
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles variable names with spaces", {
  # Create variable with space in name
  test_data_space <- summarydata_test
  names(test_data_space)[names(test_data_space) == "age_normal"] <- "patient age"

  result <- summarydata(
    data = test_data_space,
    vars = "patient age"
  )

  expect_no_error(result)
})

test_that("summarydata handles variable names with special chars", {
  # Create variables with special characters
  test_data_special <- summarydata_test
  names(test_data_special)[names(test_data_special) == "age_normal"] <- "age_(years)"
  names(test_data_special)[names(test_data_special) == "psa_mild_skew"] <- "PSA-value"

  result <- summarydata(
    data = test_data_special,
    vars = c("age_(years)", "PSA-value")
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 8: Empty or NULL Data
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles empty data frame", {
  # Empty data frame with correct column structure
  empty_data <- summarydata_test[0, ]

  # `expect_error(...) || expect_true(...)` is not a valid assertion - the || is
  # applied to expectation objects, so whichever branch ran first decided the
  # result. The analysis rejects an empty frame with a message naming the cause.
  expect_error(
    summarydata(data = empty_data, vars = "age_normal"),
    "no complete rows"
  )
})

# ═══════════════════════════════════════════════════════════
# Test 9: Distribution Characteristics
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles perfectly normal distribution", {
  # Create perfectly normal data
  test_data_normal <- data.frame(
    perfect_normal = rnorm(1000, mean = 50, sd = 10)
  )

  result <- summarydata(
    data = test_data_normal,
    vars = "perfect_normal",
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata handles highly skewed distribution", {
  # Extremely skewed data
  test_data_skewed <- data.frame(
    extreme_skew = rexp(150, rate = 0.1)
  )

  result <- summarydata(
    data = test_data_skewed,
    vars = "extreme_skew",
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata handles bimodal distribution", {
  # Bimodal distribution
  test_data_bimodal <- data.frame(
    bimodal = c(rnorm(75, mean = 30, sd = 5), rnorm(75, mean = 70, sd = 5))
  )

  result <- summarydata(
    data = test_data_bimodal,
    vars = "bimodal",
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata handles uniform distribution", {
  # Uniform distribution
  test_data_uniform <- data.frame(
    uniform = runif(150, min = 0, max = 100)
  )

  result <- summarydata(
    data = test_data_uniform,
    vars = "uniform",
    distr = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 10: Boundary Values for Options
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles decimal_places boundary minimum", {
  # Minimum decimal places (0)
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    decimal_places = 0
  )

  expect_no_error(result)
})

test_that("summarydata handles decimal_places boundary maximum", {
  # Maximum decimal places (5)
  result <- summarydata(
    data = summarydata_test,
    vars = "creatinine_lab",
    decimal_places = 5
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 11: Robustness with Missing Combinations
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles missing data + small sample", {
  # Small sample with missing data
  small_missing_data <- summarydata_test[1:10, ]
  small_missing_data$psa_mild_skew[1:5] <- NA  # column is psa_mild_skew, not psa

  result <- summarydata(
    data = small_missing_data,
    vars = "psa_mild_skew",
    distr = TRUE,
    outliers = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata handles outliers + constant variable", {
  # Constant variable with outlier detection
  test_data_const_outlier <- summarydata_test
  test_data_const_outlier$const_var <- 10
  test_data_const_outlier$const_var[1] <- 100  # Obvious outlier

  result <- summarydata(
    data = test_data_const_outlier,
    vars = "const_var",
    outliers = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 12: Stress Tests
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles many variables simultaneously", {
  # Test with a wide selection of the continuous variables that summarydata_test
  # actually ships. Seven of the names used here (bmi, systolic_bp, total_protein,
  # mitotic_count, pain_score, qol_score, fatigue_score) are not columns of that
  # dataset, so jmvcore rejected the call and the block never ran.
  all_vars <- c("age_normal", "bmi_complete", "temperature_normal",
                "systolic_bp_low_missing", "diastolic_bp",
                "hemoglobin_lab", "wbc_count", "platelet_count", "creatinine_lab",
                "albumin_no_outliers", "hba1c", "psa_mild_skew", "crp_moderate_skew",
                "ferritin", "weight_kg", "heart_rate", "tsh", "troponin", "height_cm")

  result <- summarydata(
    data = summarydata_test,
    vars = !!all_vars,
    distr = TRUE,
    outliers = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata handles repeated analysis calls", {
  # Run multiple times to check for state issues
  for (i in 1:5) {
    result <- summarydata(
      data = summarydata_test,
      vars = c("age_normal", "hemoglobin_lab"),
      distr = TRUE
    )

    expect_no_error(result)
  }
})

# ═══════════════════════════════════════════════════════════
# Test 13: Data Integrity
# ═══════════════════════════════════════════════════════════

test_that("summarydata does not modify original data", {
  # Save original data
  original_data <- summarydata_test

  # Run analysis
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "psa_mild_skew"),
    distr = TRUE,
    outliers = TRUE
  )

  # Check data unchanged
  expect_identical(summarydata_test, original_data)
})

test_that("summarydata handles data frame vs tibble", {
  # Test with base data frame
  df_data <- as.data.frame(summarydata_test)

  result1 <- summarydata(
    data = df_data,
    vars = c("age_normal", "hemoglobin_lab")
  )

  # Test with tibble (if loaded)
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble_data <- tibble::as_tibble(summarydata_test)

    result2 <- summarydata(
      data = tibble_data,
      vars = c("age_normal", "hemoglobin_lab")
    )

    expect_no_error(result2)
  }

  expect_no_error(result1)
})

# ═══════════════════════════════════════════════════════════
# End of Edge Case Tests
# ═══════════════════════════════════════════════════════════
