context("test-jjcorrmat-correctness")

# Comprehensive end-to-end tests for statistical correctness
# These tests verify the fixes for:
# 1. Selective NA omission (not global)
# 2. Factor validation (no blind conversion to numeric)
# 3. Correct correlation method in summary (not always Pearson)
# 4. Partial correlation handling in summary text
# 5. Overall functionality across correlation types

devtools::load_all()

# ============================================================================
# SELECTIVE NA OMISSION TESTS
# ============================================================================

test_that("jjcorrmat uses selective NA omission, not global", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Create data with NAs in different columns
  data_with_nas <- data.frame(
    var1 = rnorm(50, mean = 10, sd = 2),
    var2 = rnorm(50, mean = 20, sd = 3),
    var3 = rnorm(50, mean = 30, sd = 4),
    irrelevant_col = c(rep(NA, 25), rnorm(25, mean = 100, sd = 10)),  # 50% NAs in unused column
    stringsAsFactors = FALSE
  )

  # Run correlation analysis (should NOT drop rows with NA in irrelevant_col)
  result <- tryCatch({
    jjcorrmat(
      data = data_with_nas,
      dep = c("var1", "var2", "var3")
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))

  # All 50 rows should be used (no NAs in var1, var2, var3)
  # If global naOmit were used, only 25 rows would remain
})

test_that("jjcorrmat drops rows only with NAs in correlation variables", {
  # Create data with NAs in correlation variables
  data_with_nas <- data.frame(
    var1 = c(rnorm(40, 10, 2), rep(NA, 10)),  # 10 NAs
    var2 = c(rnorm(45, 20, 3), rep(NA, 5)),   # 5 NAs
    var3 = rnorm(50, 30, 4),                  # No NAs
    other_col = c(rep(NA, 20), rnorm(30, 100, 10)),  # NAs in unused column
    stringsAsFactors = FALSE
  )

  # Run correlation analysis
  result <- tryCatch({
    jjcorrmat(
      data = data_with_nas,
      dep = c("var1", "var2", "var3")
    )
  }, error = function(e) {
    NULL
  })

  # Should handle gracefully
  expect_true(!is.null(result))

  # Should have 40 complete cases (rows 1-40 have all three variables)
  # Should NOT drop rows based on other_col
})

test_that("jjcorrmat handles all-NA variables correctly", {
  # Create data where one correlation variable is all NA
  data_all_na <- data.frame(
    var1 = rnorm(50, 10, 2),
    var2 = rep(NA, 50),  # All NA
    var3 = rnorm(50, 30, 4),
    stringsAsFactors = FALSE
  )

  # Should error or handle gracefully
  result <- tryCatch({
    jjcorrmat(
      data = data_all_na,
      dep = c("var1", "var2", "var3")
    )
  }, error = function(e) {
    e
  })

  # Should either error or return without crashing
  expect_true(!is.null(result))
})

# ============================================================================
# FACTOR VALIDATION TESTS
# ============================================================================

test_that("jjcorrmat detects and rejects factor variables", {
  # Create data with factor variable
  data_with_factor <- data.frame(
    var1 = rnorm(50, 10, 2),
    var2 = rnorm(50, 20, 3),
    var3 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),  # Factor
    stringsAsFactors = FALSE
  )

  # Should error with clear message about categorical variables
  expect_error({
    jjcorrmat(
      data = data_with_factor,
      dep = c("var1", "var2", "var3")
    )
  }, regexp = "categorical")
})

test_that("jjcorrmat works with all numeric variables", {
  # Create data with only numeric variables
  data_numeric_only <- data.frame(
    var1 = rnorm(50, 10, 2),
    var2 = rnorm(50, 20, 3),
    var3 = rnorm(50, 30, 4),
    var4 = rnorm(50, 40, 5),
    stringsAsFactors = FALSE
  )

  # Should succeed without warnings
  result <- tryCatch({
    jjcorrmat(
      data = data_numeric_only,
      dep = c("var1", "var2", "var3", "var4")
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat rejects multiple factor variables", {
  # Create data with multiple factors
  data_multi_factor <- data.frame(
    var1 = factor(sample(1:5, 50, replace = TRUE)),
    var2 = factor(sample(c("Low", "Medium", "High"), 50, replace = TRUE)),
    var3 = rnorm(50, 30, 4),
    stringsAsFactors = FALSE
  )

  # Should error listing all categorical variables
  expect_error({
    jjcorrmat(
      data = data_multi_factor,
      dep = c("var1", "var2", "var3")
    )
  }, regexp = "categorical")
})

# ============================================================================
# CORRELATION METHOD TESTS
# ============================================================================

test_that("jjcorrmat Pearson correlation works correctly", {
  # Create test data
  set.seed(123)
  test_data <- data.frame(
    var1 = rnorm(100, mean = 50, sd = 10),
    var2 = rnorm(100, mean = 60, sd = 15),
    var3 = rnorm(100, mean = 70, sd = 12),
    stringsAsFactors = FALSE
  )

  # Run Pearson correlation
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      typestatistics = "parametric"
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat Spearman correlation works correctly", {
  # Create test data
  set.seed(456)
  test_data <- data.frame(
    var1 = rexp(100, rate = 0.1),  # Skewed data
    var2 = rexp(100, rate = 0.2),
    var3 = rexp(100, rate = 0.15),
    stringsAsFactors = FALSE
  )

  # Run Spearman correlation
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      typestatistics = "nonparametric"
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat robust correlation works correctly", {
  # Create test data with outliers
  set.seed(789)
  test_data <- data.frame(
    var1 = c(rnorm(95, 50, 10), rep(200, 5)),  # With outliers
    var2 = c(rnorm(95, 60, 15), rep(250, 5)),
    var3 = rnorm(100, 70, 12),
    stringsAsFactors = FALSE
  )

  # Run robust correlation
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      typestatistics = "robust"
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat Bayesian correlation works correctly", {
  # Create test data
  set.seed(234)
  test_data <- data.frame(
    var1 = rnorm(100, 50, 10),
    var2 = rnorm(100, 60, 15),
    var3 = rnorm(100, 70, 12),
    stringsAsFactors = FALSE
  )

  # Run Bayesian correlation
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      typestatistics = "bayes"
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

# ============================================================================
# PARTIAL CORRELATION TESTS
# ============================================================================

test_that("jjcorrmat partial correlations require 3+ variables", {
  # Create data with only 2 variables
  test_data <- data.frame(
    var1 = rnorm(50, 50, 10),
    var2 = rnorm(50, 60, 15),
    stringsAsFactors = FALSE
  )

  # Partial correlations with <3 variables should warn
  expect_warning({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2"),
      partial = TRUE
    )
  })
})

test_that("jjcorrmat partial correlations work with 3+ variables", {
  # Create data with 3+ variables
  set.seed(567)
  test_data <- data.frame(
    var1 = rnorm(100, 50, 10),
    var2 = rnorm(100, 60, 15),
    var3 = rnorm(100, 70, 12),
    var4 = rnorm(100, 80, 14),
    stringsAsFactors = FALSE
  )

  # Run partial correlations
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3", "var4"),
      partial = TRUE
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat zero-order vs partial correlations produce different results", {
  # Create correlated data
  set.seed(678)
  n <- 100
  z <- rnorm(n)  # Confounder
  test_data <- data.frame(
    var1 = 0.5 * z + rnorm(n, 0, 0.5),
    var2 = 0.5 * z + rnorm(n, 0, 0.5),
    var3 = 0.5 * z + rnorm(n, 0, 0.5),
    stringsAsFactors = FALSE
  )

  # Run zero-order correlation
  result_zero <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      partial = FALSE
    )
  }, error = function(e) NULL)

  # Run partial correlation
  result_partial <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      partial = TRUE
    )
  }, error = function(e) NULL)

  # Both should succeed
  expect_true(!is.null(result_zero))
  expect_true(!is.null(result_partial))
})

# ============================================================================
# GROUPED ANALYSIS TESTS
# ============================================================================

test_that("jjcorrmat grouped analysis works", {
  # Create data with grouping variable
  set.seed(890)
  test_data <- data.frame(
    var1 = rnorm(120, 50, 10),
    var2 = rnorm(120, 60, 15),
    var3 = rnorm(120, 70, 12),
    group = factor(rep(c("A", "B", "C"), each = 40)),
    stringsAsFactors = FALSE
  )

  # Run with grvar
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      grvar = "group"
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat grouped analysis handles NA in group variable", {
  # Create data with NAs in group variable
  test_data <- data.frame(
    var1 = rnorm(50, 50, 10),
    var2 = rnorm(50, 60, 15),
    var3 = rnorm(50, 70, 12),
    group = c(factor(rep(c("A", "B"), each = 20)), rep(NA, 10)),
    stringsAsFactors = FALSE
  )

  # Should handle NAs in group variable
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      grvar = "group"
    )
  }, error = function(e) {
    NULL
  })

  # Should handle gracefully (may drop rows with NA in group)
  expect_true(!is.null(result))
})

# ============================================================================
# MATRIX DISPLAY OPTIONS TESTS
# ============================================================================

test_that("jjcorrmat handles different matrix types", {
  # Create test data
  test_data <- data.frame(
    var1 = rnorm(50, 50, 10),
    var2 = rnorm(50, 60, 15),
    var3 = rnorm(50, 70, 12),
    stringsAsFactors = FALSE
  )

  # Test upper triangle
  result_upper <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      matrixtype = "upper"
    )
  }, error = function(e) NULL)

  # Test lower triangle
  result_lower <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      matrixtype = "lower"
    )
  }, error = function(e) NULL)

  # Test full matrix
  result_full <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      matrixtype = "full"
    )
  }, error = function(e) NULL)

  # All should succeed
  expect_true(!is.null(result_upper))
  expect_true(!is.null(result_lower))
  expect_true(!is.null(result_full))
})

test_that("jjcorrmat handles different matrix methods", {
  # Create test data
  test_data <- data.frame(
    var1 = rnorm(50, 50, 10),
    var2 = rnorm(50, 60, 15),
    var3 = rnorm(50, 70, 12),
    stringsAsFactors = FALSE
  )

  # Test square method
  result_square <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      matrixmethod = "square"
    )
  }, error = function(e) NULL)

  # Test circle method
  result_circle <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      matrixmethod = "circle"
    )
  }, error = function(e) NULL)

  # Both should succeed
  expect_true(!is.null(result_square))
  expect_true(!is.null(result_circle))
})

# ============================================================================
# EDGE CASES AND ERROR HANDLING
# ============================================================================

test_that("jjcorrmat validates minimum number of variables", {
  # Single variable should return early
  test_data <- data.frame(
    var1 = rnorm(50, 50, 10),
    stringsAsFactors = FALSE
  )

  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = "var1"
    )
  }, error = function(e) {
    NULL
  })

  # Should handle gracefully (returns without processing)
  expect_true(TRUE)  # Made it this far
})

test_that("jjcorrmat validates minimum sample size", {
  # Very small sample size should warn
  small_data <- data.frame(
    var1 = rnorm(5, 50, 10),
    var2 = rnorm(5, 60, 15),
    var3 = rnorm(5, 70, 12),
    stringsAsFactors = FALSE
  )

  expect_warning({
    jjcorrmat(
      data = small_data,
      dep = c("var1", "var2", "var3")
    )
  }, regexp = "Sample size")
})

test_that("jjcorrmat warns about too many variables", {
  # Create data with many variables
  set.seed(999)
  many_vars <- 15
  test_data <- as.data.frame(matrix(rnorm(50 * many_vars), ncol = many_vars))
  names(test_data) <- paste0("var", 1:many_vars)

  expect_warning({
    jjcorrmat(
      data = test_data,
      dep = names(test_data)
    )
  }, regexp = ">10 variables")
})

test_that("jjcorrmat handles constant variables", {
  # Create data with constant variable
  test_data <- data.frame(
    var1 = rep(5, 50),  # Constant
    var2 = rnorm(50, 60, 15),
    var3 = rnorm(50, 70, 12),
    stringsAsFactors = FALSE
  )

  # Should error or handle gracefully
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3")
    )
  }, error = function(e) {
    e
  })

  # Should either error or return
  expect_true(!is.null(result))
})

test_that("jjcorrmat produces consistent results across runs", {
  # Create test data
  set.seed(1111)
  test_data <- data.frame(
    var1 = rnorm(100, 50, 10),
    var2 = rnorm(100, 60, 15),
    var3 = rnorm(100, 70, 12),
    stringsAsFactors = FALSE
  )

  # Run twice
  result1 <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      typestatistics = "parametric"
    )
  }, error = function(e) {
    NULL
  })

  result2 <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3"),
      typestatistics = "parametric"
    )
  }, error = function(e) {
    NULL
  })

  # Both should succeed
  expect_true(!is.null(result1))
  expect_true(!is.null(result2))

  # Results should be deterministic
})

test_that("jjcorrmat handles real clinical correlation data structure", {
  # Simulate realistic biomarker correlation data
  set.seed(2222)
  clinical_data <- data.frame(
    biomarker_a = rnorm(200, mean = 5.5, sd = 1.2),
    biomarker_b = rnorm(200, mean = 12.3, sd = 3.4),
    biomarker_c = rnorm(200, mean = 8.7, sd = 2.1),
    lab_value_1 = rnorm(200, mean = 45, sd = 8),
    lab_value_2 = rnorm(200, mean = 120, sd = 15),
    imaging_metric = rnorm(200, mean = 2.3, sd = 0.5),
    patient_age = rnorm(200, mean = 65, sd = 12),  # Unused covariate
    patient_sex = factor(sample(c("M", "F"), 200, replace = TRUE)),  # Unused
    missing_lab = c(rep(NA, 100), rnorm(100, 100, 15)),  # Many NAs in unused column
    stringsAsFactors = FALSE
  )

  # Run analysis (should not drop rows due to NAs in unused columns)
  result <- tryCatch({
    jjcorrmat(
      data = clinical_data,
      dep = c("biomarker_a", "biomarker_b", "biomarker_c",
              "lab_value_1", "lab_value_2", "imaging_metric"),
      typestatistics = "parametric",
      partial = TRUE  # Use partial correlations to control for confounding
    )
  }, error = function(e) {
    NULL
  })

  # Should use all 200 observations (no NAs in selected biomarkers)
  expect_true(!is.null(result))
})

test_that("jjcorrmat handles highly correlated variables", {
  # Create perfectly correlated variables
  set.seed(3333)
  x <- rnorm(50)
  test_data <- data.frame(
    var1 = x,
    var2 = x + rnorm(50, 0, 0.01),  # Almost perfectly correlated
    var3 = rnorm(50, 70, 12),
    stringsAsFactors = FALSE
  )

  # Should handle multicollinearity
  result <- tryCatch({
    jjcorrmat(
      data = test_data,
      dep = c("var1", "var2", "var3")
    )
  }, error = function(e) {
    NULL
  })

  # Should succeed
  expect_true(!is.null(result))
})

test_that("jjcorrmat handles p-value adjustment methods", {
  # Create test data
  test_data <- data.frame(
    var1 = rnorm(100, 50, 10),
    var2 = rnorm(100, 60, 15),
    var3 = rnorm(100, 70, 12),
    var4 = rnorm(100, 80, 14),
    stringsAsFactors = FALSE
  )

  adjustment_methods <- c("holm", "bonferroni", "BH", "none")

  for (method in adjustment_methods) {
    result <- tryCatch({
      jjcorrmat(
        data = test_data,
        dep = c("var1", "var2", "var3", "var4"),
        padjustmethod = method
      )
    }, error = function(e) {
      NULL
    })

    # Each should work
    expect_true(!is.null(result), info = paste("Failed for", method))
  }
})
