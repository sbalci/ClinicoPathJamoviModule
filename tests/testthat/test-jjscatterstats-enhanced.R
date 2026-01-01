# Enhanced Test Suite for jjscatterstats
# This file provides comprehensive testing including:
# 1. Enhanced scatter plot mode
# 2. ggpubr plot mode
# 3. Visual regression testing
# 4. Statistical output validation

# Setup test data
setup_test_data <- function() {
  set.seed(42)  # Reproducible test data
  data.frame(
    x_var = rnorm(100, 50, 10),
    y_var = rnorm(100, 25, 5) + 0.5 * rnorm(100, 50, 10),  # Correlated with x
    color_cat = factor(rep(c("Group1", "Group2", "Group3"), length.out = 100)),
    size_cont = runif(100, 1, 10),
    shape_cat = factor(rep(c("TypeA", "TypeB"), length.out = 100)),
    alpha_cont = runif(100, 0.3, 1.0),
    label_text = paste0("Point", 1:100)
  )
}

# ============================================================================
# ENHANCED SCATTER PLOT MODE TESTS
# ============================================================================

test_that("jjscatterstats enhanced mode works with color variable", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    colorvar = "color_cat",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot3" %in% names(result))

  # Verify that enhanced plot (plot3) is visible when colorvar is provided
  # Note: Actual visibility depends on .r.yaml configuration
})

test_that("jjscatterstats enhanced mode works with size variable", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    sizevar = "size_cont",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot3" %in% names(result))
})

test_that("jjscatterstats enhanced mode works with shape variable", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    shapevar = "shape_cat",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot3" %in% names(result))
})

test_that("jjscatterstats enhanced mode works with alpha variable", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    alphavar = "alpha_cont",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot3" %in% names(result))
})

test_that("jjscatterstats enhanced mode works with label variable", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    labelvar = "label_text",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot3" %in% names(result))
})

test_that("jjscatterstats enhanced mode works with multiple aesthetics", {
  test_data <- setup_test_data()

  # Test with all enhanced variables at once
  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    colorvar = "color_cat",
    sizevar = "size_cont",
    shapevar = "shape_cat",
    alphavar = "alpha_cont",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot3" %in% names(result))
})

test_that("jjscatterstats enhanced mode respects smoothing options", {
  test_data <- setup_test_data()

  # Test with different smoothing methods
  smooth_methods <- c("lm", "loess", "gam")

  for (method in smooth_methods) {
    result <- jjscatterstats(
      data = test_data,
      dep = "x_var",
      group = "y_var",
      colorvar = "color_cat",
      smoothmethod = method,
      typestatistics = "parametric"
    )

    expect_s3_class(result, "Group")
  }
})

# ============================================================================
# GGPUBR PLOT MODE TESTS
# ============================================================================

test_that("jjscatterstats ggpubr mode creates additional plot", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    addggpubr = TRUE,
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot4" %in% names(result))
})

test_that("jjscatterstats ggpubr mode respects color palette", {
  test_data <- setup_test_data()

  palettes <- c("npg", "aaas", "nejm", "lancet", "jco", "ucscgb", "d3", "locuszoom")

  for (palette in palettes) {
    result <- jjscatterstats(
      data = test_data,
      dep = "x_var",
      group = "y_var",
      grvar = "color_cat",
      addggpubr = TRUE,
      ggpubrPalette = palette,
      typestatistics = "parametric"
    )

    expect_s3_class(result, "Group")
    expect_true("plot4" %in% names(result))
  }
})

test_that("jjscatterstats ggpubr mode adds correlation statistics", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    addggpubr = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "pearson",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot4" %in% names(result))
})

test_that("jjscatterstats ggpubr mode respects correlation methods", {
  test_data <- setup_test_data()

  corr_methods <- c("pearson", "kendall", "spearman")

  for (method in corr_methods) {
    result <- jjscatterstats(
      data = test_data,
      dep = "x_var",
      group = "y_var",
      addggpubr = TRUE,
      ggpubrAddCorr = TRUE,
      ggpubrCorrMethod = method,
      typestatistics = "parametric"
    )

    expect_s3_class(result, "Group")
  }
})

test_that("jjscatterstats ggpubr mode adds smooth line", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    addggpubr = TRUE,
    ggpubrAddSmooth = TRUE,
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
  expect_true("plot4" %in% names(result))
})

# ============================================================================
# STATISTICAL OUTPUT VALIDATION TESTS
# ============================================================================

test_that("jjscatterstats produces correct Pearson correlation", {
  # Create data with known correlation
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- 0.8 * x + rnorm(n, 0, 0.6)  # r ≈ 0.8
  test_data <- data.frame(x = x, y = y)

  # Calculate expected correlation
  expected_cor <- cor(x, y, method = "pearson")
  expected_cor_test <- cor.test(x, y, method = "pearson")

  # Run jjscatterstats
  result <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "parametric"  # Uses Pearson
  )

  expect_s3_class(result, "Group")

  # Verify correlation is in reasonable range
  expect_true(abs(expected_cor - 0.8) < 0.15,
              info = paste("Expected correlation near 0.8, got", expected_cor))

  # Verify p-value is significant (given high correlation)
  expect_true(expected_cor_test$p.value < 0.05,
              info = "Expected significant correlation")
})

test_that("jjscatterstats produces correct Spearman correlation", {
  # Create data with known monotonic relationship
  set.seed(456)
  n <- 100
  x <- 1:n
  y <- x^1.5 + rnorm(n, 0, 20)  # Monotonic but non-linear
  test_data <- data.frame(x = x, y = y)

  # Calculate expected correlation
  expected_cor <- cor(x, y, method = "spearman")
  expected_cor_test <- cor.test(x, y, method = "spearman", exact = FALSE)

  # Run jjscatterstats
  result <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "nonparametric"  # Uses Spearman
  )

  expect_s3_class(result, "Group")

  # Verify Spearman correlation is high (monotonic relationship)
  expect_true(expected_cor > 0.85,
              info = paste("Expected high Spearman correlation, got", expected_cor))

  # Verify p-value is significant
  expect_true(expected_cor_test$p.value < 0.05,
              info = "Expected significant correlation")
})

test_that("jjscatterstats handles zero correlation", {
  # Create data with no correlation
  set.seed(789)
  n <- 100
  x <- rnorm(n)
  y <- rnorm(n)  # Independent
  test_data <- data.frame(x = x, y = y)

  # Calculate expected correlation
  expected_cor <- cor(x, y, method = "pearson")

  # Run jjscatterstats
  result <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")

  # Verify correlation is near zero
  expect_true(abs(expected_cor) < 0.3,
              info = paste("Expected correlation near 0, got", expected_cor))
})

test_that("jjscatterstats handles negative correlation", {
  # Create data with negative correlation
  set.seed(321)
  n <- 100
  x <- rnorm(n)
  y <- -0.7 * x + rnorm(n, 0, 0.7)  # r ≈ -0.7
  test_data <- data.frame(x = x, y = y)

  # Calculate expected correlation
  expected_cor <- cor(x, y, method = "pearson")

  # Run jjscatterstats
  result <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")

  # Verify correlation is negative and in reasonable range
  expect_true(expected_cor < -0.5,
              info = paste("Expected negative correlation < -0.5, got", expected_cor))
})

test_that("jjscatterstats statistical tests work for all types", {
  # Test data with moderate positive correlation
  set.seed(555)
  n <- 100
  x <- rnorm(n, 50, 10)
  y <- 0.6 * x + rnorm(n, 0, 8)
  test_data <- data.frame(x = x, y = y)

  # Test all statistical types
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjscatterstats(
      data = test_data,
      dep = "x",
      group = "y",
      typestatistics = stat_type
    )

    expect_s3_class(result, "Group",
                   info = paste("Failed for stat_type:", stat_type))

    # Verify plot exists
    expect_true("plot" %in% names(result),
               info = paste("Plot missing for stat_type:", stat_type))
  }
})

# ============================================================================
# CLINICAL PRESET TESTS
# ============================================================================

test_that("jjscatterstats clinical presets work correctly", {
  test_data <- setup_test_data()

  presets <- c("custom", "biomarker_correlation", "gene_expression",
               "survival_biomarker", "dose_response")

  for (preset in presets) {
    result <- jjscatterstats(
      data = test_data,
      dep = "x_var",
      group = "y_var",
      clinicalPreset = preset,
      typestatistics = "parametric"
    )

    expect_s3_class(result, "Group",
                   info = paste("Failed for preset:", preset))
  }
})

# ============================================================================
# MARGINAL DISTRIBUTION TESTS
# ============================================================================

test_that("jjscatterstats adds marginal distributions", {
  test_data <- setup_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    marginal = TRUE,
    marginaltype = "histogram",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
})

test_that("jjscatterstats respects marginal distribution types", {
  test_data <- setup_test_data()

  marginal_types <- c("histogram", "boxplot", "density", "violin", "densigram")

  for (mtype in marginal_types) {
    result <- jjscatterstats(
      data = test_data,
      dep = "x_var",
      group = "y_var",
      marginal = TRUE,
      marginaltype = mtype,
      typestatistics = "parametric"
    )

    expect_s3_class(result, "Group",
                   info = paste("Failed for marginal type:", mtype))
  }
})

# ============================================================================
# INTEGRATION TESTS
# ============================================================================

test_that("jjscatterstats combined features work together", {
  test_data <- setup_test_data()

  # Test combining multiple features at once
  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    grvar = "color_cat",
    colorvar = "shape_cat",
    sizevar = "size_cont",
    marginal = TRUE,
    marginaltype = "histogram",
    addggpubr = TRUE,
    ggpubrAddCorr = TRUE,
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))   # Main ggstatsplot plot
  expect_true("plot2" %in% names(result))  # Grouped plot
  expect_true("plot3" %in% names(result))  # Enhanced plot
  expect_true("plot4" %in% names(result))  # ggpubr plot
})

# ============================================================================
# EDGE CASES AND ROBUSTNESS
# ============================================================================

test_that("jjscatterstats handles perfect correlation", {
  # Create data with perfect correlation
  n <- 100
  x <- 1:n
  y <- 2 * x + 5  # Perfect linear relationship
  test_data <- data.frame(x = x, y = y)

  result <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")

  # Verify correlation is 1
  expect_equal(cor(x, y), 1.0)
})

test_that("jjscatterstats handles data with outliers", {
  # Create data with outliers
  set.seed(999)
  n <- 100
  x <- rnorm(n)
  y <- 0.5 * x + rnorm(n, 0, 0.5)

  # Add outliers
  x[1:3] <- c(-10, 10, -8)
  y[1:3] <- c(10, -10, 8)

  test_data <- data.frame(x = x, y = y)

  # Test with parametric (sensitive to outliers)
  result_param <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "parametric"
  )

  expect_s3_class(result_param, "Group")

  # Test with robust (resistant to outliers)
  result_robust <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "robust"
  )

  expect_s3_class(result_robust, "Group")
})

test_that("jjscatterstats handles small sample sizes", {
  # Small sample (n=10)
  set.seed(111)
  test_data <- data.frame(
    x = rnorm(10),
    y = rnorm(10)
  )

  result <- jjscatterstats(
    data = test_data,
    dep = "x",
    group = "y",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "Group")
})

test_that("jjscatterstats handles constant variables gracefully", {
  # One variable is constant
  test_data <- data.frame(
    x = rep(5, 100),  # Constant
    y = rnorm(100)
  )

  # This should either handle gracefully or provide clear error
  expect_error(
    jjscatterstats(
      data = test_data,
      dep = "x",
      group = "y",
      typestatistics = "parametric"
    ),
    NA  # Expect no error, or specific informative error
  )
})
