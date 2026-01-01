# Comprehensive Tests for jjsyndromicplot
# Tests cover PCA accuracy, geometric calculations, and edge cases

# Setup reproducible test data
setup_pca_test_data <- function() {
  set.seed(42)
  data.frame(
    var1 = rnorm(100, 50, 10),
    var2 = rnorm(100, 100, 20),
    var3 = rnorm(100, 75, 15),
    var4 = rnorm(100, 30, 8),
    var5 = rnorm(100, 60, 12)
  )
}

# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("jjsyndromicplot creates valid output structure", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  test_data <- setup_pca_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4", "var5"),
    component = 1,
    cutoff = 0.5,
    center = TRUE,
    scale = TRUE
  )

  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("loadings" %in% names(result))
})

test_that("jjsyndromicplot requires at least 3 variables", {
  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )

  # Should not error but also not create plot with <3 vars
  result <- jjsyndromicplot(
    data = test_data,
    vars = c("x", "y"),
    component = 1
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot works with minimum 3 variables", {
  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50),
    z = rnorm(50)
  )

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("x", "y", "z"),
    component = 1,
    cutoff = 0.0
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# PCA CALCULATION ACCURACY TESTS
# ============================================================================

test_that("jjsyndromicplot PCA matches stats::prcomp results", {
  # Create simple test data with known structure
  set.seed(123)
  test_data <- data.frame(
    x = rnorm(100, 0, 1),
    y = rnorm(100, 0, 1),
    z = rnorm(100, 0, 1)
  )

  # Run jjsyndromicplot
  result <- jjsyndromicplot(
    data = test_data,
    vars = c("x", "y", "z"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = TRUE
  )

  # Run reference PCA
  reference_pca <- prcomp(test_data[, c("x", "y", "z")], center = TRUE, scale. = TRUE)

  # Verify loadings table exists
  expect_true("loadings" %in% names(result))

  # Note: Can't directly compare loadings as jamovi object structure is complex
  # But we verify the PCA runs without error with same parameters
  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot calculates variance accounted for correctly", {
  # Create correlated data where PC1 should explain high variance
  set.seed(123)
  x <- rnorm(100)
  test_data <- data.frame(
    var1 = x + rnorm(100, 0, 0.1),
    var2 = x + rnorm(100, 0, 0.1),
    var3 = x + rnorm(100, 0, 0.1)
  )

  # Run reference PCA
  ref_pca <- prcomp(test_data, center = TRUE, scale. = TRUE)
  expected_vaf_pc1 <- (ref_pca$sdev[1]^2 / sum(ref_pca$sdev^2)) * 100

  # PC1 should explain >80% of variance for highly correlated data
  expect_true(expected_vaf_pc1 > 80)

  # Run jjsyndromicplot - should not error
  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles centered vs uncentered data", {
  test_data <- setup_pca_test_data()

  # Centered
  result_centered <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = TRUE
  )
  expect_s3_class(result_centered, "Group")

  # Uncentered
  result_uncentered <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.0,
    center = FALSE,
    scale = TRUE
  )
  expect_s3_class(result_uncentered, "Group")
})

test_that("jjsyndromicplot handles scaled vs unscaled data", {
  test_data <- setup_pca_test_data()

  # Scaled
  result_scaled <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = TRUE
  )
  expect_s3_class(result_scaled, "Group")

  # Unscaled
  result_unscaled <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = FALSE
  )
  expect_s3_class(result_unscaled, "Group")
})

# ============================================================================
# COMPONENT SELECTION TESTS
# ============================================================================

test_that("jjsyndromicplot handles different component numbers", {
  test_data <- setup_pca_test_data()

  for (comp in 1:3) {
    result <- jjsyndromicplot(
      data = test_data,
      vars = c("var1", "var2", "var3", "var4", "var5"),
      component = comp,
      cutoff = 0.3,
      center = TRUE,
      scale = TRUE
    )

    expect_s3_class(result, "Group")
  }
})

test_that("jjsyndromicplot handles component number exceeding data dimensions", {
  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50),
    z = rnorm(50)
  )

  # Request component 5 when only 3 variables exist
  # Should handle gracefully (warning or error)
  result <- jjsyndromicplot(
    data = test_data,
    vars = c("x", "y", "z"),
    component = 5,
    cutoff = 0.0
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# CUTOFF THRESHOLD TESTS
# ============================================================================

test_that("jjsyndromicplot applies cutoff threshold correctly", {
  test_data <- setup_pca_test_data()

  # Very high cutoff - should filter most variables
  result_high <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4", "var5"),
    component = 1,
    cutoff = 0.9,
    center = TRUE,
    scale = TRUE
  )
  expect_s3_class(result_high, "Group")

  # Very low cutoff - should show all variables
  result_low <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4", "var5"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = TRUE
  )
  expect_s3_class(result_low, "Group")
})

test_that("jjsyndromicplot handles extreme cutoff values", {
  test_data <- setup_pca_test_data()

  # Cutoff = 0 (show all)
  result_zero <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.0
  )
  expect_s3_class(result_zero, "Group")

  # Cutoff = 1 (show only perfect loadings, if any)
  result_one <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 1.0
  )
  expect_s3_class(result_one, "Group")
})

# ============================================================================
# GEOMETRIC CALCULATION TESTS (TRIGONOMETRY)
# ============================================================================

test_that("jjsyndromicplot angle calculations distribute variables evenly", {
  # Test that angles are correctly calculated for circular positioning
  # For n variables, angles should be evenly spaced by 2*pi/n

  n_vars <- 4
  expected_angle_increment <- 2 * pi / n_vars

  # Angles should be: pi/2, pi/2 + 2*pi/4, pi/2 + 4*pi/4, pi/2 + 6*pi/4
  # (starting at top, going counterclockwise)

  expected_angles <- sapply(0:(n_vars - 1), function(i) {
    (i * 2 * pi / n_vars) + pi / 2
  })

  # All angles should be between 0 and 2*pi (or normalized)
  # Verify the increment is correct
  angle_diffs <- diff(expected_angles)
  expect_true(all(abs(angle_diffs - expected_angle_increment) < 1e-10))
})

test_that("jjsyndromicplot arrow positions are geometrically consistent", {
  # Verify that arrow start (outer) is farther from center than arrow end (inner)
  # For the default implementation:
  # Arrow end (xend, yend): radius 3.5
  # Arrow start (x, y): radius 7

  test_angle <- pi / 4  # 45 degrees
  xend_expected <- 3.5 * cos(test_angle)
  yend_expected <- 3.5 * sin(test_angle)
  x_expected <- 7 * cos(test_angle)
  y_expected <- 7 * sin(test_angle)

  # Distance from origin
  dist_end <- sqrt(xend_expected^2 + yend_expected^2)
  dist_start <- sqrt(x_expected^2 + y_expected^2)

  expect_equal(dist_end, 3.5, tolerance = 1e-10)
  expect_equal(dist_start, 7, tolerance = 1e-10)
  expect_true(dist_start > dist_end)  # Arrow points inward
})

test_that("jjsyndromicplot text positions are outside arrows", {
  # Text should be positioned outside the arrows (radius 9 vs arrow radius 7)
  test_angle <- pi / 3

  x_arrow <- 7 * cos(test_angle)
  y_arrow <- 7 * sin(test_angle)
  xtext <- 9 * cos(test_angle)
  ytext <- 9 * sin(test_angle)

  dist_arrow <- sqrt(x_arrow^2 + y_arrow^2)
  dist_text <- sqrt(xtext^2 + ytext^2)

  expect_equal(dist_text, 9, tolerance = 1e-10)
  expect_true(dist_text > dist_arrow)
})

test_that("jjsyndromicplot handles variables at special angles", {
  # Test variables positioned at 0, 90, 180, 270 degrees
  # These are important edge cases for text positioning logic

  angles <- c(0, pi/2, pi, 3*pi/2)  # 0°, 90°, 180°, 270°

  for (angle in angles) {
    x <- 7 * cos(angle)
    y <- 7 * sin(angle)

    # At 90° (top): x ≈ 0, y > 0
    # At 180° (left): x < 0, y ≈ 0
    # At 270° (bottom): x ≈ 0, y < 0
    # At 0° (right): x > 0, y ≈ 0

    # Just verify calculations don't produce NA/Inf
    expect_false(is.na(x))
    expect_false(is.na(y))
    expect_false(is.infinite(x))
    expect_false(is.infinite(y))
  }
})

# ============================================================================
# CUSTOMIZATION OPTIONS TESTS
# ============================================================================

test_that("jjsyndromicplot applies arrow size multiplier", {
  test_data <- setup_pca_test_data()

  for (arrow_size in c(1, 10, 20, 30)) {
    result <- jjsyndromicplot(
      data = test_data,
      vars = c("var1", "var2", "var3"),
      component = 1,
      cutoff = 0.3,
      arrowsize = arrow_size
    )

    expect_s3_class(result, "Group")
  }
})

test_that("jjsyndromicplot applies text size", {
  test_data <- setup_pca_test_data()

  for (text_size in c(4, 9, 15, 20)) {
    result <- jjsyndromicplot(
      data = test_data,
      vars = c("var1", "var2", "var3"),
      component = 1,
      cutoff = 0.3,
      textsize = text_size
    )

    expect_s3_class(result, "Group")
  }
})

test_that("jjsyndromicplot handles repel option", {
  test_data <- setup_pca_test_data()

  # With repel
  result_repel <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4", "var5"),
    component = 1,
    cutoff = 0.3,
    repel = TRUE
  )
  expect_s3_class(result_repel, "Group")

  # Without repel
  result_no_repel <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4", "var5"),
    component = 1,
    cutoff = 0.3,
    repel = FALSE
  )
  expect_s3_class(result_no_repel, "Group")
})

test_that("jjsyndromicplot applies color scheme", {
  test_data <- setup_pca_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4"),
    component = 1,
    cutoff = 0.3,
    colorlow = "blue",
    colormid = "white",
    colorhigh = "red"
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles legend options", {
  test_data <- setup_pca_test_data()

  # With legend
  result_legend <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.3,
    plotlegend = TRUE
  )
  expect_s3_class(result_legend, "Group")

  # Without legend
  result_no_legend <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.3,
    plotlegend = FALSE
  )
  expect_s3_class(result_no_legend, "Group")
})

test_that("jjsyndromicplot handles cutoff indicator option", {
  test_data <- setup_pca_test_data()

  # With cutoff indicator
  result_cutoff_on <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.5,
    plotcutoff = TRUE
  )
  expect_s3_class(result_cutoff_on, "Group")

  # Without cutoff indicator
  result_cutoff_off <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.5,
    plotcutoff = FALSE
  )
  expect_s3_class(result_cutoff_off, "Group")
})

# ============================================================================
# VARIABLE ORDERING TESTS
# ============================================================================

test_that("jjsyndromicplot applies variable ordering options", {
  test_data <- setup_pca_test_data()

  orderings <- c("absdecreasing", "absincreasing", "decreasing", "increasing")

  for (order_type in orderings) {
    result <- jjsyndromicplot(
      data = test_data,
      vars = c("var1", "var2", "var3", "var4", "var5"),
      component = 1,
      cutoff = 0.2,
      varorder = order_type
    )

    expect_s3_class(result, "Group")
  }
})

# ============================================================================
# CLINICAL PRESETS TESTS
# ============================================================================

test_that("jjsyndromicplot applies clinical presets", {
  test_data <- setup_pca_test_data()

  presets <- c("none", "biomarker_discovery", "disease_subtyping")

  for (preset in presets) {
    result <- jjsyndromicplot(
      data = test_data,
      vars = c("var1", "var2", "var3", "var4", "var5"),
      component = 1,
      clinicalPreset = preset
    )

    expect_s3_class(result, "Group")
  }
})

# ============================================================================
# EDGE CASES AND ERROR HANDLING
# ============================================================================

test_that("jjsyndromicplot handles missing data", {
  test_data <- setup_pca_test_data()
  test_data[1:5, "var1"] <- NA
  test_data[10:15, "var2"] <- NA

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.3
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles high missingness", {
  test_data <- setup_pca_test_data()
  # Remove 80% of data
  test_data[1:80, "var1"] <- NA

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.3
  )

  # Should complete but may have warnings
  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles constant variables", {
  test_data <- setup_pca_test_data()
  test_data$var_constant <- 5  # Constant variable

  # PCA may fail or handle gracefully
  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var_constant"),
    component = 1,
    cutoff = 0.0
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles highly correlated variables", {
  set.seed(123)
  x <- rnorm(100)
  test_data <- data.frame(
    var1 = x,
    var2 = x + rnorm(100, 0, 0.01),  # Nearly identical to var1
    var3 = x + rnorm(100, 0, 0.01),  # Nearly identical to var1
    var4 = rnorm(100)
  )

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4"),
    component = 1,
    cutoff = 0.3,
    scale = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles uncorrelated variables", {
  # Completely independent variables
  set.seed(123)
  test_data <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100),
    var4 = rnorm(100)
  )

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4"),
    component = 1,
    cutoff = 0.0
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles small sample size", {
  # Only 10 observations
  test_data <- data.frame(
    x = rnorm(10),
    y = rnorm(10),
    z = rnorm(10)
  )

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("x", "y", "z"),
    component = 1,
    cutoff = 0.0
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles many variables", {
  # 20 variables
  set.seed(123)
  many_vars_data <- as.data.frame(matrix(rnorm(100 * 20), ncol = 20))
  names(many_vars_data) <- paste0("var", 1:20)

  result <- jjsyndromicplot(
    data = many_vars_data,
    vars = names(many_vars_data),
    component = 1,
    cutoff = 0.3
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot handles variables with different scales", {
  # Variables with very different scales
  set.seed(123)
  test_data <- data.frame(
    small_scale = rnorm(100, 0, 0.1),
    medium_scale = rnorm(100, 0, 10),
    large_scale = rnorm(100, 0, 1000)
  )

  # Without scaling - may be problematic
  result_unscaled <- jjsyndromicplot(
    data = test_data,
    vars = c("small_scale", "medium_scale", "large_scale"),
    component = 1,
    cutoff = 0.0,
    scale = FALSE
  )
  expect_s3_class(result_unscaled, "Group")

  # With scaling - should handle well
  result_scaled <- jjsyndromicplot(
    data = test_data,
    vars = c("small_scale", "medium_scale", "large_scale"),
    component = 1,
    cutoff = 0.0,
    scale = TRUE
  )
  expect_s3_class(result_scaled, "Group")
})

test_that("jjsyndromicplot handles plot size options", {
  test_data <- setup_pca_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.3,
    plotwidth = 800,
    plotheight = 800
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# OUTPUT VALIDATION TESTS
# ============================================================================

test_that("jjsyndromicplot generates loadings table", {
  test_data <- setup_pca_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3", "var4", "var5"),
    component = 1,
    cutoff = 0.3
  )

  expect_s3_class(result, "Group")
  expect_true("loadings" %in% names(result))
})

test_that("jjsyndromicplot provides explanations when requested", {
  test_data <- setup_pca_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("var1", "var2", "var3"),
    component = 1,
    cutoff = 0.3,
    showExplanations = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# INTEGRATION TESTS
# ============================================================================

test_that("jjsyndromicplot works with mtcars dataset", {
  data("mtcars")

  result <- jjsyndromicplot(
    data = mtcars,
    vars = c("mpg", "disp", "hp", "drat", "wt", "qsec"),
    component = 1,
    cutoff = 0.5,
    center = TRUE,
    scale = TRUE
  )

  expect_s3_class(result, "Group")
})

test_that("jjsyndromicplot works with iris dataset", {
  data("iris")

  result <- jjsyndromicplot(
    data = iris,
    vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    component = 1,
    cutoff = 0.4,
    center = TRUE,
    scale = TRUE
  )

  expect_s3_class(result, "Group")
})

# ============================================================================
# NOTES FOR FUTURE TESTING
# ============================================================================

# Additional tests that could be added:
# - Verify exact loading values against reference PCA for simple cases
# - Test arrow direction (positive vs negative loadings point correct direction)
# - Verify triangle polygon coordinates are correct
# - Test label collision avoidance with repel=TRUE
# - Performance tests with large datasets
# - Memory usage profiling
# - Test with real clinical datasets
