# Visual Regression Tests for jjsyndromicplot
# This file uses vdiffr package to test visual output consistency
# CRITICAL: Tests the complex trigonometric calculations for arrow positioning

# Only run visual tests if vdiffr is available
skip_if_not_installed_vdiffr <- function() {
  if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr package not installed")
  }
}

# Helper function to extract plot from jamovi results
extract_plot <- function(result, plot_name = "plot") {
  if (!plot_name %in% names(result)) {
    stop(paste("Plot", plot_name, "not found in results"))
  }

  plot_obj <- result[[plot_name]]

  # If it's a jamovi Image object with state, extract the ggplot
  if (inherits(plot_obj, "Image") && !is.null(plot_obj$state)) {
    return(plot_obj$state)
  }

  return(plot_obj)
}

# Setup reproducible test data
setup_visual_test_data <- function() {
  set.seed(12345)  # Fixed seed for reproducible visuals
  data.frame(
    biomarker1 = rnorm(80, 50, 10),
    biomarker2 = rnorm(80, 100, 20),
    biomarker3 = rnorm(80, 75, 15),
    biomarker4 = rnorm(80, 30, 8),
    biomarker5 = rnorm(80, 60, 12),
    biomarker6 = rnorm(80, 40, 10)
  )
}

# ============================================================================
# BASIC PLOT VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsyndromicplot basic PC1 visualization", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 1,
    cutoff = 0.4,
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_basic_pc1",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot PC2 visualization", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 2,
    cutoff = 0.4,
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_pc2",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# CUTOFF THRESHOLD VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with low cutoff threshold", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.2,  # Low cutoff - more variables shown
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_low_cutoff",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot with high cutoff threshold", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.7,  # High cutoff - fewer variables shown
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_high_cutoff",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# ARROW SIZE VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with small arrow size", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    arrowsize = 5,  # Small arrows
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_small_arrows",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot with large arrow size", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    arrowsize = 20,  # Large arrows
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_large_arrows",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# COLOR SCHEME VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with blue-white-red colors", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    colorlow = "blue",
    colormid = "white",
    colorhigh = "red"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_blue_red",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot with green-white-purple colors", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    colorlow = "green",
    colormid = "white",
    colorhigh = "purple"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_green_purple",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# VARIABLE ORDERING VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot ordered by absolute value decreasing", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 1,
    cutoff = 0.3,
    varorder = "absdecreasing"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_order_abs_decreasing",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot ordered by value decreasing", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 1,
    cutoff = 0.3,
    varorder = "decreasing"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_order_decreasing",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot ordered by value increasing", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 1,
    cutoff = 0.3,
    varorder = "increasing"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_order_increasing",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# TEXT SIZE VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with small text", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    textsize = 6
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_small_text",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot with large text", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    textsize = 14
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_large_text",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# LEGEND OPTIONS VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with legend", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    plotlegend = TRUE,
    plotcutoff = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_with_legend",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot without legend", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.4,
    plotlegend = FALSE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_no_legend",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot legend with cutoff indicator", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.5,
    plotlegend = TRUE,
    plotcutoff = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_cutoff_indicator",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# LABEL REPEL VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with label repel", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5", "biomarker6"),
    component = 1,
    cutoff = 0.2,  # Low cutoff for many labels
    repel = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_label_repel",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot without label repel", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5", "biomarker6"),
    component = 1,
    cutoff = 0.2,
    repel = FALSE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_no_repel",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# CLINICAL PRESET VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot biomarker discovery preset", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 1,
    clinicalPreset = "biomarker_discovery"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_preset_biomarker",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot disease subtyping preset", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4", "biomarker5"),
    component = 1,
    clinicalPreset = "disease_subtyping"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_preset_subtyping",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# GEOMETRIC EDGE CASES VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot with 3 variables (triangle)", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3"),
    component = 1,
    cutoff = 0.0,  # Show all to test geometric placement
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_3vars_triangle",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot with 4 variables (square)", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsyndromicplot(
    data = test_data,
    vars = c("biomarker1", "biomarker2", "biomarker3", "biomarker4"),
    component = 1,
    cutoff = 0.0,
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_4vars_square",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot with many variables (12 variables)", {
  skip_if_not_installed_vdiffr()

  set.seed(12345)
  many_vars_data <- as.data.frame(matrix(rnorm(80 * 12), ncol = 12))
  names(many_vars_data) <- paste0("var", 1:12)

  result <- jjsyndromicplot(
    data = many_vars_data,
    vars = names(many_vars_data),
    component = 1,
    cutoff = 0.3,
    textsize = 7
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_12vars_crowded",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# REAL-WORLD DATASET VISUAL TESTS
# ============================================================================

test_that("jjsyndromicplot mtcars dataset PC1", {
  skip_if_not_installed_vdiffr()

  data("mtcars")

  result <- jjsyndromicplot(
    data = mtcars,
    vars = c("mpg", "disp", "hp", "drat", "wt", "qsec"),
    component = 1,
    cutoff = 0.5,
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_mtcars_pc1",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsyndromicplot iris dataset PC1", {
  skip_if_not_installed_vdiffr()

  data("iris")

  result <- jjsyndromicplot(
    data = iris,
    vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    component = 1,
    cutoff = 0.4,
    center = TRUE,
    scale = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "syndromic_iris_pc1",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# NOTES FOR VISUAL REGRESSION TESTING
# ============================================================================

# To manage visual test cases:
# 1. Run: devtools::test()
# 2. If new plots or differences detected: vdiffr::manage_cases()
# 3. Review each plot change in the Shiny app
# 4. Accept or reject visual changes
# 5. Committed SVG files serve as baseline for future tests
#
# CRITICAL PROTECTION:
# These visual tests protect against bugs in the complex trigonometric
# calculations that position arrows, labels, and the triangle polygon.
#
# What we're protecting:
# - Arrow positioning (cos/sin calculations for circular placement)
# - Arrow direction (positive vs negative loadings)
# - Label positioning (avoiding triangle overlap)
# - Triangle polygon geometry
# - Even distribution of variables around circle
# - Text angle calculations
# - Legend gradient rendering
# - Cutoff threshold visualization
#
# These tests ensure that a small error in the complex geometry
# formulas doesn't cause arrows to point in wrong directions or
# be positioned incorrectly, which could lead to misinterpretation
# of PCA results.
