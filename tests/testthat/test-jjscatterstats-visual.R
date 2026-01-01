# Visual Regression Tests for jjscatterstats
# This file uses vdiffr package to test visual output consistency
# Visual regression tests ensure plots don't change unexpectedly

# Only run visual tests if vdiffr is available
skip_if_not_installed_vdiffr <- function() {
  if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr package not installed")
  }
}

# Helper function to extract plot from jamovi results
extract_plot <- function(result, plot_name = "plot") {
  # jamovi results contain plot objects that need to be extracted
  # This helper handles the extraction logic
  if (!plot_name %in% names(result)) {
    stop(paste("Plot", plot_name, "not found in results"))
  }

  # Get the plot object - may need to call $state if it's a jamovi Image object
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
    biomarker_a = rnorm(80, 50, 15),
    biomarker_b = rnorm(80, 100, 25),
    treatment = factor(rep(c("Control", "DrugA", "DrugB", "Combo"), each = 20)),
    response = factor(rep(c("Responder", "Non-responder"), times = 40)),
    patient_id = paste0("PT", 1:80)
  )
}

# ============================================================================
# BASIC PLOT VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjscatterstats basic parametric plot visual regression", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    typestatistics = "parametric",
    mytitle = "Biomarker Correlation (Parametric)"
  )

  # Note: Visual snapshot testing may require manual verification
  # Run devtools::test() and then vdiffr::manage_cases() to validate
  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "basic_parametric_scatter",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjscatterstats nonparametric plot visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    typestatistics = "nonparametric",
    mytitle = "Biomarker Correlation (Spearman)"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "nonparametric_scatter",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjscatterstats robust plot visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    typestatistics = "robust",
    mytitle = "Biomarker Correlation (Robust)"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "robust_scatter",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# GROUPED PLOT VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjscatterstats grouped plot visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    grvar = "treatment",
    typestatistics = "parametric",
    mytitle = "Biomarker Correlation by Treatment"
  )

  plot <- extract_plot(result, "plot2")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "grouped_scatter_treatment",
      fig = plot
    )
  } else {
    skip("Could not extract grouped plot for visual testing")
  }
})

# ============================================================================
# ENHANCED PLOT MODE VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjscatterstats enhanced plot with color variable visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    colorvar = "treatment",
    typestatistics = "parametric",
    mytitle = "Enhanced: Color by Treatment"
  )

  plot <- extract_plot(result, "plot3")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "enhanced_scatter_color",
      fig = plot
    )
  } else {
    skip("Could not extract enhanced plot for visual testing")
  }
})

test_that("jjscatterstats enhanced plot with size variable visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()
  # Add a continuous size variable
  test_data$tumor_size <- runif(nrow(test_data), 2, 15)

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    sizevar = "tumor_size",
    typestatistics = "parametric",
    mytitle = "Enhanced: Size by Tumor Size"
  )

  plot <- extract_plot(result, "plot3")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "enhanced_scatter_size",
      fig = plot
    )
  } else {
    skip("Could not extract enhanced plot for visual testing")
  }
})

test_that("jjscatterstats enhanced plot with shape variable visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    shapevar = "response",
    typestatistics = "parametric",
    mytitle = "Enhanced: Shape by Response"
  )

  plot <- extract_plot(result, "plot3")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "enhanced_scatter_shape",
      fig = plot
    )
  } else {
    skip("Could not extract enhanced plot for visual testing")
  }
})

test_that("jjscatterstats enhanced plot with multiple aesthetics visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()
  test_data$tumor_size <- runif(nrow(test_data), 2, 15)
  test_data$alpha_val <- runif(nrow(test_data), 0.4, 1.0)

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    colorvar = "treatment",
    sizevar = "tumor_size",
    shapevar = "response",
    alphavar = "alpha_val",
    typestatistics = "parametric",
    mytitle = "Enhanced: Multiple Aesthetics"
  )

  plot <- extract_plot(result, "plot3")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "enhanced_scatter_multiple",
      fig = plot
    )
  } else {
    skip("Could not extract enhanced plot for visual testing")
  }
})

# ============================================================================
# GGPUBR PLOT MODE VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjscatterstats ggpubr plot basic visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    addggpubr = TRUE,
    ggpubrPalette = "jco",
    typestatistics = "parametric"
  )

  plot <- extract_plot(result, "plot4")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "ggpubr_scatter_basic",
      fig = plot
    )
  } else {
    skip("Could not extract ggpubr plot for visual testing")
  }
})

test_that("jjscatterstats ggpubr plot with correlation visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    addggpubr = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "pearson",
    ggpubrPalette = "npg",
    typestatistics = "parametric"
  )

  plot <- extract_plot(result, "plot4")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "ggpubr_scatter_with_correlation",
      fig = plot
    )
  } else {
    skip("Could not extract ggpubr plot for visual testing")
  }
})

test_that("jjscatterstats ggpubr plot with smooth line visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    addggpubr = TRUE,
    ggpubrAddSmooth = TRUE,
    ggpubrPalette = "lancet",
    typestatistics = "parametric"
  )

  plot <- extract_plot(result, "plot4")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "ggpubr_scatter_with_smooth",
      fig = plot
    )
  } else {
    skip("Could not extract ggpubr plot for visual testing")
  }
})

test_that("jjscatterstats ggpubr plot grouped visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    grvar = "treatment",
    addggpubr = TRUE,
    ggpubrAddCorr = TRUE,
    ggpubrCorrMethod = "spearman",
    ggpubrPalette = "aaas",
    typestatistics = "nonparametric"
  )

  plot <- extract_plot(result, "plot4")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "ggpubr_scatter_grouped",
      fig = plot
    )
  } else {
    skip("Could not extract ggpubr plot for visual testing")
  }
})

# ============================================================================
# MARGINAL DISTRIBUTION VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjscatterstats with marginal histograms visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    marginal = TRUE,
    marginaltype = "histogram",
    typestatistics = "parametric",
    mytitle = "Scatter with Marginal Histograms"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "scatter_marginal_histogram",
      fig = plot
    )
  } else {
    skip("Could not extract plot with marginals for visual testing")
  }
})

test_that("jjscatterstats with marginal density visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    marginal = TRUE,
    marginaltype = "density",
    typestatistics = "parametric",
    mytitle = "Scatter with Marginal Densities"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "scatter_marginal_density",
      fig = plot
    )
  } else {
    skip("Could not extract plot with marginals for visual testing")
  }
})

test_that("jjscatterstats with marginal boxplots visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    marginal = TRUE,
    marginaltype = "boxplot",
    typestatistics = "parametric",
    mytitle = "Scatter with Marginal Boxplots"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "scatter_marginal_boxplot",
      fig = plot
    )
  } else {
    skip("Could not extract plot with marginals for visual testing")
  }
})

# ============================================================================
# CUSTOMIZATION VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjscatterstats with custom titles visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    mytitle = "Custom Main Title",
    xtitle = "Biomarker A Expression (AU)",
    ytitle = "Biomarker B Expression (AU)",
    resultssubtitle = TRUE,
    typestatistics = "parametric"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "scatter_custom_titles",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjscatterstats with original theme visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjscatterstats(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    originaltheme = TRUE,
    typestatistics = "parametric",
    mytitle = "Original ggstatsplot Theme"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "scatter_original_theme",
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
# Visual regression tests protect against:
# - Unintended plot appearance changes
# - Theme/style regressions
# - Layout issues
# - Legend positioning problems
# - Axis label formatting changes
#
# These tests complement functional tests by ensuring
# plots look correct, not just that they run without errors.
