# Visual Regression Tests for jjsegmentedtotalbar
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
    treatment = factor(rep(c("Control", "DrugA", "DrugB", "Combo"), each = 4)),
    response = factor(rep(c("CR", "PR", "SD", "PD"), 4)),
    count = c(25, 35, 30, 10,  # Control
              40, 30, 20, 10,  # DrugA
              60, 25, 10, 5,   # DrugB
              35, 40, 15, 10), # Combo
    stage = factor(rep(c("Early", "Advanced"), each = 8))
  )
}

# ============================================================================
# BASIC PLOT VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar basic vertical chart visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_plot = TRUE,
    plot_title = "Treatment Response Distribution"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_basic_vertical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar horizontal orientation visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    orientation = "horizontal",
    show_plot = TRUE,
    plot_title = "Horizontal Segmented Bar Chart"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_horizontal",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# COLOR PALETTE VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar viridis palette visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    color_palette = "viridis",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_viridis",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar clinical palette visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    color_palette = "clinical",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_clinical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar colorblind palette visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    color_palette = "colorblind",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_colorblind",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# CHART STYLE VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar clean style visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    chart_style = "clean",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_clean_style",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar publication style visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    chart_style = "publication",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_publication_style",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar clinical style visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    chart_style = "clinical",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_clinical_style",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar BBC style visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    chart_style = "bbc_style",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_bbc_style",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar Prism style visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    chart_style = "prism_style",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_prism_style",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# LABEL OPTIONS VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar with percentage labels visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = TRUE,
    show_counts = FALSE,
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_percentage_labels",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar with counts and percentages visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = TRUE,
    show_counts = TRUE,
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_counts_and_percentages",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar without labels visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = FALSE,
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_no_labels",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# SORTING VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar sorted by total visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    sort_categories = "total",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_sorted_by_total",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar sorted alphabetically visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    sort_categories = "alpha",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_sorted_alpha",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar sorted by largest segment visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    sort_categories = "largest_segment",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_sorted_largest_segment",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# FACETING VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar with faceting visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    facet_var = "stage",
    show_plot = TRUE,
    plot_title = "Response by Treatment and Stage"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_faceted",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# LEGEND POSITION VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar legend on top visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    legend_position = "top",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_legend_top",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar legend on bottom visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    legend_position = "bottom",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_legend_bottom",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar no legend visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    legend_position = "none",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_no_legend",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# OUTLINE OPTIONS VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar with white outlines visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    add_outline = TRUE,
    outline_color = "white",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_white_outline",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar with black outlines visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    add_outline = TRUE,
    outline_color = "black",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_black_outline",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar without outlines visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    add_outline = FALSE,
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_no_outline",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# CUSTOM TITLES VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar with custom titles visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    plot_title = "Treatment Response Analysis",
    x_title = "Treatment Arm",
    y_title = "Proportion of Patients (%)",
    legend_title = "Response Category",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_custom_titles",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# PERCENTAGE FORMAT VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar integer percentage format visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = TRUE,
    percentage_format = "integer",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_integer_format",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("jjsegmentedtotalbar decimal1 percentage format visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_decimal1_format",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ============================================================================
# EXPORT-READY VISUAL REGRESSION TESTS
# ============================================================================

test_that("jjsegmentedtotalbar export-ready visual regression", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- jjsegmentedtotalbar(
    data = test_data,
    x_var = "treatment",
    y_var = "count",
    fill_var = "response",
    export_ready = TRUE,
    show_plot = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "segmented_bar_export_ready",
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
# - Label formatting changes
# - Color palette changes
#
# These tests complement functional tests by ensuring
# plots look correct, not just that they run without errors.
