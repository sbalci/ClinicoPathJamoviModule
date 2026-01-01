# Visual Regression Tests for lollipop Function
# Uses vdiffr to create visual baselines that protect against unintended changes
# in plot appearance and verify correct rendering of all customization options

library(testthat)

# ==============================================================================
# Helper Functions
# ==============================================================================

skip_if_not_installed_vdiffr <- function() {
  if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr package not installed")
  }
}

extract_plot <- function(result, plot_name = "plot") {
  if (!plot_name %in% names(result)) {
    stop(paste("Plot", plot_name, "not found in results"))
  }

  plot_obj <- result[[plot_name]]

  # Extract the actual plot from the jamovi Image object
  if (inherits(plot_obj, "Image") && !is.null(plot_obj$state)) {
    return(plot_obj$state)
  }

  return(plot_obj)
}

setup_visual_test_data <- function() {
  set.seed(42)
  data.frame(
    treatment = rep(c("Drug_A", "Drug_B", "Drug_C", "Placebo"), each = 3),
    response_score = c(
      85, 90, 88,  # Drug_A: mean=87.67
      60, 65, 62,  # Drug_B: mean=62.33
      45, 50, 48,  # Drug_C: mean=47.67
      30, 35, 32   # Placebo: mean=32.33
    ),
    patient_count = c(
      12, 15, 13,  # Drug_A
      10, 12, 11,  # Drug_B
      8, 10, 9,    # Drug_C
      6, 8, 7      # Placebo
    )
  )
}

# ==============================================================================
# Basic Lollipop Visualizations
# ==============================================================================

test_that("lollipop basic vertical chart", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_basic_vertical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop basic horizontal chart", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    orientation = "horizontal"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_basic_horizontal",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Sorting Visual Tests
# ==============================================================================

test_that("lollipop sorted ascending", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_asc"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_sorted_ascending",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop sorted descending", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_desc"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_sorted_descending",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop sorted alphabetically", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "group_alpha"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_sorted_alphabetically",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Aggregation Visual Tests
# ==============================================================================

test_that("lollipop with mean aggregation", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_aggregation_mean",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with median aggregation", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "median"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_aggregation_median",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with sum aggregation", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "patient_count",
    group = "treatment",
    aggregation = "sum"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_aggregation_sum",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Highlighting Visual Tests
# ==============================================================================

test_that("lollipop with highlighting", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    useHighlight = TRUE,
    highlight = "Drug_A"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_with_highlighting",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop highlighting in horizontal orientation", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    orientation = "horizontal",
    useHighlight = TRUE,
    highlight = "Placebo"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_highlighting_horizontal",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Conditional Coloring Visual Tests
# ==============================================================================

test_that("lollipop with conditional coloring", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    conditionalColor = TRUE,
    colorThreshold = 60
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_conditional_coloring",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop conditional coloring with highlighting", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    useHighlight = TRUE,
    highlight = "Drug_A",
    conditionalColor = TRUE,
    colorThreshold = 70
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_conditional_with_highlight",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Color Scheme Visual Tests
# ==============================================================================

test_that("lollipop default color scheme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    colorScheme = "default"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_color_default",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop clinical color scheme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    colorScheme = "clinical"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_color_clinical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop viridis color scheme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    colorScheme = "viridis"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_color_viridis",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop colorblind safe scheme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    colorScheme = "colorblind"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_color_colorblind",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Theme Visual Tests
# ==============================================================================

test_that("lollipop minimal theme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    theme = "minimal"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_theme_minimal",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop classic theme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    theme = "classic"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_theme_classic",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop publication theme", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    theme = "publication"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_theme_publication",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Customization Visual Tests
# ==============================================================================

test_that("lollipop with value labels", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    showValues = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_with_value_labels",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with mean reference line", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    showMean = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_with_mean_line",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with values and mean line", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    showValues = TRUE,
    showMean = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_values_and_mean",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with custom baseline", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    baseline = 50
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_custom_baseline",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with large points", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    pointSize = 6
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_large_points",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with thick lines", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    lineWidth = 3
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_thick_lines",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with dashed lines", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    lineType = "dashed"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_dashed_lines",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with dotted lines", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    lineType = "dotted"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_dotted_lines",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with custom labels and title", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    xlabel = "Treatment Groups",
    ylabel = "Response Score (%)",
    title = "Treatment Efficacy Comparison"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_custom_labels",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Complex Combinations Visual Tests
# ==============================================================================

test_that("lollipop clinical scenario - horizontal with highlighting and threshold", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    orientation = "horizontal",
    sortBy = "value_desc",
    useHighlight = TRUE,
    highlight = "Drug_A",
    conditionalColor = TRUE,
    colorThreshold = 60,
    showValues = TRUE,
    showMean = TRUE,
    colorScheme = "clinical",
    theme = "publication",
    title = "Treatment Response Analysis"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_clinical_scenario",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop complete customization", {
  skip_if_not_installed_vdiffr()

  test_data <- setup_visual_test_data()

  result <- lollipop(
    data = test_data,
    dep = "response_score",
    group = "treatment",
    aggregation = "mean",
    sortBy = "value_asc",
    conditionalColor = TRUE,
    colorThreshold = 70,
    showValues = TRUE,
    showMean = TRUE,
    colorScheme = "colorblind",
    theme = "minimal",
    pointSize = 5,
    lineWidth = 2,
    lineType = "dashed",
    baseline = 0,
    xlabel = "Treatment",
    ylabel = "Efficacy Score",
    title = "Comprehensive Lollipop Customization"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_complete_customization",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Edge Case Visual Tests
# ==============================================================================

test_that("lollipop with many categories", {
  skip_if_not_installed_vdiffr()

  set.seed(123)
  many_cat_data <- data.frame(
    category = paste0("Cat_", sprintf("%02d", 1:12)),
    value = rnorm(12, mean = 50, sd = 15)
  )

  result <- lollipop(
    data = many_cat_data,
    dep = "value",
    group = "category",
    sortBy = "value_desc"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_many_categories",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with negative values", {
  skip_if_not_installed_vdiffr()

  negative_data <- data.frame(
    category = c("A", "B", "C", "D", "E"),
    value = c(-10, -5, 0, 5, 10)
  )

  result <- lollipop(
    data = negative_data,
    dep = "value",
    group = "category"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_negative_values",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("lollipop with very small values", {
  skip_if_not_installed_vdiffr()

  small_data <- data.frame(
    category = c("A", "B", "C", "D"),
    value = c(0.001, 0.005, 0.01, 0.02)
  )

  result <- lollipop(
    data = small_data,
    dep = "value",
    group = "category",
    showValues = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "lollipop_small_values",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

print("All lollipop visual regression tests completed!")
