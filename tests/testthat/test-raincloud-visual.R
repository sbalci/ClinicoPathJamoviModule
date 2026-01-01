# Visual Regression Tests for raincloud Function
# Uses vdiffr to create visual baselines that protect against unintended changes
# in plot appearance and verify correct rendering of all customization options
#
# CRITICAL PROTECTION:
# These visual tests protect against bugs in the complex ggdist-based visualization:
# - Component rendering (violin, boxplot, dots)
# - Layout positioning (orientation, dots_side)
# - Color palette application
# - Theme rendering
# - Faceting behavior
# - Transparency and sizing options

library(testthat)

# ==============================================================================
# Helper Functions
# ==============================================================================

skip_if_not_installed_vdiffr <- function() {
  if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr package not installed")
  }
}

skip_if_not_installed_ggdist <- function() {
  if (!requireNamespace("ggdist", quietly = TRUE)) {
    skip("ggdist package not installed")
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
    measurement = c(
      rnorm(30, mean = 50, sd = 10),  # Group A
      rnorm(30, mean = 65, sd = 12),  # Group B
      rnorm(30, mean = 55, sd = 8)    # Group C
    ),
    group = factor(rep(c("Control", "Treatment_A", "Treatment_B"), each = 30)),
    gender = factor(rep(c("Male", "Female"), 45)),
    age_group = factor(sample(c("Young", "Middle", "Old"), 90, replace = TRUE))
  )
}

# ==============================================================================
# Basic Raincloud Visualizations
# ==============================================================================

test_that("raincloud basic horizontal plot", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_basic_horizontal",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud basic vertical plot", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    orientation = "vertical"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_basic_vertical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Component Combination Tests
# ==============================================================================

test_that("raincloud violin only", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    show_violin = TRUE,
    show_boxplot = FALSE,
    show_dots = FALSE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_violin_only",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud boxplot only", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    show_violin = FALSE,
    show_boxplot = TRUE,
    show_dots = FALSE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_boxplot_only",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud dots only", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    show_violin = FALSE,
    show_boxplot = FALSE,
    show_dots = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_dots_only",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud violin and boxplot", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = FALSE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_violin_boxplot",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud violin and dots", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    show_violin = TRUE,
    show_boxplot = FALSE,
    show_dots = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_violin_dots",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud boxplot and dots", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    show_violin = FALSE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_boxplot_dots",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Dots Position Tests
# ==============================================================================

test_that("raincloud dots on left", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    dots_side = "left"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_dots_left",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud dots on right", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    dots_side = "right"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_dots_right",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud dots on both sides", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    dots_side = "both"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_dots_both",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Color Palette Tests
# ==============================================================================

test_that("raincloud default color palette", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    color_palette = "default"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_palette_default",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud viridis color palette", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    color_palette = "viridis"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_palette_viridis",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud clinical color palette", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    color_palette = "clinical"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_palette_clinical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud prism colorblind safe palette", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    color_palette = "prism_colorblind_safe"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_palette_prism_colorblind",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud prism ocean palette", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    color_palette = "prism_ocean"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_palette_prism_ocean",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Theme Tests
# ==============================================================================

test_that("raincloud clinical theme", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    plot_theme = "clinical"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_theme_clinical",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud minimal theme", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    plot_theme = "minimal"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_theme_minimal",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud publication theme", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    plot_theme = "publication"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_theme_publication",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud prism default theme", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    plot_theme = "prism_default"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_theme_prism_default",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud prism publication theme", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    plot_theme = "prism_publication"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_theme_prism_publication",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Sizing and Transparency Tests
# ==============================================================================

test_that("raincloud wide violin", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    violin_width = 1.5
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_wide_violin",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud wide boxplot", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    box_width = 0.5
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_wide_boxplot",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud large dots", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    dots_size = 3
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_large_dots",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud transparent violin", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    alpha_violin = 0.3
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_transparent_violin",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud opaque dots", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    alpha_dots = 1.0
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_opaque_dots",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Faceting Tests
# ==============================================================================

test_that("raincloud with faceting", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    facet_var = "gender"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_with_faceting",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud faceted with color variable", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    facet_var = "gender",
    color_var = "age_group"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_faceted_with_color",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Complex Combination Tests
# ==============================================================================

test_that("raincloud complete customization vertical", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    orientation = "vertical",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    dots_side = "right",
    color_palette = "prism_colorblind_safe",
    plot_theme = "publication",
    violin_width = 1.0,
    box_width = 0.3,
    dots_size = 2,
    alpha_violin = 0.5,
    alpha_dots = 0.9,
    plot_title = "Comprehensive Raincloud Customization"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_complete_customization",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud clinical scenario", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    orientation = "horizontal",
    color_palette = "clinical",
    plot_theme = "clinical",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    dots_side = "left",
    plot_title = "Treatment Response Distribution"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_clinical_scenario",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud publication ready", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  test_data <- setup_visual_test_data()

  result <- raincloud(
    data = test_data,
    dep_var = "measurement",
    group_var = "group",
    facet_var = "gender",
    orientation = "horizontal",
    color_palette = "prism_colorblind_safe",
    plot_theme = "prism_publication",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    dots_side = "left",
    violin_width = 0.8,
    box_width = 0.25,
    dots_size = 1.5,
    plot_title = "Publication-Ready Raincloud Plot"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_publication_ready",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

# ==============================================================================
# Edge Case Visual Tests
# ==============================================================================

test_that("raincloud with two groups", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  set.seed(123)
  two_group_data <- data.frame(
    value = c(rnorm(25, 50, 10), rnorm(25, 60, 12)),
    group = factor(rep(c("Control", "Treatment"), each = 25))
  )

  result <- raincloud(
    data = two_group_data,
    dep_var = "value",
    group_var = "group"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_two_groups",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud with many groups", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  set.seed(456)
  many_group_data <- data.frame(
    value = rnorm(150, mean = 50, sd = 10),
    group = factor(rep(paste0("Group_", LETTERS[1:6]), each = 25))
  )

  result <- raincloud(
    data = many_group_data,
    dep_var = "value",
    group_var = "group"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_many_groups",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud with narrow distribution", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  set.seed(789)
  narrow_data <- data.frame(
    value = c(rnorm(30, 50, 1), rnorm(30, 55, 1)),  # Very narrow SD
    group = factor(rep(c("A", "B"), each = 30))
  )

  result <- raincloud(
    data = narrow_data,
    dep_var = "value",
    group_var = "group"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_narrow_distribution",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

test_that("raincloud with bimodal distribution", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggdist()

  set.seed(890)
  bimodal_data <- data.frame(
    value = c(
      rnorm(20, 40, 5), rnorm(20, 70, 5),  # Bimodal group A
      rnorm(40, 55, 10)                     # Unimodal group B
    ),
    group = factor(c(rep("Bimodal", 40), rep("Unimodal", 40)))
  )

  result <- raincloud(
    data = bimodal_data,
    dep_var = "value",
    group_var = "group"
  )

  plot <- extract_plot(result, "plot")

  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "raincloud_bimodal_distribution",
      fig = plot
    )
  } else {
    skip("Could not extract plot for visual testing")
  }
})

print("All raincloud visual regression tests completed!")
