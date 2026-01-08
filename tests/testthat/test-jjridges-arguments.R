# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjridges
# ═══════════════════════════════════════════════════════════
#
# Tests all possible argument combinations for the jjridges function
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test data
data(jjridges_test, package = "ClinicoPath", envir = environment())
data(jjridges_clinical, package = "ClinicoPath", envir = environment())
data(jjridges_biomarker, package = "ClinicoPath", envir = environment())
data(jjridges_treatment, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Plot Type + Scale Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines plot types with scale values", {
  devtools::load_all()

  plot_types <- c("ridgeline", "density_ridges", "density_ridges_gradient", "histogram_ridges", "violin_ridges")
  scales <- c(0.5, 1.0, 2.0)

  for (type in plot_types) {
    for (sc in scales) {
      result <- jjridges(
        data = jjridges_test,
        x_var = "ki67_index",
        y_var = "tumor_stage",
        plot_type = type,
        scale = sc
      )

      expect_s3_class(result, "jjridgesResults")
    }
  }
})

# ═══════════════════════════════════════════════════════════
# 2. Advanced Features Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines boxplot with quantiles", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    add_boxplot = TRUE,
    add_quantiles = TRUE,
    quantiles = "0.25, 0.75"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines mean and median lines", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    add_mean = TRUE,
    add_median = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines points with boxplot", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "protein_expression",
    y_var = "receptor_status",
    add_boxplot = TRUE,
    add_points = TRUE,
    point_alpha = 0.3
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines all advanced features", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    add_boxplot = TRUE,
    add_quantiles = TRUE,
    quantiles = "0.25, 0.5, 0.75",
    add_mean = TRUE,
    add_median = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Statistics + Effect Size Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines parametric test with Cohen's d", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    show_stats = TRUE,
    test_type = "parametric",
    effsize_type = "d"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines nonparametric test with Cliff's Delta", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "lymph_nodes",
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "nonparametric",
    effsize_type = "cliff_delta"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines robust test with Hedge's g", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    show_stats = TRUE,
    test_type = "robust",
    effsize_type = "g"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles p-value adjustments with statistics", {
  devtools::load_all()

  # Bonferroni
  result_bonf <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "parametric",
    p_adjust_method = "bonferroni"
  )
  expect_s3_class(result_bonf, "jjridgesResults")

  # FDR
  result_fdr <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "parametric",
    p_adjust_method = "fdr"
  )
  expect_s3_class(result_fdr, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Fill + Facet Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines fill and facet variables", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_grade",
    fill_var = "receptor_status",
    facet_var = "treatment_arm"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles fill with legend options", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "tumor_stage",
    fill_var = "receptor_status",
    show_fill_legend = TRUE,
    legend_position = "right"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Theme + Palette Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines themes with palettes", {
  devtools::load_all()

  themes <- c("theme_ridges", "theme_minimal", "theme_classic", "theme_pubr")
  palettes <- c("clinical_colorblind", "viridis", "Set1")

  for (theme in themes) {
    for (palette in palettes) {
      result <- jjridges(
        data = jjridges_test,
        x_var = "ki67_index",
        y_var = "tumor_stage",
        theme_style = theme,
        color_palette = palette
      )

      expect_s3_class(result, "jjridgesResults")
    }
  }
})

# ═══════════════════════════════════════════════════════════
# 6. Plot Type + Advanced Features
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines density gradient with statistical comparisons", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    plot_type = "density_ridges_gradient",
    gradient_low = "#0000FF",
    gradient_high = "#FF0000",
    show_stats = TRUE,
    test_type = "nonparametric"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines histogram with quantiles", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "age",
    y_var = "tumor_grade",
    plot_type = "histogram_ridges",
    binwidth = 5,
    add_quantiles = TRUE,
    quantiles = "0.5"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges combines violin with boxplot", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    plot_type = "violin_ridges",
    add_boxplot = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Annotations + Sample Size
# ═══════════════════════════════════════════════════════════

test_that("jjridges combines sample size with statistics", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    add_sample_size = TRUE,
    show_stats = TRUE,
    test_type = "parametric"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Complete Feature Combination - Clinical Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles complete clinical laboratory analysis", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    facet_var = "bmi_category",
    plot_type = "density_ridges",
    scale = 1.5,
    theme_style = "theme_pubr",
    color_palette = "clinical_colorblind",
    add_boxplot = TRUE,
    add_quantiles = TRUE,
    quantiles = "0.25, 0.5, 0.75",
    show_stats = TRUE,
    test_type = "nonparametric",
    effsize_type = "cliff_delta",
    p_adjust_method = "fdr",
    add_sample_size = TRUE,
    alpha = 0.7
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Complete Feature Combination - Biomarker Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles complete biomarker analysis", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_biomarker,
    x_var = "mutation_burden",
    y_var = "cancer_type",
    fill_var = "stage",
    plot_type = "density_ridges_gradient",
    scale = 1.2,
    theme_style = "theme_minimal",
    color_palette = "viridis",
    gradient_low = "#440154",
    gradient_high = "#FDE725",
    add_median = TRUE,
    show_stats = TRUE,
    test_type = "nonparametric",
    effsize_type = "hodges_lehmann",
    add_sample_size = TRUE,
    legend_position = "bottom"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Complete Feature Combination - Treatment Response
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles complete treatment response analysis", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint",
    facet_var = "treatment_group",
    plot_type = "violin_ridges",
    scale = 1.0,
    theme_style = "theme_ridges",
    color_palette = "Set2",
    add_boxplot = TRUE,
    add_mean = TRUE,
    add_median = TRUE,
    show_stats = TRUE,
    test_type = "parametric",
    effsize_type = "g",
    p_adjust_method = "bonferroni",
    add_sample_size = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles minimal configuration", {
  devtools::load_all()

  result_min <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )

  expect_s3_class(result_min, "jjridgesResults")
})

test_that("jjridges handles maximum configuration", {
  devtools::load_all()

  result_max <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    fill_var = "receptor_status",
    facet_var = "treatment_arm",
    plot_type = "density_ridges_gradient",
    scale = 1.5,
    bandwidth = "SJ",
    add_boxplot = TRUE,
    add_points = FALSE,
    point_alpha = 0.3,
    add_quantiles = TRUE,
    quantiles = "0.25, 0.5, 0.75",
    add_mean = TRUE,
    add_median = TRUE,
    show_stats = TRUE,
    test_type = "parametric",
    p_adjust_method = "fdr",
    effsize_type = "d",
    alpha = 0.8,
    color_palette = "clinical_colorblind",
    gradient_low = "#0000FF",
    gradient_high = "#FF0000",
    fill_ridges = TRUE,
    reverse_order = FALSE,
    show_fill_legend = TRUE,
    theme_style = "theme_pubr",
    grid_lines = FALSE,
    legend_position = "right",
    add_sample_size = TRUE
  )

  expect_s3_class(result_max, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 12. All Plot Types with Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges produces consistent results across plot types", {
  devtools::load_all()

  plot_types <- c("ridgeline", "density_ridges", "density_ridges_gradient", "histogram_ridges", "violin_ridges")

  for (type in plot_types) {
    result <- jjridges(
      data = jjridges_test,
      x_var = "tumor_size",
      y_var = "tumor_stage",
      plot_type = type,
      theme_style = "theme_ridges"
    )

    expect_s3_class(result, "jjridgesResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 13. All Statistical Tests with Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges produces consistent results across statistical tests", {
  devtools::load_all()

  test_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (test in test_types) {
    result <- jjridges(
      data = jjridges_test,
      x_var = "response_score",
      y_var = "treatment_arm",
      show_stats = TRUE,
      test_type = test
    )

    expect_s3_class(result, "jjridgesResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 14. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Minimal
  result1 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )
  expect_s3_class(result1, "jjridgesResults")

  # Step 2: Add plot type
  result2 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges"
  )
  expect_s3_class(result2, "jjridgesResults")

  # Step 3: Add boxplot
  result3 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges",
    add_boxplot = TRUE
  )
  expect_s3_class(result3, "jjridgesResults")

  # Step 4: Add statistics
  result4 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges",
    add_boxplot = TRUE,
    show_stats = TRUE,
    test_type = "parametric"
  )
  expect_s3_class(result4, "jjridgesResults")

  # Step 5: Add faceting
  result5 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    facet_var = "receptor_status",
    plot_type = "density_ridges",
    add_boxplot = TRUE,
    show_stats = TRUE,
    test_type = "parametric"
  )
  expect_s3_class(result5, "jjridgesResults")

  # Step 6: Add theme
  result6 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    facet_var = "receptor_status",
    plot_type = "density_ridges",
    add_boxplot = TRUE,
    show_stats = TRUE,
    test_type = "parametric",
    theme_style = "theme_pubr",
    color_palette = "clinical_colorblind"
  )
  expect_s3_class(result6, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 15. Clinical Presets with Different Datasets
# ═══════════════════════════════════════════════════════════

test_that("jjridges applies clinical presets appropriately", {
  devtools::load_all()

  # Biomarker distribution preset
  result_biomarker <- jjridges(
    data = jjridges_biomarker,
    x_var = "mutation_burden",
    y_var = "cancer_type",
    clinicalPreset = "biomarker_distribution"
  )
  expect_s3_class(result_biomarker, "jjridgesResults")

  # Treatment response preset
  result_treatment <- jjridges(
    data = jjridges_treatment,
    x_var = "response_score",
    y_var = "treatment_group",
    clinicalPreset = "treatment_response"
  )
  expect_s3_class(result_treatment, "jjridgesResults")

  # Lab values preset
  result_lab <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    clinicalPreset = "lab_values_by_group"
  )
  expect_s3_class(result_lab, "jjridgesResults")
})
