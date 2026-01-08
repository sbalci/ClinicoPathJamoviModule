# ═══════════════════════════════════════════════════════════
# Integration Tests: jjridges
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and integration with all datasets
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(jjridges_test, package = "ClinicoPath", envir = environment())
data(jjridges_clinical, package = "ClinicoPath", envir = environment())
data(jjridges_treatment, package = "ClinicoPath", envir = environment())
data(jjridges_biomarker, package = "ClinicoPath", envir = environment())
data(jjridges_survival, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Complete Biomarker Analysis Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles complete biomarker analysis workflow", {
  devtools::load_all()

  # Step 1: Basic distribution
  result1 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )
  expect_s3_class(result1, "jjridgesResults")

  # Step 2: Add statistical comparison
  result2 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "nonparametric"
  )
  expect_s3_class(result2, "jjridgesResults")

  # Step 3: Add boxplot overlay
  result3 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "nonparametric",
    add_boxplot = TRUE
  )
  expect_s3_class(result3, "jjridgesResults")

  # Step 4: Stratify by receptor status
  result4 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    facet_var = "receptor_status",
    show_stats = TRUE,
    test_type = "nonparametric",
    add_boxplot = TRUE
  )
  expect_s3_class(result4, "jjridgesResults")

  # Step 5: Publication-ready
  result5 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    facet_var = "receptor_status",
    plot_type = "density_ridges",
    theme_style = "theme_pubr",
    color_palette = "clinical_colorblind",
    add_boxplot = TRUE,
    add_median = TRUE,
    show_stats = TRUE,
    test_type = "nonparametric",
    effsize_type = "cliff_delta",
    p_adjust_method = "fdr",
    add_sample_size = TRUE
  )
  expect_s3_class(result5, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Complete Clinical Laboratory Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles complete clinical lab analysis workflow", {
  devtools::load_all()

  # Step 1: Glucose distribution
  result1 <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis"
  )
  expect_s3_class(result1, "jjridgesResults")

  # Step 2: Add quantiles
  result2 <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    add_quantiles = TRUE,
    quantiles = "0.25, 0.5, 0.75"
  )
  expect_s3_class(result2, "jjridgesResults")

  # Step 3: Add statistical tests
  result3 <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    add_quantiles = TRUE,
    quantiles = "0.25, 0.5, 0.75",
    show_stats = TRUE,
    test_type = "parametric"
  )
  expect_s3_class(result3, "jjridgesResults")

  # Step 4: Stratify by BMI
  result4 <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    facet_var = "bmi_category",
    add_quantiles = TRUE,
    show_stats = TRUE,
    test_type = "parametric"
  )
  expect_s3_class(result4, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Complete Treatment Response Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles complete treatment response workflow", {
  devtools::load_all()

  # Step 1: Pain scores by timepoint
  result1 <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint"
  )
  expect_s3_class(result1, "jjridgesResults")

  # Step 2: Add mean/median markers
  result2 <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint",
    add_mean = TRUE,
    add_median = TRUE
  )
  expect_s3_class(result2, "jjridgesResults")

  # Step 3: Stratify by treatment
  result3 <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint",
    facet_var = "treatment_group",
    add_mean = TRUE
  )
  expect_s3_class(result3, "jjridgesResults")

  # Step 4: Add statistical comparisons
  result4 <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint",
    facet_var = "treatment_group",
    add_mean = TRUE,
    show_stats = TRUE,
    test_type = "parametric",
    p_adjust_method = "bonferroni"
  )
  expect_s3_class(result4, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 4. All Datasets with Consistent Parameters
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles all datasets with consistent parameters", {
  devtools::load_all()

  # Test dataset
  result1 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges",
    theme_style = "theme_ridges"
  )
  expect_s3_class(result1, "jjridgesResults")

  # Clinical dataset
  result2 <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    plot_type = "density_ridges",
    theme_style = "theme_ridges"
  )
  expect_s3_class(result2, "jjridgesResults")

  # Treatment dataset
  result3 <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint",
    plot_type = "density_ridges",
    theme_style = "theme_ridges"
  )
  expect_s3_class(result3, "jjridgesResults")

  # Biomarker dataset
  result4 <- jjridges(
    data = jjridges_biomarker,
    x_var = "mutation_burden",
    y_var = "cancer_type",
    plot_type = "density_ridges",
    theme_style = "theme_ridges"
  )
  expect_s3_class(result4, "jjridgesResults")

  # Survival dataset
  result5 <- jjridges(
    data = jjridges_survival,
    x_var = "survival_months",
    y_var = "disease_stage",
    plot_type = "density_ridges",
    theme_style = "theme_ridges"
  )
  expect_s3_class(result5, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 5. All Plot Types on Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges produces consistent results across plot types", {
  devtools::load_all()

  plot_types <- c("ridgeline", "density_ridges", "density_ridges_gradient", "histogram_ridges", "violin_ridges")

  results <- list()

  for (type in plot_types) {
    results[[type]] <- jjridges(
      data = jjridges_test,
      x_var = "tumor_size",
      y_var = "tumor_stage",
      plot_type = type
    )

    expect_s3_class(results[[type]], "jjridgesResults")
  }

  expect_equal(length(results), length(plot_types))
})

# ═══════════════════════════════════════════════════════════
# 6. All Themes on Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges produces consistent results across themes", {
  devtools::load_all()

  themes <- c("theme_ridges", "theme_minimal", "theme_classic", "theme_pubr")

  results <- list()

  for (theme in themes) {
    results[[theme]] <- jjridges(
      data = jjridges_test,
      x_var = "ki67_index",
      y_var = "tumor_stage",
      theme_style = theme
    )

    expect_s3_class(results[[theme]], "jjridgesResults")
  }

  expect_equal(length(results), length(themes))
})

# ═══════════════════════════════════════════════════════════
# 7. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles progressive feature addition", {
  devtools::load_all()

  # Minimal
  result1 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )
  expect_s3_class(result1, "jjridgesResults")

  # Add plot type
  result2 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges"
  )
  expect_s3_class(result2, "jjridgesResults")

  # Add boxplot
  result3 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges",
    add_boxplot = TRUE
  )
  expect_s3_class(result3, "jjridgesResults")

  # Add statistics
  result4 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges",
    add_boxplot = TRUE,
    show_stats = TRUE
  )
  expect_s3_class(result4, "jjridgesResults")

  # Add faceting
  result5 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    facet_var = "receptor_status",
    plot_type = "density_ridges",
    add_boxplot = TRUE,
    show_stats = TRUE
  )
  expect_s3_class(result5, "jjridgesResults")

  # Add theme
  result6 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    facet_var = "receptor_status",
    plot_type = "density_ridges",
    add_boxplot = TRUE,
    show_stats = TRUE,
    theme_style = "theme_pubr",
    color_palette = "clinical_colorblind"
  )
  expect_s3_class(result6, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Repeated Analysis Consistency
# ═══════════════════════════════════════════════════════════

test_that("jjridges produces consistent results on repeated calls", {
  devtools::load_all()

  result1 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges"
  )

  result2 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges"
  )

  result3 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges"
  )

  expect_s3_class(result1, "jjridgesResults")
  expect_s3_class(result2, "jjridgesResults")
  expect_s3_class(result3, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Clinical Presets Integration
# ═══════════════════════════════════════════════════════════

test_that("jjridges integrates clinical presets with appropriate data", {
  devtools::load_all()

  # Biomarker preset
  result_bio <- jjridges(
    data = jjridges_biomarker,
    x_var = "mutation_burden",
    y_var = "cancer_type",
    clinicalPreset = "biomarker_distribution"
  )
  expect_s3_class(result_bio, "jjridgesResults")

  # Treatment response preset
  result_treat <- jjridges(
    data = jjridges_treatment,
    x_var = "response_score",
    y_var = "treatment_group",
    clinicalPreset = "treatment_response"
  )
  expect_s3_class(result_treat, "jjridgesResults")

  # Lab values preset
  result_lab <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    clinicalPreset = "lab_values_by_group"
  )
  expect_s3_class(result_lab, "jjridgesResults")

  # Survival preset
  result_surv <- jjridges(
    data = jjridges_survival,
    x_var = "survival_months",
    y_var = "disease_stage",
    clinicalPreset = "survival_time_distribution"
  )
  expect_s3_class(result_surv, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Multiple Variables from Same Dataset
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles multiple analyses from same dataset", {
  devtools::load_all()

  variables <- c("ki67_index", "tumor_size", "protein_expression", "age")

  results <- list()

  for (var in variables) {
    results[[var]] <- jjridges(
      data = jjridges_test,
      x_var = var,
      y_var = "tumor_stage",
      plot_type = "density_ridges"
    )
    expect_s3_class(results[[var]], "jjridgesResults")
  }

  expect_equal(length(results), length(variables))
})
