# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: raincloud
# ═══════════════════════════════════════════════════════════
#
# Tests all possible argument combinations for the raincloud function
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test data
data(raincloud_test, package = "ClinicoPath", envir = environment())
data(raincloud_clinical, package = "ClinicoPath", envir = environment())
data(raincloud_biomarker, package = "ClinicoPath", envir = environment())
data(raincloud_skewed, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Facet + Color Variable Combinations
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines facet and color variables", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    color_var = "gender"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles facet without color", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity",
    facet_var = "hospital_site"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles color without facet", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    color_var = "disease_severity"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Component Combinations
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines visualization components", {
  devtools::load_all()

  # Violin + Box
  result_vb <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = FALSE
  )
  expect_s3_class(result_vb, "raincloudResults")

  # Violin + Dots
  result_vd <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = FALSE,
    show_dots = TRUE
  )
  expect_s3_class(result_vd, "raincloudResults")

  # Box + Dots
  result_bd <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = FALSE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )
  expect_s3_class(result_bd, "raincloudResults")

  # All three components
  result_all <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )
  expect_s3_class(result_all, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Theme + Palette Combinations
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines themes with palettes", {
  devtools::load_all()

  # Clinical theme + viridis palette
  result1 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    theme = "clinical",
    palette = "viridis"
  )
  expect_s3_class(result1, "raincloudResults")

  # Minimal theme + floral palette
  result2 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    theme = "minimal",
    palette = "floral"
  )
  expect_s3_class(result2, "raincloudResults")

  # Publication theme + colorblind safe
  result3 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "publication",
    palette = "colorblind_safe"
  )
  expect_s3_class(result3, "raincloudResults")

  # Prism theme + candy bright
  result4 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "protein_expression",
    group_var = "receptor_status",
    theme = "prism",
    palette = "candy_bright"
  )
  expect_s3_class(result4, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Statistical Tests + Display Options
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines statistical tests with display options", {
  devtools::load_all()

  # Normality test + mean
  result1 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    normality_test = TRUE,
    show_mean = TRUE
  )
  expect_s3_class(result1, "raincloudResults")

  # Comparison test + median
  result2 <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity",
    comparison_test = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result2, "raincloudResults")

  # Outlier detection + all statistics
  result3 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_outliers = TRUE,
    show_mean = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result3, "raincloudResults")

  # All statistical features
  result4 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    normality_test = TRUE,
    comparison_test = TRUE,
    show_outliers = TRUE,
    show_mean = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result4, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Size and Alpha Customization
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines size and alpha parameters", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    violin_width = 0.8,
    box_width = 0.3,
    dots_size = 2.5,
    violin_alpha = 0.7,
    box_alpha = 0.9,
    dots_alpha = 0.6
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Facet + Components + Statistics
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines faceting with components and statistics", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_mean = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Color + Theme + Components
# ═══════════════════════════════════════════════════════════

test_that("raincloud combines color variable with theme and components", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity",
    color_var = "gender",
    theme = "publication",
    palette = "colorblind_safe",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = FALSE
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Complete Feature Combination - Clinical Lab Analysis
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete clinical lab analysis workflow", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    facet_var = "age_category",
    color_var = "bmi_category",
    theme = "clinical",
    palette = "colorblind_safe",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_outliers = TRUE,
    show_mean = TRUE,
    show_median = TRUE,
    violin_width = 0.7,
    box_width = 0.2,
    dots_size = 2.0
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Complete Feature Combination - Treatment Effect
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete treatment effect analysis", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "diagnosis",
    facet_var = "age_category",
    theme = "publication",
    palette = "viridis",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = FALSE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_mean = TRUE,
    show_median = TRUE,
    violin_alpha = 0.6,
    box_alpha = 0.8
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Complete Feature Combination - Biomarker Expression
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete biomarker expression workflow", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    facet_var = "cancer_type",
    color_var = "receptor_status",
    theme = "prism",
    palette = "floral",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_outliers = TRUE,
    show_mean = TRUE,
    violin_width = 0.8,
    dots_size = 1.5,
    violin_alpha = 0.5
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles minimal configuration", {
  devtools::load_all()

  result_min <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )

  expect_s3_class(result_min, "raincloudResults")
})

test_that("raincloud handles maximum configuration", {
  devtools::load_all()

  result_max <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    color_var = "gender",
    theme = "publication",
    palette = "colorblind_safe",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    violin_width = 0.75,
    box_width = 0.25,
    dots_size = 2.0,
    violin_alpha = 0.7,
    box_alpha = 0.9,
    dots_alpha = 0.5,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_outliers = TRUE,
    show_mean = TRUE,
    show_median = TRUE
  )

  expect_s3_class(result_max, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 12. All Themes with Same Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud applies all themes consistently", {
  devtools::load_all()

  themes <- c("clinical", "minimal", "classic", "publication", "tidyquant",
              "prism", "prism_whitespace", "prism_light")

  for (th in themes) {
    result <- raincloud(
      data = raincloud_test,
      dep_var = "symptom_score",
      group_var = "treatment_group",
      theme = th
    )

    expect_s3_class(result, "raincloudResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 13. All Palettes with Same Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud applies all palettes consistently", {
  devtools::load_all()

  palettes <- c("default", "viridis", "magma", "plasma", "floral",
                "candy_bright", "office", "pastels", "colorblind_safe",
                "ocean", "spring")

  for (pal in palettes) {
    result <- raincloud(
      data = raincloud_biomarker,
      dep_var = "ki67_index",
      group_var = "grade",
      palette = pal
    )

    expect_s3_class(result, "raincloudResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 14. Different Distribution Types with Same Parameters
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles different distribution types with consistent parameters", {
  devtools::load_all()

  distributions <- c("normal", "right_skewed", "left_skewed", "bimodal", "lognormal")

  for (dist in distributions) {
    result <- raincloud(
      data = raincloud_skewed,
      dep_var = dist,
      group_var = "condition",
      theme = "publication",
      show_violin = TRUE,
      show_boxplot = TRUE,
      show_dots = TRUE,
      normality_test = TRUE,
      show_mean = TRUE
    )

    expect_s3_class(result, "raincloudResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 15. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Minimal
  result1 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )
  expect_s3_class(result1, "raincloudResults")

  # Step 2: Add faceting
  result2 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity"
  )
  expect_s3_class(result2, "raincloudResults")

  # Step 3: Add color
  result3 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    color_var = "gender"
  )
  expect_s3_class(result3, "raincloudResults")

  # Step 4: Add theme
  result4 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    color_var = "gender",
    theme = "publication"
  )
  expect_s3_class(result4, "raincloudResults")

  # Step 5: Add statistics
  result5 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    color_var = "gender",
    theme = "publication",
    normality_test = TRUE,
    comparison_test = TRUE
  )
  expect_s3_class(result5, "raincloudResults")

  # Step 6: Add displays
  result6 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    color_var = "gender",
    theme = "publication",
    normality_test = TRUE,
    comparison_test = TRUE,
    show_mean = TRUE,
    show_median = TRUE,
    show_outliers = TRUE
  )
  expect_s3_class(result6, "raincloudResults")
})
