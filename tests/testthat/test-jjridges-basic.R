# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjridges
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of the jjridges function
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jjridges_test, package = "ClinicoPath", envir = environment())
data(jjridges_clinical, package = "ClinicoPath", envir = environment())
data(jjridges_treatment, package = "ClinicoPath", envir = environment())
data(jjridges_biomarker, package = "ClinicoPath", envir = environment())
data(jjridges_survival, package = "ClinicoPath", envir = environment())
data(jjridges_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Function Existence and Basic Execution
# ═══════════════════════════════════════════════════════════

test_that("jjridges function exists and runs with minimal arguments", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Different Continuous Variables
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles different continuous x variables", {
  devtools::load_all()

  # Ki67 proliferation index
  result_ki67 <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )
  expect_s3_class(result_ki67, "jjridgesResults")

  # Tumor size
  result_size <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage"
  )
  expect_s3_class(result_size, "jjridgesResults")

  # Protein expression
  result_protein <- jjridges(
    data = jjridges_test,
    x_var = "protein_expression",
    y_var = "receptor_status"
  )
  expect_s3_class(result_protein, "jjridgesResults")

  # Age
  result_age <- jjridges(
    data = jjridges_test,
    x_var = "age",
    y_var = "treatment_arm"
  )
  expect_s3_class(result_age, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Different Grouping Variables
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles different y grouping variables", {
  devtools::load_all()

  # 4-level grouping (tumor stage)
  result_4level <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage"
  )
  expect_s3_class(result_4level, "jjridgesResults")

  # 3-level grouping (treatment)
  result_3level <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm"
  )
  expect_s3_class(result_3level, "jjridgesResults")

  # Binary grouping (receptor status)
  result_binary <- jjridges(
    data = jjridges_test,
    x_var = "protein_expression",
    y_var = "receptor_status"
  )
  expect_s3_class(result_binary, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Plot Types
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles basic ridgeline plot type", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "ridgeline"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles density ridges plot type", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    plot_type = "density_ridges"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles density ridges with gradient", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    plot_type = "density_ridges_gradient"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles histogram ridges", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "age",
    y_var = "tumor_grade",
    plot_type = "histogram_ridges"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles violin ridges", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    plot_type = "violin_ridges"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Ridge Height Scaling
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles different scale values", {
  devtools::load_all()

  # No overlap (scale < 1)
  result_separate <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    scale = 0.5
  )
  expect_s3_class(result_separate, "jjridgesResults")

  # Standard overlap (scale = 1)
  result_standard <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    scale = 1.0
  )
  expect_s3_class(result_standard, "jjridgesResults")

  # More overlap (scale > 1)
  result_overlap <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    scale = 2.0
  )
  expect_s3_class(result_overlap, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Advanced Features
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles boxplot inside ridges", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    add_boxplot = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles data points overlay", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_small,  # Use small dataset for points
    x_var = "measurement",
    y_var = "group",
    add_points = TRUE,
    point_alpha = 0.5
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles quantile lines", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    add_quantiles = TRUE,
    quantiles = "0.25, 0.5, 0.75"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles mean line", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_grade",
    add_mean = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles median line", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    add_median = TRUE
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Statistical Comparisons
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles parametric statistics", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "parametric"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles nonparametric statistics", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "lymph_nodes",  # Right-skewed variable
    y_var = "tumor_stage",
    show_stats = TRUE,
    test_type = "nonparametric"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles robust statistics", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_clinical,
    x_var = "triglycerides",
    y_var = "bmi_category",
    show_stats = TRUE,
    test_type = "robust"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles Bayesian statistics", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    show_stats = TRUE,
    test_type = "bayes"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Themes
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles ridge theme", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    theme_style = "theme_ridges"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles minimal theme", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_grade",
    theme_style = "theme_minimal"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles classic theme", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis",
    theme_style = "theme_classic"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles publication theme", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    theme_style = "theme_pubr"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Color Palettes
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles clinical colorblind-safe palette", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    color_palette = "clinical_colorblind"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles viridis family palettes", {
  devtools::load_all()

  # Viridis
  result_viridis <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    color_palette = "viridis"
  )
  expect_s3_class(result_viridis, "jjridgesResults")

  # Plasma
  result_plasma <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    color_palette = "plasma"
  )
  expect_s3_class(result_plasma, "jjridgesResults")

  # Inferno
  result_inferno <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    color_palette = "inferno"
  )
  expect_s3_class(result_inferno, "jjridgesResults")

  # Magma
  result_magma <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    color_palette = "magma"
  )
  expect_s3_class(result_magma, "jjridgesResults")
})

test_that("jjridges handles ColorBrewer palettes", {
  devtools::load_all()

  # Set1
  result_set1 <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    color_palette = "Set1"
  )
  expect_s3_class(result_set1, "jjridgesResults")

  # Set2
  result_set2 <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    color_palette = "Set2"
  )
  expect_s3_class(result_set2, "jjridgesResults")

  # Dark2
  result_dark2 <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    color_palette = "Dark2"
  )
  expect_s3_class(result_dark2, "jjridgesResults")

  # Paired
  result_paired <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    color_palette = "Paired"
  )
  expect_s3_class(result_paired, "jjridgesResults")
})

test_that("jjridges handles custom colors", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "treatment_arm",
    color_palette = "custom",
    custom_colors = "#FF5733,#33FF57,#3357FF"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Fill and Facet Variables
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles fill variable", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "ki67_index",
    y_var = "tumor_stage",
    fill_var = "receptor_status"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles facet variable", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_grade",
    facet_var = "receptor_status"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles both fill and facet variables", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "response_score",
    y_var = "tumor_stage",
    fill_var = "receptor_status",
    facet_var = "treatment_arm"
  )

  expect_s3_class(result, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 11. All Test Datasets
# ═══════════════════════════════════════════════════════════

test_that("jjridges works with all test datasets", {
  devtools::load_all()

  # jjridges_test
  result1 <- jjridges(data = jjridges_test, x_var = "ki67_index", y_var = "tumor_stage")
  expect_s3_class(result1, "jjridgesResults")

  # jjridges_clinical
  result2 <- jjridges(data = jjridges_clinical, x_var = "glucose", y_var = "diagnosis")
  expect_s3_class(result2, "jjridgesResults")

  # jjridges_treatment
  result3 <- jjridges(data = jjridges_treatment, x_var = "pain_score", y_var = "timepoint")
  expect_s3_class(result3, "jjridgesResults")

  # jjridges_biomarker
  result4 <- jjridges(data = jjridges_biomarker, x_var = "mutation_burden", y_var = "cancer_type")
  expect_s3_class(result4, "jjridgesResults")

  # jjridges_survival
  result5 <- jjridges(data = jjridges_survival, x_var = "survival_months", y_var = "disease_stage")
  expect_s3_class(result5, "jjridgesResults")

  # jjridges_small
  result6 <- jjridges(data = jjridges_small, x_var = "measurement", y_var = "group")
  expect_s3_class(result6, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Clinical Laboratory Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles clinical laboratory measurements", {
  devtools::load_all()

  # Glucose levels by diagnosis
  result_glucose <- jjridges(
    data = jjridges_clinical,
    x_var = "glucose",
    y_var = "diagnosis"
  )
  expect_s3_class(result_glucose, "jjridgesResults")

  # HbA1c by diagnosis
  result_hba1c <- jjridges(
    data = jjridges_clinical,
    x_var = "hemoglobin_a1c",
    y_var = "diagnosis"
  )
  expect_s3_class(result_hba1c, "jjridgesResults")

  # Triglycerides by BMI
  result_trig <- jjridges(
    data = jjridges_clinical,
    x_var = "triglycerides",
    y_var = "bmi_category"
  )
  expect_s3_class(result_trig, "jjridgesResults")

  # Systolic BP by age group
  result_bp <- jjridges(
    data = jjridges_clinical,
    x_var = "systolic_bp",
    y_var = "age_group"
  )
  expect_s3_class(result_bp, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Treatment Response Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles treatment response over time", {
  devtools::load_all()

  # Pain score by timepoint
  result_pain <- jjridges(
    data = jjridges_treatment,
    x_var = "pain_score",
    y_var = "timepoint"
  )
  expect_s3_class(result_pain, "jjridgesResults")

  # Fatigue by treatment group
  result_fatigue <- jjridges(
    data = jjridges_treatment,
    x_var = "fatigue_score",
    y_var = "treatment_group"
  )
  expect_s3_class(result_fatigue, "jjridgesResults")

  # QoL by timepoint
  result_qol <- jjridges(
    data = jjridges_treatment,
    x_var = "qol_score",
    y_var = "timepoint"
  )
  expect_s3_class(result_qol, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Biomarker Expression Data
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles biomarker expression across cancer types", {
  devtools::load_all()

  # Mutation burden by cancer type
  result_mut <- jjridges(
    data = jjridges_biomarker,
    x_var = "mutation_burden",
    y_var = "cancer_type"
  )
  expect_s3_class(result_mut, "jjridgesResults")

  # PD-L1 expression by cancer type
  result_pdl1 <- jjridges(
    data = jjridges_biomarker,
    x_var = "pdl1_expression",
    y_var = "cancer_type"
  )
  expect_s3_class(result_pdl1, "jjridgesResults")

  # TILs by stage
  result_tils <- jjridges(
    data = jjridges_biomarker,
    x_var = "tils_percent",
    y_var = "stage"
  )
  expect_s3_class(result_tils, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 15. Survival Time Distribution
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles survival time distributions", {
  devtools::load_all()

  # Survival months by treatment
  result_surv_tx <- jjridges(
    data = jjridges_survival,
    x_var = "survival_months",
    y_var = "treatment"
  )
  expect_s3_class(result_surv_tx, "jjridgesResults")

  # Survival by stage
  result_surv_stage <- jjridges(
    data = jjridges_survival,
    x_var = "survival_months",
    y_var = "disease_stage"
  )
  expect_s3_class(result_surv_stage, "jjridgesResults")

  # Disease-free survival by age category
  result_dfs <- jjridges(
    data = jjridges_survival,
    x_var = "dfs_months",
    y_var = "age_category"
  )
  expect_s3_class(result_dfs, "jjridgesResults")
})

# ═══════════════════════════════════════════════════════════
# 16. Clinical Presets
# ═══════════════════════════════════════════════════════════

test_that("jjridges handles biomarker distribution preset", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_biomarker,
    x_var = "mutation_burden",
    y_var = "cancer_type",
    clinicalPreset = "biomarker_distribution"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles treatment response preset", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_treatment,
    x_var = "response_score",
    y_var = "treatment_group",
    clinicalPreset = "treatment_response"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles age by stage preset", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "age",
    y_var = "tumor_stage",
    clinicalPreset = "age_by_stage"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles tumor size comparison preset", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_test,
    x_var = "tumor_size",
    y_var = "tumor_stage",
    clinicalPreset = "tumor_size_comparison"
  )

  expect_s3_class(result, "jjridgesResults")
})

test_that("jjridges handles survival time distribution preset", {
  devtools::load_all()

  result <- jjridges(
    data = jjridges_survival,
    x_var = "survival_months",
    y_var = "disease_stage",
    clinicalPreset = "survival_time_distribution"
  )

  expect_s3_class(result, "jjridgesResults")
})
