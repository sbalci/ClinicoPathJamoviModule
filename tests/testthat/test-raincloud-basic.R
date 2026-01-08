# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: raincloud
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of the raincloud function
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(raincloud_test, package = "ClinicoPath", envir = environment())
data(raincloud_clinical, package = "ClinicoPath", envir = environment())
data(raincloud_treatment, package = "ClinicoPath", envir = environment())
data(raincloud_biomarker, package = "ClinicoPath", envir = environment())
data(raincloud_small, package = "ClinicoPath", envir = environment())
data(raincloud_skewed, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Function Existence and Basic Execution
# ═══════════════════════════════════════════════════════════

test_that("raincloud function exists and runs with minimal arguments", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Different Continuous Variables
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles different continuous dependent variables", {
  devtools::load_all()

  # Symptom score (0-100 scale)
  result_symptom <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )
  expect_s3_class(result_symptom, "raincloudResults")

  # Quality of life (0-100 scale)
  result_qol <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity"
  )
  expect_s3_class(result_qol, "raincloudResults")

  # Pain intensity (0-10 scale)
  result_pain <- raincloud(
    data = raincloud_test,
    dep_var = "pain_intensity",
    group_var = "treatment_group"
  )
  expect_s3_class(result_pain, "raincloudResults")

  # Response time (milliseconds)
  result_rt <- raincloud(
    data = raincloud_test,
    dep_var = "response_time",
    group_var = "age_group"
  )
  expect_s3_class(result_rt, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Grouping Variables
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles different grouping variables", {
  devtools::load_all()

  # Binary grouping (Male/Female)
  result_binary <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "gender"
  )
  expect_s3_class(result_binary, "raincloudResults")

  # 3-level grouping (disease severity)
  result_3level <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity"
  )
  expect_s3_class(result_3level, "raincloudResults")

  # 4-level grouping (treatment groups)
  result_4level <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )
  expect_s3_class(result_4level, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Faceting Variable
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles facet variable for multiple panels", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles faceting by hospital site", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "gender",
    facet_var = "hospital_site"
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Color Variable
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles additional color variable", {
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
# 6. Visualization Components
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles violin plot component", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = FALSE,
    show_dots = FALSE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles boxplot component", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = FALSE,
    show_boxplot = TRUE,
    show_dots = FALSE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles dot plot component", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = FALSE,
    show_boxplot = FALSE,
    show_dots = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles all components combined", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Themes
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles clinical theme", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    theme = "clinical"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles minimal theme", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "minimal"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles classic theme", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity",
    theme = "classic"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles publication theme", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "publication"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles Prism themes", {
  devtools::load_all()

  # Prism default
  result_prism <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "diagnosis",
    theme = "prism"
  )
  expect_s3_class(result_prism, "raincloudResults")

  # Prism whitespace
  result_white <- raincloud(
    data = raincloud_clinical,
    dep_var = "cholesterol",
    group_var = "bmi_category",
    theme = "prism_whitespace"
  )
  expect_s3_class(result_white, "raincloudResults")

  # Prism light
  result_light <- raincloud(
    data = raincloud_clinical,
    dep_var = "hemoglobin_a1c",
    group_var = "diagnosis",
    theme = "prism_light"
  )
  expect_s3_class(result_light, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Color Palettes
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles default palette", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    palette = "default"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles viridis palette", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity",
    palette = "viridis"
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles GraphPad Prism palettes", {
  devtools::load_all()

  # Floral
  result_floral <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    palette = "floral"
  )
  expect_s3_class(result_floral, "raincloudResults")

  # Candy bright
  result_candy <- raincloud(
    data = raincloud_biomarker,
    dep_var = "protein_expression",
    group_var = "receptor_status",
    palette = "candy_bright"
  )
  expect_s3_class(result_candy, "raincloudResults")

  # Colorblind safe
  result_cb <- raincloud(
    data = raincloud_biomarker,
    dep_var = "immune_score",
    group_var = "stage",
    palette = "colorblind_safe"
  )
  expect_s3_class(result_cb, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Statistical Features
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles normality tests", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    normality_test = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles group comparison tests", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    comparison_test = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles outlier detection", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_outliers = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles mean display", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "quality_of_life",
    group_var = "disease_severity",
    show_mean = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

test_that("raincloud handles median display", {
  devtools::load_all()

  result <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_median = TRUE
  )

  expect_s3_class(result, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 10. All Test Datasets
# ═══════════════════════════════════════════════════════════

test_that("raincloud works with all test datasets", {
  devtools::load_all()

  # raincloud_test
  result1 <- raincloud(data = raincloud_test, dep_var = "symptom_score", group_var = "treatment_group")
  expect_s3_class(result1, "raincloudResults")

  # raincloud_clinical
  result2 <- raincloud(data = raincloud_clinical, dep_var = "glucose", group_var = "diagnosis")
  expect_s3_class(result2, "raincloudResults")

  # raincloud_treatment
  result3 <- raincloud(data = raincloud_treatment, dep_var = "tumor_size", group_var = "treatment")
  expect_s3_class(result3, "raincloudResults")

  # raincloud_biomarker
  result4 <- raincloud(data = raincloud_biomarker, dep_var = "ki67_index", group_var = "cancer_type")
  expect_s3_class(result4, "raincloudResults")

  # raincloud_small
  result5 <- raincloud(data = raincloud_small, dep_var = "measurement", group_var = "group")
  expect_s3_class(result5, "raincloudResults")

  # raincloud_skewed
  result6 <- raincloud(data = raincloud_skewed, dep_var = "right_skewed", group_var = "group")
  expect_s3_class(result6, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Clinical Laboratory Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles clinical laboratory measurements", {
  devtools::load_all()

  # Glucose across diagnoses
  result_glucose <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis"
  )
  expect_s3_class(result_glucose, "raincloudResults")

  # HbA1c across diagnoses
  result_hba1c <- raincloud(
    data = raincloud_clinical,
    dep_var = "hemoglobin_a1c",
    group_var = "diagnosis"
  )
  expect_s3_class(result_hba1c, "raincloudResults")

  # Blood pressure by age
  result_bp <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "age_category"
  )
  expect_s3_class(result_bp, "raincloudResults")

  # Cholesterol by BMI
  result_chol <- raincloud(
    data = raincloud_clinical,
    dep_var = "cholesterol",
    group_var = "bmi_category"
  )
  expect_s3_class(result_chol, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Treatment Effect Visualization
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles treatment effect data", {
  devtools::load_all()

  # Tumor size by treatment
  result_tumor <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment"
  )
  expect_s3_class(result_tumor, "raincloudResults")

  # Tumor size by timepoint
  result_time <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "timepoint"
  )
  expect_s3_class(result_time, "raincloudResults")

  # Symptom burden by response category
  result_response <- raincloud(
    data = raincloud_treatment,
    dep_var = "symptom_burden",
    group_var = "response_category"
  )
  expect_s3_class(result_response, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Biomarker Expression Analysis
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles biomarker expression data", {
  devtools::load_all()

  # Ki67 by grade
  result_ki67 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade"
  )
  expect_s3_class(result_ki67, "raincloudResults")

  # Protein expression by receptor status
  result_protein <- raincloud(
    data = raincloud_biomarker,
    dep_var = "protein_expression",
    group_var = "receptor_status"
  )
  expect_s3_class(result_protein, "raincloudResults")

  # Mutation burden by cancer type
  result_mutation <- raincloud(
    data = raincloud_biomarker,
    dep_var = "mutation_burden",
    group_var = "cancer_type"
  )
  expect_s3_class(result_mutation, "raincloudResults")

  # Immune score by stage
  result_immune <- raincloud(
    data = raincloud_biomarker,
    dep_var = "immune_score",
    group_var = "stage"
  )
  expect_s3_class(result_immune, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Distribution Types
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles different distribution types", {
  devtools::load_all()

  # Normal distribution
  result_normal <- raincloud(
    data = raincloud_skewed,
    dep_var = "normal",
    group_var = "condition"
  )
  expect_s3_class(result_normal, "raincloudResults")

  # Right-skewed distribution
  result_right <- raincloud(
    data = raincloud_skewed,
    dep_var = "right_skewed",
    group_var = "condition"
  )
  expect_s3_class(result_right, "raincloudResults")

  # Left-skewed distribution
  result_left <- raincloud(
    data = raincloud_skewed,
    dep_var = "left_skewed",
    group_var = "condition"
  )
  expect_s3_class(result_left, "raincloudResults")

  # Bimodal distribution
  result_bimodal <- raincloud(
    data = raincloud_skewed,
    dep_var = "bimodal",
    group_var = "condition"
  )
  expect_s3_class(result_bimodal, "raincloudResults")

  # Log-normal distribution
  result_lognormal <- raincloud(
    data = raincloud_skewed,
    dep_var = "lognormal",
    group_var = "condition"
  )
  expect_s3_class(result_lognormal, "raincloudResults")
})
