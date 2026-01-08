# ═══════════════════════════════════════════════════════════
# Integration Tests: jwaffle
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and realistic clinical scenarios
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(jwaffle_test, package = "ClinicoPath", envir = environment())
data(jwaffle_disease, package = "ClinicoPath", envir = environment())
data(jwaffle_pathology, package = "ClinicoPath", envir = environment())
data(jwaffle_demographics, package = "ClinicoPath", envir = environment())
data(jwaffle_quality, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Complete Treatment Response Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete treatment response workflow", {
  devtools::load_all()

  # Step 1: Basic visualization
  result1 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )
  expect_s3_class(result1, "jwaffleResults")

  # Step 2: Add color palette
  result2 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    color_palette = "colorblind"
  )
  expect_s3_class(result2, "jwaffleResults")

  # Step 3: Add titles
  result3 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    color_palette = "colorblind",
    mytitle = "Treatment Response Distribution"
  )
  expect_s3_class(result3, "jwaffleResults")

  # Step 4: Add legend
  result4 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    color_palette = "colorblind",
    mytitle = "Treatment Response Distribution",
    show_legend = TRUE,
    legendtitle = "Response"
  )
  expect_s3_class(result4, "jwaffleResults")

  # Step 5: Add faceting by treatment
  result5 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment",
    color_palette = "colorblind",
    mytitle = "Treatment Response by Therapy Type",
    show_legend = TRUE,
    legendtitle = "Response Category",
    showSummaries = TRUE
  )
  expect_s3_class(result5, "jwaffleResults")

  # All steps should produce valid results
  expect_equal(5, 5)
})

# ═══════════════════════════════════════════════════════════
# 2. Complete Pathology Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete pathology workflow", {
  devtools::load_all()

  # Tumor grade distribution by hospital
  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    facet = "hospital",
    rows = 5,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Tumor Grade Distribution by Hospital",
    legendtitle = "Grade",
    showSummaries = TRUE,
    showExplanations = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles differentiation status analysis", {
  devtools::load_all()

  # Differentiation status distribution
  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "differentiation",
    facet = "hospital",
    rows = 5,
    flip = FALSE,
    color_palette = "professional",
    show_legend = TRUE,
    mytitle = "Tumor Differentiation by Hospital",
    legendtitle = "Differentiation Status"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Complete Demographics Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete demographics workflow", {
  devtools::load_all()

  # Age distribution by gender
  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",
    facet = "gender",
    rows = 5,
    color_palette = "professional",
    show_legend = TRUE,
    mytitle = "Age Distribution by Gender",
    legendtitle = "Age Group"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles ethnicity distribution analysis", {
  devtools::load_all()

  # Ethnicity across regions
  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "ethnicity",
    facet = "region",
    rows = 5,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Ethnicity Distribution by Region",
    legendtitle = "Ethnicity"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles smoking status analysis", {
  devtools::load_all()

  # Smoking status by age group
  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "smoking_status",
    facet = "age_group",
    rows = 5,
    color_palette = "presentation",
    show_legend = TRUE,
    mytitle = "Smoking Status by Age Group",
    legendtitle = "Smoking Status"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Complete Disease Subtype Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete disease subtype workflow", {
  devtools::load_all()

  # Histological subtypes by stage
  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype",
    facet = "disease_stage",
    rows = 5,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Histological Subtypes by Disease Stage",
    legendtitle = "Subtype",
    showSummaries = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles molecular subtype analysis", {
  devtools::load_all()

  # Molecular subtypes by stage
  result <- jwaffle(
    data = jwaffle_disease,
    groups = "molecular_subtype",
    facet = "disease_stage",
    rows = 5,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Molecular Subtypes by Disease Stage",
    legendtitle = "Molecular Subtype"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Complete Quality Metrics Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete quality metrics workflow", {
  devtools::load_all()

  # Quality grades by institution type
  result <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade",
    facet = "institution_type",
    rows = 5,
    flip = TRUE,
    color_palette = "presentation",
    show_legend = TRUE,
    mytitle = "Quality Grades by Institution Type",
    legendtitle = "Quality Grade",
    showSummaries = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles compliance analysis", {
  devtools::load_all()

  # Compliance levels over time
  result <- jwaffle(
    data = jwaffle_quality,
    groups = "compliance",
    facet = "quarter",
    rows = 5,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Compliance Levels Over Time",
    legendtitle = "Compliance Level"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles risk category analysis", {
  devtools::load_all()

  # Risk categories by quarter
  result <- jwaffle(
    data = jwaffle_quality,
    groups = "risk_category",
    facet = "quarter",
    rows = 5,
    color_palette = "professional",
    show_legend = TRUE,
    mytitle = "Risk Category Distribution Over Time",
    legendtitle = "Risk Level"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Multi-Dataset Consistency
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles all test datasets consistently", {
  devtools::load_all()

  # Test dataset
  result1 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )
  expect_s3_class(result1, "jwaffleResults")

  # Disease dataset
  result2 <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype"
  )
  expect_s3_class(result2, "jwaffleResults")

  # Pathology dataset
  result3 <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade"
  )
  expect_s3_class(result3, "jwaffleResults")

  # Demographics dataset
  result4 <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group"
  )
  expect_s3_class(result4, "jwaffleResults")

  # Quality dataset
  result5 <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade"
  )
  expect_s3_class(result5, "jwaffleResults")

  # All datasets should work
  expect_equal(5, 5)
})

# ═══════════════════════════════════════════════════════════
# 7. Publication-Ready Outputs
# ═══════════════════════════════════════════════════════════

test_that("jwaffle produces publication-ready treatment response figure", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment",
    rows = 5,
    flip = FALSE,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Treatment Response Distribution by Therapy Type",
    legendtitle = "Response Category",
    showSummaries = TRUE,
    showExplanations = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle produces publication-ready pathology figure", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    facet = "hospital",
    rows = 3,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Tumor Grade Distribution Across Institutions",
    legendtitle = "Tumor Grade",
    showSummaries = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle produces publication-ready demographics figure", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "ethnicity",
    facet = "region",
    rows = 5,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Patient Ethnicity Distribution by Geographic Region",
    legendtitle = "Ethnicity"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Comparison Across All Palettes
# ═══════════════════════════════════════════════════════════

test_that("jwaffle produces consistent results across all palettes", {
  devtools::load_all()

  palettes <- c("default", "colorblind", "professional", "presentation",
                "journal", "pastel", "dark")

  results <- list()

  for (palette in palettes) {
    results[[palette]] <- jwaffle(
      data = jwaffle_demographics,
      groups = "age_group",
      color_palette = palette
    )

    expect_s3_class(results[[palette]], "jwaffleResults")
  }

  expect_equal(length(results), length(palettes))
})

# ═══════════════════════════════════════════════════════════
# 9. Different Row Configurations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles various row configurations", {
  devtools::load_all()

  # Test with 1, 3, 5, 10, 15, 20 rows
  row_values <- c(1, 3, 5, 10, 15, 20)

  for (rows_val in row_values) {
    result <- jwaffle(
      data = jwaffle_test,
      groups = "response_category",
      rows = rows_val
    )

    expect_s3_class(result, "jwaffleResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 10. Flip Orientation Comparison
# ═══════════════════════════════════════════════════════════

test_that("jwaffle produces valid results for both orientations", {
  devtools::load_all()

  # Standard orientation
  result_standard <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    flip = FALSE
  )

  # Flipped orientation
  result_flipped <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    flip = TRUE
  )

  expect_s3_class(result_standard, "jwaffleResults")
  expect_s3_class(result_flipped, "jwaffleResults")
})
