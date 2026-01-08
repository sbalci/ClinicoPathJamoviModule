# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jwaffle
# ═══════════════════════════════════════════════════════════
#
# Tests all parameter combinations and interactions
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jwaffle_test, package = "ClinicoPath", envir = environment())
data(jwaffle_disease, package = "ClinicoPath", envir = environment())
data(jwaffle_pathology, package = "ClinicoPath", envir = environment())
data(jwaffle_demographics, package = "ClinicoPath", envir = environment())
data(jwaffle_quality, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Palette + Rows Combinations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles colorblind palette + 10 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    color_palette = "colorblind",
    rows = 10
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles professional palette + 3 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    color_palette = "professional",
    rows = 3
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles journal palette + 5 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_stage",
    color_palette = "journal",
    rows = 5
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Palette + Flip Combinations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles presentation palette + flipped", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade",
    color_palette = "presentation",
    flip = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles pastel palette + flipped", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",
    color_palette = "pastel",
    flip = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Legend + Title Combinations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles legend + custom title", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    show_legend = TRUE,
    mytitle = "Treatment Response Distribution"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles legend + custom titles", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "differentiation",
    show_legend = TRUE,
    mytitle = "Tumor Differentiation Status",
    legendtitle = "Differentiation Grade"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Faceting + Palette Combinations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles faceting + colorblind palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment",
    color_palette = "colorblind"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting + professional palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",
    facet = "gender",
    color_palette = "professional"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting + journal palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype",
    facet = "disease_stage",
    color_palette = "journal"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Faceting + Rows Combinations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles faceting + 3 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade",
    facet = "institution_type",
    rows = 3
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting + 10 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    facet = "hospital",
    rows = 10
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Summaries + Explanations Combinations
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles summaries without explanations", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    showSummaries = TRUE,
    showExplanations = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles explanations without summaries", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    showSummaries = FALSE,
    showExplanations = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles both summaries and explanations", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_stage",
    showSummaries = TRUE,
    showExplanations = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Complete Treatment Response Workflow
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete treatment response analysis", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment",
    rows = 5,
    flip = FALSE,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Treatment Response by Therapy Type",
    legendtitle = "Response Category",
    showSummaries = TRUE,
    showExplanations = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Complete Demographics Workflow
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete demographics analysis", {
  devtools::load_all()

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

# ═══════════════════════════════════════════════════════════
# 9. Complete Pathology Workflow
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete pathology analysis", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    facet = "hospital",
    rows = 3,
    flip = FALSE,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Tumor Grade Distribution by Hospital",
    legendtitle = "Grade",
    showSummaries = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Complete Disease Subtype Workflow
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete disease subtype analysis", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype",
    facet = "disease_stage",
    rows = 5,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Histological Subtypes by Disease Stage",
    legendtitle = "Subtype"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Complete Quality Metrics Workflow
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles complete quality metrics analysis", {
  devtools::load_all()

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

# ═══════════════════════════════════════════════════════════
# 12. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles minimal configuration", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles maximum configuration", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    counts = "patient_count",
    facet = "treatment",
    rows = 5,
    flip = FALSE,
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Comprehensive Treatment Response Analysis",
    legendtitle = "Response Category",
    showSummaries = TRUE,
    showExplanations = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Basic waffle
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

  # Step 3: Add title
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
    show_legend = TRUE
  )
  expect_s3_class(result4, "jwaffleResults")

  # Step 5: Add faceting
  result5 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment",
    color_palette = "colorblind",
    mytitle = "Treatment Response Distribution",
    show_legend = TRUE
  )
  expect_s3_class(result5, "jwaffleResults")

  # Step 6: Add summaries
  result6 <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment",
    color_palette = "colorblind",
    mytitle = "Treatment Response Distribution",
    show_legend = TRUE,
    showSummaries = TRUE
  )
  expect_s3_class(result6, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Different Palettes with Same Data
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
# 15. Ethnicity Analysis (5 levels)
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles ethnicity with multiple levels", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "ethnicity",
    facet = "region",
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Ethnicity Distribution by Region",
    legendtitle = "Ethnicity"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 16. Molecular Subtypes Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles molecular subtypes", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "molecular_subtype",
    facet = "disease_stage",
    rows = 5,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Molecular Subtypes by Disease Stage",
    legendtitle = "Molecular Subtype"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 17. Margin Status Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles margin status with 3 categories", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "margin_status",
    facet = "hospital",
    color_palette = "professional",
    show_legend = TRUE,
    mytitle = "Surgical Margin Status by Hospital",
    legendtitle = "Margin Status"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 18. Smoking Status Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles smoking status", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "smoking_status",
    facet = "age_group",
    color_palette = "colorblind",
    show_legend = TRUE,
    mytitle = "Smoking Status by Age Group",
    legendtitle = "Smoking Status"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 19. Risk Category Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles risk categories", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "risk_category",
    facet = "quarter",
    color_palette = "presentation",
    show_legend = TRUE,
    mytitle = "Risk Category Distribution Over Time",
    legendtitle = "Risk Level"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 20. Compliance Level Analysis
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles compliance levels with 4 categories", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "compliance",
    facet = "institution_type",
    rows = 5,
    color_palette = "journal",
    show_legend = TRUE,
    mytitle = "Compliance Levels by Institution Type",
    legendtitle = "Compliance Level"
  )

  expect_s3_class(result, "jwaffleResults")
})
