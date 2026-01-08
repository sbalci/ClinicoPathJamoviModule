# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jwaffle
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality with single parameters
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
# 1. Basic Function Execution
# ═══════════════════════════════════════════════════════════

test_that("jwaffle function exists and runs with minimal arguments", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles treatment response data", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles disease subtype data", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles pathology data", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Rows Parameter
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles default rows (5)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    rows = 5
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 10 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_stage",
    rows = 10
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 3 rows", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "differentiation",
    rows = 3
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 1 row (minimum)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "gender",
    rows = 1
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Flip Parameter
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles standard orientation (flip=false)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    flip = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles flipped orientation (flip=true)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade",
    flip = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Color Palettes
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles default palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    color_palette = "default"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles colorblind palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",
    color_palette = "colorblind"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles professional palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_stage",
    color_palette = "professional"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles presentation palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    color_palette = "presentation"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles journal palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "risk_category",
    color_palette = "journal"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles pastel palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "ethnicity",
    color_palette = "pastel"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles dark palette", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "molecular_subtype",
    color_palette = "dark"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Legend Options
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles legend hidden (default)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    show_legend = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles legend shown", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "differentiation",
    show_legend = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Title Options
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles no title (default)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype",
    mytitle = ''
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles custom title", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    mytitle = "Treatment Response Distribution"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles custom legend title", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    show_legend = TRUE,
    legendtitle = "Tumor Grade"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles both custom titles", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade",
    show_legend = TRUE,
    mytitle = "Quality Metrics Distribution",
    legendtitle = "Quality Grade"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Faceting
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles no faceting (default)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting by 2-level variable", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",
    facet = "gender"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting by 3-level variable", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    facet = "treatment"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting by 4-level variable", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype",
    facet = "disease_stage"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles faceting by 5-level variable", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "smoking_status",
    facet = "age_group"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Summary Options
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles summaries hidden (default)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    showSummaries = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles summaries shown", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_pathology,
    groups = "tumor_grade",
    showSummaries = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles explanations hidden (default)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_stage",
    showExplanations = FALSE
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles explanations shown", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_quality,
    groups = "quality_grade",
    showExplanations = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Different Categorical Variables
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles 2-level categorical", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "gender"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 3-level categorical", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "smoking_status"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 4-level categorical", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 5-level categorical", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_disease,
    groups = "disease_subtype"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 5-level age groups", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 5-level ethnicity", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "ethnicity"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Count Variables (Pre-aggregated Data)
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles data without count variable", {
  devtools::load_all()

  # No counts specified - each row counted once
  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles data with count variable", {
  devtools::load_all()

  # With counts specified
  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    counts = "patient_count"
  )

  expect_s3_class(result, "jwaffleResults")
})
