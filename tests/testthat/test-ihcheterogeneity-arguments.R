# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# Tests all argument combinations and option interactions
# for the ihcheterogeneity jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(ihcheterogeneity_test, package = "ClinicoPath")
data(ihcheterogeneity_ki67, package = "ClinicoPath")
data(ihcheterogeneity_compartments, package = "ClinicoPath")

test_that("ihcheterogeneity handles Ki67 data appropriately", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_ki67,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles ER H-score data", {
  data(ihcheterogeneity_er_hscore, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_er_hscore,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles all analysis options combined", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4",
    spatial_id = "spatial_id",
    compareCompartments = FALSE,
    compartmentTests = FALSE,
    analysis_type = "comprehensive",
    sampling_strategy = "unknown",
    cv_threshold = 20.0,
    correlation_threshold = 0.80,
    show_variability_plots = TRUE,
    variance_components = TRUE,
    power_analysis = TRUE,
    generate_recommendations = TRUE,
    showSummary = TRUE,
    showGlossary = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles reproducibility-focused analysis", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    analysis_type = "reproducibility",
    correlation_threshold = 0.90
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles bias-focused analysis", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    analysis_type = "bias"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles variability-focused analysis", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    analysis_type = "variability",
    cv_threshold = 15.0,
    variance_components = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles compartment comparison with tests", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4",
    spatial_id = "spatial_id",
    compareCompartments = TRUE,
    compartmentTests = TRUE,
    show_variability_plots = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles high heterogeneity data", {
  data(ihcheterogeneity_high_hetero, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_high_hetero,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    cv_threshold = 30.0,
    variance_components = TRUE,
    generate_recommendations = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles different CV thresholds", {
  cv_thresholds <- c(15.0, 20.0, 25.0, 30.0)

  for (cv_thresh in cv_thresholds) {
    result <- ihcheterogeneity(
      data = ihcheterogeneity_test,
      wholesection = "wholesection",
      biopsy1 = "biopsy1",
      biopsy2 = "biopsy2",
      cv_threshold = cv_thresh
    )

    expect_s3_class(result, "ihcheterogeneityClass")
  }
})

test_that("ihcheterogeneity handles different correlation thresholds", {
  corr_thresholds <- c(0.60, 0.70, 0.80, 0.90)

  for (corr_thresh in corr_thresholds) {
    result <- ihcheterogeneity(
      data = ihcheterogeneity_test,
      wholesection = "wholesection",
      biopsy1 = "biopsy1",
      biopsy2 = "biopsy2",
      correlation_threshold = corr_thresh
    )

    expect_s3_class(result, "ihcheterogeneityClass")
  }
})

test_that("ihcheterogeneity handles all display options", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    show_variability_plots = TRUE,
    showSummary = TRUE,
    showGlossary = TRUE,
    generate_recommendations = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles inter-regional comparison without reference", {
  data(ihcheterogeneity_no_reference, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_no_reference,
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4",
    spatial_id = "spatial_id",
    analysis_type = "comprehensive",
    variance_components = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles systematic sampling strategy", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    sampling_strategy = "systematic",
    analysis_type = "variability"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles stratified sampling strategy", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    spatial_id = "spatial_id",
    sampling_strategy = "stratified",
    compareCompartments = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})
