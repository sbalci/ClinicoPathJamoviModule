# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the ihcheterogeneity jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(ihcheterogeneity_test, package = "ClinicoPath")

test_that("ihcheterogeneity function exists and is accessible", {
  # Check function exists
  expect_true(exists("ihcheterogeneity"))

  # Check it's a function
  expect_type(ihcheterogeneity, "closure")
})

test_that("ihcheterogeneity runs with minimal required arguments", {
  # Only biopsy1 is required
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    biopsy1 = "biopsy1"
  )

  # Should return a result object
  expect_s3_class(result, "ihcheterogeneityClass")

  # Should have a results component
  expect_true("results" %in% names(result))
})

test_that("ihcheterogeneity runs with reference and two biopsies", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity runs with multiple regional measurements", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles reference-based analysis", {
  # With wholesection (reference measurement)
  result_ref <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result_ref, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles inter-regional analysis without reference", {
  # Load dataset without reference
  data(ihcheterogeneity_no_reference, package = "ClinicoPath")

  # Without wholesection
  result_no_ref <- ihcheterogeneity(
    data = ihcheterogeneity_no_reference,
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3"
  )

  expect_s3_class(result_no_ref, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles spatial compartment identification", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    spatial_id = "spatial_id"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles compartment comparison", {
  data(ihcheterogeneity_compartments, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    spatial_id = "spatial_id",
    compareCompartments = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles compartment tests", {
  data(ihcheterogeneity_compartments, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    spatial_id = "spatial_id",
    compareCompartments = TRUE,
    compartmentTests = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles different analysis types", {
  analysis_types <- c("reproducibility", "bias", "variability", "comprehensive")

  for (type in analysis_types) {
    result <- ihcheterogeneity(
      data = ihcheterogeneity_test,
      wholesection = "wholesection",
      biopsy1 = "biopsy1",
      biopsy2 = "biopsy2",
      analysis_type = type
    )

    expect_s3_class(result, "ihcheterogeneityClass")
  }
})

test_that("ihcheterogeneity handles different sampling strategies", {
  strategies <- c("random", "systematic", "stratified", "unknown")

  for (strategy in strategies) {
    result <- ihcheterogeneity(
      data = ihcheterogeneity_test,
      wholesection = "wholesection",
      biopsy1 = "biopsy1",
      biopsy2 = "biopsy2",
      sampling_strategy = strategy
    )

    expect_s3_class(result, "ihcheterogeneityClass")
  }
})

test_that("ihcheterogeneity handles custom thresholds", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    cv_threshold = 15.0,
    correlation_threshold = 0.85
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity produces plots when requested", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    show_variability_plots = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles variance component analysis", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    variance_components = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles power analysis", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    power_analysis = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity generates recommendations", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    generate_recommendations = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity shows plain-language summary", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    showSummary = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity shows statistical glossary", {
  result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    showGlossary = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity runs with small dataset", {
  # Load small test data
  data(ihcheterogeneity_test_small, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_test_small,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  # Should complete
  expect_s3_class(result, "ihcheterogeneityClass")
})
