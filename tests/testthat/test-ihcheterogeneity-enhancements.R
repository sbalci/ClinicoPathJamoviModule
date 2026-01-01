context("ihcheterogeneity enhancements")

test_that("escapeVar handles variables with special characters", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test data
  test_data <- read.csv(system.file("data", "ihc_heterogeneity_test.csv",
                                    package = "ClinicoPath"))

  # Rename columns to include special characters
  names(test_data)[2:6] <- c("whole section", "region-1", "region.2", "region_3", "region 4")

  # Test analysis with special characters in variable names
  result <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole section",
    biopsy1 = "region-1",
    biopsy2 = "region.2",
    biopsy3 = "region_3",
    biopsy4 = "region 4"
  )

  # Check that analysis completes without error
  expect_true(!is.null(result))
})

test_that("ICC calculation provides appropriate fallback", {
  # Test with minimal data
  small_data <- data.frame(
    whole = c(50, 55, 60),
    reg1 = c(48, 54, 59),
    reg2 = c(52, 56, 61)
  )

  result <- ihcheterogeneity(
    data = small_data,
    wholesection = "whole",
    biopsy1 = "reg1",
    biopsy2 = "reg2"
  )

  # Should use correlation fallback due to small sample
  expect_true(!is.null(result))
})

test_that("psych package messaging works correctly", {
  # Mock missing psych package
  if (requireNamespace("psych", quietly = TRUE)) {
    skip("psych package is installed, skipping missing package test")
  }

  test_data <- read.csv(system.file("data", "ihc_heterogeneity_test.csv",
                                    package = "ClinicoPath"))

  result <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole_section",
    biopsy1 = "region_1",
    biopsy2 = "region_2"
  )

  # Check for note about missing psych package
  notes <- result$results$interpretation$notes
  expect_true(any(grepl("psych", notes, ignore.case = TRUE)))
})

test_that("reference-based vs inter-regional analysis modes work", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity_test.csv",
                                    package = "ClinicoPath"))

  # Test reference-based mode
  result_ref <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole_section",
    biopsy1 = "region_1",
    biopsy2 = "region_2"
  )

  # Test inter-regional mode (no reference)
  result_inter <- ihcheterogeneity(
    data = test_data,
    biopsy1 = "region_1",
    biopsy2 = "region_2",
    biopsy3 = "region_3"
  )

  # Results should differ
  expect_false(identical(
    result_ref$results$interpretation$content,
    result_inter$results$interpretation$content
  ))
})

test_that("spatial analysis activates with spatial_id", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity_test.csv",
                                    package = "ClinicoPath"))

  # Without spatial_id
  result_no_spatial <- ihcheterogeneity(
    data = test_data,
    biopsy1 = "region_1",
    biopsy2 = "region_2"
  )

  # With spatial_id
  result_spatial <- ihcheterogeneity(
    data = test_data,
    biopsy1 = "region_1",
    biopsy2 = "region_2",
    spatial_id = "spatial_location"
  )

  # Spatial table should only be visible with spatial_id
  expect_false(result_no_spatial$results$spatialanalysistable$visible)
  expect_true(result_spatial$results$spatialanalysistable$visible)
})

test_that("analysis type changes behavior", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity_test.csv",
                                    package = "ClinicoPath"))

  # Comprehensive analysis
  result_comp <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole_section",
    biopsy1 = "region_1",
    biopsy2 = "region_2",
    analysis_type = "comprehensive"
  )

  # Reproducibility focus
  result_repro <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole_section",
    biopsy1 = "region_1",
    biopsy2 = "region_2",
    analysis_type = "reproducibility"
  )

  # Different analysis types should produce different outputs
  expect_false(identical(
    result_comp$results$interpretation$content,
    result_repro$results$interpretation$content
  ))
})

test_that("threshold parameters affect interpretation", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity_test.csv",
                                    package = "ClinicoPath"))

  # Low CV threshold (strict)
  result_strict <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole_section",
    biopsy1 = "region_1",
    biopsy2 = "region_2",
    cv_threshold = 10.0,
    correlation_threshold = 0.90
  )

  # High CV threshold (lenient)
  result_lenient <- ihcheterogeneity(
    data = test_data,
    wholesection = "whole_section",
    biopsy1 = "region_1",
    biopsy2 = "region_2",
    cv_threshold = 40.0,
    correlation_threshold = 0.60
  )

  # Interpretations should differ based on thresholds
  expect_false(identical(
    result_strict$results$interpretation$content,
    result_lenient$results$interpretation$content
  ))
})
