context("ihcheterogeneity enhancements")

test_that("escapeVar handles variables with special characters", {
  skip_if_not_installed('jmvReadWrite')
  # Load test data
  test_data <- read.csv(system.file("data", "ihc_heterogeneity.csv",
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

test_that("ICC falls back to mean correlation when psych's requirements are unmet", {
  # 3 cases is below the deliberate 5-case minimum: the analysis rejects
  small_data <- data.frame(
    whole = c(50, 55, 60),
    reg1 = c(48, 54, 59),
    reg2 = c(52, 56, 61)
  )

  expect_error(
    ihcheterogeneity(
      data = small_data,
      wholesection = "whole",
      biopsy1 = "reg1",
      biopsy2 = "reg2"
    ),
    "At least 5 complete cases"
  )

  # The genuine fallback path: enough cases, but a zero-variance region makes
  # the ICC unestimable -> the table must label the value as a correlation,
  # NOT an ICC, and carry the explanatory note.
  set.seed(7)
  fb_data <- data.frame(
    whole = c(50, 55, 60, 45, 52, 58),
    reg1  = rep(50, 6),                       # zero variance
    reg2  = c(52, 56, 61, 44, 51, 59)
  )
  res <- ihcheterogeneity(
    data = fb_data, wholesection = "whole",
    biopsy1 = "reg1", biopsy2 = "reg2"
  )
  repro <- res$reproducibilitytable$asDF
  icc_row <- repro[grep("ICC", repro$metric), , drop = FALSE]
  expect_true(nrow(icc_row) == 1)
  expect_match(icc_row$metric, "not estimable")
  notes <- vapply(res$reproducibilitytable$notes, function(n) n$note, character(1))
  expect_true(any(grepl("NOT an ICC", notes)))
})

test_that("psych package messaging works correctly", {
  # Mock missing psych package
  if (requireNamespace("psych", quietly = TRUE)) {
    skip("psych package is installed, skipping missing package test")
  }

  test_data <- read.csv(system.file("data", "ihc_heterogeneity.csv",
                                    package = "ClinicoPath"))

  result <- ihcheterogeneity(
    data = test_data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2"
  )

  # Check for note about missing psych package
  notes <- result$interpretation$notes
  expect_true(any(grepl("psych", notes, ignore.case = TRUE)))
})

test_that("reference-based vs inter-regional analysis modes work", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity.csv",
                                    package = "ClinicoPath"))

  # Test reference-based mode
  result_ref <- ihcheterogeneity(
    data = test_data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2"
  )

  # Test inter-regional mode (no reference)
  result_inter <- ihcheterogeneity(
    data = test_data,
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    biopsy3 = "ki67_region3"
  )

  # Results should differ
  expect_false(identical(
    result_ref$interpretation$content,
    result_inter$interpretation$content
  ))
})

test_that("spatial analysis activates with spatial_id", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity.csv",
                                    package = "ClinicoPath"))

  # Without spatial_id
  result_no_spatial <- ihcheterogeneity(
    data = test_data,
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2"
  )

  # With spatial_id
  result_spatial <- ihcheterogeneity(
    data = test_data,
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    spatial_id = "spatial_region"
  )

  # Spatial table should only be visible with spatial_id
  expect_false(result_no_spatial$spatialanalysistable$visible)
  expect_true(result_spatial$spatialanalysistable$visible)
})

test_that("analysis type changes behavior", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity.csv",
                                    package = "ClinicoPath"))

  # Comprehensive analysis
  result_comp <- ihcheterogeneity(
    data = test_data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    analysis_type = "comprehensive"
  )

  # Reproducibility focus
  result_repro <- ihcheterogeneity(
    data = test_data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    analysis_type = "reproducibility"
  )

  # Different analysis types should produce different outputs
  expect_false(identical(
    result_comp$interpretation$content,
    result_repro$interpretation$content
  ))
})

test_that("threshold parameters affect interpretation", {
  test_data <- read.csv(system.file("data", "ihc_heterogeneity.csv",
                                    package = "ClinicoPath"))

  # Low CV threshold (strict)
  result_strict <- ihcheterogeneity(
    data = test_data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    cv_threshold = 10.0,
    correlation_threshold = 0.90
  )

  # High CV threshold (lenient)
  result_lenient <- ihcheterogeneity(
    data = test_data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    cv_threshold = 40.0,
    correlation_threshold = 0.60
  )

  # Interpretations should differ based on thresholds
  expect_false(identical(
    result_strict$interpretation$content,
    result_lenient$interpretation$content
  ))
})
