
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

test_that("a small cohort is rejected, and a constant region still yields a real ICC", {
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

  # A zero-variance region is a measurement that cannot tell the cases apart:
  # the absolute-agreement ICC is defined (and low). Until 2026-09-18 the module
  # replaced it with a mean Spearman correlation; it is now computed and checked
  # against psych, and the constant region is named in the correlation note.
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
  icc_row <- repro[grepl("absolute agreement", repro$metric), , drop = FALSE]
  expect_equal(nrow(icc_row), 1)
  expect_equal(icc_row$value, psych::ICC(as.matrix(fb_data), lmer = FALSE)$results$ICC[2], tolerance = 1e-8)
  expect_false(any(grepl("Mean correlation", repro$metric)))
  notes <- vapply(res$reproducibilitytable$notes, function(n) n$note, character(1))
  expect_true(any(grepl("same value in every case are not defined: reg1", notes, fixed = TRUE)))
})

test_that("the ICC is computed without the psych package", {
  # 2026-09-18: ICC(2,1)/ICC(3,1) come from closed-form mean squares (psych::ICC
  # fitted an n-level aov and took 73 s at n = 2000). There is no
  # "psych not available" path any more, so no such note can appear.
  src <- testthat::test_path("..", "..", "R", "ihcheterogeneity.b.R")
  skip_if_not(file.exists(src))
  code <- sub("#.*$", "", readLines(src, warn = FALSE))
  expect_false(any(grepl("psych::", code, fixed = TRUE)))
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

  # The focus decides which additional tables are computed and shown (it no
  # longer adds a note to the interpretation - the Comprehensive note listed
  # modules that had not run).
  expect_true(result_comp$variancetable$visible)
  expect_false(result_repro$variancetable$visible)
  expect_true(result_comp$samplesizetable$visible)
  expect_false(result_repro$samplesizetable$visible)
  expect_gt(result_comp$samplesizetable$rowCount, 0)
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
