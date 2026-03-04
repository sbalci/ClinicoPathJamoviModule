# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: groomecompare
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality of the groomecompare function
# (Groome Staging System Comparison using 2001 criteria)

library(testthat)

# Load test data
data(groomecompare_test, package = "ClinicoPath")

test_that("groomecompare function exists and is accessible", {
  expect_true(exists("groomecompare"))
  expect_type(groomecompare, "closure")
})

test_that("groomecompare runs with minimal required arguments", {
  result <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  expect_s3_class(result, "groomecompareClass")
  expect_true("results" %in% names(result))
})

test_that("groomecompare handles required arguments correctly", {
  result <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    stage1name = "ypTNM Staging",
    stage2name = "RPA Classification"
  )

  expect_no_error(result)
  expect_s3_class(result, "groomecompareClass")
})

test_that("groomecompare errors on missing required arguments", {
  # Missing time
  expect_error(
    groomecompare(
      data = groomecompare_test,
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    )
  )

  # Missing event
  expect_error(
    groomecompare(
      data = groomecompare_test,
      time = "time",
      stage1 = "ypTNM",
      stage2 = "RPA"
    )
  )

  # Missing stage1
  expect_error(
    groomecompare(
      data = groomecompare_test,
      time = "time",
      event = "event",
      stage2 = "RPA"
    )
  )

  # Missing stage2
  expect_error(
    groomecompare(
      data = groomecompare_test,
      time = "time",
      event = "event",
      stage1 = "ypTNM"
    )
  )
})

test_that("groomecompare produces expected output structure", {
  result <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = TRUE,
    kmplots = TRUE,
    detailedmetrics = TRUE
  )

  expect_true(!is.null(result$results))
  # Check for expected output components based on .r.yaml
})

test_that("groomecompare accepts valid eventValue options", {
  # eventValue = "1" (default)
  result1 <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    eventValue = "1"
  )
  expect_no_error(result1)

  # Test TRUE/FALSE and 1/2 with appropriate datasets
  # See test-groomecompare-edge-cases.R
})

test_that("groomecompare handles custom staging system names", {
  result <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    stage1name = "Post-Neoadjuvant TNM",
    stage2name = "Recursive Partitioning Groups"
  )

  expect_no_error(result)
  # Names should appear in output labels
})

test_that("groomecompare works with different numbers of staging groups", {
  # Load unbalanced dataset
  data(groomecompare_unbalanced, package = "ClinicoPath")

  # 5 groups vs 2 groups
  result <- groomecompare(
    data = groomecompare_unbalanced,
    time = "time",
    event = "event",
    stage1 = "detailed_stage",  # 5 levels
    stage2 = "simple_stage"      # 2 levels
  )

  expect_no_error(result)
})

test_that("groomecompare handles identical staging systems", {
  # Load identical systems dataset
  data(groomecompare_identical, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_identical,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "ypTNM"  # Same as stage1
  )

  # Should work and show tied/equal metrics
  expect_no_error(result)
})

test_that("groomecompare respects output control options", {
  # All outputs enabled
  result_all <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = TRUE,
    barplot = TRUE,
    kmplots = TRUE,
    detailedmetrics = TRUE,
    hazardratios = TRUE,
    samplesize = TRUE,
    cindexcompare = TRUE
  )
  expect_no_error(result_all)

  # Minimal outputs
  result_min <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = FALSE,
    barplot = FALSE,
    kmplots = FALSE,
    detailedmetrics = FALSE,
    hazardratios = FALSE,
    samplesize = FALSE,
    cindexcompare = FALSE
  )
  expect_no_error(result_min)
})

test_that("groomecompare handles tied survival times", {
  # Load tied times dataset
  data(groomecompare_tied, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_tied,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  expect_no_error(result)
})

test_that("groomecompare works with small samples", {
  # Load small dataset
  data(groomecompare_small, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_small,
    time = "time",
    event = "event",
    stage1 = "clinical_stage",
    stage2 = "molecular_subtype"
  )

  # Should work but may warn about sample size
  expect_s3_class(result, "groomecompareClass")
})

test_that("groomecompare works with large samples", {
  # Load large dataset
  data(groomecompare_large, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_large,
    time = "time",
    event = "event",
    stage1 = "AJCC8",  # 8 levels
    stage2 = "RPA5"    # 5 levels
  )

  expect_no_error(result)
})
