# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: statsplot2
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("statsplot2 function exists and loads", {

  expect_true(exists("statsplot2"))
})

test_that("statsplot2 runs with minimal required arguments", {

  data(statsplot2_test, package = "ClinicoPath")

  # Minimal required arguments (continuous vs categorical)
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 produces expected output structure", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    direction = "independent"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))
})

test_that("statsplot2 handles continuous vs categorical (violin plot)", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    direction = "independent",
    distribution = "p"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles categorical vs categorical (bar chart)", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "response_status",
    group = "treatment",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles continuous vs continuous (scatter)", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "biomarker_level",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles parametric distribution", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "qol_score",
    group = "treatment",
    distribution = "p"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles nonparametric distribution", {

  data(statsplot2_skewed)

  result <- statsplot2(
    data = statsplot2_skewed,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "np"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles robust distribution", {

  data(statsplot2_outliers)

  result <- statsplot2(
    data = statsplot2_outliers,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "r"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles Bayesian distribution", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "qol_score",
    group = "treatment",
    distribution = "bf"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles independent direction", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles repeated direction", {

  data(statsplot2_repeated)

  result <- statsplot2(
    data = statsplot2_repeated,
    dep = "symptom_severity",
    group = "timepoint",
    direction = "repeated"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles split-by variable (grvar)", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "sex",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles custom labels", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    plotTitle = "Custom Title",
    xlab = "X Label",
    ylab = "Y Label"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles multi-level categorical grouping", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "tumor_stage",
    grvar = "age_group",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles two-level factors", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "sex",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles three-level factors", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles multi-level factors", {

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "age_group",
    direction = "independent"
  )

  expect_s3_class(result, "statsplot2Results")
})
