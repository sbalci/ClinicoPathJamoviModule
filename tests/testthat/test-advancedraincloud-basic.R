# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: advancedraincloud
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("advancedraincloud function exists and loads", {
  devtools::load_all()

  # Function should be accessible
  expect_true(exists("advancedraincloud"))
})

test_that("advancedraincloud runs with minimal required arguments", {
  devtools::load_all()

  # Load test data
  data(advancedraincloud_test, package = "ClinicoPath")
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  # Basic execution with minimal arguments
  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  # Should return a results object
  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud produces expected output structure", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    show_statistics = TRUE
  )

  # Check for main output elements
  expect_true(!is.null(result$plot))

  # Check that statistics table exists when requested
  expect_true(!is.null(result$statistics))
})

test_that("advancedraincloud handles cross-sectional data", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    rain_side = "l"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles longitudinal data", {
  devtools::load_all()

  data(advancedraincloud_test)

  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    id_var = "patient_id",
    show_longitudinal = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles different rain_side options", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  # Test all rain_side options
  rain_sides <- c("l", "r", "f", "f1x1", "f2x2")

  for (side in rain_sides) {
    result <- advancedraincloud(
      data = baseline_data,
      y_var = "pain_score",
      x_var = "treatment",
      rain_side = side
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for rain_side:", side))
  }
})

test_that("advancedraincloud handles Likert mode", {
  devtools::load_all()

  data(advancedraincloud_test)

  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "satisfaction",
    x_var = "treatment",
    likert_mode = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles optional fill variable", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Without fill_var
  result1 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint"
  )
  expect_s3_class(result1, "advancedraincloudResults")

  # With fill_var
  result2 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment"
  )
  expect_s3_class(result2, "advancedraincloudResults")
})

test_that("advancedraincloud handles color palettes", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  palettes <- c("clinical", "viridis", "set1", "set2", "pastel", "dark2")

  for (pal in palettes) {
    result <- advancedraincloud(
      data = baseline_data,
      y_var = "pain_score",
      x_var = "treatment",
      color_palette = pal
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for palette:", pal))
  }
})

test_that("advancedraincloud respects visual parameter settings", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    point_size = 2.5,
    point_alpha = 0.5,
    violin_alpha = 0.8,
    boxplot_width = 0.2,
    jitter_seed = 123
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles custom labels", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    plot_title = "Custom Title",
    x_label = "Treatment Groups",
    y_label = "Pain Score (VAS 0-100)"
  )

  expect_s3_class(result, "advancedraincloudResults")
})
