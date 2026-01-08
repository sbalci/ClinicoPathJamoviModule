# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: waterfall
# ═══════════════════════════════════════════════════════════
#
# Tests all argument combinations and option interactions

library(testthat)
library(ClinicoPath)

# Load test data
data(waterfall_test, package = "ClinicoPath")
data(waterfall_spider_test, package = "ClinicoPath")
data(waterfall_phase2, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# GROUPING VARIABLE COMBINATIONS
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles groupVar option", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  expect_no_error(result)
  expect_s3_class(result, "waterfallClass")
})

test_that("waterfall handles factor vs character groupVar", {
  # Character groupVar
  test_data_char <- waterfall_test
  test_data_char$treatment <- as.character(test_data_char$treatment)

  result_char <- waterfall(
    data = test_data_char,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  # Factor groupVar
  test_data_factor <- waterfall_test
  test_data_factor$treatment <- as.factor(test_data_factor$treatment)

  result_factor <- waterfall(
    data = test_data_factor,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  expect_no_error(result_char)
  expect_no_error(result_factor)
})

test_that("waterfall handles many groups", {
  # Create data with many groups
  many_groups_data <- waterfall_test
  many_groups_data$many_groups <- sample(
    paste0("Group_", 1:15),
    nrow(many_groups_data),
    replace = TRUE
  )

  result <- waterfall(
    data = many_groups_data,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "many_groups"
  )

  # Should work but may warn about too many groups
  expect_s3_class(result, "waterfallClass")
})

# ═══════════════════════════════════════════════════════════
# COLOR SCHEME COMBINATIONS
# ═══════════════════════════════════════════════════════════

test_that("waterfall colorBy RECIST categories", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    colorBy = "recist",
    colorScheme = "recist"
  )

  expect_no_error(result)
})

test_that("waterfall colorBy patient groups", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    colorBy = "group",
    colorScheme = "colorful"
  )

  expect_no_error(result)
})

test_that("waterfall handles all color scheme combinations", {
  color_schemes <- c("jamovi", "recist", "simple", "colorful", "colorblind")
  color_by_options <- c("recist", "group")

  for (scheme in color_schemes) {
    for (color_by in color_by_options) {
      result <- waterfall(
        data = waterfall_test,
        patientID = "patientID",
        responseVar = "best_response",
        groupVar = "treatment",
        colorBy = color_by,
        colorScheme = scheme
      )
      expect_no_error(result)
    }
  }
})

# ═══════════════════════════════════════════════════════════
# VISUAL CUSTOMIZATION OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles bar transparency settings", {
  # Transparent bars
  result_transparent <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barAlpha = 0.5
  )

  # Opaque bars
  result_opaque <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barAlpha = 1.0
  )

  expect_no_error(result_transparent)
  expect_no_error(result_opaque)
})

test_that("waterfall handles bar width settings", {
  # Narrow bars
  result_narrow <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barWidth = 0.3
  )

  # Wide bars
  result_wide <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barWidth = 0.9
  )

  expect_no_error(result_narrow)
  expect_no_error(result_wide)
})

test_that("waterfall handles median and CI options", {
  # With median and CI
  result_full <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showMedian = TRUE,
    showCI = TRUE
  )

  # Median only
  result_median <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showMedian = TRUE,
    showCI = FALSE
  )

  expect_no_error(result_full)
  expect_no_error(result_median)
})

test_that("waterfall handles outlier labeling options", {
  # With outlier labels
  result_labeled <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    labelOutliers = TRUE,
    minResponseForLabel = 50
  )

  # Different threshold
  result_threshold <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    labelOutliers = TRUE,
    minResponseForLabel = 30
  )

  expect_no_error(result_labeled)
  expect_no_error(result_threshold)
})

# ═══════════════════════════════════════════════════════════
# SPIDER PLOT OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("waterfall spider plot color by options", {
  # Color by response
  result_response <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    showSpiderPlot = TRUE,
    spiderColorBy = "response"
  )

  # Color by group
  result_group <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    groupVar = "treatment",
    showSpiderPlot = TRUE,
    spiderColorBy = "group"
  )

  expect_no_error(result_response)
  expect_no_error(result_group)
})

test_that("waterfall spider plot color schemes", {
  spider_schemes <- c("classic", "jamovi", "colorful", "colorblind")

  for (scheme in spider_schemes) {
    result <- waterfall(
      data = waterfall_spider_test,
      patientID = "patientID",
      responseVar = "pct_change",
      timeVar = "time",
      showSpiderPlot = TRUE,
      spiderColorScheme = scheme
    )
    expect_no_error(result)
  }
})

test_that("waterfall spider plot time unit labels", {
  time_units <- c("generic", "days", "weeks", "months", "years")

  for (unit in time_units) {
    result <- waterfall(
      data = waterfall_spider_test,
      patientID = "patientID",
      responseVar = "pct_change",
      timeVar = "time",
      showSpiderPlot = TRUE,
      timeUnitLabel = unit
    )
    expect_no_error(result)
  }
})

# ═══════════════════════════════════════════════════════════
# CLINICAL REPORTING COMBINATIONS
# ═══════════════════════════════════════════════════════════

test_that("waterfall all reporting options enabled", {
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    generateCopyReadyReport = TRUE,
    showClinicalSignificance = TRUE,
    showConfidenceIntervals = TRUE,
    showExplanations = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall reporting with grouping", {
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "cohort",
    generateCopyReadyReport = TRUE,
    showConfidenceIntervals = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall guided mode option", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    enableGuidedMode = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# BOTH PLOT TYPES SIMULTANEOUSLY
# ═══════════════════════════════════════════════════════════

test_that("waterfall generates both waterfall and spider plots", {
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    groupVar = "treatment",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE
  )

  expect_no_error(result)

  # Both plots should be present
  expect_true("waterfallPlot" %in% names(result$results) ||
              "plot" %in% names(result$results))
  expect_true("spiderPlot" %in% names(result$results) ||
              "spider" %in% names(result$results))
})

# ═══════════════════════════════════════════════════════════
# COMPREHENSIVE OPTION COMBINATION
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles maximal option combination", {
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    groupVar = "treatment",
    inputType = "percentage",
    sortBy = "response",
    showThresholds = TRUE,
    labelOutliers = TRUE,
    showMedian = TRUE,
    showCI = TRUE,
    minResponseForLabel = 40,
    colorBy = "group",
    colorScheme = "colorful",
    barAlpha = 0.8,
    barWidth = 0.7,
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    spiderColorBy = "group",
    spiderColorScheme = "colorful",
    timeUnitLabel = "months",
    generateCopyReadyReport = TRUE,
    showClinicalSignificance = TRUE,
    showConfidenceIntervals = TRUE,
    enableGuidedMode = FALSE,
    showExplanations = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# VARIABLE TYPE HANDLING
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles numeric vs character patient IDs", {
  # Numeric IDs
  numeric_data <- waterfall_test
  numeric_data$patientID_numeric <- 1:nrow(numeric_data)

  result_numeric <- waterfall(
    data = numeric_data,
    patientID = "patientID_numeric",
    responseVar = "best_response"
  )

  # Character IDs (original)
  result_char <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result_numeric)
  expect_no_error(result_char)
})

test_that("waterfall handles integer vs numeric response values", {
  # Integer response
  int_data <- waterfall_test
  int_data$best_response_int <- as.integer(int_data$best_response)

  result_int <- waterfall(
    data = int_data,
    patientID = "patientID",
    responseVar = "best_response_int"
  )

  expect_no_error(result_int)
})
