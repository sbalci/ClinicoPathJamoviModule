# ═══════════════════════════════════════════════════════════
# Integration Tests: waterfall
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other functions and realistic workflows

library(testthat)
library(ClinicoPath)

# Load test data
data(waterfall_test, package = "ClinicoPath")
data(waterfall_spider_test, package = "ClinicoPath")
data(waterfall_phase2, package = "ClinicoPath")
data(waterfall_raw_test, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# REALISTIC CLINICAL WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall complete Phase II trial analysis workflow", {
  # Typical Phase II oncology trial workflow
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "cohort",
    inputType = "percentage",
    sortBy = "response",
    showThresholds = TRUE,
    colorBy = "recist",
    colorScheme = "recist",
    generateCopyReadyReport = TRUE,
    showClinicalSignificance = TRUE,
    showConfidenceIntervals = TRUE
  )

  expect_no_error(result)
  expect_s3_class(result, "waterfallClass")
})

test_that("waterfall biomarker correlation study workflow", {
  # Workflow: Analyze response by biomarker status
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "pdl1_status",
    colorBy = "group",
    colorScheme = "colorful",
    showWaterfallPlot = TRUE,
    generateCopyReadyReport = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall dose escalation study workflow", {
  # Workflow: Compare response across dose levels
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "cohort",
    colorBy = "group",
    sortBy = "response",
    showThresholds = TRUE,
    showConfidenceIntervals = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# DATA TRANSFORMATION WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: raw measurements to spider plot", {
  # Workflow: Process raw measurements and generate spider plot
  result <- waterfall(
    data = waterfall_raw_test,
    patientID = "patientID",
    responseVar = "tumor_size",
    timeVar = "time",
    groupVar = "treatment",
    inputType = "raw",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    timeUnitLabel = "months"
  )

  expect_no_error(result)
})

test_that("waterfall workflow: longitudinal data to best response", {
  # Workflow: Extract best response from longitudinal data
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    groupVar = "treatment",
    inputType = "percentage",
    showWaterfallPlot = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# PUBLICATION-READY OUTPUT WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall publication-ready waterfall plot", {
  # Clean plot for manuscript
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    sortBy = "response",
    showThresholds = TRUE,
    colorBy = "recist",
    colorScheme = "recist",
    barAlpha = 0.9,
    barWidth = 0.8,
    showWaterfallPlot = TRUE,
    showSpiderPlot = FALSE,
    generateCopyReadyReport = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall publication-ready combined plots", {
  # Both waterfall and spider for comprehensive figure
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    groupVar = "treatment",
    sortBy = "response",
    showThresholds = TRUE,
    colorScheme = "colorblind",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    spiderColorBy = "response",
    spiderColorScheme = "colorblind",
    timeUnitLabel = "months"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# PROGRESSIVE ANALYSIS WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: basic to advanced analysis", {
  # Step 1: Basic waterfall
  result_basic <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_no_error(result_basic)

  # Step 2: Add grouping
  result_grouped <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )
  expect_no_error(result_grouped)

  # Step 3: Full analysis with reporting
  result_full <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    colorBy = "group",
    generateCopyReadyReport = TRUE,
    showConfidenceIntervals = TRUE
  )
  expect_no_error(result_full)
})

# ═══════════════════════════════════════════════════════════
# DATA SUBSETTING WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: subset by response category", {
  # Analyze only responders (CR/PR)
  responders <- waterfall_test[waterfall_test$best_response <= -30, ]

  result <- waterfall(
    data = responders,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  expect_s3_class(result, "waterfallClass")
})

test_that("waterfall workflow: subset by treatment group", {
  # Analyze single treatment arm
  single_arm <- waterfall_test[waterfall_test$treatment == "Monotherapy", ]

  result <- waterfall(
    data = single_arm,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_s3_class(result, "waterfallClass")
})

# ═══════════════════════════════════════════════════════════
# COMPARATIVE ANALYSIS WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: compare color schemes", {
  # Generate same analysis with different color schemes
  schemes <- c("jamovi", "recist", "colorblind")

  results <- lapply(schemes, function(scheme) {
    waterfall(
      data = waterfall_test,
      patientID = "patientID",
      responseVar = "best_response",
      groupVar = "treatment",
      colorBy = "recist",
      colorScheme = scheme
    )
  })

  # All should complete successfully
  expect_true(all(sapply(results, function(r) inherits(r, "waterfallClass"))))
})

test_that("waterfall workflow: RECIST vs group-based coloring", {
  # Compare RECIST-based and group-based coloring
  result_recist <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    colorBy = "recist"
  )

  result_group <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    colorBy = "group"
  )

  expect_no_error(result_recist)
  expect_no_error(result_group)
})

# ═══════════════════════════════════════════════════════════
# MULTI-ARM TRIAL WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: three-arm comparison", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    colorBy = "group",
    colorScheme = "colorful",
    sortBy = "response",
    generateCopyReadyReport = TRUE,
    showConfidenceIntervals = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall workflow: dose-level comparison", {
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "cohort",
    colorBy = "group",
    sortBy = "response",
    showConfidenceIntervals = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# TIME-SERIES ANALYSIS WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: response trajectory analysis", {
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    groupVar = "treatment",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    spiderColorBy = "response",
    timeUnitLabel = "months"
  )

  expect_no_error(result)
})

test_that("waterfall workflow: early vs late response", {
  # Filter to early timepoints only
  early_response <- waterfall_spider_test[waterfall_spider_test$time <= 4, ]

  result <- waterfall(
    data = early_response,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    showSpiderPlot = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# REPORTING AND DOCUMENTATION WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: complete clinical report", {
  result <- waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "cohort",
    generateCopyReadyReport = TRUE,
    showClinicalSignificance = TRUE,
    showConfidenceIntervals = TRUE,
    showExplanations = TRUE,
    enableGuidedMode = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall workflow: minimal report for quick check", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    generateCopyReadyReport = FALSE,
    showConfidenceIntervals = FALSE,
    showExplanations = FALSE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# REAL-WORLD DATA SCENARIOS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: handle incomplete follow-up", {
  # Some patients have only baseline, some have multiple follow-ups
  incomplete_data <- waterfall_spider_test[
    !(waterfall_spider_test$patientID == "PT001" & waterfall_spider_test$time > 4), ]

  result <- waterfall(
    data = incomplete_data,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE
  )

  expect_s3_class(result, "waterfallClass")
})

test_that("waterfall workflow: mixed measurement schedules", {
  # Different patients measured at different timepoints (realistic scenario)
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    showSpiderPlot = TRUE,
    timeUnitLabel = "months"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# PERFORMANCE TESTING
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: large dataset performance", {
  data(waterfall_large, package = "ClinicoPath")

  # Test that large dataset completes in reasonable time
  start_time <- Sys.time()

  result <- waterfall(
    data = waterfall_large,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment",
    showWaterfallPlot = TRUE
  )

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  expect_no_error(result)
  # Should complete within reasonable time (adjust threshold as needed)
  expect_lt(elapsed, 30)  # 30 seconds threshold
})

# ═══════════════════════════════════════════════════════════
# ERROR RECOVERY WORKFLOWS
# ═══════════════════════════════════════════════════════════

test_that("waterfall workflow: recover from data issues", {
  # Start with problematic data
  data(waterfall_missing, package = "ClinicoPath")

  # Should still produce usable output despite missing values
  result <- waterfall(
    data = waterfall_missing,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  expect_s3_class(result, "waterfallClass")
})
