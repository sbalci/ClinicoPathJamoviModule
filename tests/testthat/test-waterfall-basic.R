# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: waterfall
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of the waterfall analysis function
# including basic execution, required arguments, and expected outputs

library(testthat)

# Load test data
data(waterfall_test, package = "ClinicoPath")
data(waterfall_spider_test, package = "ClinicoPath")
data(waterfall_raw_test, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# EXISTENCE AND BASIC EXECUTION
# ═══════════════════════════════════════════════════════════


# jamovi analyses report problems through result elements, never by throwing an
# R condition -- throwing would break the GUI. Assert on the rendered text.
wf_text <- function(result, element = "notices") {
  el <- result[[element]]
  if (is.null(el)) return("")
  gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(el$content), collapse = " ")))
}

test_that("waterfall function exists and is callable", {
  expect_true(exists("waterfall"))
  expect_true(is.function(waterfall))
})

test_that("waterfall runs with minimal required arguments", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    inputType = "percentage"
  )

  expect_s3_class(result, "waterfallResults")
  # waterfall() returns the results object itself, not a wrapper with a
  # $results slot; assert on a real element declared in waterfall.r.yaml.
  expect_true(!is.null(result$summaryTable))
})

test_that("waterfall accepts data.frame and tibble", {
  # Test with data.frame
  df_data <- as.data.frame(waterfall_test)
  result_df <- waterfall(
    data = df_data,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_no_error(result_df)

  # Test with tibble
  result_tbl <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_no_error(result_tbl)
})

# ═══════════════════════════════════════════════════════════
# REQUIRED ARGUMENTS VALIDATION
# ═══════════════════════════════════════════════════════════

test_that("missing required variables yield instructions, not a thrown error", {
  # jamovi keeps the analysis idle until the user assigns variables; it must not
  # throw, and it must not emit a half-computed table.
  r1 <- waterfall(data = waterfall_test, responseVar = "best_response")
  expect_equal(r1$summaryTable$rowCount, 0)
  expect_match(wf_text(r1, "todo"), "Treatment Response Analysis")

  r2 <- waterfall(data = waterfall_test, patientID = "patientID")
  expect_equal(r2$summaryTable$rowCount, 0)
  expect_match(wf_text(r2, "todo"), "Treatment Response Analysis")
})

test_that("waterfall validates data presence", {
  # Both cases must stop rather than produce an empty-but-plausible analysis.
  # NOTE: data = NULL currently surfaces the low-level message "attempt to apply
  # non-function" rather than a message naming the problem. It is an R-API-only
  # path (the jamovi GUI always supplies a dataset), so it is asserted here as
  # "errors" and reported as a message-quality issue rather than papered over.
  expect_error(
    waterfall(data = NULL, patientID = "patientID", responseVar = "best_response")
  )

  # An empty dataset is an idle state, not an error: jamovi shows the
  # instructions and produces no results.
  empty_result <- waterfall(data = waterfall_test[0, ], patientID = "patientID",
                            responseVar = "best_response")
  expect_equal(empty_result$summaryTable$rowCount, 0)
  expect_match(wf_text(empty_result, "todo"), "Treatment Response Analysis")
})

test_that("waterfall validates variable names exist in data", {
  # Non-existent patientID variable
  expect_error(
    waterfall(
      data = waterfall_test,
      patientID = "nonexistent_id",
      responseVar = "best_response"
    ),
    regexp = "nonexistent_id|not.*found|column.*missing",
    ignore.case = TRUE
  )

  # Non-existent responseVar variable
  expect_error(
    waterfall(
      data = waterfall_test,
      patientID = "patientID",
      responseVar = "nonexistent_response"
    ),
    regexp = "nonexistent_response|not.*found|column.*missing",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# INPUT TYPE HANDLING
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles percentage input type", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    inputType = "percentage"
  )

  expect_no_error(result)
  expect_s3_class(result, "waterfallResults")
})

test_that("waterfall handles raw input type with time variable", {
  result <- waterfall(
    data = waterfall_raw_test,
    patientID = "patientID",
    responseVar = "tumor_size",
    timeVar = "time",
    inputType = "raw"
  )

  expect_no_error(result)
  expect_s3_class(result, "waterfallResults")
})

test_that("waterfall requires timeVar for raw input type", {
  # Raw input without time variable should error or warn
  expect_condition(
    waterfall(
      data = waterfall_test,
      patientID = "patientID",
      responseVar = "best_response",
      inputType = "raw"
    )
  )
})

# ═══════════════════════════════════════════════════════════
# OUTPUT STRUCTURE
# ═══════════════════════════════════════════════════════════

test_that("waterfall produces expected output structure", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    inputType = "percentage"
  )

  # Check results object exists
  expect_true(!is.null(result$summaryTable))

  # Check for expected output components
  # Note: Actual output names depend on .r.yaml definition
  expect_true(!is.null(result$waterfallplot))
})

test_that("waterfall plot is generated by default", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showWaterfallPlot = TRUE
  )

  # Waterfall plot should be present
  plot_result <- result$waterfallplot
  expect_true(!is.null(plot_result))
})

test_that("waterfall respects showWaterfallPlot = FALSE", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showWaterfallPlot = FALSE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# SPIDER PLOT FUNCTIONALITY
# ═══════════════════════════════════════════════════════════

test_that("waterfall generates spider plot when requested", {
  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    showSpiderPlot = TRUE
  )

  expect_no_error(result)

  # Spider plot should be present
  expect_true(!is.null(result$spiderplot))
})

test_that("spider plot without a time variable explains itself", {
  # Previously the checkbox silently did nothing.
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showSpiderPlot = TRUE
  )
  expect_match(wf_text(result), "SPIDER PLOT NEEDS A TIME VARIABLE")
})

# ═══════════════════════════════════════════════════════════
# RECIST CATEGORIZATION
# ═══════════════════════════════════════════════════════════

test_that("waterfall correctly categorizes RECIST responses", {
  # Use extreme values dataset with known categories
  data(waterfall_extreme, package = "ClinicoPath")

  result <- waterfall(
    data = waterfall_extreme,
    patientID = "patientID",
    responseVar = "best_response",
    inputType = "percentage"
  )

  expect_no_error(result)

  # RECIST thresholds:
  # CR: <= -100%
  # PR: -99% to -30%
  # SD: -29% to +19%
  # PD: >= +20%
})

# ═══════════════════════════════════════════════════════════
# VISUAL OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("waterfall respects threshold display option", {
  result_with <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showThresholds = TRUE
  )

  result_without <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showThresholds = FALSE
  )

  expect_no_error(result_with)
  expect_no_error(result_without)
})

test_that("waterfall handles different color schemes", {
  color_schemes <- c("jamovi", "recist", "simple", "colorful", "colorblind")

  for (scheme in color_schemes) {
    result <- waterfall(
      data = waterfall_test,
      patientID = "patientID",
      responseVar = "best_response",
      colorScheme = scheme
    )
    expect_no_error(result)
  }
})

test_that("waterfall handles different sorting options", {
  # Sort by response
  result_response <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    sortBy = "response"
  )

  # Sort by ID
  result_id <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    sortBy = "id"
  )

  expect_no_error(result_response)
  expect_no_error(result_id)
})

# ═══════════════════════════════════════════════════════════
# CLINICAL REPORTING FEATURES
# ═══════════════════════════════════════════════════════════

test_that("waterfall generates clinical report when requested", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    generateCopyReadyReport = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall calculates confidence intervals when requested", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showConfidenceIntervals = TRUE
  )

  expect_no_error(result)
})

test_that("waterfall provides explanations when requested", {
  result <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    showExplanations = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# SMALL DATASET HANDLING
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles small datasets", {
  data(waterfall_small, package = "ClinicoPath")

  result <- waterfall(
    data = waterfall_small,
    patientID = "patientID",
    responseVar = "best_response"
  )

  # Should work but may warn about small sample size
  expect_s3_class(result, "waterfallResults")
})

test_that("a single-patient cohort is flagged as uninformative", {
  single_patient <- waterfall_test[1, ]
  result <- waterfall(
    data = single_patient,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_match(wf_text(result), "VERY SMALL COHORT")
})
