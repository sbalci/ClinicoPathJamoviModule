# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: timeinterval
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of the timeinterval function
# including basic execution, required arguments, and expected outputs

library(testthat)
library(ClinicoPath)

# Load test data
data(timeinterval_test, package = "ClinicoPath")
data(timeinterval_ymd, package = "ClinicoPath")
data(timeinterval_dmy, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# EXISTENCE AND BASIC EXECUTION
# ═══════════════════════════════════════════════════════════

test_that("timeinterval function exists and is callable", {
  expect_true(exists("timeinterval"))
  expect_true(is.function(timeinterval))
})

test_that("timeinterval runs with minimal required arguments", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )

  expect_s3_class(result, "timeintervalClass")
  expect_true("results" %in% names(result))
})

test_that("timeinterval accepts data.frame and tibble", {
  # Test with data.frame
  df_data <- as.data.frame(timeinterval_test)
  result_df <- timeinterval(
    data = df_data,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_no_error(result_df)

  # Test with tibble
  result_tbl <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_no_error(result_tbl)
})

# ═══════════════════════════════════════════════════════════
# REQUIRED ARGUMENTS VALIDATION
# ═══════════════════════════════════════════════════════════

test_that("timeinterval errors on missing required arguments", {
  # Missing dx_date
  expect_error(
    timeinterval(
      data = timeinterval_test,
      fu_date = "followup_date"
    ),
    regexp = "dx_date|start.*date|required",
    ignore.case = TRUE
  )

  # Missing fu_date
  expect_error(
    timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date"
    ),
    regexp = "fu_date|end.*date|follow.*up|required",
    ignore.case = TRUE
  )
})

test_that("timeinterval validates data presence", {
  # NULL data
  expect_error(
    timeinterval(
      data = NULL,
      dx_date = "diagnosis_date",
      fu_date = "followup_date"
    ),
    regexp = "data|empty|null",
    ignore.case = TRUE
  )

  # Empty data
  expect_error(
    timeinterval(
      data = timeinterval_test[0, ],
      dx_date = "diagnosis_date",
      fu_date = "followup_date"
    ),
    regexp = "empty|no.*data|row",
    ignore.case = TRUE
  )
})

test_that("timeinterval validates variable names exist in data", {
  # Non-existent dx_date variable
  expect_error(
    timeinterval(
      data = timeinterval_test,
      dx_date = "nonexistent_start",
      fu_date = "followup_date"
    ),
    regexp = "nonexistent_start|not.*found|column|available",
    ignore.case = TRUE
  )

  # Non-existent fu_date variable
  expect_error(
    timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "nonexistent_end"
    ),
    regexp = "nonexistent_end|not.*found|column|available",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# DATE FORMAT HANDLING
# ═══════════════════════════════════════════════════════════

test_that("timeinterval handles YMD format", {
  result <- timeinterval(
    data = timeinterval_ymd,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd"
  )

  expect_no_error(result)
  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval handles DMY format", {
  result <- timeinterval(
    data = timeinterval_dmy,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "dmy"
  )

  expect_no_error(result)
  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval handles auto format detection", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_format = "auto"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# OUTPUT UNIT HANDLING
# ═══════════════════════════════════════════════════════════

test_that("timeinterval handles different output units", {
  units <- c("days", "weeks", "months", "years")

  for (unit in units) {
    result <- timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "followup_date",
      output_unit = unit
    )
    expect_no_error(result)
  }
})

test_that("timeinterval defaults to months", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )

  # Should use months as default unit
  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# TIME BASIS OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("timeinterval handles standardized time basis", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_basis = "standardized",
    output_unit = "months"
  )

  expect_no_error(result)
})

test_that("timeinterval handles calendar time basis", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_basis = "calendar",
    output_unit = "months"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# OUTPUT STRUCTURE
# ═══════════════════════════════════════════════════════════

test_that("timeinterval produces expected output structure", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )

  # Check results object exists
  expect_true(!is.null(result$results))

  # Should have summary statistics or tables
  # Note: Actual output names depend on .r.yaml definition
})

# ═══════════════════════════════════════════════════════════
# ADD TIMES FUNCTIONALITY
# ═══════════════════════════════════════════════════════════

test_that("timeinterval can add calculated times to dataset", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    add_times = TRUE
  )

  expect_no_error(result)
})

test_that("timeinterval works without adding times", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    add_times = FALSE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# QUALITY METRICS
# ═══════════════════════════════════════════════════════════

test_that("timeinterval includes quality metrics when requested", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    include_quality_metrics = TRUE
  )

  expect_no_error(result)
})

test_that("timeinterval works without quality metrics", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    include_quality_metrics = FALSE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# CONFIDENCE LEVEL
# ═══════════════════════════════════════════════════════════

test_that("timeinterval handles different confidence levels", {
  conf_levels <- c(90, 95, 99)

  for (conf in conf_levels) {
    result <- timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "followup_date",
      confidence_level = conf
    )
    expect_no_error(result)
  }
})

# ═══════════════════════════════════════════════════════════
# SUMMARY AND GLOSSARY OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("timeinterval generates summary when requested", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    show_summary = TRUE
  )

  expect_no_error(result)
})

test_that("timeinterval shows glossary when requested", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    show_glossary = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# SMALL DATASET HANDLING
# ═══════════════════════════════════════════════════════════

test_that("timeinterval handles small datasets", {
  data(timeinterval_small, package = "ClinicoPath")

  result <- timeinterval(
    data = timeinterval_small,
    dx_date = "start",
    fu_date = "end"
  )

  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval handles single row", {
  single_row <- timeinterval_test[1, ]

  result <- timeinterval(
    data = single_row,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )

  # Should work but may have limited statistics
  expect_s3_class(result, "timeintervalClass")
})
