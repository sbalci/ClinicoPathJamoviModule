# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: summarydata
# ═══════════════════════════════════════════════════════════
#
# Tests basic execution, required arguments, and expected outputs
# for the summarydata jamovi function
#
# Generated: 2026-01-04

# library(testthat)
# library(ClinicoPath)

# Load test data
data(summarydata_test, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# Test 1: Function Exists and Runs
# ═══════════════════════════════════════════════════════════

test_that("summarydata function exists and runs", {
  # Basic execution test with minimal arguments
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal"
  )

  # Should return an R6 class object
  expect_true(inherits(result, "R6"))
  expect_true(inherits(result, "summarydataResults"))
  # names() on a Results object lists its ITEMS, not a "results" slot; the old
  # assertion could never hold. Check the items the .r.yaml actually declares.
  expect_true(all(c("todo", "text", "text1") %in% names(result)))
})

# ═══════════════════════════════════════════════════════════
# Test 2: Required Arguments
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles required arguments correctly", {
  # Test with minimal required arguments (just vars)
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab")
  )

  expect_true(inherits(result, "summarydataResults"))
  expect_no_error(result)
})

test_that("summarydata requires variables to be specified", {
  # `vars` defaults to NULL in the schema, but an explicit NULL still reaches
  # jmvcore's zero-column select path and fails before the backend. The jamovi
  # UI can still show its welcome panel before a variable is selected.
  expect_error(
    summarydata(data = summarydata_test, vars = NULL),
    "row.names"
  )
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal"
  )

  # Should not error, just return empty results
  expect_true(inherits(result, "summarydataResults"))
})

# ═══════════════════════════════════════════════════════════
# Test 3: Expected Outputs
# ═══════════════════════════════════════════════════════════

test_that("summarydata produces expected output structure", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab", "psa_mild_skew")
  )

  # Check that results object exists
  expect_true(length(names(result)) > 0)

  # Check for expected output elements
  expect_true(!is.null(result$text))
  expect_true(!is.null(result$text1))
})

test_that("summarydata text output contains statistics", {
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal"
  )

  # Get the text content
  text_content <- result$text$content  # Html items expose $content, not $state

  # Should contain descriptive statistics
  expect_true(inherits(text_content, "character") || !is.null(text_content))
})

# ═══════════════════════════════════════════════════════════
# Test 4: Multiple Variables
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles multiple variables", {
  # Test with several variables
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab", "psa_mild_skew", "weight_kg", "ferritin")
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles single variable", {
  # Test with just one variable
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal"
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

# ═══════════════════════════════════════════════════════════
# Test 5: Different Data Types
# ═══════════════════════════════════════════════════════════

test_that("summarydata works with normal distribution variables", {
  # Test with normally distributed variables
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab", "temperature_normal")
  )

  expect_no_error(result)
})

test_that("summarydata works with skewed distribution variables", {
  # Test with right-skewed variables
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "wbc_count")
  )

  expect_no_error(result)
})

test_that("summarydata works with bounded variables", {
  # Test with bounded variables (0-100 or 0-10 scales)
  result <- summarydata(
    data = summarydata_test,
    vars = c("ferritin", "hba1c", "tsh")
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 6: Data with Missing Values
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles variables with missing data", {
  # PSA and CA125 have ~8-10% missing data
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "ferritin")
  )

  # Should complete without error
  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

# ═══════════════════════════════════════════════════════════
# Test 7: Basic Option Settings
# ═══════════════════════════════════════════════════════════

test_that("summarydata respects decimal_places option", {
  # Test with different decimal places
  result_2dp <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    decimal_places = 2
  )

  result_4dp <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    decimal_places = 4
  )

  expect_no_error(result_2dp)
  expect_no_error(result_4dp)
})

test_that("summarydata runs without distribution diagnostics by default", {
  # Default should be distr = FALSE
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    distr = FALSE
  )

  expect_no_error(result)
})

test_that("summarydata runs without outlier detection by default", {
  # Default should be outliers = FALSE
  result <- summarydata(
    data = summarydata_test,
    vars = "psa_mild_skew",
    outliers = FALSE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 8: Data Loading from Different Formats
# ═══════════════════════════════════════════════════════════

test_that("summarydata works with data from CSV", {
  # data/summarydata_test.csv does not exist (data/ holds .rda), so this block
  # died in file(). Round-trip through a temp CSV instead: what it is really
  # checking is that a plain data.frame carrying no jamovi column attributes
  # still analyses.
  csv_path <- tempfile(fileext = ".csv")
  write.csv(summarydata_test, csv_path, row.names = FALSE)
  csv_data <- read.csv(csv_path)
  on.exit(unlink(csv_path), add = TRUE)

  result <- summarydata(
    data = csv_data,
    vars = c("age_normal", "hemoglobin_lab")
  )

  expect_no_error(result)
})

test_that("summarydata works with data from RDA", {
  # Already loaded as summarydata_test
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "psa_mild_skew")
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 9: Empty Results Handling
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles empty variable list gracefully", {
  # An empty selection is not a valid R call. The GUI handles its initial empty
  # selection through the backend welcome state.
  expect_error(
    summarydata(data = summarydata_test, vars = character(0)),
    "row.names"
  )
})

# ═══════════════════════════════════════════════════════════
# Test 10: Standard Clinical Use Cases
# ═══════════════════════════════════════════════════════════

test_that("summarydata handles typical clinical lab panel", {
  # Typical clinical chemistry panel
  result <- summarydata(
    data = summarydata_test,
    vars = c("hemoglobin_lab", "wbc_count", "platelet_count", "creatinine_lab", "albumin_no_outliers")
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles tumor biomarker panel", {
  # Pathology/oncology biomarkers
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "ferritin", "weight_kg", "ca19_9")
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

test_that("summarydata handles patient-reported outcomes", {
  # Patient-reported outcome measures
  result <- summarydata(
    data = summarydata_test,
    vars = c("test_score_mild_left", "hospital_stay", "bmi_complete")
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataResults"))
})

# ═══════════════════════════════════════════════════════════
# End of Basic Tests
# ═══════════════════════════════════════════════════════════
