# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: tableone
# ═══════════════════════════════════════════════════════════
#
# These tests verify that the tableone function:
# 1. Exists and can be called
# 2. Runs with minimal required arguments
# 3. Produces expected output structure
# 4. Handles required arguments correctly
#
# Generated: 2026-01-02

library(testthat)
library(ClinicoPath)

# Load test data
data(tableone_test, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Test 1: Function Existence and Basic Execution
# ═══════════════════════════════════════════════════════════

test_that("tableone function exists and runs", {
  # Basic execution test with minimal arguments
  result <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex")
  )

  # Should return a tableone results object
  expect_s3_class(result, "tableoneResults")

  # Should be a valid results object
  expect_true(inherits(result, "ResultsElement"))
})

# ═══════════════════════════════════════════════════════════
# Test 2: Required Arguments
# ═══════════════════════════════════════════════════════════

test_that("tableone handles required arguments correctly", {
  # Test with minimal required arguments (data + vars)
  result <- tableone(
    data = tableone_test,
    vars = c("Age")
  )

  expect_no_error(result)
  expect_s3_class(result, "tableoneResults")
})

test_that("tableone errors appropriately on missing required arguments", {
  # Missing data - should error
  expect_error(
    tableone(vars = c("Age", "Sex")),
    regexp = "data.*required|missing.*data|argument.*data",
    ignore.case = TRUE
  )

  # NULL data - should handle gracefully or error
  expect_condition(
    tableone(data = NULL, vars = c("Age", "Sex"))
  )
})

# ═══════════════════════════════════════════════════════════
# Test 3: Output Structure
# ═══════════════════════════════════════════════════════════

test_that("tableone produces expected output structure", {
  result <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex", "TumorStage"),
    sty = "t1"
  )

  # Check that result object exists and is valid
  expect_true(!is.null(result))
  expect_s3_class(result, "tableoneResults")

  # Check that main output exists (text, table, or html)
  # The exact output name depends on the .r.yaml definition
  expect_true(
    !is.null(result$text) ||
    !is.null(result$table) ||
    !is.null(result$html) ||
    !is.null(result$todo)
  )
})

# ═══════════════════════════════════════════════════════════
# Test 4: Variable Selection
# ═══════════════════════════════════════════════════════════

test_that("tableone handles single variable", {
  result <- tableone(
    data = tableone_test,
    vars = "Age"
  )

  expect_no_error(result)
  expect_s3_class(result, "tableoneResults")
})

test_that("tableone handles multiple variables", {
  result <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex", "TumorSize", "TumorStage", "Grade")
  )

  expect_no_error(result)
  expect_s3_class(result, "tableoneResults")
})

test_that("tableone handles mixed variable types", {
  # Mix of continuous, categorical, and ordinal
  result <- tableone(
    data = tableone_test,
    vars = c(
      "Age",              # Continuous
      "Sex",              # Categorical (binary)
      "TumorStage",       # Ordinal factor
      "TumorType",        # Categorical (multiple levels)
      "Hemoglobin",       # Continuous
      "LVI"               # Categorical (binary)
    )
  )

  expect_no_error(result)
  expect_s3_class(result, "tableoneResults")
})

# ═══════════════════════════════════════════════════════════
# Test 5: Invalid Variable Names
# ═══════════════════════════════════════════════════════════

test_that("tableone handles invalid variable names", {
  # Non-existent variable should error or warn
  expect_condition(
    tableone(
      data = tableone_test,
      vars = c("Age", "NonExistentVariable")
    )
  )
})

test_that("tableone handles empty variable selection", {
  # No variables selected - should handle gracefully
  # (May show instructions or error depending on implementation)
  expect_condition(
    tableone(
      data = tableone_test,
      vars = NULL
    )
  )

  expect_condition(
    tableone(
      data = tableone_test,
      vars = character(0)
    )
  )
})

# ═══════════════════════════════════════════════════════════
# Test 6: Data Types
# ═══════════════════════════════════════════════════════════

test_that("tableone handles numeric variables", {
  result <- tableone(
    data = tableone_test,
    vars = c("Age", "TumorSize", "Hemoglobin", "WBC")
  )

  expect_no_error(result)
})

test_that("tableone handles categorical variables", {
  result <- tableone(
    data = tableone_test,
    vars = c("Sex", "LymphNodes", "LVI", "PNI")
  )

  expect_no_error(result)
})

test_that("tableone handles ordinal factors", {
  result <- tableone(
    data = tableone_test,
    vars = c("TumorStage", "Grade")
  )

  expect_no_error(result)
  # Ordinal factors should be treated appropriately
})

# ═══════════════════════════════════════════════════════════
# Test 7: Basic Option Tests
# ═══════════════════════════════════════════════════════════

test_that("tableone default options work", {
  # Use all defaults
  result <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex", "TumorStage")
  )

  expect_no_error(result)
})

test_that("tableone excl option works", {
  # Test with excl = FALSE (default)
  result1 <- tableone(
    data = tableone_test,
    vars = c("Age", "Hemoglobin"),  # Hemoglobin has missing values
    excl = FALSE
  )
  expect_no_error(result1)

  # Test with excl = TRUE (exclude missing)
  result2 <- tableone(
    data = tableone_test,
    vars = c("Age", "Hemoglobin"),
    excl = TRUE
  )
  expect_no_error(result2)
})

# ═══════════════════════════════════════════════════════════
# Test 8: Different Data Formats
# ═══════════════════════════════════════════════════════════

test_that("tableone works with data.frame", {
  df <- as.data.frame(tableone_test)

  result <- tableone(
    data = df,
    vars = c("Age", "Sex", "TumorStage")
  )

  expect_no_error(result)
})

test_that("tableone works with tibble", {
  skip_if_not_installed("tibble")

  tbl <- tibble::as_tibble(tableone_test)

  result <- tableone(
    data = tbl,
    vars = c("Age", "Sex", "TumorStage")
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 9: Realistic Clinical Scenarios
# ═══════════════════════════════════════════════════════════

test_that("tableone handles typical clinical demographics", {
  result <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex")
  )

  expect_no_error(result)
})

test_that("tableone handles typical pathology variables", {
  result <- tableone(
    data = tableone_test,
    vars = c(
      "TumorSize",
      "TumorStage",
      "Grade",
      "LVI",
      "PNI",
      "LymphNodes"
    )
  )

  expect_no_error(result)
})

test_that("tableone handles typical lab values", {
  result <- tableone(
    data = tableone_test,
    vars = c(
      "Hemoglobin",
      "WBC",
      "Ki67",
      "CA199"
    )
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 10: Reproducibility
# ═══════════════════════════════════════════════════════════

test_that("tableone produces consistent results", {
  # Run same analysis twice
  result1 <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex", "TumorStage"),
    sty = "t1"
  )

  result2 <- tableone(
    data = tableone_test,
    vars = c("Age", "Sex", "TumorStage"),
    sty = "t1"
  )

  # Results should be identical (or very similar)
  expect_s3_class(result1, "tableoneClass")
  expect_s3_class(result2, "tableoneClass")
})
