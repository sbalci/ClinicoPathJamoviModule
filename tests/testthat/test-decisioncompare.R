context("Medical Decision Test Comparison")

# Load required data
if (requireNamespace("ClinicoPath", quietly = TRUE)) {
  data("histopathology", package = "ClinicoPath")
}

test_that("decisioncompare works with basic 2 test comparison", {
  skip_if_not_installed("ClinicoPath")
  
  # Test basic functionality with 2 tests
  result <- ClinicoPath::decisioncompare(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test", 
    test1Positive = "1",
    test2 = "Measurement1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL
  )
  
  # Check that result is returned
  expect_true(!is.null(result))
  
  # Check that comparison table exists and has content
  expect_true(result$comparisonTable$rowCount() > 0)
  
  # Check that individual test tables exist
  expect_true(result$cTable1$rowCount() > 0)
  expect_true(result$cTable2$rowCount() > 0)
})

test_that("decisioncompare works with confidence intervals", {
  skip_if_not_installed("ClinicoPath")
  
  result <- ClinicoPath::decisioncompare(
    data = histopathology,
    gold = "Golden Standart", 
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Measurement1", 
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    ci = TRUE
  )
  
  # Check CI tables are visible when requested
  expect_true(result$epirTable1$visible)
  expect_true(result$epirTable2$visible)
})

test_that("decisioncompare works with statistical comparison", {
  skip_if_not_installed("ClinicoPath") 
  
  result <- ClinicoPath::decisioncompare(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1", 
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Measurement1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    statComp = TRUE
  )
  
  # Check that statistical comparison tables have content
  expect_true(result$mcnemarTable$rowCount() > 0)
  expect_true(result$diffTable$rowCount() > 0)
})

test_that("decisioncompare works with 3 tests", {
  skip_if_not_installed("ClinicoPath")
  
  result <- ClinicoPath::decisioncompare(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test", 
    test1Positive = "1",
    test2 = "Measurement1",
    test2Positive = "1", 
    test3 = "Measurement2",
    test3Positive = "1",
    statComp = TRUE
  )
  
  # Check that all 3 test tables exist
  expect_true(result$cTable1$rowCount() > 0)
  expect_true(result$cTable2$rowCount() > 0)
  expect_true(result$cTable3$rowCount() > 0)
  
  # Check comparison table has 3 tests
  expect_equal(result$comparisonTable$rowCount(), 3)
  
  # Check that statistical comparisons include multiple pairs
  expect_true(result$mcnemarTable$rowCount() >= 3) # Should have 3 pairwise comparisons
})

test_that("decisioncompare works with footnotes and original data", {
  skip_if_not_installed("ClinicoPath")
  
  result <- ClinicoPath::decisioncompare(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1", 
    test2 = "Measurement1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    fnote = TRUE,
    od = TRUE
  )
  
  # Check that original data is displayed
  expect_true(!is.null(result$text1$content))
  expect_true(!is.null(result$text2$content))
})

test_that("decisioncompare handles missing data gracefully", {
  skip_if_not_installed("ClinicoPath") 
  
  # Create data with some missing values
  test_data <- histopathology[1:50, ]
  test_data$`Golden Standart`[1:5] <- NA
  
  result <- ClinicoPath::decisioncompare(
    data = test_data,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Measurement1", 
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL
  )
  
  # Should still work with missing data removed
  expect_true(!is.null(result))
  expect_true(result$comparisonTable$rowCount() > 0)
})

test_that("decisioncompare fails gracefully with invalid input", {
  skip_if_not_installed("ClinicoPath")
  
  # Test with no gold standard
  expect_error(
    ClinicoPath::decisioncompare(
      data = histopathology,
      test1 = "New Test",
      test1Positive = "1",
      test2 = NULL,
      test2Positive = NULL,
      test3 = NULL,
      test3Positive = NULL
    )
  )
})

test_that("decisioncompare plotting functionality works", {
  skip_if_not_installed("ClinicoPath")
  
  result <- ClinicoPath::decisioncompare(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1", 
    test2 = "Measurement1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    plot = TRUE
  )
  
  # Check that plot object exists
  expect_true(!is.null(result$plot1))
  expect_true(result$plot1$visible)
})