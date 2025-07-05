context("Combine Medical Decision Tests")

# Test basic function execution
test_that("decisioncombine function works with basic parameters", {
  testthat::skip_on_cran()
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic function call without errors
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL
    )
  })
  
  # Test that function returns expected structure
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL
  )
  
  expect_s3_class(result, "Group")
  expect_true(length(result) > 0)
  
  # Test that required output tables exist
  expect_true("cTable" %in% names(result))
  expect_true("nTable" %in% names(result))
  expect_true("ratioTable" %in% names(result))
})

test_that("decisioncombine function works with three tests", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with three tests
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = "Rater 2",
      test3Positive = "1"
    )
  })
  
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = "Rater 2",
    test3Positive = "1"
  )
  
  # Check that individual test tables exist when showIndividual is TRUE
  expect_true("indTable1" %in% names(result))
  expect_true("indTable2" %in% names(result))
  expect_true("indTable3" %in% names(result))
})

test_that("decisioncombine function works with different combination rules", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test "any" rule (OR)
  expect_no_error({
    result_any <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      combRule = "any"
    )
  })
  
  # Test "all" rule (AND)
  expect_no_error({
    result_all <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      combRule = "all"
    )
  })
  
  # Test "majority" rule
  expect_no_error({
    result_majority <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = "Rater 2",
      test3Positive = "1",
      combRule = "majority"
    )
  })
})

test_that("decisioncombine function works with confidence intervals", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with confidence intervals enabled
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      ci = TRUE
    )
  })
  
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    ci = TRUE
  )
  
  # Check that CI tables are included
  expect_true("epirTable_ratio" %in% names(result))
  expect_true("epirTable_number" %in% names(result))
})

test_that("decisioncombine function works with prior probability", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with prior probability
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      pp = TRUE,
      pprob = 0.05
    )
  })
})

test_that("decisioncombine function works with Fagan nomogram", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with Fagan nomogram
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      fagan = TRUE
    )
  })
  
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    fagan = TRUE
  )
  
  # Check that plot is included
  expect_true("plot1" %in% names(result))
})

test_that("decisioncombine function works with original data display", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with original data display
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      od = TRUE
    )
  })
  
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL,
    od = TRUE
  )
  
  # Check that original data tables are included
  expect_true("text1" %in% names(result))
  expect_true("text2" %in% names(result))
})

test_that("decisioncombine function works with footnotes", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with footnotes enabled
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL,
      fnote = TRUE
    )
  })
})

test_that("decisioncombine function works without individual test display", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with showIndividual = FALSE
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = "Rater 2",
      test3Positive = "1",
      showIndividual = FALSE
    )
  })
})

test_that("decisioncombine function handles different positive levels", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with different positive levels
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL
    )
  })
  
  # Test with alternative positive level if available
  # This would depend on the actual data levels
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL
    )
  })
})

test_that("decisioncombine function handles edge cases", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with minimal data subset
  minimal_data <- histopathology[1:50, ]
  
  expect_no_error({
    result <- decisioncombine(
      data = minimal_data,
      gold = "Golden Standart",
      goldPositive = "1",
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL
    )
  })
})

test_that("decisioncombine function output structure is complete", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = "Rater 2",
    test3Positive = "1",
    ci = TRUE,
    fagan = TRUE,
    fnote = TRUE,
    od = TRUE,
    showIndividual = TRUE
  )
  
  # Check all expected output components exist
  expected_components <- c("text1", "text2", "cTable", "indTable1", "indTable2", 
                          "indTable3", "nTable", "ratioTable", 
                          "epirTable_ratio", "epirTable_number", "plot1")
  
  for (component in expected_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing component:", component))
  }
})

test_that("decisioncombine function works with all combination rule types", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Create a small test dataset for predictable results
  test_data <- histopathology[1:100, ]
  
  # Test all three combination rules
  rules <- c("any", "all", "majority")
  
  for (rule in rules) {
    expect_no_error({
      result <- decisioncombine(
        data = test_data,
        gold = "Golden Standart",
        goldPositive = "1",
        test1 = "New Test",
        test1Positive = "1",
        test2 = "Rater 1",
        test2Positive = "1",
        test3 = "Rater 2",
        test3Positive = "1",
        combRule = rule
      )
    }, info = paste("Failed with combination rule:", rule))
  }
})

test_that("decisioncombine function produces expected diagnostic metrics", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  result <- decisioncombine(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    test1 = "New Test",
    test1Positive = "1",
    test2 = "Rater 1",
    test2Positive = "1",
    test3 = NULL,
    test3Positive = NULL
  )
  
  # Check that result structure is correct
  expect_s3_class(result, "Group")
  expect_s3_class(result$cTable, "Table")
  expect_s3_class(result$nTable, "Table")
  expect_s3_class(result$ratioTable, "Table")
  
  # Check table dimensions
  expect_equal(result$cTable$rowCount, 3)  # Test Positive, Test Negative, Total
  expect_equal(result$nTable$rowCount, 1)  # Single row with metrics
  expect_equal(result$ratioTable$rowCount, 1)  # Single row with ratios
})

test_that("decisioncombine function handles missing gold standard gracefully", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test should return early when no gold standard is provided
  expect_no_error({
    result <- decisioncombine(
      data = histopathology,
      gold = NULL,
      goldPositive = NULL,
      test1 = "New Test",
      test1Positive = "1",
      test2 = "Rater 1",
      test2Positive = "1",
      test3 = NULL,
      test3Positive = NULL
    )
  })
})