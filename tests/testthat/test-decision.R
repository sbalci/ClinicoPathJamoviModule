context("Medical Decision Analysis")

# Test utility functions
test_that("sensitivity calculation is correct", {
  expect_equal(calculate_sensitivity(tp = 90, fn = 10), 0.9)
  expect_equal(calculate_sensitivity(tp = 0, fn = 10), 0)
  expect_equal(calculate_sensitivity(tp = 50, fn = 50), 0.5)
  expect_true(is.na(calculate_sensitivity(tp = NA, fn = 1)))
})

test_that("specificity calculation is correct", {
  expect_equal(calculate_specificity(tn = 80, fp = 20), 0.8)
  expect_equal(calculate_specificity(tn = 0, fp = 20), 0)
  expect_equal(calculate_specificity(tn = 50, fp = 50), 0.5)
  expect_true(is.na(calculate_specificity(tn = 1, fp = NA)))
})

test_that("AUC calculation is correct", {
  expect_equal(calculate_auc(sens = 0.9, spec = 0.8), 0.53)
  expect_true(calculate_auc(sens = 1, spec = 1) <= 1)
  expect_true(calculate_auc(sens = 0, spec = 0) >= 0)
  expect_true(is.na(calculate_auc(NA, 0.5)))
})

# Test main decision() function
test_that("decision function works with basic parameters", {
  testthat::skip_on_cran()
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic function call without errors
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1"
    )
  })
  
  # Test that function returns expected structure
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1"
  )
  
  expect_s3_class(result, "Group")
  expect_true(length(result) > 0)
  
  # Test that required output tables exist
  expect_true("cTable" %in% names(result))
  expect_true("nTable" %in% names(result))
  expect_true("ratioTable" %in% names(result))
})

test_that("decision function works with confidence intervals", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with confidence intervals enabled
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      ci = TRUE
    )
  })
  
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    ci = TRUE
  )
  
  # Check that CI tables are included
  expect_true("epirTable_ratio" %in% names(result))
  expect_true("epirTable_number" %in% names(result))
})

test_that("decision function works with prior probability", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with prior probability
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      pp = TRUE,
      pprob = 0.05
    )
  })
})

test_that("decision function works with Fagan nomogram", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test with Fagan nomogram
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      fagan = TRUE
    )
  })
  
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    fagan = TRUE
  )
  
  # Check that plot is included
  expect_true("plot1" %in% names(result))
})

test_that("decision function works with alternative datasets", {
  testthat::skip_on_cran()
  
  # Test with breast cancer data
  data("breast_cancer_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decision(
      data = breast_cancer_data,
      gold = "cancer_status",
      goldPositive = "1",
      newtest = "mammography",
      testPositive = "1"
    )
  })
  
  # Test with COVID screening data
  data("covid_screening_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decision(
      data = covid_screening_data,
      gold = "covid_status",
      goldPositive = "1",
      newtest = "rapid_antigen",
      testPositive = "1"
    )
  })
})

test_that("decision function validates parameters correctly", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Test invalid prior probability
  expect_error({
    decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      pp = TRUE,
      pprob = 1.5  # Invalid: > 1
    )
  })
  
  expect_error({
    decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      pp = TRUE,
      pprob = -0.1  # Invalid: < 0
    )
  })
})

test_that("decision function handles missing data appropriately", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Create dataset with some missing values
  test_data <- histopathology
  test_data[1:5, "New Test"] <- NA
  
  # Function should handle missing data gracefully
  expect_no_error({
    result <- decision(
      data = test_data,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1"
    )
  })
})

test_that("decision function produces correct metrics", {
  testthat::skip_on_cran()
  
  # Create a simple known dataset for validation
  test_data <- data.frame(
    gold_standard = c(1, 1, 1, 1, 0, 0, 0, 0),
    new_test = c(1, 1, 0, 0, 1, 0, 0, 0)
  )
  
  result <- decision(
    data = test_data,
    gold = "gold_standard",
    goldPositive = "1",
    newtest = "new_test",
    testPositive = "1"
  )
  
  # Check that result structure is correct
  expect_s3_class(result, "Group")
  expect_s3_class(result$cTable, "Table")
  expect_s3_class(result$ratioTable, "Table")
})

test_that("decision function output structure is complete", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    od = TRUE,
    fnote = TRUE,
    ci = TRUE,
    fagan = TRUE
  )
  
  # Check all expected output components exist
  expected_components <- c("text1", "text2", "cTable", "nTable", "ratioTable", 
                          "epirTable_ratio", "epirTable_number", "plot1")
  
  for (component in expected_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing component:", component))
  }
})
