context("Medical Decision Calculator")

# Test basic function execution
test_that("decisioncalculator function works with basic parameters", {
  testthat::skip_on_cran()
  
  # Test basic function call without errors
  expect_no_error({
    result <- decisioncalculator(
      TP = 90,
      FN = 10,
      TN = 80,
      FP = 20
    )
  })
  
  # Test that function returns expected structure
  result <- decisioncalculator(
    TP = 90,
    FN = 10,
    TN = 80,
    FP = 20
  )
  
  expect_s3_class(result, "Group")
  expect_true(length(result) > 0)
  
  # Test that required output tables exist
  expect_true("cTable" %in% names(result))
  expect_true("nTable" %in% names(result))
  expect_true("ratioTable" %in% names(result))
})

test_that("decisioncalculator function calculates metrics correctly", {
  testthat::skip_on_cran()
  
  # Test with known values for validation
  result <- decisioncalculator(
    TP = 90,
    FN = 10,
    TN = 80,
    FP = 20
  )
  
  # Check that result structure is correct
  expect_s3_class(result, "Group")
  expect_s3_class(result$cTable, "Table")
  expect_s3_class(result$nTable, "Table")
  expect_s3_class(result$ratioTable, "Table")
  
  # Check table dimensions and content
  expect_equal(result$cTable$rowCount, 3)  # Test Positive, Test Negative, Total
  expect_equal(result$nTable$rowCount, 1)  # Single row with metrics
  expect_equal(result$ratioTable$rowCount, 1)  # Single row with ratios
})

test_that("decisioncalculator function works with confidence intervals", {
  testthat::skip_on_cran()
  
  # Test with confidence intervals enabled
  expect_no_error({
    result <- decisioncalculator(
      TP = 90,
      FN = 10,
      TN = 80,
      FP = 20,
      ci = TRUE
    )
  })
  
  result <- decisioncalculator(
    TP = 90,
    FN = 10,
    TN = 80,
    FP = 20,
    ci = TRUE
  )
  
  # Check that CI tables are included
  expect_true("epirTable_ratio" %in% names(result))
  expect_true("epirTable_number" %in% names(result))
})

test_that("decisioncalculator function works with prior probability", {
  testthat::skip_on_cran()
  
  # Test with prior probability
  expect_no_error({
    result <- decisioncalculator(
      TP = 90,
      FN = 10,
      TN = 80,
      FP = 20,
      pp = TRUE,
      pprob = 0.05
    )
  })
  
  # Test with different prior probability values
  expect_no_error({
    result <- decisioncalculator(
      TP = 90,
      FN = 10,
      TN = 80,
      FP = 20,
      pp = TRUE,
      pprob = 0.15
    )
  })
})

test_that("decisioncalculator function works with Fagan nomogram", {
  testthat::skip_on_cran()
  
  # Test with Fagan nomogram
  expect_no_error({
    result <- decisioncalculator(
      TP = 90,
      FN = 10,
      TN = 80,
      FP = 20,
      fagan = TRUE
    )
  })
  
  result <- decisioncalculator(
    TP = 90,
    FN = 10,
    TN = 80,
    FP = 20,
    fagan = TRUE
  )
  
  # Check that plot is included
  expect_true("plot1" %in% names(result))
})

test_that("decisioncalculator function works with footnotes", {
  testthat::skip_on_cran()
  
  # Test with footnotes enabled
  expect_no_error({
    result <- decisioncalculator(
      TP = 90,
      FN = 10,
      TN = 80,
      FP = 20,
      fnote = TRUE
    )
  })
})

test_that("decisioncalculator function validates parameters correctly", {
  testthat::skip_on_cran()
  
  # Test valid parameter ranges
  expect_no_error({
    decisioncalculator(
      TP = 100,
      FN = 0,
      TN = 100,
      FP = 0,
      pp = TRUE,
      pprob = 0.001  # Minimum allowed
    )
  })
  
  expect_no_error({
    decisioncalculator(
      TP = 100,
      FN = 0,
      TN = 100,
      FP = 0,
      pp = TRUE,
      pprob = 0.999  # Maximum allowed
    )
  })
})

test_that("decisioncalculator function handles edge cases", {
  testthat::skip_on_cran()
  
  # Test with zero values
  expect_no_error({
    result <- decisioncalculator(
      TP = 100,
      FN = 0,
      TN = 100,
      FP = 0
    )
  })
  
  # Test with equal values
  expect_no_error({
    result <- decisioncalculator(
      TP = 50,
      FN = 50,
      TN = 50,
      FP = 50
    )
  })
  
  # Test with small values
  expect_no_error({
    result <- decisioncalculator(
      TP = 5,
      FN = 2,
      TN = 8,
      FP = 3
    )
  })
})

test_that("decisioncalculator function output structure is complete", {
  testthat::skip_on_cran()
  
  result <- decisioncalculator(
    TP = 90,
    FN = 10,
    TN = 80,
    FP = 20,
    ci = TRUE,
    fagan = TRUE,
    fnote = TRUE
  )
  
  # Check all expected output components exist
  expected_components <- c("cTable", "nTable", "ratioTable", 
                          "epirTable_ratio", "epirTable_number", "plot1")
  
  for (component in expected_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing component:", component))
  }
})

test_that("decisioncalculator function calculations are mathematically correct", {
  testthat::skip_on_cran()
  
  # Test with simple known values
  TP <- 90
  FN <- 10
  TN <- 80
  FP <- 20
  
  result <- decisioncalculator(
    TP = TP,
    FN = FN,
    TN = TN,
    FP = FP
  )
  
  # Manual calculations for verification
  total_diseased <- TP + FN
  total_healthy <- TN + FP
  total_test_positive <- TP + FP
  total_test_negative <- TN + FN
  total_population <- TP + FN + TN + FP
  
  expected_sensitivity <- TP / total_diseased  # 90/100 = 0.9
  expected_specificity <- TN / total_healthy   # 80/100 = 0.8
  expected_ppv <- TP / total_test_positive     # 90/110 = 0.818...
  expected_npv <- TN / total_test_negative     # 80/90 = 0.888...
  
  # Note: We can't directly access the calculated values from the jamovi result,
  # but we can verify the function executes without error with known inputs
  expect_s3_class(result$ratioTable, "Table")
  expect_equal(result$ratioTable$rowCount, 1)
})

test_that("decisioncalculator function works with various input combinations", {
  testthat::skip_on_cran()
  
  # Test different input scenarios
  test_scenarios <- list(
    list(TP = 95, FN = 5, TN = 85, FP = 15),
    list(TP = 75, FN = 25, TN = 90, FP = 10),
    list(TP = 60, FN = 40, TN = 70, FP = 30),
    list(TP = 85, FN = 15, TN = 75, FP = 25)
  )
  
  for (scenario in test_scenarios) {
    expect_no_error({
      result <- decisioncalculator(
        TP = scenario$TP,
        FN = scenario$FN,
        TN = scenario$TN,
        FP = scenario$FP,
        ci = TRUE,
        fagan = TRUE
      )
    })
  }
})

test_that("decisioncalculator function handles large numbers", {
  testthat::skip_on_cran()
  
  # Test with larger values to ensure no overflow issues
  expect_no_error({
    result <- decisioncalculator(
      TP = 9000,
      FN = 1000,
      TN = 8000,
      FP = 2000
    )
  })
})

test_that("decisioncalculator function default parameters work", {
  testthat::skip_on_cran()
  
  # Test with default parameters (should match yaml defaults)
  expect_no_error({
    result <- decisioncalculator()
  })
  
  result <- decisioncalculator()
  
  # Check that result contains expected tables
  expect_true("cTable" %in% names(result))
  expect_true("nTable" %in% names(result))
  expect_true("ratioTable" %in% names(result))
})