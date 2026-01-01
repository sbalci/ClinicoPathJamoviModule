# Test Decision Panel Optimization Module
# Comprehensive testing of all features and arguments

library(testthat)
library(ClinicoPath)

# Test data setup ----
setup_test_data <- function() {
  # Create minimal test dataset for decisionpanel
  test_data <- data.frame(
    patient_id = 1:100,
    test1 = factor(sample(c("Negative", "Positive"), 100, replace = TRUE, prob = c(0.7, 0.3))),
    test2 = factor(sample(c("Negative", "Positive"), 100, replace = TRUE, prob = c(0.6, 0.4))),
    test3 = factor(sample(c("Normal", "Abnormal"), 100, replace = TRUE, prob = c(0.8, 0.2))),
    gold_standard = factor(sample(c("No Disease", "Disease"), 100, replace = TRUE, prob = c(0.85, 0.15))),
    stringsAsFactors = FALSE
  )
  return(test_data)
}

# Basic functionality tests ----
test_that("decisionpanel function works with basic parameters", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test basic function call without errors
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "all",
      optimizationCriteria = "accuracy"
    )
  })
  
  # Test that function returns expected structure
  result <- decisionpanel(
    data = test_data,
    tests = c("test1", "test2"),
    testLevels = "Positive,Positive", 
    gold = "gold_standard",
    goldPositive = "Disease",
    strategies = "all",
    optimizationCriteria = "accuracy"
  )
  
  expect_s3_class(result, "Group")
  expect_true(length(result) > 0)
  
  # Test that required output tables exist
  expect_true("summary" %in% names(result))
  expect_true("optimalPanel" %in% names(result))
  expect_true("individualTests" %in% names(result))
})

test_that("decisionpanel works with auto-detection of positive levels", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test with auto-detection (empty testLevels)
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "single"
    )
  })
})

test_that("decisionpanel works with single positive level for all tests", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test with single level applied to all tests
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive",
      gold = "gold_standard", 
      goldPositive = "Disease",
      strategies = "single"
    )
  })
})

# Testing strategy tests ----
test_that("All testing strategies work correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  strategies <- c("all", "single", "parallel", "sequential")
  
  for (strategy in strategies) {
    expect_no_error({
      result <- decisionpanel(
        data = test_data,
        tests = c("test1", "test2"),
        testLevels = "Positive,Positive",
        gold = "gold_standard",
        goldPositive = "Disease",
        strategies = strategy,
        optimizationCriteria = "accuracy"
      )
    })
  }
})

test_that("Parallel testing rules work correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  parallel_rules <- c("any", "all", "majority", "custom")
  
  for (rule in parallel_rules) {
    expect_no_error({
      result <- decisionpanel(
        data = test_data,
        tests = c("test1", "test2", "test3"),
        testLevels = "Positive,Positive,Abnormal",
        gold = "gold_standard",
        goldPositive = "Disease",
        strategies = "parallel",
        parallelRules = rule,
        customThreshold = 2
      )
    })
  }
})

test_that("Sequential testing stop rules work correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  stop_rules <- c("positive", "negative", "confirmatory", "exclusion")
  
  for (rule in stop_rules) {
    expect_no_error({
      result <- decisionpanel(
        data = test_data,
        tests = c("test1", "test2"),
        testLevels = "Positive,Positive",
        gold = "gold_standard",
        goldPositive = "Disease",
        strategies = "sequential",
        sequentialStop = rule
      )
    })
  }
})

# Optimization criteria tests ----
test_that("All optimization criteria work correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  criteria <- c("accuracy", "sensitivity", "specificity", "ppv", "npv", "youden", "utility", "efficiency")
  
  for (criterion in criteria) {
    expect_no_error({
      result <- decisionpanel(
        data = test_data,
        tests = c("test1", "test2"),
        testLevels = "Positive,Positive",
        gold = "gold_standard",
        goldPositive = "Disease",
        strategies = "all",
        optimizationCriteria = criterion
      )
    })
  }
})

# Cost analysis tests ----
test_that("Cost analysis works correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test with cost analysis enabled
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "all",
      useCosts = TRUE,
      testCosts = "10,25",
      fpCost = 100,
      fnCost = 1000,
      optimizationCriteria = "utility"
    )
  })
})

test_that("Efficiency optimization works with costs", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "all",
      useCosts = TRUE,
      testCosts = "5,15",
      fpCost = 50,
      fnCost = 500,
      optimizationCriteria = "efficiency"
    )
  })
})

# Decision tree tests ----
test_that("Decision tree creation works correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  tree_methods <- c("cart", "conditional", "costSensitive", "ensemble")
  
  for (method in tree_methods) {
    expect_no_error({
      result <- decisionpanel(
        data = test_data,
        tests = c("test1", "test2"),
        testLevels = "Positive,Positive",
        gold = "gold_standard",
        goldPositive = "Disease",
        createTree = TRUE,
        treeMethod = method,
        maxDepth = 3,
        minSplit = 10
      )
    })
  }
})

# Cross-validation and bootstrap tests ----
test_that("Cross-validation works correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      crossValidate = TRUE,
      nFolds = 3,
      seed = 123
    )
  })
})

test_that("Bootstrap confidence intervals work correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      bootstrap = TRUE,
      bootReps = 100,  # Small number for testing
      seed = 123
    )
  })
})

# Visualization tests ----
test_that("All visualization options work correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test all plot options
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "all",
      createTree = TRUE,
      useCosts = TRUE,
      testCosts = "10,20",
      plotTree = TRUE,
      plotComparison = TRUE,
      plotCostEffect = TRUE,
      plotROC = TRUE
    )
  })
})

# Alternative dataset tests ----
test_that("decisionpanel works with real datasets", {
  testthat::skip_on_cran()
  
  # Test with COVID screening data
  data("covid_screening_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decisionpanel(
      data = covid_screening_data,
      tests = c("rapid_antigen", "pcr"),
      testLevels = "Positive,Positive",
      gold = "covid_status",
      goldPositive = "Positive",
      strategies = "all"
    )
  })
  
  # Test with breast cancer data
  data("breast_cancer_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decisionpanel(
      data = breast_cancer_data,
      tests = c("clinical_exam", "mammography"),
      testLevels = "Abnormal,BIRADS 3-5",
      gold = "cancer_status", 
      goldPositive = "Cancer",
      strategies = "all"
    )
  })
  
  # Test with MI rule-out data
  data("mi_ruleout_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decisionpanel(
      data = mi_ruleout_data,
      tests = c("ecg", "troponin_initial"),
      testLevels = "Ischemic changes,Elevated",
      gold = "mi_status",
      goldPositive = "MI",
      strategies = "all"
    )
  })
})

# Parameter validation tests ----
test_that("decisionpanel validates parameters correctly", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test invalid number of test costs
  expect_error({
    decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      useCosts = TRUE,
      testCosts = "10"  # Only one cost for two tests
    )
  })
  
  # Test invalid positive level
  expect_error({
    decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "InvalidLevel,Positive",
      gold = "gold_standard",
      goldPositive = "Disease"
    )
  })
  
  # Test mismatch between number of levels and tests
  expect_error({
    decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive,Extra",  # Three levels for two tests
      gold = "gold_standard",
      goldPositive = "Disease"
    )
  })
})

test_that("decisionpanel handles constraint validation", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test with high minimum constraints
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      minSensitivity = 0.95,
      minSpecificity = 0.95
    )
  })
})

# Missing data handling tests ----
test_that("decisionpanel handles missing data appropriately", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Create dataset with some missing values
  test_data_missing <- test_data
  test_data_missing[1:10, "test1"] <- NA
  test_data_missing[5:15, "test2"] <- NA
  
  # Function should handle missing data gracefully
  expect_no_error({
    result <- decisionpanel(
      data = test_data_missing,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "all"
    )
  })
})

# Output structure tests ----
test_that("decisionpanel output structure is complete", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  result <- decisionpanel(
    data = test_data,
    tests = c("test1", "test2"),
    testLevels = "Positive,Positive",
    gold = "gold_standard",
    goldPositive = "Disease",
    strategies = "all",
    compareStrategies = TRUE,
    createTree = TRUE,
    crossValidate = TRUE,
    bootstrap = TRUE,
    bootReps = 100,
    showAllCombinations = TRUE
  )
  
  # Check essential output components exist
  essential_components <- c("summary", "optimalPanel", "individualTests")
  
  for (component in essential_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing essential component:", component))
  }
  
  # Check conditional components based on options
  expect_true("strategyComparison" %in% names(result))
  expect_true("treeStructure" %in% names(result))
  expect_true("crossValidation" %in% names(result))
  expect_true("bootstrapResults" %in% names(result))
  expect_true("allCombinations" %in% names(result))
})

test_that("decisionpanel table structures are correct", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  result <- decisionpanel(
    data = test_data,
    tests = c("test1", "test2"),
    testLevels = "Positive,Positive",
    gold = "gold_standard",
    goldPositive = "Disease"
  )
  
  # Check that main tables have correct structure
  expect_s3_class(result$optimalPanel, "Table")
  expect_s3_class(result$individualTests, "Table")
})

# Edge cases and robustness tests ----
test_that("decisionpanel handles edge cases correctly", {
  testthat::skip_on_cran()
  
  # Test with single test
  test_data <- setup_test_data()
  
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = "test1",
      testLevels = "Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "single"
    )
  })
  
  # Test with many tests
  test_data_many <- test_data
  test_data_many$test4 <- factor(sample(c("Low", "High"), 100, replace = TRUE))
  test_data_many$test5 <- factor(sample(c("Normal", "Abnormal"), 100, replace = TRUE))
  
  expect_no_error({
    result <- decisionpanel(
      data = test_data_many,
      tests = c("test1", "test2", "test3", "test4", "test5"),
      testLevels = "Positive,Positive,Abnormal,High,Abnormal",
      gold = "gold_standard",
      goldPositive = "Disease",
      maxTests = 3
    )
  })
})

test_that("decisionpanel handles different prevalence settings", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test with custom prevalence
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      prevalence = 0.05
    )
  })
  
  # Test with sample prevalence (default)
  expect_no_error({
    result <- decisionpanel(
      data = test_data,
      tests = c("test1", "test2"),
      testLevels = "Positive,Positive",
      gold = "gold_standard",
      goldPositive = "Disease",
      prevalence = 0
    )
  })
})

# Performance with larger datasets ----
test_that("decisionpanel performance with larger datasets", {
  testthat::skip_on_cran()
  
  # Create larger test dataset
  large_data <- data.frame(
    patient_id = 1:500,
    test1 = factor(sample(c("Negative", "Positive"), 500, replace = TRUE)),
    test2 = factor(sample(c("Negative", "Positive"), 500, replace = TRUE)),
    test3 = factor(sample(c("Normal", "Abnormal"), 500, replace = TRUE)),
    gold_standard = factor(sample(c("No Disease", "Disease"), 500, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Test that it completes within reasonable time
  start_time <- Sys.time()
  
  expect_no_error({
    result <- decisionpanel(
      data = large_data,
      tests = c("test1", "test2", "test3"),
      testLevels = "Positive,Positive,Abnormal",
      gold = "gold_standard",
      goldPositive = "Disease",
      strategies = "all"
    )
  })
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within 30 seconds
  expect_lt(execution_time, 30)
})

# Integration tests ----
test_that("decisionpanel integrates well with other functions", {
  testthat::skip_on_cran()
  
  test_data <- setup_test_data()
  
  # Test that output can be processed further
  result <- decisionpanel(
    data = test_data,
    tests = c("test1", "test2"),
    testLevels = "Positive,Positive",
    gold = "gold_standard",
    goldPositive = "Disease"
  )
  
  # Check that tables can be converted to data frames
  expect_no_error({
    df1 <- as.data.frame(result$optimalPanel)
    df2 <- as.data.frame(result$individualTests)
  })
  
  # Check basic properties
  df1 <- as.data.frame(result$optimalPanel)
  expect_true(nrow(df1) > 0)
  expect_true(ncol(df1) > 0)
})
