# =============================================================================
# COMPREHENSIVE TESTS FOR PSYCHOPDAROC ROC ANALYSIS MODULE
# =============================================================================
# 
# This test suite covers the highly sophisticated psychopdaROC function which
# includes advanced ROC analysis features like IDI/NRI, DeLong test, multiple
# cutpoint optimization methods, and comprehensive plotting capabilities.

library(testthat)
library(jmvcore)
if (!exists(".")) . <- function(x, ...) x

source("../../R/utils.R")
source("../../R/psychopdaROC_utilities.R")
source("../../R/psychopdaROC.h.R")
source("../../R/psychopdaROC.b.R")

# Helper function to create test data
create_roc_test_data <- function(n = 200, seed = 123) {
  set.seed(seed)
  
  # Create binary outcome
  outcome <- sample(c("Disease", "Healthy"), n, replace = TRUE, prob = c(0.3, 0.7))
  
  # Create continuous test variables with different discriminatory abilities
  # Good discriminator
  test1 <- ifelse(outcome == "Disease", 
                  rnorm(sum(outcome == "Disease"), mean = 3, sd = 1),
                  rnorm(sum(outcome == "Healthy"), mean = 1, sd = 1))
  
  # Moderate discriminator  
  test2 <- ifelse(outcome == "Disease",
                  rnorm(sum(outcome == "Disease"), mean = 2.5, sd = 1.2),
                  rnorm(sum(outcome == "Healthy"), mean = 1.5, sd = 1.2))
  
  # Poor discriminator
  test3 <- ifelse(outcome == "Disease",
                  rnorm(sum(outcome == "Disease"), mean = 2, sd = 1.5),
                  rnorm(sum(outcome == "Healthy"), mean = 1.8, sd = 1.5))
  
  # Subgroup variable
  subgroup <- sample(c("Group1", "Group2"), n, replace = TRUE)
  
  data.frame(
    outcome = factor(outcome, levels = c("Healthy", "Disease")),
    test1 = test1,
    test2 = test2, 
    test3 = test3,
    subgroup = factor(subgroup),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("psychopdaROC function exists and loads properly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Check function existence
  expect_true(exists("psychopdaROC"))
  expect_true(is.function(psychopdaROC))
})

test_that("basic ROC analysis works with default parameters", {
  # Create test data
  data <- create_roc_test_data(n = 100)
  
  # Test basic functionality
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease"
    )
  })
  
  # Check result structure
  result <- psychopdaROC(
    data = data,
    dependentVars = "test1", 
    classVar = "outcome",
    positiveClass = "Disease"
  )
  
  expect_s3_class(result, "psychopdaROCResults")
  expect_true("resultsTable" %in% names(result))
  expect_true("plotROC" %in% names(result))
})

test_that("multiple test variables work correctly", {
  data <- create_roc_test_data(n = 150)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2", "test3"),
      classVar = "outcome", 
      positiveClass = "Disease",
      combinePlots = TRUE
    )
  })
})

# =============================================================================
# CUTPOINT OPTIMIZATION TESTS
# =============================================================================

test_that("different cutpoint methods work correctly", {
  data <- create_roc_test_data(n = 100)
  
  # Test various cutpoint methods
  methods <- c("maximize_metric", "minimize_metric", "oc_youden_kernel", "oc_manual")
  
  for (method in methods) {
    if (method == "oc_manual") {
      expect_no_error({
        result <- psychopdaROC(
          data = data,
          dependentVars = "test1",
          classVar = "outcome",
          positiveClass = "Disease",
          method = method,
          specifyCutScore = "2.0"
        )
      })
    } else {
      expect_no_error({
        result <- psychopdaROC(
          data = data,
          dependentVars = "test1", 
          classVar = "outcome",
          positiveClass = "Disease",
          method = method
        )
      })
    }
  }
})

test_that("custom cutpoint methods work correctly", {
  data <- create_roc_test_data(n = 100)
  
  # Test custom methods
  custom_methods <- c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")
  
  for (method in custom_methods) {
    expect_no_error({
      result <- psychopdaROC(
        data = data,
        dependentVars = "test1",
        classVar = "outcome", 
        positiveClass = "Disease",
        method = method
      )
    })
  }
})

test_that("different optimization metrics work correctly", {
  data <- create_roc_test_data(n = 100)
  
  # Test various metrics
  metrics <- c("youden", "accuracy", "F1_score", "cohens_kappa", "sum_sens_spec")
  
  for (metric in metrics) {
    expect_no_error({
      result <- psychopdaROC(
        data = data,
        dependentVars = "test1",
        classVar = "outcome",
        positiveClass = "Disease", 
        metric = metric
      )
    })
  }
})

# =============================================================================
# ADVANCED STATISTICAL TESTS
# =============================================================================

test_that("DeLong test works for comparing multiple AUCs", {
  data <- create_roc_test_data(n = 150)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome",
      positiveClass = "Disease",
      delongTest = TRUE
    )
  })
  
  # Check that DeLong test results are visible
  result <- psychopdaROC(
    data = data,
    dependentVars = c("test1", "test2"),
    classVar = "outcome", 
    positiveClass = "Disease",
    delongTest = TRUE
  )
  
  expect_true(result$delongTest$visible)
})

test_that("IDI calculation works correctly", {
  data <- create_roc_test_data(n = 100)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome",
      positiveClass = "Disease",
      calculateIDI = TRUE,
      refVar = "test1",
      idiNriBootRuns = 100  # Reduced for faster testing
    )
  })
})

test_that("NRI calculation works correctly", {
  data <- create_roc_test_data(n = 100)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"), 
      classVar = "outcome",
      positiveClass = "Disease",
      calculateNRI = TRUE,
      refVar = "test1",
      nriThresholds = "0.3,0.7",
      idiNriBootRuns = 100
    )
  })
})

test_that("partial AUC calculation works", {
  data <- create_roc_test_data(n = 100)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      partialAUC = TRUE,
      partialAUCfrom = 0.8,
      partialAUCto = 1.0
    )
  })
})

test_that("bootstrap confidence intervals work", {
  data <- create_roc_test_data(n = 80)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      bootstrapCI = TRUE,
      bootstrapReps = 100  # Reduced for faster testing
    )
  })
})

# =============================================================================
# SUBGROUP ANALYSIS TESTS  
# =============================================================================

test_that("subgroup analysis works correctly", {
  data <- create_roc_test_data(n = 120)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      subGroup = "subgroup"
    )
  })
})

test_that("subgroup analysis with DeLong test throws appropriate error", {
  data <- create_roc_test_data(n = 100)
  
  expect_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome",
      positiveClass = "Disease",
      subGroup = "subgroup",
      delongTest = TRUE
    )
  }, "DeLong's test does not currently support the group variable")
})

# =============================================================================
# PLOTTING TESTS
# =============================================================================

test_that("basic ROC plotting works", {
  data <- create_roc_test_data(n = 80)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1", 
      classVar = "outcome",
      positiveClass = "Disease",
      plotROC = TRUE
    )
  })
})

test_that("combined plotting works with multiple variables", {
  data <- create_roc_test_data(n = 100)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome",
      positiveClass = "Disease",
      plotROC = TRUE,
      combinePlots = TRUE
    )
  })
})

test_that("additional plot types work", {
  data <- create_roc_test_data(n = 80)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome", 
      positiveClass = "Disease",
      plotROC = TRUE,
      showCriterionPlot = TRUE,
      showPrevalencePlot = TRUE,
      showDotPlot = TRUE
    )
  })
})

test_that("precision-recall curves work", {
  data <- create_roc_test_data(n = 80)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease", 
      precisionRecallCurve = TRUE
    )
  })
})

# =============================================================================
# TABLE OUTPUT TESTS
# =============================================================================

test_that("sensitivity/specificity table works", {
  data <- create_roc_test_data(n = 80)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      sensSpecTable = TRUE
    )
  })
})

test_that("threshold table works", {
  data <- create_roc_test_data(n = 80)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      showThresholdTable = TRUE,
      maxThresholds = 10
    )
  })
})

test_that("classifier comparison table works", {
  data <- create_roc_test_data(n = 100)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome", 
      positiveClass = "Disease",
      compareClassifiers = TRUE
    )
  })
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("function handles missing inputs appropriately", {
  data <- create_roc_test_data(n = 50)
  
  # Test missing dependent vars
  expect_error({
    result <- psychopdaROC(
      data = data,
      classVar = "outcome",
      positiveClass = "Disease"
    )
  })
  
  # Test missing class var  
  expect_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      positiveClass = "Disease"
    )
  })
})

test_that("function handles insufficient variables for advanced tests", {
  data <- create_roc_test_data(n = 50)
  
  # Test DeLong with only one variable
  expect_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease", 
      delongTest = TRUE
    )
  }, "DeLong's test requires at least 2 test variables")
  
  # Test IDI with only one variable
  expect_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      calculateIDI = TRUE
    )
  }, "Please specify at least two dependent variables")
})

test_that("function handles manual cutpoint without score", {
  data <- create_roc_test_data(n = 50)
  
  expect_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      method = "oc_manual",
      specifyCutScore = ""
    )
  }, "Suggestion: Enter a numeric value in the 'Manual Cut Score' field")
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("function handles small sample sizes", {
  data <- create_roc_test_data(n = 20)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease"
    )
  })
})

test_that("function handles imbalanced classes", {
  set.seed(123)
  # Create highly imbalanced data
  outcome <- sample(c("Disease", "Healthy"), 100, replace = TRUE, prob = c(0.05, 0.95))
  test_vals <- rnorm(100)
  data <- data.frame(
    outcome = factor(outcome, levels = c("Healthy", "Disease")),
    test_vals = test_vals
  )
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test_vals",
      classVar = "outcome",
      positiveClass = "Disease"
    )
  })
})

test_that("function handles perfect separation", {
  set.seed(123)
  # Create perfectly separable data
  outcome <- rep(c("Disease", "Healthy"), each = 25)
  test_vals <- c(rnorm(25, mean = 5), rnorm(25, mean = 0))
  data <- data.frame(
    outcome = factor(outcome, levels = c("Healthy", "Disease")),
    test_vals = test_vals
  )
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "test_vals", 
      classVar = "outcome",
      positiveClass = "Disease"
    )
  })
})

# =============================================================================
# UTILITY FUNCTION TESTS
# =============================================================================

test_that("utility functions work correctly", {
  # Test HTML table creation
  html_table <- print.sensSpecTable(
    Title = "Test Matrix",
    TP = 20, FP = 5, TN = 30, FN = 8
  )
  expect_type(html_table, "character")
  expect_true(nchar(html_table) > 0)
  
  # Test raw to probability conversion
  values <- c(1, 2, 3, 4, 5)
  actual <- c(0, 0, 1, 1, 1)
  probs <- raw_to_prob(values, actual, direction = ">=")
  expect_type(probs, "double")
  expect_equal(length(probs), length(values))
  expect_true(all(probs >= 0 & probs <= 1, na.rm = TRUE))
  
  # Test input validation
  validation <- validateROCInputs(values, actual, pos_class = "1")
  expect_type(validation, "list")
  expect_true("valid" %in% names(validation))
})

test_that("bootstrap IDI calculation works", {
  set.seed(123)
  new_values <- rnorm(50, mean = 1)
  ref_values <- rnorm(50, mean = 0)
  actual <- sample(c(0, 1), 50, replace = TRUE)
  
  expect_no_error({
    idi_result <- bootstrapIDI(
      new_values = new_values,
      ref_values = ref_values,
      actual = actual,
      n_boot = 100  # Reduced for testing
    )
  })
  
  idi_result <- bootstrapIDI(
    new_values = new_values,
    ref_values = ref_values,
    actual = actual,
    n_boot = 100
  )
  
  expect_type(idi_result, "list")
  expect_true("idi" %in% names(idi_result))
  expect_true("ci_lower" %in% names(idi_result))
  expect_true("ci_upper" %in% names(idi_result))
  expect_true("p_value" %in% names(idi_result))
})

test_that("bootstrap NRI calculation works", {
  set.seed(123)
  new_values <- rnorm(50, mean = 1)
  ref_values <- rnorm(50, mean = 0)
  actual <- sample(c(0, 1), 50, replace = TRUE)
  
  expect_no_error({
    nri_result <- bootstrapNRI(
      new_values = new_values,
      ref_values = ref_values,
      actual = actual,
      n_boot = 100
    )
  })
  
  nri_result <- bootstrapNRI(
    new_values = new_values,
    ref_values = ref_values,
    actual = actual,
    n_boot = 100
  )
  
  expect_type(nri_result, "list")
  expect_true("nri" %in% names(nri_result))
  expect_true("event_nri" %in% names(nri_result))
  expect_true("non_event_nri" %in% names(nri_result))
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

test_that("function performs reasonably with larger datasets", {
  data <- create_roc_test_data(n = 500)
  
  start_time <- Sys.time()
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome",
      positiveClass = "Disease"
    )
  })
  end_time <- Sys.time()
  
  # Should complete within reasonable time (adjust threshold as needed)
  expect_true(as.numeric(end_time - start_time, units = "secs") < 30)
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("comprehensive analysis with all features works", {
  data <- create_roc_test_data(n = 150)
  
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("test1", "test2"),
      classVar = "outcome",
      positiveClass = "Disease",
      method = "maximize_metric",
      metric = "youden", 
      plotROC = TRUE,
      combinePlots = TRUE,
      sensSpecTable = TRUE,
      showThresholdTable = TRUE,
      delongTest = TRUE,
      calculateIDI = TRUE,
      calculateNRI = TRUE,
      refVar = "test1",
      idiNriBootRuns = 100,  # Reduced for testing
      compareClassifiers = TRUE,
      precisionRecallCurve = TRUE
    )
  })
})

test_that("results contain expected components", {
  data <- create_roc_test_data(n = 100)
  
  result <- psychopdaROC(
    data = data,
    dependentVars = "test1",
    classVar = "outcome", 
    positiveClass = "Disease",
    sensSpecTable = TRUE,
    showThresholdTable = TRUE
  )
  
  # Check that all expected result components exist
  expected_components <- c("resultsTable", "sensSpecTable", "plotROC", 
                          "simpleResultsTable", "aucSummaryTable")
  
  for (component in expected_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing component:", component))
  }
})
