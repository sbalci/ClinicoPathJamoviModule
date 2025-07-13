# Test file for survival function
# Tests basic functionality and new features

# Load required libraries for testing
library(testthat)
library(ClinicoPathJamoviModule)

# Test data setup
setup_test_data <- function() {
  set.seed(123)
  n <- 100
  
  # Create test survival data
  test_data <- data.frame(
    patient_id = 1:n,
    time = round(rexp(n, rate = 0.01), 0),  # Exponential survival times
    event = rbinom(n, 1, 0.7),  # 70% event rate
    treatment = factor(sample(c("Control", "Treatment"), n, replace = TRUE)),
    age_group = factor(sample(c("Young", "Old"), n, replace = TRUE)),
    stage = factor(sample(1:3, n, replace = TRUE)),
    # Date variables for testing date-based calculations
    dx_date = as.Date("2020-01-01") + sample(0:365, n, replace = TRUE),
    fu_date = as.Date("2020-01-01") + sample(366:1095, n, replace = TRUE)
  )
  
  # Ensure follow-up dates are after diagnosis dates
  test_data$fu_date <- pmax(test_data$dx_date + 30, test_data$fu_date)
  
  return(test_data)
}

# Test basic survival analysis
test_that("Basic survival analysis works", {
  test_data <- setup_test_data()
  
  # Test basic survival analysis
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment"
    )
  })
})

test_that("Date-based survival calculation works", {
  test_data <- setup_test_data()
  
  # Test date-based survival
  expect_no_error({
    result <- survival(
      data = test_data,
      tint = TRUE,
      dxdate = "dx_date",
      fudate = "fu_date",
      timetypedata = "ymd",
      timetypeoutput = "days",
      outcome = "event",
      explanatory = "treatment"
    )
  })
})

test_that("Person-time analysis works", {
  test_data <- setup_test_data()
  
  # Test person-time analysis
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      person_time = TRUE,
      time_intervals = "100, 300, 500",
      rate_multiplier = 1000
    )
  })
})

test_that("RMST analysis works", {
  test_data <- setup_test_data()
  
  # Test RMST analysis
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      rmst_analysis = TRUE,
      rmst_tau = 500
    )
  })
})

test_that("Stratified Cox regression works", {
  test_data <- setup_test_data()
  
  # Test stratified Cox regression
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      stratified_cox = TRUE,
      strata_variable = "age_group"
    )
  })
})

test_that("Landmark analysis works", {
  test_data <- setup_test_data()
  
  # Test landmark analysis
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      uselandmark = TRUE,
      landmark = 100
    )
  })
})

test_that("Pairwise comparisons work", {
  test_data <- setup_test_data()
  
  # Create data with 3 groups for pairwise testing
  test_data$treatment3 <- factor(sample(c("A", "B", "C"), nrow(test_data), replace = TRUE))
  
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment3",
      pw = TRUE,
      padjustmethod = "holm"
    )
  })
})

test_that("Residual diagnostics work", {
  test_data <- setup_test_data()
  
  # Test residual diagnostics
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      residual_diagnostics = TRUE
    )
  })
})

test_that("Export survival data works", {
  test_data <- setup_test_data()
  
  # Test survival data export
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      export_survival_data = TRUE
    )
  })
})

test_that("Proportional hazards testing works", {
  test_data <- setup_test_data()
  
  # Test proportional hazards assumption testing
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      ph_cox = TRUE
    )
  })
})

test_that("Plot generation works", {
  test_data <- setup_test_data()
  
  # Test various plot types
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      sc = TRUE,      # Survival curves
      ce = TRUE,      # Cumulative events
      ch = TRUE,      # Cumulative hazard
      loglog = TRUE   # Log-log plot
    )
  })
})

test_that("Error handling works correctly", {
  test_data <- setup_test_data()
  
  # Test with invalid outcome variable
  test_data$bad_outcome <- sample(0:2, nrow(test_data), replace = TRUE)
  
  expect_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "bad_outcome",
      explanatory = "treatment"
    )
  }, regexp = "Outcome variable must be binary")
  
  # Test with missing required variables
  expect_error({
    result <- survival(
      data = test_data,
      elapsedtime = "nonexistent_time",
      outcome = "event",
      explanatory = "treatment"
    )
  })
})

test_that("Multiple event levels work", {
  test_data <- setup_test_data()
  
  # Create multi-state outcome
  test_data$multi_outcome <- factor(
    sample(c("Alive", "Dead_Disease", "Dead_Other"), 
           nrow(test_data), replace = TRUE)
  )
  
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "multi_outcome",
      multievent = TRUE,
      dod = "Dead_Disease",
      dooc = "Dead_Other",
      awd = "Alive",
      awod = "Alive",
      analysistype = "overall",
      explanatory = "treatment"
    )
  })
})

test_that("Comprehensive analysis with all features works", {
  test_data <- setup_test_data()
  
  # Test comprehensive analysis with multiple features enabled
  expect_no_error({
    result <- survival(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      person_time = TRUE,
      time_intervals = "100, 300",
      rate_multiplier = 1000,
      rmst_analysis = TRUE,
      rmst_tau = 400,
      pw = TRUE,
      padjustmethod = "BH",
      ph_cox = TRUE,
      sc = TRUE,
      endplot = 800,
      cutp = "100, 300, 500"
    )
  })
})

# Performance test for larger datasets
test_that("Performance with larger dataset", {
  # Create larger test dataset
  set.seed(456)
  n <- 1000
  large_data <- data.frame(
    time = round(rexp(n, rate = 0.005), 0),
    event = rbinom(n, 1, 0.6),
    treatment = factor(sample(c("A", "B", "C", "D"), n, replace = TRUE))
  )
  
  # Should complete within reasonable time
  expect_no_error({
    start_time <- Sys.time()
    result <- survival(
      data = large_data,
      elapsedtime = "time",
      outcome = "event",
      explanatory = "treatment",
      pw = TRUE
    )
    end_time <- Sys.time()
    
    # Should complete within 30 seconds
    expect_lt(as.numeric(end_time - start_time), 30)
  })
})