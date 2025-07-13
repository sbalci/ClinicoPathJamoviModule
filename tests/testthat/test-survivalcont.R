# Test file for survivalcont function
# Tests basic functionality and new features

# Load required libraries for testing
library(testthat)
library(ClinicoPathJamoviModule)

# Test data setup
setup_survivalcont_test_data <- function() {
  set.seed(123)
  n <- 150
  
  # Create test survival data with continuous variable
  test_data <- data.frame(
    patient_id = 1:n,
    time = round(rexp(n, rate = 0.008), 0),  # Exponential survival times
    event = rbinom(n, 1, 0.6),  # 60% event rate
    age = round(rnorm(n, mean = 65, sd = 12), 1),  # Continuous age variable
    biomarker = round(rnorm(n, mean = 100, sd = 25), 2),  # Continuous biomarker
    stage = factor(sample(1:3, n, replace = TRUE)),
    treatment = factor(sample(c("A", "B"), n, replace = TRUE)),
    # Date variables for testing date-based calculations
    dx_date = as.Date("2018-01-01") + sample(0:730, n, replace = TRUE),
    fu_date = as.Date("2018-01-01") + sample(731:1825, n, replace = TRUE),
    # Multi-state outcome
    multi_outcome = factor(sample(c("Alive", "Dead_Disease", "Dead_Other"), n, 
                                replace = TRUE, prob = c(0.4, 0.4, 0.2)))
  )
  
  # Ensure follow-up dates are after diagnosis dates
  test_data$fu_date <- pmax(test_data$dx_date + 30, test_data$fu_date)
  
  return(test_data)
}

# Test basic survivalcont analysis
test_that("Basic survivalcont analysis works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test basic survival analysis with continuous variable
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age"
    )
  })
})

test_that("Date-based survivalcont calculation works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test date-based survival
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      tint = TRUE,
      dxdate = "dx_date",
      fudate = "fu_date",
      timetypedata = "ymd",
      timetypeoutput = "days",
      outcome = "event",
      contexpl = "biomarker"
    )
  })
})

test_that("Optimal cutoff analysis works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test optimal cutoff finding
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      findcut = TRUE,
      sc = TRUE
    )
  })
})

test_that("Multiple cutoffs analysis works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test multiple cutoffs with different methods
  methods <- c("quantile", "recursive", "tree", "minpval")
  
  for (method in methods) {
    expect_no_error({
      result <- survivalcont(
        data = test_data,
        elapsedtime = "time",
        outcome = "event",
        contexpl = "biomarker",
        multiple_cutoffs = TRUE,
        num_cutoffs = "two",
        cutoff_method = method,
        min_group_size = 15,
        sc = TRUE
      )
    }, info = paste("Method:", method))
  }
})

test_that("Person-time analysis works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test person-time analysis
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      person_time = TRUE,
      time_intervals = "100, 300, 500",
      rate_multiplier = 1000
    )
  })
})

test_that("RMST analysis works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test RMST analysis
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker",
      rmst_analysis = TRUE,
      rmst_tau = 400
    )
  })
  
  # Test RMST with cutoff analysis
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      findcut = TRUE,
      rmst_analysis = TRUE,
      rmst_tau = 300
    )
  })
})

test_that("Residual diagnostics work", {
  test_data <- setup_survivalcont_test_data()
  
  # Test residual diagnostics
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker",
      residual_diagnostics = TRUE
    )
  })
  
  # Test residual diagnostics with cutoff analysis
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      findcut = TRUE,
      residual_diagnostics = TRUE
    )
  })
})

test_that("Landmark analysis works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test landmark analysis
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker",
      uselandmark = TRUE,
      landmark = 150,
      findcut = TRUE
    )
  })
})

test_that("Plot generation works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test various plot types
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      findcut = TRUE,
      sc = TRUE,      # Survival curves
      ce = TRUE,      # Cumulative events
      ch = TRUE,      # Cumulative hazard
      kmunicate = TRUE, # KMunicate plot
      loglog = TRUE   # Log-log plot
    )
  })
})

test_that("Multiple event levels work", {
  test_data <- setup_survivalcont_test_data()
  
  # Test with multi-state outcome - overall survival
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "multi_outcome",
      multievent = TRUE,
      dod = "Dead_Disease",
      dooc = "Dead_Other",
      awd = "Alive",
      awod = "Alive",
      analysistype = "overall",
      contexpl = "biomarker"
    )
  })
  
  # Test with multi-state outcome - cause-specific
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "multi_outcome",
      multievent = TRUE,
      dod = "Dead_Disease",
      dooc = "Dead_Other",
      awd = "Alive",
      awod = "Alive",
      analysistype = "cause",
      contexpl = "age"
    )
  })
})

test_that("Data export functionality works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test cutoff group export
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker",
      findcut = TRUE,
      calculatedcutoff = TRUE
    )
  })
  
  # Test multiple cutoff export
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      multiple_cutoffs = TRUE,
      calculatedmulticut = TRUE
    )
  })
  
  # Test calculated time export
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      tint = TRUE,
      dxdate = "dx_date",
      fudate = "fu_date",
      outcome = "event",
      contexpl = "biomarker",
      calculatedtime = TRUE
    )
  })
})

test_that("Error handling works correctly", {
  test_data <- setup_survivalcont_test_data()
  
  # Test with invalid outcome variable (more than 2 levels for binary)
  test_data$bad_outcome <- sample(0:3, nrow(test_data), replace = TRUE)
  
  expect_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "bad_outcome",
      contexpl = "age"
    )
  })
  
  # Test with missing required variables
  expect_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "nonexistent_time",
      outcome = "event",
      contexpl = "age"
    )
  })
})

test_that("Comprehensive analysis with all features works", {
  test_data <- setup_survivalcont_test_data()
  
  # Test comprehensive analysis with multiple features enabled
  expect_no_error({
    result <- survivalcont(
      data = test_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker",
      findcut = TRUE,
      multiple_cutoffs = TRUE,
      num_cutoffs = "two",
      cutoff_method = "quantile",
      person_time = TRUE,
      time_intervals = "100, 300",
      rate_multiplier = 1000,
      rmst_analysis = TRUE,
      rmst_tau = 400,
      residual_diagnostics = TRUE,
      sc = TRUE,
      ce = TRUE,
      ch = TRUE,
      endplot = 800,
      cutp = "100, 300, 500"
    )
  })
})

test_that("Different cutoff methods work", {
  test_data <- setup_survivalcont_test_data()
  
  cutoff_methods <- c("quantile", "recursive", "tree", "minpval")
  
  for (method in cutoff_methods) {
    expect_no_error({
      result <- survivalcont(
        data = test_data,
        elapsedtime = "time",
        outcome = "event",
        contexpl = "age",
        multiple_cutoffs = TRUE,
        cutoff_method = method,
        num_cutoffs = "two",
        min_group_size = 10
      )
    }, info = paste("Testing cutoff method:", method))
  }
})

test_that("Different numbers of cutoffs work", {
  test_data <- setup_survivalcont_test_data()
  
  num_cutoffs_options <- c("two", "three", "four")
  
  for (num_cuts in num_cutoffs_options) {
    expect_no_error({
      result <- survivalcont(
        data = test_data,
        elapsedtime = "time",
        outcome = "event",
        contexpl = "biomarker",
        multiple_cutoffs = TRUE,
        num_cutoffs = num_cuts,
        cutoff_method = "quantile",
        min_group_size = 8
      )
    }, info = paste("Testing number of cutoffs:", num_cuts))
  }
})

# Performance test for larger datasets
test_that("Performance with larger dataset", {
  # Create larger test dataset
  set.seed(456)
  n <- 800
  large_data <- data.frame(
    time = round(rexp(n, rate = 0.005), 0),
    event = rbinom(n, 1, 0.5),
    biomarker = rnorm(n, mean = 50, sd = 15),
    age = round(rnorm(n, mean = 60, sd = 10), 1)
  )
  
  # Should complete within reasonable time
  expect_no_error({
    start_time <- Sys.time()
    result <- survivalcont(
      data = large_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker",
      findcut = TRUE,
      multiple_cutoffs = TRUE
    )
    end_time <- Sys.time()
    
    # Should complete within 45 seconds
    expect_lt(as.numeric(end_time - start_time), 45)
  })
})

test_that("Edge cases are handled", {
  test_data <- setup_survivalcont_test_data()
  
  # Test with very small dataset
  small_data <- test_data[1:20, ]
  expect_no_error({
    result <- survivalcont(
      data = small_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "age",
      findcut = TRUE
    )
  })
  
  # Test with no events
  no_events_data <- test_data
  no_events_data$event <- 0
  expect_no_error({
    result <- survivalcont(
      data = no_events_data,
      elapsedtime = "time",
      outcome = "event",
      contexpl = "biomarker"
    )
  })
})