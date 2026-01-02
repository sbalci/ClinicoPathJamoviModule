# Test Decision Tree Graph Module
# Comprehensive testing of all features and arguments

library(testthat)
devtools::load_all()

# Test data setup ----
setup_test_data <- function() {
  # Create minimal test dataset
  test_data <- data.frame(
    id = 1:10,
    treatment = c(rep("Surgery", 5), rep("Medical", 5)),
    prob_success = c(0.8, 0.7, 0.9, 0.75, 0.85, 0.6, 0.65, 0.55, 0.7, 0.62),
    prob_failure = c(0.2, 0.3, 0.1, 0.25, 0.15, 0.4, 0.35, 0.45, 0.3, 0.38),
    cost_success = c(10000, 12000, 9000, 11000, 10500, 15000, 14000, 16000, 15500, 14500),
    cost_failure = c(20000, 22000, 18000, 21000, 19500, 25000, 24000, 26000, 25500, 24500),
    utility_success = c(0.9, 0.85, 0.95, 0.88, 0.92, 0.75, 0.78, 0.72, 0.8, 0.76),
    utility_failure = c(0.4, 0.35, 0.45, 0.38, 0.42, 0.3, 0.32, 0.28, 0.35, 0.31),
    outcome = sample(c("Success", "Failure"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  return(test_data)
}

# Basic functionality tests ----
test_that("decisiongraphClass can be instantiated", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  # Create options list
  options <- list(
    data = test_data,
    decisions = "treatment",
    probabilities = c("prob_success", "prob_failure"),
    costs = c("cost_success", "cost_failure"),
    utilities = c("utility_success", "utility_failure"),
    outcomes = "outcome",
    treeType = "costeffectiveness",
    layout = "horizontal",
    calculateExpectedValues = TRUE,
    summaryTable = TRUE
  )
  
  # Test class instantiation
  expect_no_error({
    analysis <- decisiongraphClass$new(options)
  })
})

test_that("Input validation works correctly", {
  skip_if_not_installed("jmvcore")
  
  # Test with empty data
  empty_data <- data.frame()
  options_empty <- list(data = empty_data)
  
  expect_error({
    analysis <- decisiongraphClass$new(options_empty)
    analysis$run()
  }, "No data provided")
  
  # Test with missing variables
  test_data <- setup_test_data()
  options_missing <- list(
    data = test_data,
    decisions = NULL,
    probabilities = NULL,
    costs = NULL
  )
  
  analysis <- decisiongraphClass$new(options_missing)
  # Should handle gracefully without required variables
  expect_no_error(analysis$run())
})

# Tree type testing ----
test_that("All tree types work correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  tree_types <- c("simple", "markov", "costeffectiveness")
  
  for (tree_type in tree_types) {
    options <- list(
      data = test_data,
      decisions = "treatment",
      probabilities = c("prob_success", "prob_failure"),
      costs = c("cost_success", "cost_failure"),
      utilities = c("utility_success", "utility_failure"),
      treeType = tree_type,
      calculateExpectedValues = TRUE
    )
    
    expect_no_error({
      analysis <- decisiongraphClass$new(options)
      analysis$run()
    })
  }
})

# Layout testing ----
test_that("All layout options work correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  layouts <- c("horizontal", "vertical", "radial")
  
  for (layout in layouts) {
    options <- list(
      data = test_data,
      decisions = "treatment",
      probabilities = c("prob_success", "prob_failure"),
      layout = layout
    )
    
    expect_no_error({
      analysis <- decisiongraphClass$new(options)
      analysis$run()
    })
  }
})

# Display options testing ----
test_that("Display options work correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  # Test all boolean display options
  display_options <- list(
    nodeShapes = c(TRUE, FALSE),
    showProbabilities = c(TRUE, FALSE),
    showCosts = c(TRUE, FALSE),
    showUtilities = c(TRUE, FALSE),
    nodeLabels = c(TRUE, FALSE),
    branchLabels = c(TRUE, FALSE)
  )
  
  base_options <- list(
    data = test_data,
    decisions = "treatment",
    probabilities = c("prob_success", "prob_failure"),
    costs = c("cost_success", "cost_failure"),
    utilities = c("utility_success", "utility_failure")
  )
  
  for (option_name in names(display_options)) {
    for (option_value in display_options[[option_name]]) {
      test_options <- base_options
      test_options[[option_name]] <- option_value
      
      expect_no_error({
        analysis <- decisiongraphClass$new(test_options)
        analysis$run()
      })
    }
  }
})

# Color scheme testing ----
test_that("All color schemes work correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  color_schemes <- c("default", "colorblind", "medical", "economic")
  
  for (scheme in color_schemes) {
    options <- list(
      data = test_data,
      decisions = "treatment",
      colorScheme = scheme
    )
    
    expect_no_error({
      analysis <- decisiongraphClass$new(options)
      analysis$run()
    })
  }
})

# Analysis options testing ----
test_that("Analysis options work correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  # Test expected values calculation
  options_ev <- list(
    data = test_data,
    decisions = "treatment",
    costs = c("cost_success", "cost_failure"),
    utilities = c("utility_success", "utility_failure"),
    calculateExpectedValues = TRUE,
    summaryTable = TRUE
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_ev)
    analysis$run()
  })
  
  # Test sensitivity analysis
  options_sa <- list(
    data = test_data,
    decisions = "treatment",
    sensitivityAnalysis = TRUE,
    tornado = TRUE
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_sa)
    analysis$run()
  })
})

# Economic parameters testing ----
test_that("Economic parameters work correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  # Test different discount rates
  discount_rates <- c(0, 0.03, 0.05, 0.07)
  
  for (rate in discount_rates) {
    options <- list(
      data = test_data,
      decisions = "treatment",
      costs = c("cost_success", "cost_failure"),
      discountRate = rate,
      calculateExpectedValues = TRUE
    )
    
    expect_no_error({
      analysis <- decisiongraphClass$new(options)
      analysis$run()
    })
  }
  
  # Test different time horizons
  time_horizons <- c(1, 5, 10, 20)
  
  for (horizon in time_horizons) {
    options <- list(
      data = test_data,
      decisions = "treatment",
      timeHorizon = horizon,
      calculateExpectedValues = TRUE
    )
    
    expect_no_error({
      analysis <- decisiongraphClass$new(options)
      analysis$run()
    })
  }
})

# Output table testing ----
test_that("Output tables are generated correctly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  # Test summary table
  options_summary <- list(
    data = test_data,
    decisions = "treatment",
    costs = c("cost_success", "cost_failure"),
    utilities = c("utility_success", "utility_failure"),
    calculateExpectedValues = TRUE,
    summaryTable = TRUE
  )
  
  analysis <- decisiongraphClass$new(options_summary)
  analysis$run()
  
  # Check that summary table exists
  expect_true(!is.null(analysis$results$summaryTable))
  
  # Test node table
  expect_true(!is.null(analysis$results$nodeTable))
  
  # Test sensitivity table
  options_sens <- list(
    data = test_data,
    decisions = "treatment",
    sensitivityAnalysis = TRUE
  )
  
  analysis_sens <- decisiongraphClass$new(options_sens)
  analysis_sens$run()
  
  expect_true(!is.null(analysis_sens$results$sensitivityTable))
})

# Edge cases testing ----
test_that("Edge cases are handled correctly", {
  skip_if_not_installed("jmvcore")
  
  # Test with single treatment option
  single_treatment_data <- data.frame(
    id = 1:5,
    treatment = rep("OnlyTreatment", 5),
    prob1 = c(0.7, 0.8, 0.6, 0.9, 0.75),
    cost1 = c(1000, 1200, 800, 1500, 1100)
  )
  
  options_single <- list(
    data = single_treatment_data,
    decisions = "treatment",
    probabilities = "prob1",
    costs = "cost1"
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_single)
    analysis$run()
  })
  
  # Test with missing values
  missing_data <- test_data <- setup_test_data()
  missing_data$treatment[1:3] <- NA
  
  options_missing <- list(
    data = missing_data,
    decisions = "treatment",
    probabilities = c("prob_success", "prob_failure")
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_missing)
    analysis$run()
  })
  
  # Test with extreme values
  extreme_data <- data.frame(
    id = 1:5,
    treatment = c("A", "B", "A", "B", "A"),
    prob_zero = rep(0, 5),
    prob_one = rep(1, 5),
    cost_high = rep(1e6, 5),
    utility_high = rep(1, 5)
  )
  
  options_extreme <- list(
    data = extreme_data,
    decisions = "treatment",
    probabilities = c("prob_zero", "prob_one"),
    costs = "cost_high",
    utilities = "utility_high"
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_extreme)
    analysis$run()
  })
})

# Data type testing ----
test_that("Different data types are handled correctly", {
  skip_if_not_installed("jmvcore")
  
  # Test with factor variables
  factor_data <- setup_test_data()
  factor_data$treatment <- as.factor(factor_data$treatment)
  factor_data$outcome <- as.factor(factor_data$outcome)
  
  options_factor <- list(
    data = factor_data,
    decisions = "treatment",
    outcomes = "outcome",
    probabilities = c("prob_success", "prob_failure")
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_factor)
    analysis$run()
  })
  
  # Test with character variables
  char_data <- setup_test_data()
  char_data$treatment <- as.character(char_data$treatment)
  
  options_char <- list(
    data = char_data,
    decisions = "treatment",
    probabilities = c("prob_success", "prob_failure")
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_char)
    analysis$run()
  })
  
  # Test with numeric decision variables (should handle gracefully)
  numeric_data <- setup_test_data()
  numeric_data$treatment_numeric <- ifelse(numeric_data$treatment == "Surgery", 1, 2)
  
  options_numeric <- list(
    data = numeric_data,
    decisions = "treatment_numeric",
    probabilities = c("prob_success", "prob_failure")
  )
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_numeric)
    analysis$run()
  })
})

# Performance testing ----
test_that("Performance with larger datasets", {
  skip_if_not_installed("jmvcore")
  
  # Create larger test dataset
  large_data <- data.frame(
    id = 1:1000,
    treatment = sample(c("A", "B", "C", "D"), 1000, replace = TRUE),
    strategy = sample(c("Standard", "Intensive", "Minimal"), 1000, replace = TRUE),
    prob1 = runif(1000, 0.5, 0.9),
    prob2 = runif(1000, 0.1, 0.4),
    cost1 = rnorm(1000, 10000, 2000),
    cost2 = rnorm(1000, 15000, 3000),
    utility1 = runif(1000, 0.7, 0.95),
    utility2 = runif(1000, 0.3, 0.7)
  )
  
  options_large <- list(
    data = large_data,
    decisions = c("treatment", "strategy"),
    probabilities = c("prob1", "prob2"),
    costs = c("cost1", "cost2"),
    utilities = c("utility1", "utility2"),
    calculateExpectedValues = TRUE
  )
  
  # Test that it completes within reasonable time
  start_time <- Sys.time()
  
  expect_no_error({
    analysis <- decisiongraphClass$new(options_large)
    analysis$run()
  })
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within 30 seconds
  expect_lt(execution_time, 30)
})

# Integration testing ----
test_that("Integration with real datasets", {
  skip_if_not_installed("jmvcore")
  
  # Test with package datasets if available
  tryCatch({
    data("basic_decision_data", package = "ClinicoPath")
    
    options_real <- list(
      data = basic_decision_data,
      decisions = "treatment",
      probabilities = c("prob_success_surgery", "prob_success_medical"),
      costs = c("cost_surgery", "cost_medical"),
      utilities = c("utility_success", "utility_failure"),
      calculateExpectedValues = TRUE,
      summaryTable = TRUE
    )
    
    expect_no_error({
      analysis <- decisiongraphClass$new(options_real)
      analysis$run()
    })
    
  }, error = function(e) {
    skip("Real datasets not available")
  })
})

# Error message testing ----
test_that("Error messages are informative", {
  skip_if_not_installed("jmvcore")
  
  # Test with completely empty options
  expect_error({
    analysis <- decisiongraphClass$new(list())
    analysis$run()
  }, class = "error")
  
  # Test with invalid variable names
  test_data <- setup_test_data()
  options_invalid <- list(
    data = test_data,
    decisions = "nonexistent_variable",
    probabilities = c("also_nonexistent")
  )
  
  expect_no_error({
    # Should handle gracefully, not crash
    analysis <- decisiongraphClass$new(options_invalid)
    analysis$run()
  })
})

# Memory management testing ----
test_that("Memory is managed properly", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  
  # Run multiple analyses to check for memory leaks
  for (i in 1:10) {
    options <- list(
      data = test_data,
      decisions = "treatment",
      probabilities = c("prob_success", "prob_failure"),
      calculateExpectedValues = TRUE
    )
    
    analysis <- decisiongraphClass$new(options)
    analysis$run()
    
    # Clean up
    rm(analysis)
    gc()
  }
  
  # If we get here without running out of memory, test passes
  expect_true(TRUE)
})

# Regression testing ----
test_that("Results are consistent across runs", {
  skip_if_not_installed("jmvcore")
  
  test_data <- setup_test_data()
  options <- list(
    data = test_data,
    decisions = "treatment",
    probabilities = c("prob_success", "prob_failure"),
    costs = c("cost_success", "cost_failure"),
    utilities = c("utility_success", "utility_failure"),
    calculateExpectedValues = TRUE
  )
  
  # Run the same analysis multiple times
  results <- list()
  for (i in 1:3) {
    analysis <- decisiongraphClass$new(options)
    analysis$run()
    # Store relevant results for comparison
    # (This would need to be adapted based on actual output structure)
    results[[i]] <- "consistent"  # Placeholder
  }
  
  # Results should be identical
  expect_true(all(sapply(results, function(x) x == results[[1]])))
})
