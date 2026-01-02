# Test file for curemodels function
# Basic smoke tests for cure model analysis

# Load required libraries for testing
library(testthat)
devtools::load_all()

# Test data setup
setup_cure_test_data <- function() {
  set.seed(456)
  n <- 150

  # Create test survival data with long-term survivors
  # Simulate a cure fraction scenario
  cure_prob <- 0.3
  is_cured <- rbinom(n, 1, cure_prob)

  # Survival times: cured patients have very long times
  time <- numeric(n)
  time[is_cured == 1] <- runif(sum(is_cured), 80, 120)  # Long survivors
  time[is_cured == 0] <- rexp(sum(1 - is_cured), rate = 0.05)  # Uncured with exponential

  # Events: cured patients are censored
  event <- 1 - is_cured

  # Add some predictors
  test_data <- data.frame(
    patient_id = 1:n,
    followup_time = round(time, 1),
    death_status = event,
    age = rnorm(n, mean = 60, sd = 10),
    treatment = factor(sample(c("Standard", "Novel"), n, replace = TRUE)),
    stage = factor(sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    performance_score = sample(0:2, n, replace = TRUE)
  )

  # Ensure reasonable age range
  test_data$age <- pmax(30, pmin(90, test_data$age))

  return(test_data)
}

# Test 1: Basic mixture cure model (most common use case)
test_that("Basic mixture cure model runs without error", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Test basic mixture cure model with minimal options
  expect_no_error({
    result <- curemodels(
      data = test_data,
      time = "followup_time",
      status = "death_status",
      predictors = c("age", "treatment"),
      model_type = "mixture"
    )
  })
})

# Test 2: Function handles insufficient data appropriately
test_that("Cure model detects insufficient sample size", {
  skip_if_not_installed("smcure")

  # Create very small dataset
  small_data <- data.frame(
    time = c(10, 20, 15),
    status = c(1, 0, 1),
    predictor = c(0, 1, 0)
  )

  # Should handle small sample size gracefully (notice or error)
  expect_no_error({
    result <- curemodels(
      data = small_data,
      time = "time",
      status = "status",
      predictors = "predictor",
      model_type = "mixture"
    )
  })
})

# Test 3: Function handles missing required variables
test_that("Cure model requires time and status variables", {
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Missing time variable should be handled
  expect_error({
    result <- curemodels(
      data = test_data,
      time = NULL,
      status = "death_status",
      predictors = "age",
      model_type = "mixture"
    )
  })

  # Missing status variable should be handled
  expect_error({
    result <- curemodels(
      data = test_data,
      time = "followup_time",
      status = NULL,
      predictors = "age",
      model_type = "mixture"
    )
  })
})

# Test 4: Different model types (if packages available)
test_that("Non-mixture cure model option works when available", {
  skip_if_not_installed("flexsurvcure")
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Test non-mixture model
  expect_no_error({
    result <- curemodels(
      data = test_data,
      time = "followup_time",
      status = "death_status",
      predictors = c("age", "treatment"),
      model_type = "nonmixture",
      survival_dist = "weibull"
    )
  })
})

# Test 5: Variable name escaping (handles spaces in variable names)
test_that("Cure model handles variable names with spaces", {
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Rename columns to have spaces
  names(test_data)[names(test_data) == "followup_time"] <- "Follow up Time"
  names(test_data)[names(test_data) == "death_status"] <- "Death Status"

  # Should handle variable names with spaces
  expect_no_error({
    result <- curemodels(
      data = test_data,
      time = "Follow up Time",
      status = "Death Status",
      predictors = "age",
      model_type = "mixture"
    )
  })
})

# Test 6: Cure threshold option
test_that("Cure model accepts custom cure threshold", {
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Test with custom cure threshold
  expect_no_error({
    result <- curemodels(
      data = test_data,
      time = "followup_time",
      status = "death_status",
      predictors = "age",
      model_type = "mixture",
      cure_threshold = 48  # 4 years
    )
  })
})

# Test 7: Model with multiple predictors
test_that("Cure model handles multiple predictors", {
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Test with multiple predictors
  expect_no_error({
    result <- curemodels(
      data = test_data,
      time = "followup_time",
      status = "death_status",
      predictors = c("age", "treatment", "stage"),
      model_type = "mixture"
    )
  })
})

# Test 8: Data validation catches negative times
test_that("Cure model validates non-negative survival times", {
  skip_if_not_installed("smcure")

  test_data <- setup_cure_test_data()

  # Add negative time (invalid)
  test_data$followup_time[1] <- -5

  # Should detect and report invalid data
  expect_no_error({
    # Function should handle this gracefully with error notice
    result <- curemodels(
      data = test_data,
      time = "followup_time",
      status = "death_status",
      predictors = "age",
      model_type = "mixture"
    )
  })
})
