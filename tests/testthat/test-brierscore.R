# Test suite for brierscore function
# Simplified tests that provide all required parameters

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("brierscore handles basic inputs", {
  skip_if_not_installed("survival")
  
  set.seed(123)
  n <- 100
  
  # Create survival data with proper structure
  data <- data.frame(
    time = rexp(n, 0.01),
    event = factor(sample(c("Alive", "Death"), n, replace = TRUE)),
    pred_surv = runif(n, 0.3, 0.9),
    strat = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  
  # Run analysis with all required params
  result <- brierscore(
    data = data,
    time = "time",
    event = "event", 
    event_code = "Death",
    predicted_survival = "pred_surv",
    linear_predictor = NULL,
    prediction_formula = "",
    prediction_time = 50,
    multiple_time_points = FALSE,
    time_points = "12, 36, 60",
    calculate_ibs = FALSE,
    ibs_start_time = 0,
    ibs_end_time = 60,
    scaled_brier = TRUE,
    reference_model = FALSE,
    reference_predictions = NULL,
    reference_formula = "",
    confidence_intervals = FALSE,
    ci_method = "bootstrap",
    bootstrap_samples = 500,
    confidence_level = 0.95,
    compare_multiple_models = FALSE,
    additional_predictions = NULL,
    model_names = "Model 1, Model 2, Model 3",
    competing_risks = FALSE,
    cause_specific = TRUE,
    plot_brier_over_time = FALSE,
    plot_calibration_curve = FALSE,
    calibration_groups = 10,
    plot_model_comparison = FALSE,
    plot_integrated_brier = FALSE,
    stratified_brier = FALSE,
    stratify_by = NULL,
    inverse_probability_weighting = TRUE,
    external_validation = FALSE,
    temporal_validation = FALSE,
    development_period = "",
    missing_handling = "complete",
    random_seed = 123
  )
  
  expect_true(inherits(result, "brierscoreResults"))
  expect_true(result$brierSummary$rowCount > 0)
})

test_that("brierscore rejects IBS (not implemented)", {
  skip_if_not_installed("survival")
  
  set.seed(123)
  n <- 100
  data <- data.frame(
    time = rexp(n, 0.01),
    event = factor(sample(c("Alive", "Death"), n, replace = TRUE)),
    pred_surv = runif(n, 0.3, 0.9),
    strat = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  
  expect_error({
    result <- brierscore(
      data = data,
      time = "time",
      event = "event",
      event_code = "Death",
      predicted_survival = "pred_surv",
      linear_predictor = NULL,
      prediction_formula = "",
      prediction_time = 50,
      multiple_time_points = FALSE,
      time_points = "12, 36, 60",
      calculate_ibs = TRUE,  # This should trigger error
      ibs_start_time = 0,
      ibs_end_time = 60,
      scaled_brier = TRUE,
      reference_model = FALSE,
      reference_predictions = NULL,
      reference_formula = "",
      confidence_intervals = FALSE,
      ci_method = "bootstrap",
      bootstrap_samples = 500,
      confidence_level = 0.95,
      compare_multiple_models = FALSE,
      additional_predictions = NULL,
      model_names = "Model 1, Model 2, Model 3",
      competing_risks = FALSE,
      cause_specific = TRUE,
      plot_brier_over_time = FALSE,
      plot_calibration_curve = FALSE,
      calibration_groups = 10,
      plot_model_comparison = FALSE,
      plot_integrated_brier = FALSE,
      stratified_brier = FALSE,
      stratify_by = NULL,
      inverse_probability_weighting = TRUE,
      external_validation = FALSE,
      temporal_validation = FALSE,
      development_period = "",
      missing_handling = "complete",
      random_seed = 123
    )
  }, "NOT SUPPORTED")
})
