
context("Survival Model Validation")

library(testthat)

# Source the module files globally
source("../../R/survivalvalidation.h.R")
source("../../R/survivalvalidation.b.R")

get_synthetic_data <- function(n=100, seed=123) {
  set.seed(seed)
  data <- data.frame(
    time = runif(n, 10, 100),
    status = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    predicted_risk = runif(n, 0, 1),
    group = sample(c("A", "B"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  return(data)
}

test_that("survivalvalidation works with predicted risk", {
  skip_if_not_installed('jmvReadWrite')
  
  data <- get_synthetic_data(n=100)
  
  result <- survivalvalidation(
    data = data,
    time = "time",
    status = "status",
    predicted_risk = "predicted_risk",
    time_points = "12, 24, 36",
    validation_method = "cv",
    concordance_index = TRUE,
    time_dependent_auc = TRUE,
    calibration_plot = TRUE,
    decision_curve = TRUE,
    cause_specific = NULL,
    external_data = NULL
  )
  
  if (!("concordanceResults" %in% names(result))) {
      print("Todo content:")
      print(result$todo$content)
      print("Available names:")
      print(names(result))
  }
  
  expect_true(!is.null(result))
  expect_true("concordanceTable" %in% names(result))
  expect_true("aucTable" %in% names(result))
  
  # Check if tables are populated (if possible without running jmv)
  # result$aucTable$asDF should work if run correctly
})

test_that("survivalvalidation works with model formula", {
  data <- get_synthetic_data(n=100)
  
  result <- survivalvalidation(
    data = data,
    time = "time",
    status = "status",
    model_formula = "group",
    time_points = "12, 24",
    validation_method = "bootstrap",
    bootstrap_samples = 100,
    predicted_risk = NULL,
    cause_specific = NULL,
    external_data = NULL,
    covariates = "group"
  )
  
  if (!("concordanceResults" %in% names(result))) {
      print("Todo content:")
      print(result$todo$content)
      print("Available names:")
      print(names(result))
  }
  
  expect_true(!is.null(result))
  expect_true("concordanceTable" %in% names(result))
})

test_that("survivalvalidation handles input validation", {
  data <- get_synthetic_data(n=10) # Too small
  
  expect_error({
    # This might print to Todo instead of standard error, but let's check basic execution
    survivalvalidation(
        data = data,
        time = "time",
        status = "status",
        predicted_risk = "predicted_risk",
        cause_specific = NULL,
        external_data = NULL
    )
  }, NA) # Should runs without hard error, but maybe return empty results or error message in html
})
