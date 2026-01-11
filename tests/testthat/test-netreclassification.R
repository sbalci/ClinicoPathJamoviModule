# Tests for netreclassification function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("netreclassification works with basic inputs", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    outcome = factor(sample(c("Event", "NoEvent"), n, replace = TRUE)),
    baseline_risk = runif(n, 0, 1),
    new_risk = runif(n, 0, 1)
  )
  
  expect_no_error({
    result <- netreclassification(
      data = data,
      outcome = "outcome",
      baseline_risk = "baseline_risk",
      new_risk = "new_risk",
      time_var = NULL,
      subgroup_var = NULL,
      nri_type = "categorical",
      bootstrap_samples = 100,
      plot_reclassification = FALSE,
      plot_risk_distribution = FALSE,
      plot_improvement = FALSE
    )
  })
})

test_that("netreclassification returns results object", {
  set.seed(456)
  n <- 80
  data <- data.frame(
    outcome = factor(sample(c("Yes", "No"), n, replace = TRUE)),
    baseline_risk = runif(n, 0, 1),
    new_risk = runif(n, 0, 1)
  )
  
  result <- netreclassification(
    data = data,
    outcome = "outcome",
    baseline_risk = "baseline_risk",
    new_risk = "new_risk",
    time_var = NULL,
    subgroup_var = NULL,
    nri_type = "categorical",
    bootstrap_samples = 100,
    plot_reclassification = FALSE,
    plot_risk_distribution = FALSE,
    plot_improvement = FALSE
  )
  
  expect_true(inherits(result, "netreclassificationResults"))
})
