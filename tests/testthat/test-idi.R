# Tests for idi (Integrated Discrimination Improvement) function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("idi works with basic inputs", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    outcome = factor(sample(c("Event", "NoEvent"), n, replace = TRUE)),
    baseline_risk = runif(n, 0, 1),
    new_risk = runif(n, 0, 1)
  )
  
  expect_no_error({
    result <- idi(
      data = data,
      outcome = "outcome",
      baseline_risk = "baseline_risk",
      new_risk = "new_risk",
      time_var = NULL,
      subgroup_var = NULL,
      idi_type = "standard",
      bootstrap_samples = 100,
      show_summary = TRUE,
      show_distributions = FALSE,
      plot_risk_distributions = FALSE,
      plot_discrimination = FALSE,
      plot_scatter = FALSE
    )
  })
})

test_that("idi returns results object", {
  set.seed(456)
  n <- 80
  data <- data.frame(
    outcome = factor(sample(c("Yes", "No"), n, replace = TRUE)),
    baseline_risk = runif(n, 0, 1),
    new_risk = runif(n, 0, 1)
  )
  
  result <- idi(
    data = data,
    outcome = "outcome",
    baseline_risk = "baseline_risk", 
    new_risk = "new_risk",
    time_var = NULL,
    subgroup_var = NULL,
    idi_type = "standard",
    bootstrap_samples = 100,
    plot_risk_distributions = FALSE,
    plot_discrimination = FALSE,
    plot_scatter = FALSE
  )
  
  expect_true(inherits(result, "idiResults"))
})
