# Tests for costeffectiveness function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("costeffectiveness works with basic inputs", {
  set.seed(123)
  n <- 50
  data <- data.frame(
    strategy = factor(sample(c("Strategy A", "Strategy B"), n, replace = TRUE), levels = c("Strategy A", "Strategy B")),
    cost = runif(n, 1000, 5000),
    effectiveness = runif(n, 0.5, 0.9),
    subgroup = factor(sample(c("Male", "Female"), n, replace = TRUE))
  )
  
  expect_no_error({
    result <- costeffectiveness(
      data = data,
      strategy = "strategy",
      comparator_level = "Strategy A",
      cost = "cost",
      effectiveness = "effectiveness",
      effectiveness_type = "custom",
      subgroup_analysis = FALSE,
      subgroup_variable = NULL,
      plot_ce_plane = FALSE
    )
  })
})

test_that("costeffectiveness returns results object", {
  set.seed(456)
  n <- 60
  data <- data.frame(
    treat = factor(sample(c("New", "Old"), n, replace = TRUE), levels = c("New", "Old")),
    cost = rnorm(n, 5000, 1000),
    qaly = rnorm(n, 0.8, 0.1)
  )
  
  result <- costeffectiveness(
    data = data,
    strategy = "treat",
    comparator_level = "Old",
    cost = "cost", 
    effectiveness = "qaly",
    effectiveness_type = "qaly",
    subgroup_analysis = FALSE,
    subgroup_variable = NULL,
    plot_ce_plane = FALSE
  )
  
  expect_true(inherits(result, "costeffectivenessResults"))
})
