
library(testthat)
library(jmvcore)
library(R6)
library(ggplot2)
library(dplyr)
library(ggstatsplot)
library(broom)
library(survival)
library(lme4)

# Source the necessary files
source("R/jjcoefstats.h.R")
source("R/jjcoefstats.b.R")

test_that("jjcoefstats handles pre-computed data correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  data <- data.frame(
    term = c("A", "B", "C"),
    estimate = c(1.5, 0.5, -0.2),
    std_error = c(0.2, 0.1, 0.3),
    conf_low = c(1.1, 0.3, -0.8),
    conf_high = c(1.9, 0.7, 0.4),
    p_value = c(0.001, 0.02, 0.5)
  )
  
  options <- jjcoefstatsOptions$new(
    inputMode = "precomputed",
    term = "term",
    estimate = "estimate",
    stdError = "std_error",
    confLow = "conf_low",
    confHigh = "conf_high",
    pValue = "p_value",
    degreesOfFreedom = 0,
    plotWidth = 700,
    plotHeight = 500,
    ciLevel = 0.95,
    showexplanations = TRUE
  )
  
  analysis <- jjcoefstatsClass$new(
    options = options,
    data = data
  )
  
  # This should fail if 'about' is missing
  expect_error(analysis$run(), NA)
})

test_that("jjcoefstats handles model fitting (lm) correctly", {
  
  set.seed(123)
  data <- data.frame(
    outcome = rnorm(100),
    pred1 = rnorm(100),
    pred2 = sample(c("A", "B"), 100, replace = TRUE)
  )
  
  options <- jjcoefstatsOptions$new(
    inputMode = "fitmodel",
    modelType = "lm",
    outcome = "outcome",
    predictors = c("pred1", "pred2"),
    showexplanations = TRUE,
    degreesOfFreedom = 0
  )
  
  analysis <- jjcoefstatsClass$new(
    options = options,
    data = data
  )
  
  expect_error(analysis$run(), NA)
})
