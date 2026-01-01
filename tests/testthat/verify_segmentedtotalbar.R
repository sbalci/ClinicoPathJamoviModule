
library(testthat)
library(jmvcore)
library(R6)
library(ggplot2)
library(dplyr)

# Source the necessary files
source("R/jjsegmentedtotalbar.h.R")
source("R/jjsegmentedtotalbar.b.R")

# Mock the ggsegmentedtotalbar package if it's missing or external
# (Assuming it might be a typo or a missing dependency, but let's see if it runs without it first if we don't use that plot option)

test_that("jjsegmentedtotalbar handles raw data correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  set.seed(123)
  data <- data.frame(
    group = sample(c("A", "B", "C"), 100, replace = TRUE),
    response = sample(c("Yes", "No"), 100, replace = TRUE),
    value = rnorm(100, 10, 2) # Numeric value for y_var
  )
  
  options <- jjsegmentedtotalbarOptions$new(
    x_var = "group",
    y_var = "value",
    fill_var = "response",
    show_ggplot2_plot = TRUE
  )
  
  analysis <- jjsegmentedtotalbarClass$new(
    options = options,
    data = data
  )
  
  # We can't easily run analysis$run() because of the image render functions being called by jamovi
  # But we can inspect the private methods if we could access them, or just try to run it and catch errors.
  
  # Let's try to access the private .processData method via a trick or just rely on the fact that we sourced the file.
  # Since R6 privates are hard to access, we might need to modify the class to make them public for testing, 
  # or use a helper that exposes them.
  
  # Alternatively, we can check if the .run method executes without error.
  expect_error(analysis$run(), NA)
})

test_that("jjsegmentedtotalbar handles aggregated data correctly", {
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    response = c("Yes", "No", "Yes", "No"),
    count = c(10, 20, 30, 40)
  )
  
  options <- jjsegmentedtotalbarOptions$new(
    x_var = "group",
    y_var = "count",
    fill_var = "response",
    show_ggplot2_plot = TRUE
  )
  
  analysis <- jjsegmentedtotalbarClass$new(
    options = options,
    data = data
  )
  
  expect_error(analysis$run(), NA)
})
