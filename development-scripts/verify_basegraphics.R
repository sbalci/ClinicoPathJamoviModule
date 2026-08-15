
# Verification script for basegraphics function
library(jmvcore)
library(janitor)
library(dplyr)
library(labelled)

# Load the package functions (sourcing the file directly for testing)
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/basegraphics.b.R")
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/basegraphics.h.R")

# Create dummy data
set.seed(123)
N <- 100
data <- data.frame(
  Age = rnorm(N, 50, 10),
  BMI = rnorm(N, 25, 5),
  Group = factor(sample(c("A", "B", "C"), N, replace = TRUE)),
  Outcome = factor(sample(c("Yes", "No"), N, replace = TRUE)),
  Time = rexp(N, 0.1),
  stringsAsFactors = TRUE
)

# Test 1: Scatter Plot
print("Testing Scatter Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "scatter",
    x_var = "Age",
    y_var = "BMI",
    group_var = "Group",
    show_statistics = TRUE
  )
  print("Scatter Plot: Success")
}, error = function(e) {
  print(paste("Scatter Plot: Failed -", e$message))
})

# Test 2: Histogram
print("Testing Histogram...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "histogram",
    x_var = "Age",
    bins = 20,
    show_statistics = TRUE
  )
  print("Histogram: Success")
}, error = function(e) {
  print(paste("Histogram: Failed -", e$message))
})

# Test 3: Box Plot
print("Testing Box Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "boxplot",
    x_var = "BMI",
    group_var = "Group",
    color_scheme = "rainbow"
  )
  print("Box Plot: Success")
}, error = function(e) {
  print(paste("Box Plot: Failed -", e$message))
})

# Test 4: Bar Plot
print("Testing Bar Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "barplot",
    x_var = "Group",
    color_scheme = "heat"
  )
  print("Bar Plot: Success")
}, error = function(e) {
  print(paste("Bar Plot: Failed -", e$message))
})

# Test 5: Density Plot
print("Testing Density Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "density",
    x_var = "Age",
    group_var = "Outcome",
    add_legend = TRUE
  )
  print("Density Plot: Success")
}, error = function(e) {
  print(paste("Density Plot: Failed -", e$message))
})

# Test 6: Line Plot
print("Testing Line Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "line",
    x_var = "Time",
    y_var = "BMI"
  )
  print("Line Plot: Success")
}, error = function(e) {
  print(paste("Line Plot: Failed -", e$message))
})

# Test 7: Pairs Plot
print("Testing Pairs Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "pairs",
    group_var = "Group"
  )
  print("Pairs Plot: Success")
}, error = function(e) {
  print(paste("Pairs Plot: Failed -", e$message))
})

# Test 8: Matrix Plot
print("Testing Matrix Plot...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "matplot"
  )
  print("Matrix Plot: Success")
}, error = function(e) {
  print(paste("Matrix Plot: Failed -", e$message))
})

# Test 9: Custom Limits
print("Testing Custom Limits...")
tryCatch({
  basegraphics(
    data = data,
    plot_type = "scatter",
    x_var = "Age",
    y_var = "BMI",
    custom_limits = TRUE,
    x_min = "20",
    x_max = "80",
    y_min = "10",
    y_max = "40"
  )
  print("Custom Limits: Success")
}, error = function(e) {
  print(paste("Custom Limits: Failed -", e$message))
})
