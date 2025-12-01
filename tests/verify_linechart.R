
# Verification Script for linechart
# This script generates plots and tables to verify the functionality and statistical validity of linechart

library(ggplot2)
library(dplyr)
library(jmvcore)

# Source the implementation
# We need to ensure jmvcore is loaded so linechartClass is created
if (!requireNamespace("jmvcore", quietly = TRUE)) {
  stop("jmvcore is required")
}

# Source the file to get linechartClass definition
# We need to mock linechartBase first because linechartClass inherits from it
linechartBase <- R6::R6Class("linechartBase") 
source("R/linechart.b.R")

# Extract private methods from linechartClass
# linechartClass is an R6 generator object
private_methods <- linechartClass$private_methods

# Add our mock helper methods to private_methods if needed, or just mock them in the class
# We need to mock .checkpoint because it's not in the file (it's likely in linechartBase or Analysis)
private_methods$.checkpoint <- function(flush = TRUE) {}

# Define MockLineChart using the extracted private methods
MockLineChart <- R6::R6Class(
  "MockLineChart",
  public = list(
    data = NULL,
    options = NULL,
    results = NULL,
    
    initialize = function(data, options) {
      self$data <- data
      
      # Add mock translate function to options
      options$translate <- function(text, ...) return(text)
      self$options <- options
      
      # Mock results structure
      self$results <- list(
        todo = list(
          content = "",
          setContent = function(x) { self$results$todo$content <- x },
          setVisible = function(x) {},
          visible = TRUE
        ),
        summary = list(
          rows = list(),
          deleteRows = function() { self$results$summary$rows <- list() },
          addRow = function(rowKey, values) { 
            self$results$summary$rows[[length(self$results$summary$rows) + 1]] <- values 
          },
          setVisible = function(x) {},
          visible = TRUE
        ),
        correlation = list(
          rows = list(),
          deleteRows = function() { self$results$correlation$rows <- list() },
          addRow = function(rowKey, values) { 
            self$results$correlation$rows[[length(self$results$correlation$rows) + 1]] <- values 
          },
          setVisible = function(x) {},
          visible = TRUE
        ),
        assumptions = list(
          content = "",
          setContent = function(x) { self$results$assumptions$content <- x },
          setVisible = function(x) {},
          visible = TRUE
        ),
        plot = list(
          state = NULL,
          setState = function(x) { self$results$plot$state <- x },
          setVisible = function(x) {},
          visible = TRUE
        )
      )
      
      # Initialize
      private$.init()
    },
    
    run = function() {
      private$.run()
    },
    
    # Helper to access results
    get_summary = function() {
      return(self$results$summary$rows)
    },
    
    get_correlation = function() {
      return(self$results$correlation$rows)
    },
    
    get_plot_data = function() {
      return(self$results$plot$state)
    },
    
    get_todo_content = function() {
      return(self$results$todo$content)
    }
  ),
  private = private_methods
)

# Helper to run analysis and print results
run_analysis <- function(title, data, options) {
  cat("\n" %+% paste(rep("=", 60), collapse = "") %+% "\n")
  cat("TEST CASE: " %+% title %+% "\n")
  cat(paste(rep("=", 60), collapse = "") %+% "\n")
  
  # Create analysis instance
  # Note: We are using our own initialize signature: (data, options)
  analysis <- MockLineChart$new(data, options)
  
  # Run analysis
  tryCatch({
    analysis$run()
    
    # Print Summary
    cat("\n--- Summary Table ---\n")
    summary_rows <- analysis$get_summary()
    for (row in summary_rows) {
      cat(paste0(row$statistic, ": ", row$value, "\n"))
    }
    
    # Print Correlation
    cat("\n--- Correlation Table ---\n")
    corr_rows <- analysis$get_correlation()
    if (length(corr_rows) > 0) {
      for (row in corr_rows) {
        cat(paste0(row$measure, ": ", row$value, " (", row$interpretation, ")\n"))
      }
    } else {
      cat("No correlation results.\n")
    }
    
    # Print Warnings/Todo
    todo_content <- analysis$get_todo_content()
    if (nchar(todo_content) > 0) {
      cat("\n--- Warnings/Messages ---\n")
      # Strip HTML for readability
      clean_msg <- gsub("<[^>]+>", "", todo_content)
      cat(clean_msg, "\n")
    }
    
  }, error = function(e) {
    cat("\nERROR: ", e$message, "\n")
    print(e) # Print stack trace if possible
  })
}

# Load test data
load("data/linechart_hemoglobin.rda") # Longitudinal (repeated measures)

# Test Case 1: Basic Time Series (Single Group, Independent-ish)
# We'll subset to one patient to simulate a single time series
single_patient <- hemoglobin_data[hemoglobin_data$patient_id == "P001", ]
options_1 <- list(
  xvar = "visit_week",
  yvar = "hemoglobin_g_dl",
  groupby = NULL,
  confidence = TRUE,
  trendline = TRUE,
  points = TRUE,
  smooth = FALSE,
  refline = 12,
  reflineLabel = "Lower Limit",
  colorPalette = "default",
  theme = "publication",
  xlabel = "Week",
  ylabel = "Hemoglobin (g/dL)",
  title = "Patient P001 Hemoglobin"
)
run_analysis("Single Patient Time Series", single_patient, options_1)

# Test Case 2: Grouped Longitudinal Data (Repeated Measures)
# This should trigger warnings about repeated measures
options_2 <- list(
  xvar = "visit_week",
  yvar = "hemoglobin_g_dl",
  groupby = "treatment_group",
  confidence = TRUE,
  trendline = TRUE,
  points = FALSE,
  smooth = TRUE,
  refline = NULL,
  colorPalette = "clinical",
  theme = "publication",
  xlabel = "Week",
  ylabel = "Hemoglobin (g/dL)",
  title = "Hemoglobin by Treatment Group"
)
run_analysis("Grouped Longitudinal Data (Repeated Measures)", hemoglobin_data, options_2)

# Test Case 3: Repeated Measures without Grouping
# All patients, no grouping variable. Should definitely warn about repeated measures.
options_3 <- list(
  xvar = "visit_week",
  yvar = "hemoglobin_g_dl",
  groupby = NULL,
  confidence = TRUE,
  trendline = TRUE,
  points = FALSE,
  smooth = TRUE,
  refline = NULL,
  colorPalette = "default",
  theme = "default",
  xlabel = "Week",
  ylabel = "Hemoglobin (g/dL)",
  title = "Overall Hemoglobin Trend"
)
run_analysis("Repeated Measures (No Grouping)", hemoglobin_data, options_3)
