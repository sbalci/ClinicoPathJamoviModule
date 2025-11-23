
library(jmvcore)
library(dplyr)
library(ggplot2)

# Source the swimmerplot implementation
source("R/swimmerplot.h.R")
source("R/swimmerplot.b.R")

# Mock the ggswim package if not available (for testing purposes if needed)
# But ideally we want to test with the actual package if installed.

# Function to run analysis
run_swimmer_test <- function(data, options, test_name) {
  cat(paste0("\nTesting ", test_name, "...\n"))
  
  # Create analysis object
  analysis <- swimmerplotClass$new(
    options = options,
    data = data
  )
  
  # Initialize
  cat("Initializing analysis...\n")
  analysis$init()
  cat("Analysis initialized.\n")
  
  # Run analysis
  cat("Running analysis (private)...\n")
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Analysis finished.\n")
    
    # Check results
    if (!is.null(analysis$results$summary$state)) {
        print(analysis$results$summary$state)
    }
    
    cat(paste0(test_name, ": Success\n"))
  }, error = function(e) {
    cat(paste0(test_name, ": Failed - ", e$message, "\n"))
    print(e)
  })
}

# --- Test Case 1: Basic Numeric Data ---
cat("\n--- Test Case 1: Basic Numeric Data ---\n")
data_numeric <- data.frame(
  PatientID = c("P01", "P02", "P03", "P04", "P05"),
  Start = c(0, 0, 0, 0, 0),
  End = c(10, 15, 8, 20, 12),
  Response = c("CR", "PR", "SD", "PD", "PR"),
  stringsAsFactors = FALSE
)

options_numeric <- swimmerplotOptions$new(
  patientID = "PatientID",
  startTime = "Start",
  endTime = "End",
  responseVar = "Response",
  timeType = "raw",
  timeUnit = "months",
  showInterpretation = TRUE,
  personTimeAnalysis = TRUE,
  responseAnalysis = TRUE
)

run_swimmer_test(data_numeric, options_numeric, "Basic Numeric Analysis")

# --- Test Case 2: Date Data (Absolute) ---
cat("\n--- Test Case 2: Date Data (Absolute) ---\n")
data_date <- data.frame(
  PatientID = c("P01", "P02", "P03"),
  Start = c("2023-01-01", "2023-02-01", "2023-03-01"),
  End = c("2023-06-01", "2023-08-01", "2023-05-01"),
  Response = c("CR", "PR", "SD"),
  stringsAsFactors = FALSE
)

options_date <- swimmerplotOptions$new(
  patientID = "PatientID",
  startTime = "Start",
  endTime = "End",
  responseVar = "Response",
  timeType = "datetime",
  dateFormat = "ymd",
  timeDisplay = "absolute",
  timeUnit = "months"
)

run_swimmer_test(data_date, options_date, "Date Analysis (Absolute)")

# --- Test Case 3: Milestones and Events ---
cat("\n--- Test Case 3: Milestones and Events ---\n")
data_complex <- data.frame(
  PatientID = c("P01", "P02", "P03"),
  Start = c(0, 0, 0),
  End = c(12, 18, 6),
  Response = c("CR", "PR", "PD"),
  Surgery = c(2, NA, 1),
  Progression = c(NA, 15, 5),
  EventTime = c(6, 9, 3),
  EventType = c("Toxicity", "Scan", "Toxicity"),
  stringsAsFactors = FALSE
)

options_complex <- swimmerplotOptions$new(
  patientID = "PatientID",
  startTime = "Start",
  endTime = "End",
  responseVar = "Response",
  milestone1Name = "Surgery",
  milestone1Date = "Surgery",
  milestone2Name = "Progression",
  milestone2Date = "Progression",
  showEventMarkers = TRUE,
  eventVar = "EventType",
  eventTimeVar = "EventTime",
  timeType = "raw"
)

run_swimmer_test(data_complex, options_complex, "Complex Analysis with Milestones")

# --- Test Case 4: Missing Data Handling ---
cat("\n--- Test Case 4: Missing Data Handling ---\n")
data_missing <- data.frame(
  PatientID = c("P01", "P02", "P03"),
  Start = c(0, 0, NA),
  End = c(10, NA, 5),
  Response = c("CR", "PR", "SD"),
  stringsAsFactors = FALSE
)

options_missing <- swimmerplotOptions$new(
  patientID = "PatientID",
  startTime = "Start",
  endTime = "End",
  responseVar = "Response",
  timeType = "raw"
)

run_swimmer_test(data_missing, options_missing, "Missing Data Analysis")
