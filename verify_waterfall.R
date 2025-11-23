
library(jmvcore)
library(R6)
library(ggplot2)
library(dplyr)

source("R/waterfall.h.R")
source("R/waterfall.b.R")

# Helper to run analysis and catch errors
run_waterfall_analysis <- function(data, options_list, test_name) {
  cat(paste0("\nTesting ", test_name, "...\n"))
  
  tryCatch({
    # Initialize options
    options <- waterfallOptions$new(
      patientID = options_list$patientID,
      responseVar = options_list$responseVar,
      timeVar = options_list$timeVar,
      groupVar = options_list$groupVar,
      inputType = options_list$inputType,
      sortBy = options_list$sortBy,
      showThresholds = options_list$showThresholds,
      labelOutliers = options_list$labelOutliers,
      showMedian = options_list$showMedian,
      showCI = options_list$showCI,
      minResponseForLabel = options_list$minResponseForLabel,
      colorBy = options_list$colorBy,
      colorScheme = options_list$colorScheme,
      barAlpha = options_list$barAlpha,
      barWidth = options_list$barWidth,
      showWaterfallPlot = options_list$showWaterfallPlot,
      showSpiderPlot = options_list$showSpiderPlot,
      spiderColorBy = options_list$spiderColorBy,
      spiderColorScheme = options_list$spiderColorScheme,
      timeUnitLabel = options_list$timeUnitLabel,
      generateCopyReadyReport = options_list$generateCopyReadyReport,
      showClinicalSignificance = options_list$showClinicalSignificance,
      showConfidenceIntervals = options_list$showConfidenceIntervals,
      enableGuidedMode = options_list$enableGuidedMode
    )
    
    # Initialize analysis
    cat("Initializing analysis...\n")
    analysis <- waterfallClass$new(
      options = options,
      data = data
    )
    cat("Analysis initialized.\n")
    
    # Run analysis
    cat("Running analysis (private)...\n")
    analysis$.__enclos_env__$private$.run()
    cat("Analysis finished.\n")
    
    cat(paste0(test_name, ": Success\n"))
    return(NULL)
    
  }, error = function(e) {
    cat(paste0(test_name, ": Failed - ", e$message, "\n"))
    return(NULL)
  })
}

# Test Data
data_pct <- data.frame(
  PatientID = paste0("PT", 1:5),
  Response = c(-60, -35, -10, 15, 45),
  Group = c("A", "A", "B", "B", "A"),
  stringsAsFactors = FALSE
)

data_raw <- data.frame(
  PatientID = rep(paste0("PT", 1:3), each = 3),
  Time = rep(c(0, 2, 4), 3),
  Measurement = c(50, 30, 25, 60, 45, 40, 55, 50, 48),
  Group = rep(c("A", "B", "A"), each = 3),
  stringsAsFactors = FALSE
)

# Test 1: Basic Percentage Analysis
run_waterfall_analysis(
  data = data_pct,
  options_list = list(
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    sortBy = "response",
    showThresholds = TRUE,
    showWaterfallPlot = TRUE
  ),
  test_name = "Basic Percentage Analysis"
)

# Test 2: Raw Data Analysis
run_waterfall_analysis(
  data = data_raw,
  options_list = list(
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time",
    inputType = "raw",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE
  ),
  test_name = "Raw Data Analysis"
)

# Test 3: Grouped Analysis
run_waterfall_analysis(
  data = data_pct,
  options_list = list(
    patientID = "PatientID",
    responseVar = "Response",
    groupVar = "Group",
    inputType = "percentage",
    colorBy = "group",
    colorScheme = "colorful"
  ),
  test_name = "Grouped Analysis"
)

# Test 4: Missing Time Variable for Raw Data (Should Fail Gracefully)
run_waterfall_analysis(
  data = data_raw,
  options_list = list(
    patientID = "PatientID",
    responseVar = "Measurement",
    inputType = "raw"
  ),
  test_name = "Missing Time Variable (Expected Failure/Warning)"
)

# Test 5: Clinical Reporting
run_waterfall_analysis(
  data = data_pct,
  options_list = list(
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    generateCopyReadyReport = TRUE,
    showClinicalSignificance = TRUE
  ),
  test_name = "Clinical Reporting"
)
