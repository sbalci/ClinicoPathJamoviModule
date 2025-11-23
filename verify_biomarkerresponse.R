
# Verification script for biomarkerresponse function

# Load necessary libraries
if (!requireNamespace("jmvcore", quietly = TRUE)) stop("jmvcore needed")
if (!requireNamespace("R6", quietly = TRUE)) stop("R6 needed")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 needed")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr needed")
if (!requireNamespace("pROC", quietly = TRUE)) stop("pROC needed")

library(jmvcore)
library(R6)
library(ggplot2)
library(dplyr)
library(pROC)

# Source the R files
source("R/biomarkerresponse.h.R")
source("R/biomarkerresponse.b.R")

# Create test data
set.seed(123)
N <- 200
test_data <- data.frame(
  biomarker = rlnorm(N, meanlog = 2, sdlog = 0.5),
  response_binary = factor(sample(c("Responder", "Non-responder"), N, replace = TRUE, prob = c(0.4, 0.6))),
  response_categorical = factor(sample(c("CR", "PR", "SD", "PD"), N, replace = TRUE)),
  response_continuous = rnorm(N, 50, 15),
  group = factor(sample(c("Treatment", "Control"), N, replace = TRUE))
)

# Add some signal
test_data$biomarker[test_data$response_binary == "Responder"] <- test_data$biomarker[test_data$response_binary == "Responder"] * 1.5
test_data$response_continuous <- test_data$response_continuous + 0.5 * test_data$biomarker

# Helper function to run analysis
run_biomarker_analysis <- function(options_list, test_name) {
  cat(paste0("\nTesting ", test_name, "...\n"))
  
  tryCatch({
    # Helper to get option or default
    get_opt <- function(opt, default) {
      if (is.null(opt)) return(default)
      return(opt)
    }

    # Initialize options
    options <- biomarkerresponseOptions$new(
      biomarker = options_list$biomarker,
      response = options_list$response,
      responseType = get_opt(options_list$responseType, "categorical"),
      positiveLevel = options_list$positiveLevel,
      plotType = get_opt(options_list$plotType, "boxplot"),
      showThreshold = get_opt(options_list$showThreshold, FALSE),
      thresholdValue = get_opt(options_list$thresholdValue, ""),
      thresholdMethod = get_opt(options_list$thresholdMethod, "median"),
      addTrendLine = get_opt(options_list$addTrendLine, FALSE),
      trendMethod = get_opt(options_list$trendMethod, "loess"),
      performTests = get_opt(options_list$performTests, FALSE),
      groupVariable = options_list$groupVariable,
      showCorrelation = get_opt(options_list$showCorrelation, FALSE),
      logTransform = get_opt(options_list$logTransform, FALSE),
      outlierHandling = get_opt(options_list$outlierHandling, "highlight"),
      confidenceLevel = get_opt(options_list$confidenceLevel, "0.95")
    )
    
    # Initialize analysis
    analysis <- biomarkerresponseClass$new(
      options = options,
      data = test_data
    )
    
    # Run analysis
    analysis$run()
    
    cat(paste0(test_name, ": Success\n"))
    return(NULL)
    
  }, error = function(e) {
    cat(paste0(test_name, ": Failed - ", e$message, "\n"))
    return(NULL)
  })
}

# 1. Test Binary Response
run_biomarker_analysis(list(
  biomarker = "biomarker",
  response = "response_binary",
  responseType = "binary",
  positiveLevel = "Responder",
  plotType = "boxplot",
  showThreshold = TRUE,
  thresholdMethod = "optimal",
  performTests = TRUE,
  outlierHandling = "highlight",
  confidenceLevel = "0.95"
), "Binary Response Analysis")

# 2. Test Categorical Response
run_biomarker_analysis(list(
  biomarker = "biomarker",
  response = "response_categorical",
  responseType = "categorical",
  plotType = "violin",
  performTests = TRUE,
  outlierHandling = "none"
), "Categorical Response Analysis")

# 3. Test Continuous Response
run_biomarker_analysis(list(
  biomarker = "biomarker",
  response = "response_continuous",
  responseType = "continuous",
  plotType = "scatter",
  addTrendLine = TRUE,
  trendMethod = "lm",
  showCorrelation = TRUE,
  logTransform = TRUE
), "Continuous Response Analysis")

# 4. Test Grouping Variable
run_biomarker_analysis(list(
  biomarker = "biomarker",
  response = "response_binary",
  responseType = "binary",
  groupVariable = "group",
  plotType = "boxplot"
), "Grouped Analysis")

# 5. Test Manual Threshold
run_biomarker_analysis(list(
  biomarker = "biomarker",
  response = "response_binary",
  responseType = "binary",
  showThreshold = TRUE,
  thresholdMethod = "manual",
  thresholdValue = "10"
), "Manual Threshold Analysis")

# 6. Test Missing Optional Arguments (should use defaults)
run_biomarker_analysis(list(
  biomarker = "biomarker",
  response = "response_binary",
  responseType = "binary"
), "Minimal Arguments Analysis")
