
library(jmvcore)
library(dplyr)
library(ggplot2)

# Source the ihcheterogeneity implementation
source("R/ihcheterogeneity.h.R")
source("R/ihcheterogeneity.b.R")

# Function to run analysis
run_ihc_test <- function(data, options, test_name) {
  cat(paste0("\nTesting ", test_name, "...\n"))
  
  # Create analysis object
  analysis <- ihcheterogeneityClass$new(
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
    if (!is.null(analysis$results$reproducibilitytable$state)) {
        print(analysis$results$reproducibilitytable$state)
    }
    
    cat(paste0(test_name, ": Success\n"))
  }, error = function(e) {
    cat(paste0(test_name, ": Failed - ", e$message, "\n"))
    print(e)
  })
}

# --- Test Case 1: Reference-Based Analysis ---
cat("\n--- Test Case 1: Reference-Based Analysis ---\n")
set.seed(123)
n_cases <- 20
data_ref <- data.frame(
  WholeSection = runif(n_cases, 10, 90),
  Biopsy1 = runif(n_cases, 10, 90),
  Biopsy2 = runif(n_cases, 10, 90),
  Biopsy3 = runif(n_cases, 10, 90)
)
# Add some correlation
data_ref$Biopsy1 <- data_ref$WholeSection + rnorm(n_cases, 0, 5)
data_ref$Biopsy2 <- data_ref$WholeSection + rnorm(n_cases, 0, 10)
data_ref$Biopsy3 <- data_ref$WholeSection + rnorm(n_cases, 0, 15)

options_ref <- ihcheterogeneityOptions$new(
  wholesection = "WholeSection",
  biopsy1 = "Biopsy1",
  biopsy2 = "Biopsy2",
  biopsies = c("Biopsy3"),
  analysis_type = "comprehensive",
  showSummary = TRUE,
  showGlossary = TRUE
)

run_ihc_test(data_ref, options_ref, "Reference-Based Analysis")

# --- Test Case 2: Inter-Regional Analysis ---
cat("\n--- Test Case 2: Inter-Regional Analysis ---\n")
data_inter <- data.frame(
  Biopsy1 = runif(n_cases, 10, 90),
  Biopsy2 = runif(n_cases, 10, 90),
  Biopsy3 = runif(n_cases, 10, 90)
)
# Add some correlation
data_inter$Biopsy2 <- data_inter$Biopsy1 + rnorm(n_cases, 0, 5)
data_inter$Biopsy3 <- data_inter$Biopsy1 + rnorm(n_cases, 0, 8)

options_inter <- ihcheterogeneityOptions$new(
  biopsy1 = "Biopsy1",
  biopsy2 = "Biopsy2",
  biopsies = c("Biopsy3"),
  analysis_type = "variability",
  showSummary = TRUE
)

run_ihc_test(data_inter, options_inter, "Inter-Regional Analysis")

# --- Test Case 3: Spatial Analysis ---
cat("\n--- Test Case 3: Spatial Analysis ---\n")
data_spatial <- data_ref
data_spatial$RegionID <- rep(c("Central", "Peripheral"), each = 10)

options_spatial <- ihcheterogeneityOptions$new(
  wholesection = "WholeSection",
  biopsy1 = "Biopsy1",
  biopsy2 = "Biopsy2",
  spatial_id = "RegionID",
  analysis_type = "comprehensive"
)

run_ihc_test(data_spatial, options_spatial, "Spatial Analysis")

# --- Test Case 4: Insufficient Data ---
cat("\n--- Test Case 4: Insufficient Data ---\n")
data_small <- data_ref[1:3, ]

options_small <- ihcheterogeneityOptions$new(
  wholesection = "WholeSection",
  biopsy1 = "Biopsy1",
  biopsy2 = "Biopsy2",
  analysis_type = "comprehensive"
)

run_ihc_test(data_small, options_small, "Insufficient Data Analysis")

# --- Test Case 5: Misuse Detection (Negative Values) ---
cat("\n--- Test Case 5: Misuse Detection (Negative Values) ---\n")
data_neg <- data_ref
data_neg$Biopsy1[1] <- -5

options_neg <- ihcheterogeneityOptions$new(
  wholesection = "WholeSection",
  biopsy1 = "Biopsy1",
  biopsy2 = "Biopsy2",
  analysis_type = "comprehensive"
)

run_ihc_test(data_neg, options_neg, "Misuse Detection Analysis")
