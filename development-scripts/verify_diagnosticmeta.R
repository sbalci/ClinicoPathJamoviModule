
library(jmvcore)
library(dplyr)
library(mada)
library(metafor)

# Source the diagnosticmeta implementation
source("R/diagnosticmeta.h.R")
source("R/diagnosticmeta.b.R")

# Function to run analysis
run_meta_test <- function(data, options, test_name) {
  cat(paste0("\nTesting ", test_name, "...\n"))
  
  # Create analysis object
  analysis <- diagnosticmetaClass$new(
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
    if (!is.null(analysis$results$bivariateresults$state)) {
        print(analysis$results$bivariateresults$state)
    }
    
    cat(paste0(test_name, ": Success\n"))
  }, error = function(e) {
    cat(paste0(test_name, ": Failed - ", e$message, "\n"))
    print(e)
  })
}

# --- Test Case 1: Basic Bivariate Analysis ---
cat("\n--- Test Case 1: Basic Bivariate Analysis ---\n")
# Use built-in mada dataset
data(AuditC)
audit_data <- AuditC
audit_data$study <- rownames(AuditC)
colnames(audit_data) <- c("tp", "fp", "fn", "tn", "study")

options_basic <- diagnosticmetaOptions$new(
  study = "study",
  true_positives = "tp",
  false_positives = "fp",
  false_negatives = "fn",
  true_negatives = "tn",
  bivariate_analysis = TRUE,
  forest_plot = TRUE,
  sroc_plot = TRUE,
  show_analysis_summary = TRUE
)

run_meta_test(audit_data, options_basic, "Basic Bivariate Analysis")

# --- Test Case 2: HSROC Analysis ---
cat("\n--- Test Case 2: HSROC Analysis ---\n")
options_hsroc <- diagnosticmetaOptions$new(
  study = "study",
  true_positives = "tp",
  false_positives = "fp",
  false_negatives = "fn",
  true_negatives = "tn",
  hsroc_analysis = TRUE
)

run_meta_test(audit_data, options_hsroc, "HSROC Analysis")

# --- Test Case 3: Meta-Regression ---
cat("\n--- Test Case 3: Meta-Regression ---\n")
# Add a dummy covariate
set.seed(123)
audit_data$quality <- sample(c("High", "Low"), nrow(audit_data), replace = TRUE)

options_metareg <- diagnosticmetaOptions$new(
  study = "study",
  true_positives = "tp",
  false_positives = "fp",
  false_negatives = "fn",
  true_negatives = "tn",
  meta_regression = TRUE,
  covariate = "quality"
)

run_meta_test(audit_data, options_metareg, "Meta-Regression")

# --- Test Case 4: Publication Bias ---
cat("\n--- Test Case 4: Publication Bias ---\n")
options_pubbias <- diagnosticmetaOptions$new(
  study = "study",
  true_positives = "tp",
  false_positives = "fp",
  false_negatives = "fn",
  true_negatives = "tn",
  publication_bias = TRUE,
  funnel_plot = TRUE
)

run_meta_test(audit_data, options_pubbias, "Publication Bias")

# --- Test Case 5: Heterogeneity Analysis ---
cat("\n--- Test Case 5: Heterogeneity Analysis ---\n")
options_het <- diagnosticmetaOptions$new(
  study = "study",
  true_positives = "tp",
  false_positives = "fp",
  false_negatives = "fn",
  true_negatives = "tn",
  heterogeneity_analysis = TRUE
)

run_meta_test(audit_data, options_het, "Heterogeneity Analysis")
