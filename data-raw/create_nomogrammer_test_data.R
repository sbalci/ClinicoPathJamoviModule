#' @title Create Test Data for Nomogrammer Function
#' @description Generate comprehensive test datasets for validating the nomogrammer function
#' @author ClinicoPath Development Team

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(42)

#' =============================================================================
#' Test Dataset 1: Standard Clinical Scenarios
#' =============================================================================

# Common diagnostic test scenarios with known sens/spec values
clinical_scenarios <- data.frame(
  scenario = c(
    "Mammography screening",
    "COVID-19 rapid test", 
    "Troponin for MI",
    "PSA for prostate cancer",
    "Pap smear for cervical cancer",
    "Chest X-ray for pneumonia",
    "D-dimer for PE",
    "Glucose for diabetes",
    "Colonoscopy for CRC",
    "Stress test for CAD"
  ),
  prevalence = c(0.005, 0.10, 0.15, 0.25, 0.02, 0.08, 0.05, 0.12, 0.03, 0.20),
  sensitivity = c(0.85, 0.95, 0.99, 0.80, 0.85, 0.70, 0.95, 0.88, 0.95, 0.75),
  specificity = c(0.95, 0.85, 0.85, 0.75, 0.98, 0.90, 0.60, 0.92, 0.90, 0.85),
  stringsAsFactors = FALSE
)

# Calculate likelihood ratios
clinical_scenarios$PLR <- clinical_scenarios$sensitivity / (1 - clinical_scenarios$specificity)
clinical_scenarios$NLR <- (1 - clinical_scenarios$sensitivity) / clinical_scenarios$specificity

# Calculate posterior probabilities
clinical_scenarios$prior_odds <- clinical_scenarios$prevalence / (1 - clinical_scenarios$prevalence)
clinical_scenarios$post_odds_pos <- clinical_scenarios$prior_odds * clinical_scenarios$PLR
clinical_scenarios$post_odds_neg <- clinical_scenarios$prior_odds * clinical_scenarios$NLR
clinical_scenarios$post_prob_pos <- clinical_scenarios$post_odds_pos / (1 + clinical_scenarios$post_odds_pos)
clinical_scenarios$post_prob_neg <- clinical_scenarios$post_odds_neg / (1 + clinical_scenarios$post_odds_neg)

#' =============================================================================
#' Test Dataset 2: Edge Cases and Extreme Values
#' =============================================================================

edge_cases <- data.frame(
  case_type = c(
    "Very low prevalence",
    "Very high prevalence", 
    "Perfect sensitivity",
    "Perfect specificity",
    "Poor sensitivity",
    "Poor specificity",
    "Balanced test",
    "High PLR scenario",
    "Low NLR scenario",
    "Uninformative test"
  ),
  prevalence = c(0.001, 0.999, 0.10, 0.10, 0.10, 0.10, 0.50, 0.30, 0.30, 0.50),
  sensitivity = c(0.90, 0.90, 0.999, 0.70, 0.30, 0.90, 0.80, 0.95, 0.90, 0.50),
  specificity = c(0.90, 0.90, 0.80, 0.999, 0.90, 0.30, 0.80, 0.90, 0.95, 0.50),
  stringsAsFactors = FALSE
)

# Calculate derived values for edge cases
edge_cases$PLR <- edge_cases$sensitivity / (1 - edge_cases$specificity)
edge_cases$NLR <- (1 - edge_cases$sensitivity) / edge_cases$specificity

#' =============================================================================
#' Test Dataset 3: Likelihood Ratio Inputs (for back-calculation testing)
#' =============================================================================

lr_scenarios <- data.frame(
  lr_case = c(
    "Excellent test",
    "Good test",
    "Fair test", 
    "Poor test",
    "Rule-in test",
    "Rule-out test",
    "High PLR only",
    "Low NLR only",
    "Moderate both",
    "Asymmetric LRs"
  ),
  prevalence = c(0.20, 0.15, 0.30, 0.25, 0.10, 0.05, 0.40, 0.35, 0.50, 0.18),
  PLR = c(20.0, 10.0, 5.0, 2.0, 15.0, 8.0, 25.0, 6.0, 4.0, 12.5),
  NLR = c(0.05, 0.10, 0.20, 0.50, 0.30, 0.02, 0.40, 0.08, 0.25, 0.33),
  stringsAsFactors = FALSE
)

# Back-calculate sensitivity and specificity
lr_scenarios$specificity <- (lr_scenarios$PLR - 1) / (lr_scenarios$PLR - lr_scenarios$NLR)
lr_scenarios$sensitivity <- lr_scenarios$PLR * (1 - lr_scenarios$specificity)

#' =============================================================================
#' Test Dataset 4: Mathematical Validation Cases
#' =============================================================================

# Create systematic test cases for mathematical validation
math_validation <- expand.grid(
  prevalence = c(0.01, 0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.90),
  sensitivity = c(0.70, 0.80, 0.85, 0.90, 0.95),
  specificity = c(0.70, 0.80, 0.85, 0.90, 0.95)
)

# Calculate all derived measures
math_validation$PLR <- math_validation$sensitivity / (1 - math_validation$specificity)
math_validation$NLR <- (1 - math_validation$sensitivity) / math_validation$specificity

# Test back-calculation
math_validation$spec_back <- (math_validation$PLR - 1) / (math_validation$PLR - math_validation$NLR)
math_validation$sens_back <- math_validation$PLR * (1 - math_validation$spec_back)

# Check for mathematical consistency
math_validation$spec_match <- abs(math_validation$specificity - math_validation$spec_back) < 1e-10
math_validation$sens_match <- abs(math_validation$sensitivity - math_validation$sens_back) < 1e-10

#' =============================================================================
#' Test Dataset 5: Error Cases (for validation testing)
#' =============================================================================

# These should trigger appropriate errors or warnings
error_test_cases <- list(
  invalid_prevalence = list(
    prevalence = c(-0.1, 0, 1, 1.5),
    description = "Invalid prevalence values"
  ),
  invalid_sensitivity = list(
    sensitivity = c(-0.1, 0, 1, 1.2),
    description = "Invalid sensitivity values"
  ),
  invalid_specificity = list(
    specificity = c(-0.1, 0, 1, 1.1),
    description = "Invalid specificity values"
  ),
  invalid_PLR = list(
    PLR = c(-1, 0, 0.5, 0.9),
    description = "Invalid positive likelihood ratios"
  ),
  invalid_NLR = list(
    NLR = c(-0.1, 1.1, 2.0),
    description = "Invalid negative likelihood ratios"
  ),
  inconsistent_LRs = list(
    PLR = c(2.0, 2.0),
    NLR = c(2.0, 2.0),
    description = "PLR equals NLR (uninformative test)"
  )
)

#' =============================================================================
#' Save Test Data
#' =============================================================================

# Save all test datasets
save(clinical_scenarios, file = "data/nomogrammer_clinical_test_data.rda")
save(edge_cases, file = "data/nomogrammer_edge_cases_test_data.rda") 
save(lr_scenarios, file = "data/nomogrammer_lr_test_data.rda")
save(math_validation, file = "data/nomogrammer_math_validation_data.rda")
save(error_test_cases, file = "data/nomogrammer_error_test_cases.rda")

# Create a comprehensive combined dataset
nomogrammer_test_data <- list(
  clinical_scenarios = clinical_scenarios,
  edge_cases = edge_cases,
  lr_scenarios = lr_scenarios,
  math_validation = math_validation,
  error_test_cases = error_test_cases
)

save(nomogrammer_test_data, file = "data/nomogrammer_test_data.rda")

#' =============================================================================
#' Summary Statistics
#' =============================================================================

cat("=== Nomogrammer Test Data Summary ===\n")
cat("Clinical scenarios:", nrow(clinical_scenarios), "\n")
cat("Edge cases:", nrow(edge_cases), "\n") 
cat("LR scenarios:", nrow(lr_scenarios), "\n")
cat("Math validation cases:", nrow(math_validation), "\n")
cat("Error test cases:", length(error_test_cases), "\n")

cat("\nMathematical consistency check:\n")
cat("All specificity calculations correct:", all(math_validation$spec_match), "\n")
cat("All sensitivity calculations correct:", all(math_validation$sens_match), "\n")

if (!all(math_validation$spec_match) || !all(math_validation$sens_match)) {
  cat("WARNING: Mathematical inconsistencies detected!\n")
  inconsistent_rows <- which(!math_validation$spec_match | !math_validation$sens_match)
  cat("Inconsistent rows:", inconsistent_rows, "\n")
}

cat("\nTest data generation completed successfully!\n")