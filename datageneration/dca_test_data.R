# ============================================================================
# DECISION CURVE ANALYSIS TEST DATA GENERATOR
# ============================================================================
# This script creates comprehensive test data for the Decision Curve Analysis module
# with multiple prediction models of varying performance characteristics

library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(12345)

# ============================================================================
# SCENARIO: PREDICTING CARDIAC EVENTS IN HIGH-RISK PATIENTS
# ============================================================================

# Sample size
n <- 800

# Generate patient characteristics
generate_dca_test_data <- function() {

  # Patient demographics and risk factors
  age <- round(rnorm(n, mean = 65, sd = 12))
  age <- pmax(30, pmin(90, age))  # Constrain to reasonable range

  sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4))
  sex_numeric <- as.numeric(sex == "Male")

  diabetes <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.35, 0.65))
  diabetes_numeric <- as.numeric(diabetes == "Yes")

  hypertension <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.45, 0.55))
  hypertension_numeric <- as.numeric(hypertension == "Yes")

  smoking <- sample(c("Current", "Former", "Never"), n, replace = TRUE, prob = c(0.25, 0.35, 0.40))
  smoking_current <- as.numeric(smoking == "Current")
  smoking_former <- as.numeric(smoking == "Former")

  # Continuous biomarkers
  cholesterol <- round(rnorm(n, mean = 220, sd = 40))
  cholesterol <- pmax(120, pmin(350, cholesterol))

  troponin <- round(rlnorm(n, meanlog = 1.5, sdlog = 0.8), 2)
  troponin <- pmax(0.01, pmin(50, troponin))

  creatinine <- round(rnorm(n, mean = 1.1, sd = 0.3), 2)
  creatinine <- pmax(0.5, pmin(3.0, creatinine))

  # ============================================================================
  # GENERATE TRUE OUTCOME (CARDIAC EVENT WITHIN 1 YEAR)
  # ============================================================================

  # Create true underlying risk based on realistic clinical relationships
  log_odds_true <- -3.5 +
    0.05 * (age - 65) +                    # Age effect
    0.6 * sex_numeric +                    # Male higher risk
    0.8 * diabetes_numeric +               # Diabetes effect
    0.5 * hypertension_numeric +           # Hypertension effect
    0.9 * smoking_current +                # Current smoking high risk
    0.3 * smoking_former +                 # Former smoking moderate risk
    0.01 * (cholesterol - 220) +           # Cholesterol effect
    0.15 * log(troponin) +                 # Troponin effect (log scale)
    0.7 * (creatinine - 1.1) +             # Creatinine effect
    rnorm(n, 0, 0.3)                       # Individual variation

  # Convert to probability
  true_prob <- plogis(log_odds_true)

  # Generate actual outcomes
  cardiac_event <- rbinom(n, 1, true_prob)
  cardiac_event_factor <- factor(cardiac_event, levels = c(0, 1), labels = c("No", "Yes"))

  # ============================================================================
  # GENERATE PREDICTION MODELS WITH DIFFERENT CHARACTERISTICS
  # ============================================================================

  # MODEL 1: Basic Clinical Model (Good discrimination, well-calibrated)
  # Uses age, sex, diabetes, hypertension
  log_odds_basic <- -3.2 +
    0.045 * (age - 65) +
    0.55 * sex_numeric +
    0.75 * diabetes_numeric +
    0.45 * hypertension_numeric +
    rnorm(n, 0, 0.4)  # Add some prediction error

  basic_model_prob <- plogis(log_odds_basic)
  basic_model_prob <- pmax(0.01, pmin(0.99, basic_model_prob))  # Constrain to valid range

  # MODEL 2: Enhanced Clinical Model (Better discrimination, well-calibrated)
  # Adds smoking and cholesterol
  log_odds_enhanced <- -3.3 +
    0.048 * (age - 65) +
    0.58 * sex_numeric +
    0.78 * diabetes_numeric +
    0.48 * hypertension_numeric +
    0.85 * smoking_current +
    0.28 * smoking_former +
    0.008 * (cholesterol - 220) +
    rnorm(n, 0, 0.35)  # Less prediction error

  enhanced_model_prob <- plogis(log_odds_enhanced)
  enhanced_model_prob <- pmax(0.01, pmin(0.99, enhanced_model_prob))

  # MODEL 3: Biomarker Model (Excellent discrimination, well-calibrated)
  # Includes troponin and creatinine
  log_odds_biomarker <- -3.4 +
    0.047 * (age - 65) +
    0.57 * sex_numeric +
    0.76 * diabetes_numeric +
    0.47 * hypertension_numeric +
    0.82 * smoking_current +
    0.27 * smoking_former +
    0.009 * (cholesterol - 220) +
    0.14 * log(troponin) +
    0.65 * (creatinine - 1.1) +
    rnorm(n, 0, 0.25)  # Lowest prediction error

  biomarker_model_prob <- plogis(log_odds_biomarker)
  biomarker_model_prob <- pmax(0.01, pmin(0.99, biomarker_model_prob))

  # MODEL 4: Miscalibrated Model (Good discrimination, poor calibration)
  # Systematically overestimates risk
  miscalibrated_prob <- basic_model_prob * 1.8  # Inflate probabilities
  miscalibrated_prob <- pmax(0.01, pmin(0.99, miscalibrated_prob))

  # MODEL 5: Poor Model (Poor discrimination and calibration)
  # Adds lots of noise and bias
  log_odds_poor <- -2.8 +
    0.02 * (age - 65) +
    0.2 * sex_numeric +
    0.3 * diabetes_numeric +
    rnorm(n, 0, 1.2)  # Lots of noise

  poor_model_prob <- plogis(log_odds_poor)
  poor_model_prob <- pmax(0.01, pmin(0.99, poor_model_prob))

  # ============================================================================
  # CREATE FINAL DATASET
  # ============================================================================

  test_data <- data.frame(
    # Patient ID and demographics
    patient_id = 1:n,
    age = age,
    sex = sex,
    diabetes = diabetes,
    hypertension = hypertension,
    smoking = smoking,

    # Continuous variables
    cholesterol = cholesterol,
    troponin = troponin,
    creatinine = creatinine,

    # Outcome variables
    cardiac_event_numeric = cardiac_event,
    cardiac_event = cardiac_event_factor,
    true_risk = round(true_prob, 3),

    # Prediction models (probabilities)
    basic_model = round(basic_model_prob, 3),
    enhanced_model = round(enhanced_model_prob, 3),
    biomarker_model = round(biomarker_model_prob, 3),
    miscalibrated_model = round(miscalibrated_prob, 3),
    poor_model = round(poor_model_prob, 3),

    # Additional test variables
    risk_category = cut(true_prob,
                       breaks = c(0, 0.1, 0.2, 0.4, 1),
                       labels = c("Low", "Moderate", "High", "Very High"),
                       include.lowest = TRUE),

    hospital = sample(c("Hospital A", "Hospital B", "Hospital C"), n, replace = TRUE),

    stringsAsFactors = FALSE
  )

  return(test_data)
}

# Generate the test data
dca_test_data <- generate_dca_test_data()

# ============================================================================
# DATA SUMMARY AND VALIDATION
# ============================================================================

# Print summary statistics
cat("=== DCA Test Data Summary ===\n")
cat("Sample size:", nrow(dca_test_data), "\n")
cat("Event rate:", round(mean(dca_test_data$cardiac_event_numeric) * 100, 1), "%\n")
cat("\nOutcome distribution:\n")
print(table(dca_test_data$cardiac_event))

cat("\nAge distribution:\n")
print(summary(dca_test_data$age))

cat("\nModel performance preview:\n")
for (model_name in c("basic_model", "enhanced_model", "biomarker_model", "miscalibrated_model", "poor_model")) {
  auc <- as.numeric(pROC::auc(dca_test_data$cardiac_event_numeric, dca_test_data[[model_name]]))
  cat(sprintf("%-20s AUC: %.3f\n", model_name, auc))
}

# ============================================================================
# SAVE TEST DATA
# ============================================================================

# Save as CSV for jamovi import
write.csv(dca_test_data, "./data/dca_test_data.csv", row.names = FALSE)

# Save as RData for direct R use
save(dca_test_data, file = "./data/dca_test_data.RData")

cat("\nTest data saved as:\n")
cat("- dca_test_data.csv (for jamovi)\n")
cat("- dca_test_data.RData (for R)\n")

# ============================================================================
# EXAMPLE USAGE SCENARIOS
# ============================================================================

cat("\n=== Example Usage Scenarios ===\n")

cat("\n1. BASIC COMPARISON:\n")
cat("   Outcome: cardiac_event (select 'Yes' as positive)\n")
cat("   Models: basic_model, enhanced_model, biomarker_model\n")
cat("   Threshold range: Clinical (5% to 50%)\n")

cat("\n2. MODEL VALIDATION:\n")
cat("   Outcome: cardiac_event\n")
cat("   Models: biomarker_model vs miscalibrated_model\n")
cat("   Features: Enable bootstrap CI, clinical impact analysis\n")

cat("\n3. FULL ANALYSIS:\n")
cat("   Outcome: cardiac_event\n")
cat("   Models: All 5 models\n")
cat("   Features: All plots, weighted AUC, model comparison\n")
cat("   Clinical impact: Population size 1000\n")

cat("\n4. THRESHOLD OPTIMIZATION:\n")
cat("   Outcome: cardiac_event\n")
cat("   Models: biomarker_model\n")
cat("   Custom range: 10% to 40% (cardiac intervention thresholds)\n")
cat("   Highlight range: 15% to 25%\n")

# ============================================================================
# EXPECTED RESULTS PREVIEW
# ============================================================================

cat("\n=== Expected Results ===\n")

cat("\nModel ranking (best to worst expected performance):\n")
cat("1. biomarker_model - Highest AUC, best calibration\n")
cat("2. enhanced_model - Good AUC, good calibration\n")
cat("3. basic_model - Moderate AUC, good calibration\n")
cat("4. miscalibrated_model - Good AUC, poor calibration\n")
cat("5. poor_model - Poor AUC, poor calibration\n")

cat("\nExpected optimal thresholds:\n")
cat("- biomarker_model: ~15-20% (best net benefit)\n")
cat("- enhanced_model: ~18-25%\n")
cat("- basic_model: ~20-30%\n")

cat("\nClinical interpretation:\n")
cat("- biomarker_model should show clear benefit over treat-all/treat-none\n")
cat("- miscalibrated_model may show good discrimination but offset by calibration issues\n")
cat("- poor_model should show minimal or no benefit\n")

print("Test data generation complete!")
