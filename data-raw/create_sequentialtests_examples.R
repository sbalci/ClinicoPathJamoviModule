# Create Example Data for Sequential Testing Analysis
# Realistic clinical scenarios demonstrating different testing strategies

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tidyr)

# =============================================================================
# CLINICAL SEQUENTIAL TESTING SCENARIOS
# =============================================================================

# Scenario 1: COVID-19 Testing - Rapid test followed by RT-PCR confirmation
covid_scenarios <- data.frame(
  scenario = "COVID-19 Testing",
  clinical_setting = c("Community screening", "Contact tracing", "Symptomatic patients", "Hospital admission"),
  test1_name = "Rapid Antigen Test",
  test1_sens = 0.85,
  test1_spec = 0.95,
  test2_name = "RT-PCR",
  test2_sens = 0.95,
  test2_spec = 0.99,
  prevalence = c(0.02, 0.15, 0.40, 0.10),
  strategy = "serial_positive",
  rationale = c(
    "Screen large populations efficiently, confirm positives",
    "Higher prevalence among contacts, confirmation needed",
    "High pre-test probability, reduce false positives", 
    "Screening protocol requires confirmation"
  ),
  clinical_impact = c(
    "Reduces unnecessary isolation from false positives",
    "Balances rapid results with accuracy for contact tracing",
    "High confidence in diagnosis for treatment decisions",
    "Prevents unnecessary infection control measures"
  ),
  stringsAsFactors = FALSE
)

# Scenario 2: Cancer Screening - Imaging followed by tissue confirmation
cancer_scenarios <- data.frame(
  scenario = c("Breast Cancer Screening", "Lung Cancer Screening", "Colorectal Cancer Screening"),
  clinical_setting = c("Annual mammography", "Low-dose CT screening", "Fecal occult blood testing"),
  test1_name = c("Mammography", "Low-dose CT", "FOBT"),
  test1_sens = c(0.80, 0.85, 0.75),
  test1_spec = c(0.90, 0.87, 0.95),
  test2_name = c("Tissue biopsy", "PET-CT + biopsy", "Colonoscopy"),
  test2_sens = c(0.95, 0.90, 0.95),
  test2_spec = c(0.98, 0.95, 0.98),
  prevalence = c(0.008, 0.012, 0.025),
  strategy = "serial_positive",
  rationale = c(
    "Cost-effective screening with invasive confirmation",
    "Non-invasive screening with definitive diagnosis",
    "Simple screening with comprehensive evaluation"
  ),
  clinical_impact = c(
    "Reduces unnecessary biopsies while maintaining sensitivity",
    "Optimizes radiation exposure and surgical interventions",
    "Prevents unnecessary colonoscopies for false positives"
  ),
  stringsAsFactors = FALSE
)

# Scenario 3: Cardiac Disease - Exercise test followed by catheterization
cardiac_scenarios <- data.frame(
  scenario = "Coronary Artery Disease",
  clinical_setting = c("Asymptomatic high risk", "Chest pain evaluation", "Pre-operative assessment"),
  test1_name = "Exercise Stress Test",
  test1_sens = c(0.85, 0.80, 0.90),
  test1_spec = c(0.75, 0.70, 0.80),
  test2_name = "Cardiac Catheterization",
  test2_sens = 0.95,
  test2_spec = 0.98,
  prevalence = c(0.15, 0.35, 0.25),
  strategy = "serial_positive",
  rationale = c(
    "Non-invasive screening before invasive procedure",
    "Functional assessment before anatomical evaluation",
    "Risk stratification before surgery"
  ),
  clinical_impact = c(
    "Reduces unnecessary catheterizations in low-risk patients",
    "Provides functional significance of anatomical findings",
    "Optimizes surgical planning and patient counseling"
  ),
  stringsAsFactors = FALSE
)

# Scenario 4: Infectious Disease - TB screening with confirmation
tb_scenarios <- data.frame(
  scenario = "Tuberculosis Screening",
  clinical_setting = c("General population", "High-risk contacts", "Immunocompromised"),
  test1_name = "Tuberculin Skin Test",
  test1_sens = c(0.80, 0.90, 0.70),
  test1_spec = c(0.85, 0.80, 0.90),
  test2_name = "Chest X-ray + Sputum",
  test2_sens = 0.85,
  test2_spec = 0.95,
  prevalence = c(0.002, 0.05, 0.15),
  strategy = "serial_positive",
  rationale = c(
    "Cost-effective population screening",
    "Enhanced surveillance for high-risk groups",
    "Careful evaluation in vulnerable populations"
  ),
  clinical_impact = c(
    "Identifies cases while minimizing false positive treatment",
    "Balances sensitivity and specificity for contact investigation",
    "Reduces treatment toxicity from false positive diagnosis"
  ),
  stringsAsFactors = FALSE
)

# Scenario 5: Emergency Medicine - Parallel testing for MI rule-out
emergency_scenarios <- data.frame(
  scenario = "Myocardial Infarction Rule-out",
  clinical_setting = c("Emergency Department - Low risk", "Emergency Department - Intermediate risk"),
  test1_name = "Troponin",
  test1_sens = c(0.90, 0.95),
  test1_spec = c(0.85, 0.80),
  test2_name = "ECG",
  test2_sens = c(0.70, 0.75),
  test2_spec = c(0.90, 0.85),
  prevalence = c(0.05, 0.20),
  strategy = "parallel",
  rationale = c(
    "Rapid rule-out requires high sensitivity",
    "Multiple modalities improve diagnostic accuracy"
  ),
  clinical_impact = c(
    "Safely discharges low-risk patients",
    "Captures MI missed by single test modality"
  ),
  stringsAsFactors = FALSE
)

# Scenario 6: Rare Disease Screening - Serial negative strategy
rare_disease_scenarios <- data.frame(
  scenario = "Rare Genetic Disease",
  clinical_setting = c("Family screening", "Population screening"),
  test1_name = "Basic Screening Panel",
  test1_sens = c(0.70, 0.65),
  test1_spec = c(0.98, 0.99),
  test2_name = "Comprehensive Genetic Testing",
  test2_sens = c(0.95, 0.98),
  test2_spec = c(0.90, 0.85),
  prevalence = c(0.10, 0.001),
  strategy = "serial_negative",
  rationale = c(
    "Cannot afford to miss cases in families",
    "Maximize sensitivity for rare conditions"
  ),
  clinical_impact = c(
    "Ensures comprehensive family evaluation",
    "Identifies rare cases that would be missed by single test"
  ),
  stringsAsFactors = FALSE
)

# Combine all scenarios into main dataset
sequential_testing_examples <- bind_rows(
  covid_scenarios,
  cancer_scenarios,
  cardiac_scenarios, 
  tb_scenarios,
  emergency_scenarios,
  rare_disease_scenarios
)

# Calculate combined metrics for each scenario
sequential_testing_examples <- sequential_testing_examples %>%
  mutate(
    # Calculate combined sensitivity and specificity based on strategy
    combined_sens = case_when(
      strategy == "serial_positive" ~ test1_sens * test2_sens,
      strategy == "serial_negative" ~ test1_sens + (1 - test1_sens) * test2_sens,
      strategy == "parallel" ~ test1_sens + test2_sens - (test1_sens * test2_sens),
      TRUE ~ NA_real_
    ),
    combined_spec = case_when(
      strategy == "serial_positive" ~ test1_spec + (1 - test1_spec) * test2_spec,
      strategy == "serial_negative" ~ test1_spec * test2_spec,
      strategy == "parallel" ~ test1_spec * test2_spec,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    # Calculate predictive values
    test1_ppv = (test1_sens * prevalence) / ((test1_sens * prevalence) + ((1 - test1_spec) * (1 - prevalence))),
    test1_npv = (test1_spec * (1 - prevalence)) / ((test1_spec * (1 - prevalence)) + ((1 - test1_sens) * prevalence)),
    test2_ppv = (test2_sens * prevalence) / ((test2_sens * prevalence) + ((1 - test2_spec) * (1 - prevalence))),
    test2_npv = (test2_spec * (1 - prevalence)) / ((test2_spec * (1 - prevalence)) + ((1 - test2_sens) * prevalence)),
    combined_ppv = (combined_sens * prevalence) / ((combined_sens * prevalence) + ((1 - combined_spec) * (1 - prevalence))),
    combined_npv = (combined_spec * (1 - prevalence)) / ((combined_spec * (1 - prevalence)) + ((1 - combined_sens) * prevalence))
  ) %>%
  mutate(
    # Calculate likelihood ratios
    test1_plr = test1_sens / (1 - test1_spec),
    test1_nlr = (1 - test1_sens) / test1_spec,
    test2_plr = test2_sens / (1 - test2_spec),
    test2_nlr = (1 - test2_sens) / test2_spec,
    combined_plr = combined_sens / (1 - combined_spec),
    combined_nlr = (1 - combined_sens) / combined_spec
  ) %>%
  mutate(
    # Clinical interpretation
    strategy_benefit = case_when(
      strategy == "serial_positive" ~ paste0("Specificity improved from ", 
                                            round(pmax(test1_spec, test2_spec) * 100, 1), 
                                            "% to ", round(combined_spec * 100, 1), "%"),
      strategy == "serial_negative" ~ paste0("Sensitivity improved from ", 
                                            round(pmax(test1_sens, test2_sens) * 100, 1), 
                                            "% to ", round(combined_sens * 100, 1), "%"),
      strategy == "parallel" ~ paste0("Sensitivity improved from max ", 
                                     round(pmax(test1_sens, test2_sens) * 100, 1), 
                                     "% to ", round(combined_sens * 100, 1), "%"),
      TRUE ~ "Unknown strategy"
    ),
    ppv_improvement = case_when(
      combined_ppv > pmax(test1_ppv, test2_ppv) ~ paste0("PPV improved by ", 
                                                         round((combined_ppv - pmax(test1_ppv, test2_ppv)) * 100, 1), " percentage points"),
      TRUE ~ paste0("PPV changed by ", 
                   round((combined_ppv - pmax(test1_ppv, test2_ppv)) * 100, 1), " percentage points")
    )
  )

# =============================================================================
# STRATEGY COMPARISON EXAMPLES
# =============================================================================

# Example showing how different strategies perform with the same tests
strategy_comparison <- expand.grid(
  test1_sens = c(0.80, 0.90, 0.95),
  test1_spec = c(0.85, 0.90, 0.95),
  test2_sens = c(0.85, 0.90, 0.95),
  test2_spec = c(0.90, 0.95, 0.98),
  prevalence = c(0.01, 0.05, 0.10, 0.25),
  stringsAsFactors = FALSE
) %>%
  slice_sample(n = 20) %>%  # Sample 20 combinations to keep dataset manageable
  mutate(
    scenario = "Strategy Comparison",
    test1_name = "Test A",
    test2_name = "Test B"
  ) %>%
  # Calculate for all three strategies
  crossing(strategy = c("serial_positive", "serial_negative", "parallel")) %>%
  mutate(
    combined_sens = case_when(
      strategy == "serial_positive" ~ test1_sens * test2_sens,
      strategy == "serial_negative" ~ test1_sens + (1 - test1_sens) * test2_sens,
      strategy == "parallel" ~ test1_sens + test2_sens - (test1_sens * test2_sens)
    ),
    combined_spec = case_when(
      strategy == "serial_positive" ~ test1_spec + (1 - test1_spec) * test2_spec,
      strategy == "serial_negative" ~ test1_spec * test2_spec,
      strategy == "parallel" ~ test1_spec * test2_spec
    ),
    combined_ppv = (combined_sens * prevalence) / ((combined_sens * prevalence) + ((1 - combined_spec) * (1 - prevalence))),
    combined_npv = (combined_spec * (1 - prevalence)) / ((combined_spec * (1 - prevalence)) + ((1 - combined_sens) * prevalence))
  )

# =============================================================================
# COST-EFFECTIVENESS SCENARIOS
# =============================================================================

# Examples showing economic considerations in test sequencing
cost_effectiveness_examples <- data.frame(
  scenario = c("High-cost confirmation", "Rapid screening", "Resource-limited setting"),
  test1_name = c("Basic screening", "Point-of-care test", "Clinical assessment"),
  test1_cost = c(10, 25, 5),
  test1_sens = c(0.85, 0.90, 0.70),
  test1_spec = c(0.80, 0.85, 0.85),
  test2_name = c("Advanced imaging", "Laboratory confirmation", "Specialist referral"),
  test2_cost = c(500, 100, 200),
  test2_sens = c(0.95, 0.95, 0.90),
  test2_spec = c(0.98, 0.98, 0.95),
  prevalence = c(0.05, 0.10, 0.15),
  strategy = "serial_positive",
  population_size = 1000,
  stringsAsFactors = FALSE
) %>%
  mutate(
    # Calculate testing costs for different strategies
    diseased = population_size * prevalence,
    healthy = population_size - diseased,
    
    # Serial positive strategy costs
    test1_positives = (diseased * test1_sens) + (healthy * (1 - test1_spec)),
    serial_total_cost = (population_size * test1_cost) + (test1_positives * test2_cost),
    
    # Parallel strategy costs (everyone gets both tests)
    parallel_total_cost = population_size * (test1_cost + test2_cost),
    
    # Cost savings from serial strategy
    cost_savings = parallel_total_cost - serial_total_cost,
    cost_per_case_found = serial_total_cost / (diseased * test1_sens * test2_sens)
  )

# =============================================================================
# EDUCATIONAL EXAMPLES
# =============================================================================

# Simple examples for teaching concepts
teaching_examples <- data.frame(
  scenario = c("Perfect Test Demonstration", "Complementary Tests", "Trade-off Example"),
  test1_name = c("Perfect Screening", "Test for Type A", "High Sensitivity Test"),
  test1_sens = c(1.00, 0.95, 0.95),
  test1_spec = c(0.90, 0.70, 0.60),
  test2_name = c("Perfect Confirmation", "Test for Type B", "High Specificity Test"),
  test2_sens = c(0.80, 0.60, 0.70),
  test2_spec = c(1.00, 0.95, 0.98),
  prevalence = c(0.10, 0.10, 0.10),
  strategy = c("serial_positive", "parallel", "serial_positive"),
  teaching_point = c(
    "Shows maximum possible improvement with perfect second test",
    "Demonstrates how parallel testing captures different disease subtypes",
    "Illustrates sensitivity-specificity trade-offs in test selection"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# SAVE DATASETS
# =============================================================================

# Save main clinical scenarios
use_data_multi_format(sequential_testing_examples, overwrite = TRUE, save_csv = TRUE)

# Save strategy comparison data
use_data_multi_format(strategy_comparison, overwrite = TRUE, save_csv = TRUE)

# Save cost-effectiveness examples
use_data_multi_format(cost_effectiveness_examples, overwrite = TRUE, save_csv = TRUE)

# Save teaching examples
use_data_multi_format(teaching_examples, overwrite = TRUE, save_csv = TRUE)

# =============================================================================
# CREATE REFERENCE DATA FOR COMMON TEST COMBINATIONS
# =============================================================================

# Common clinical test combinations with literature-based performance
common_test_combinations <- data.frame(
  clinical_area = c(
    "Infectious Disease", "Infectious Disease", "Cardiology", "Cardiology",
    "Oncology", "Oncology", "Emergency Medicine", "Emergency Medicine",
    "Pulmonology", "Neurology"
  ),
  condition = c(
    "HIV", "Tuberculosis", "Coronary Artery Disease", "Heart Failure",
    "Breast Cancer", "Lung Cancer", "Pulmonary Embolism", "Acute MI",
    "Pneumonia", "Stroke"
  ),
  test1_name = c(
    "ELISA", "Tuberculin Skin Test", "Exercise Stress Test", "BNP",
    "Mammography", "Chest X-ray", "D-dimer", "Troponin",
    "Chest X-ray", "Clinical Assessment"
  ),
  test1_sens_range = c(
    "0.99-1.00", "0.80-0.90", "0.80-0.90", "0.85-0.95",
    "0.75-0.85", "0.60-0.80", "0.95-0.98", "0.90-0.95",
    "0.70-0.80", "0.80-0.90"
  ),
  test1_spec_range = c(
    "0.995-0.999", "0.80-0.90", "0.70-0.80", "0.75-0.85",
    "0.88-0.92", "0.85-0.95", "0.40-0.60", "0.85-0.90",
    "0.80-0.90", "0.70-0.80"
  ),
  test2_name = c(
    "Western Blot", "Chest X-ray + Sputum", "Cardiac Catheterization", "Echocardiogram",
    "Tissue Biopsy", "CT + Biopsy", "CT Pulmonary Angiogram", "Coronary Angiography",
    "CT Chest", "MRI Brain"
  ),
  test2_sens_range = c(
    "0.999-1.00", "0.80-0.90", "0.95-0.98", "0.90-0.95",
    "0.95-0.98", "0.90-0.95", "0.90-0.95", "0.95-0.98",
    "0.90-0.95", "0.95-0.98"
  ),
  test2_spec_range = c(
    "0.9999-1.00", "0.90-0.95", "0.95-0.98", "0.85-0.95",
    "0.95-0.98", "0.95-0.98", "0.95-0.98", "0.98-0.99",
    "0.85-0.95", "0.90-0.95"
  ),
  typical_prevalence = c(
    "0.001-0.05", "0.002-0.15", "0.05-0.40", "0.10-0.30",
    "0.005-0.02", "0.01-0.05", "0.02-0.15", "0.05-0.25",
    "0.05-0.20", "0.10-0.30"
  ),
  recommended_strategy = c(
    "Serial positive", "Serial positive", "Serial positive", "Serial positive",
    "Serial positive", "Serial positive", "Serial positive", "Serial positive",
    "Parallel (emergency)", "Parallel (emergency)"
  ),
  clinical_rationale = c(
    "Minimize false positives for life-altering diagnosis",
    "Cost-effective screening with definitive diagnosis",
    "Non-invasive screening before invasive procedure",
    "Functional assessment with structural confirmation",
    "Population screening with tissue confirmation",
    "Non-invasive screening with tissue confirmation",
    "Rapid rule-out requires high sensitivity",
    "Time-sensitive diagnosis requires comprehensive evaluation",
    "Emergency setting requires rapid, sensitive diagnosis",
    "Time-sensitive intervention requires comprehensive assessment"
  ),
  stringsAsFactors = FALSE
)

use_data_multi_format(common_test_combinations, overwrite = TRUE, save_csv = TRUE)

# Print summary
cat("Created sequential testing example datasets:\n")
cat("- sequential_testing_examples: ", nrow(sequential_testing_examples), " clinical scenarios\n")
cat("- strategy_comparison: ", nrow(strategy_comparison), " strategy comparison examples\n") 
cat("- cost_effectiveness_examples: ", nrow(cost_effectiveness_examples), " cost-effectiveness scenarios\n")
cat("- teaching_examples: ", nrow(teaching_examples), " educational examples\n")
cat("- common_test_combinations: ", nrow(common_test_combinations), " reference test combinations\n")
