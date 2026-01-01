# Create Example Data for Screening Calculator
# Clinical scenarios with realistic test characteristics

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# =============================================================================
# CLINICAL SCREENING SCENARIOS
# =============================================================================

# Scenario 1: COVID-19 Rapid Antigen Testing
covid_scenarios <- data.frame(
  scenario = "COVID-19 Rapid Test",
  test_type = "Antigen",
  sensitivity = 0.85,
  specificity = 0.95,
  setting = c("Community (low prevalence)", "Outbreak (high prevalence)", "Symptomatic patients"),
  prevalence = c(0.02, 0.15, 0.40),
  clinical_context = c(
    "Mass screening in low-prevalence community setting",
    "Testing during outbreak with increased community transmission", 
    "Testing symptomatic patients presenting to clinic"
  ),
  stringsAsFactors = FALSE
)

# Scenario 2: Mammography Screening
mammography_scenarios <- data.frame(
  scenario = "Mammography Screening",
  test_type = "Imaging",
  sensitivity = c(0.80, 0.85, 0.75),
  specificity = c(0.90, 0.88, 0.92),
  setting = c("Ages 50-69", "Ages 40-49", "Ages 70+"),
  prevalence = c(0.008, 0.005, 0.012),
  clinical_context = c(
    "Standard screening in women aged 50-69",
    "Earlier screening in women aged 40-49",
    "Screening in elderly women aged 70+"
  ),
  stringsAsFactors = FALSE
)

# Scenario 3: Cardiovascular Disease Screening
cardiac_scenarios <- data.frame(
  scenario = "Cardiac Stress Test",
  test_type = "Functional",
  sensitivity = c(0.85, 0.90, 0.80),
  specificity = c(0.75, 0.70, 0.85),
  setting = c("Asymptomatic low risk", "Chest pain", "High risk factors"),
  prevalence = c(0.05, 0.25, 0.40),
  clinical_context = c(
    "Screening asymptomatic patients with low cardiac risk",
    "Evaluating patients presenting with chest pain",
    "Testing high-risk patients with multiple risk factors"
  ),
  stringsAsFactors = FALSE
)

# Scenario 4: Cancer Biomarker Testing
biomarker_scenarios <- data.frame(
  scenario = "PSA Screening",
  test_type = "Biomarker",
  sensitivity = c(0.70, 0.75, 0.65),
  specificity = c(0.80, 0.75, 0.85),
  setting = c("Ages 50-70", "Family history", "Previous elevated PSA"),
  prevalence = c(0.03, 0.08, 0.15),
  clinical_context = c(
    "Routine PSA screening in average-risk men",
    "Screening men with family history of prostate cancer",
    "Follow-up testing in men with previously elevated PSA"
  ),
  stringsAsFactors = FALSE
)

# Scenario 5: Infectious Disease Testing
infectious_scenarios <- data.frame(
  scenario = "HIV Testing",
  test_type = "Serology",
  sensitivity = c(0.995, 0.98, 0.999),
  specificity = c(0.999, 0.995, 0.998),
  setting = c("General population", "High-risk population", "Confirmatory testing"),
  prevalence = c(0.001, 0.05, 0.80),
  clinical_context = c(
    "Routine screening in general population",
    "Testing in high-risk populations",
    "Confirmatory testing after positive screen"
  ),
  stringsAsFactors = FALSE
)

# Combine all scenarios
screening_examples <- bind_rows(
  covid_scenarios,
  mammography_scenarios, 
  cardiac_scenarios,
  biomarker_scenarios,
  infectious_scenarios
)

# Add calculated metrics for reference
screening_examples <- screening_examples %>%
  mutate(
    ppv = (sensitivity * prevalence) / ((sensitivity * prevalence) + ((1 - specificity) * (1 - prevalence))),
    npv = (specificity * (1 - prevalence)) / ((specificity * (1 - prevalence)) + ((1 - sensitivity) * prevalence)),
    positive_lr = sensitivity / (1 - specificity),
    negative_lr = (1 - sensitivity) / specificity,
    pre_test_odds = prevalence / (1 - prevalence),
    post_test_odds_positive = pre_test_odds * positive_lr,
    post_test_odds_negative = pre_test_odds * negative_lr
  ) %>%
  mutate(
    interpretation = case_when(
      ppv < 0.10 ~ "Low PPV - High false positive rate",
      ppv < 0.50 ~ "Moderate PPV - Consider confirmatory testing",
      ppv < 0.80 ~ "Good PPV - Reasonable positive predictive value",
      TRUE ~ "Excellent PPV - High confidence in positive results"
    ),
    clinical_action = case_when(
      ppv < 0.10 ~ "Confirmatory testing essential",
      ppv < 0.50 ~ "Consider additional testing",
      ppv < 0.80 ~ "Proceed with appropriate clinical follow-up",
      TRUE ~ "High confidence - proceed with treatment planning"
    )
  )

# =============================================================================
# EDUCATIONAL EXAMPLES FOR DIFFERENT CONCEPTS
# =============================================================================

# Example 1: Demonstrating prevalence effects
prevalence_demo <- expand.grid(
  sensitivity = 0.90,
  specificity = 0.90,
  prevalence = c(0.001, 0.01, 0.05, 0.10, 0.20, 0.50),
  stringsAsFactors = FALSE
) %>%
  mutate(
    scenario = "Prevalence Effect Demo",
    setting = paste0("Prevalence = ", prevalence * 100, "%"),
    ppv = (sensitivity * prevalence) / ((sensitivity * prevalence) + ((1 - specificity) * (1 - prevalence))),
    npv = (specificity * (1 - prevalence)) / ((specificity * (1 - prevalence)) + ((1 - sensitivity) * prevalence))
  )

# Example 2: Demonstrating test performance effects
performance_demo <- expand.grid(
  sensitivity = c(0.70, 0.80, 0.90, 0.95),
  specificity = c(0.70, 0.80, 0.90, 0.95),
  prevalence = 0.10,
  stringsAsFactors = FALSE
) %>%
  mutate(
    scenario = "Test Performance Demo",
    setting = paste0("Sens = ", sensitivity, ", Spec = ", specificity),
    ppv = (sensitivity * prevalence) / ((sensitivity * prevalence) + ((1 - specificity) * (1 - prevalence))),
    npv = (specificity * (1 - prevalence)) / ((specificity * (1 - prevalence)) + ((1 - sensitivity) * prevalence))
  )

# Example 3: Sequential testing demonstration
sequential_demo <- data.frame(
  scenario = "Sequential Testing Demo",
  test_sequence = c("Single test", "Two positive tests", "Two negative tests", 
                   "Positive then negative", "Negative then positive"),
  sensitivity = 0.85,
  specificity = 0.90,
  initial_prevalence = 0.10,
  stringsAsFactors = FALSE
)

# Calculate sequential probabilities
seq_sens <- 0.85
seq_spec <- 0.90
seq_prev <- 0.10

# First test
seq_ppv1 <- (seq_sens * seq_prev) / ((seq_sens * seq_prev) + ((1 - seq_spec) * (1 - seq_prev)))
seq_npv1 <- (seq_spec * (1 - seq_prev)) / ((seq_spec * (1 - seq_prev)) + ((1 - seq_sens) * seq_prev))

# Second test scenarios
seq_ppv_pp <- (seq_sens * seq_ppv1) / ((seq_sens * seq_ppv1) + ((1 - seq_spec) * (1 - seq_ppv1)))
seq_npv_nn <- (seq_spec * (1 - (1 - seq_npv1))) / ((seq_spec * (1 - (1 - seq_npv1))) + ((1 - seq_sens) * (1 - seq_npv1)))
seq_prob_nn <- 1 - seq_npv_nn

sequential_demo$final_probability <- c(
  seq_ppv1,  # Single positive
  seq_ppv_pp, # Two positives  
  seq_prob_nn, # Two negatives
  1 - (seq_spec * (1 - seq_ppv1)) / ((seq_spec * (1 - seq_ppv1)) + ((1 - seq_sens) * seq_ppv1)), # Pos then neg
  (seq_sens * (1 - seq_npv1)) / ((seq_sens * (1 - seq_npv1)) + ((1 - seq_spec) * (1 - (1 - seq_npv1)))) # Neg then pos
)

# =============================================================================
# SAVE EXAMPLE DATASETS
# =============================================================================

# Save main clinical scenarios
use_data_multi_format(screening_examples, overwrite = TRUE, save_csv = TRUE)

# Save educational demonstrations
use_data_multi_format(prevalence_demo, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(performance_demo, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(sequential_demo, overwrite = TRUE, save_csv = TRUE)

# =============================================================================
# CREATE QUICK REFERENCE TABLES
# =============================================================================

# Common test characteristics reference
common_tests <- data.frame(
  test_name = c(
    "Rapid COVID-19 Antigen", "RT-PCR COVID-19", "Mammography", 
    "Exercise Stress Test", "PSA", "Fecal Occult Blood", 
    "Pap Smear", "HIV ELISA", "Tuberculin Skin Test"
  ),
  sensitivity_range = c(
    "0.80-0.90", "0.95-0.99", "0.75-0.85",
    "0.80-0.90", "0.65-0.75", "0.60-0.80",
    "0.85-0.95", "0.99+", "0.85-0.95"
  ),
  specificity_range = c(
    "0.95-0.99", "0.98-0.99", "0.88-0.92", 
    "0.70-0.80", "0.75-0.85", "0.85-0.95",
    "0.95-0.99", "0.99+", "0.70-0.80"
  ),
  typical_prevalence = c(
    "Variable", "Variable", "0.5-1.5%",
    "5-40%", "3-8%", "2-6%", 
    "0.1-0.5%", "0.1-5%", "Variable"
  ),
  clinical_use = c(
    "Rapid screening", "Diagnostic confirmation", "Cancer screening",
    "CAD evaluation", "Prostate cancer screening", "Colorectal cancer screening",
    "Cervical cancer screening", "HIV screening", "TB screening"
  ),
  stringsAsFactors = FALSE
)

use_data_multi_format(common_tests, overwrite = TRUE, save_csv = TRUE)

# Print summary
cat("Created screening calculator example datasets:\n")
cat("- screening_examples: ", nrow(screening_examples), " clinical scenarios\n")
cat("- prevalence_demo: ", nrow(prevalence_demo), " prevalence effect examples\n") 
cat("- performance_demo: ", nrow(performance_demo), " test performance examples\n")
cat("- sequential_demo: ", nrow(sequential_demo), " sequential testing examples\n")
cat("- common_tests: ", nrow(common_tests), " reference test characteristics\n")
