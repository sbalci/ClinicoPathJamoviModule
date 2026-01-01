# Decision Tree Test Data Generation
# This script creates comprehensive test datasets for the decision tree graph module

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(42)

# 1. BASIC DECISION TREE DATA ----
# Simple treatment comparison: Surgery vs Medical Treatment

n_patients <- 100

basic_decision_data <- tibble(
  patient_id = 1:n_patients,
  
  # Decision variables
  treatment = sample(c("Surgery", "Medical Treatment"), n_patients, replace = TRUE),
  hospital_type = sample(c("Academic", "Community"), n_patients, replace = TRUE),
  
  # Probability variables (for chance nodes)
  prob_success_surgery = runif(n_patients, 0.7, 0.9),
  prob_success_medical = runif(n_patients, 0.5, 0.7),
  prob_complications = runif(n_patients, 0.1, 0.3),
  prob_recurrence = runif(n_patients, 0.2, 0.4),
  
  # Cost variables
  cost_surgery = rnorm(n_patients, 15000, 2000),
  cost_medical = rnorm(n_patients, 8000, 1500),
  cost_complications = rnorm(n_patients, 5000, 1000),
  cost_followup = rnorm(n_patients, 2000, 500),
  cost_recurrence = rnorm(n_patients, 12000, 2500),
  
  # Utility/effectiveness variables (quality-adjusted life years)
  utility_success = runif(n_patients, 0.8, 0.95),
  utility_failure = runif(n_patients, 0.4, 0.6),
  utility_complications = runif(n_patients, 0.3, 0.5),
  utility_recurrence = runif(n_patients, 0.2, 0.4),
  
  # Outcome variables
  clinical_outcome = sample(c("Complete Response", "Partial Response", "No Response"), 
                           n_patients, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  adverse_events = sample(c("None", "Mild", "Moderate", "Severe"), 
                         n_patients, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05))
)

# Ensure costs are positive
basic_decision_data <- basic_decision_data %>%
  mutate(across(starts_with("cost_"), ~pmax(.x, 1000)))

# 2. COMPLEX MARKOV MODEL DATA ----
# Multi-stage disease progression model

n_states <- 200

markov_decision_data <- tibble(
  state_id = 1:n_states,
  
  # Decision variables
  treatment_strategy = sample(c("Immediate Treatment", "Watch and Wait", "Combination Therapy"), 
                             n_states, replace = TRUE),
  patient_risk_group = sample(c("Low Risk", "Intermediate Risk", "High Risk"), 
                             n_states, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  
  # Transition probabilities
  prob_healthy_to_sick = runif(n_states, 0.05, 0.15),
  prob_sick_to_recovered = runif(n_states, 0.3, 0.7),
  prob_sick_to_dead = runif(n_states, 0.01, 0.05),
  prob_recovered_to_sick = runif(n_states, 0.1, 0.3),
  prob_progression = runif(n_states, 0.2, 0.5),
  
  # State-specific costs (annual)
  cost_healthy_state = rnorm(n_states, 1000, 200),
  cost_sick_state = rnorm(n_states, 8000, 1500),
  cost_recovered_state = rnorm(n_states, 3000, 500),
  cost_treatment_annual = rnorm(n_states, 12000, 3000),
  cost_monitoring = rnorm(n_states, 2000, 400),
  
  # State utilities (annual)
  utility_healthy = runif(n_states, 0.9, 1.0),
  utility_sick = runif(n_states, 0.4, 0.7),
  utility_recovered = runif(n_states, 0.8, 0.9),
  utility_treatment = runif(n_states, 0.6, 0.8),
  
  # Time-dependent variables
  cycle_length = rep(1, n_states), # 1 year cycles
  time_horizon = sample(10:20, n_states, replace = TRUE)
)

# 3. PHARMACEUTICAL COST-EFFECTIVENESS DATA ----
# Drug comparison study

n_drugs <- 150

pharma_decision_data <- tibble(
  study_id = 1:n_drugs,
  
  # Decision variables
  drug_regimen = sample(c("Drug A", "Drug B", "Drug C", "Standard Care"), 
                       n_drugs, replace = TRUE),
  dosing_strategy = sample(c("Standard Dose", "High Dose", "Personalized Dose"), 
                          n_drugs, replace = TRUE),
  administration = sample(c("Oral", "IV", "Subcutaneous"), 
                         n_drugs, replace = TRUE),
  
  # Efficacy probabilities
  prob_response = case_when(
    drug_regimen == "Drug A" ~ runif(n_drugs, 0.6, 0.8),
    drug_regimen == "Drug B" ~ runif(n_drugs, 0.5, 0.7),
    drug_regimen == "Drug C" ~ runif(n_drugs, 0.7, 0.9),
    TRUE ~ runif(n_drugs, 0.3, 0.5)  # Standard Care
  ),
  prob_severe_ae = case_when(
    drug_regimen == "Drug A" ~ runif(n_drugs, 0.1, 0.2),
    drug_regimen == "Drug B" ~ runif(n_drugs, 0.15, 0.25),
    drug_regimen == "Drug C" ~ runif(n_drugs, 0.05, 0.15),
    TRUE ~ runif(n_drugs, 0.2, 0.3)
  ),
  prob_discontinuation = runif(n_drugs, 0.1, 0.3),
  
  # Cost components
  cost_drug_per_cycle = case_when(
    drug_regimen == "Drug A" ~ rnorm(n_drugs, 5000, 500),
    drug_regimen == "Drug B" ~ rnorm(n_drugs, 3000, 400),
    drug_regimen == "Drug C" ~ rnorm(n_drugs, 8000, 1000),
    TRUE ~ rnorm(n_drugs, 1000, 200)
  ),
  cost_administration = case_when(
    administration == "Oral" ~ rnorm(n_drugs, 100, 20),
    administration == "IV" ~ rnorm(n_drugs, 500, 100),
    TRUE ~ rnorm(n_drugs, 300, 50)
  ),
  cost_monitoring = rnorm(n_drugs, 800, 150),
  cost_ae_management = rnorm(n_drugs, 3000, 800),
  cost_progression = rnorm(n_drugs, 15000, 3000),
  
  # Utility values
  utility_response = runif(n_drugs, 0.75, 0.9),
  utility_stable = runif(n_drugs, 0.6, 0.75),
  utility_progression = runif(n_drugs, 0.3, 0.5),
  utility_severe_ae = runif(n_drugs, 0.4, 0.6),
  
  # Clinical outcomes
  progression_free_survival = rnorm(n_drugs, 12, 4),  # months
  overall_survival = rnorm(n_drugs, 24, 8),           # months
  quality_of_life_score = rnorm(n_drugs, 70, 15)     # 0-100 scale
)

# 4. SCREENING PROGRAM DATA ----
# Cancer screening cost-effectiveness

n_scenarios <- 120

screening_decision_data <- tibble(
  scenario_id = 1:n_scenarios,
  
  # Screening strategies
  screening_strategy = sample(c("No Screening", "Annual Screening", "Biennial Screening", 
                               "Risk-Based Screening"), n_scenarios, replace = TRUE),
  screening_method = sample(c("Method A", "Method B", "Method C"), 
                           n_scenarios, replace = TRUE),
  target_population = sample(c("General Population", "High Risk", "Genetic Risk"), 
                            n_scenarios, replace = TRUE),
  
  # Test performance characteristics
  sensitivity = runif(n_scenarios, 0.7, 0.95),
  specificity = runif(n_scenarios, 0.8, 0.98),
  positive_predictive_value = runif(n_scenarios, 0.1, 0.5),
  negative_predictive_value = runif(n_scenarios, 0.95, 0.999),
  
  # Disease parameters
  disease_prevalence = case_when(
    target_population == "General Population" ~ runif(n_scenarios, 0.001, 0.005),
    target_population == "High Risk" ~ runif(n_scenarios, 0.01, 0.05),
    TRUE ~ runif(n_scenarios, 0.05, 0.2)
  ),
  prob_early_detection = runif(n_scenarios, 0.6, 0.9),
  prob_cure_early = runif(n_scenarios, 0.8, 0.95),
  prob_cure_late = runif(n_scenarios, 0.2, 0.5),
  
  # Costs
  cost_screening_test = case_when(
    screening_method == "Method A" ~ rnorm(n_scenarios, 200, 50),
    screening_method == "Method B" ~ rnorm(n_scenarios, 500, 100),
    TRUE ~ rnorm(n_scenarios, 1000, 200)
  ),
  cost_diagnostic_workup = rnorm(n_scenarios, 2000, 400),
  cost_treatment_early = rnorm(n_scenarios, 25000, 5000),
  cost_treatment_late = rnorm(n_scenarios, 80000, 15000),
  cost_false_positive = rnorm(n_scenarios, 1500, 300),
  
  # Utilities
  utility_healthy = runif(n_scenarios, 0.95, 1.0),
  utility_early_disease = runif(n_scenarios, 0.8, 0.9),
  utility_late_disease = runif(n_scenarios, 0.4, 0.6),
  utility_cured = runif(n_scenarios, 0.9, 0.95),
  utility_false_positive_anxiety = runif(n_scenarios, 0.85, 0.95),
  
  # Outcomes
  life_years_gained = rnorm(n_scenarios, 5, 2),
  quality_adjusted_life_years = rnorm(n_scenarios, 4, 1.5)
)

# 5. MINIMAL TEST DATA ----
# Simple dataset for basic functionality testing

minimal_test_data <- tibble(
  id = 1:10,
  treatment = c(rep("A", 5), rep("B", 5)),
  prob1 = c(0.7, 0.8, 0.6, 0.9, 0.75, 0.5, 0.6, 0.55, 0.65, 0.7),
  prob2 = 1 - prob1,
  cost1 = c(1000, 1200, 800, 1500, 1100, 2000, 1800, 2200, 1900, 2100),
  cost2 = cost1 * 1.5,
  utility1 = c(0.8, 0.85, 0.75, 0.9, 0.82, 0.6, 0.65, 0.58, 0.62, 0.68),
  utility2 = utility1 - 0.2,
  outcome = sample(c("Success", "Failure"), 10, replace = TRUE)
)

# 6. EDGE CASE DATA ----
# Data to test edge cases and error handling

edge_case_data <- tibble(
  id = 1:20,
  
  # Missing values
  treatment_missing = c(rep("Treatment", 10), rep(NA, 5), rep("Control", 5)),
  
  # Extreme values
  prob_zero = rep(0, 20),
  prob_one = rep(1, 20),
  prob_negative = c(rep(0.5, 15), rep(-0.1, 5)),
  prob_over_one = c(rep(0.5, 15), rep(1.1, 5)),
  
  # Cost extremes
  cost_zero = rep(0, 20),
  cost_negative = c(rep(1000, 15), rep(-500, 5)),
  cost_very_high = c(rep(10000, 15), rep(1e8, 5)),
  
  # Utility extremes
  utility_negative = c(rep(0.8, 15), rep(-0.2, 5)),
  utility_over_one = c(rep(0.8, 15), rep(1.5, 5)),
  
  # Single category variables
  single_treatment = rep("Only_Treatment", 20),
  
  # Many categories
  many_categories = sample(LETTERS[1:15], 20, replace = TRUE)
)

# Save all datasets ----
use_data_multi_format(basic_decision_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(markov_decision_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(pharma_decision_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(screening_decision_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(minimal_test_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(edge_case_data, overwrite = TRUE, save_csv = TRUE)

# Create CSV versions for jamovi
write.csv(basic_decision_data, "inst/extdata/basic_decision_data.csv", row.names = FALSE)
write.csv(markov_decision_data, "inst/extdata/markov_decision_data.csv", row.names = FALSE)
write.csv(pharma_decision_data, "inst/extdata/pharma_decision_data.csv", row.names = FALSE)
write.csv(screening_decision_data, "inst/extdata/screening_decision_data.csv", row.names = FALSE)
write.csv(minimal_test_data, "inst/extdata/minimal_test_data.csv", row.names = FALSE)
write.csv(edge_case_data, "inst/extdata/edge_case_data.csv", row.names = FALSE)

cat("Test datasets created successfully!\n")
cat("Datasets generated:\n")
cat("1. basic_decision_data (", nrow(basic_decision_data), " rows) - Basic treatment comparison\n")
cat("2. markov_decision_data (", nrow(markov_decision_data), " rows) - Markov model data\n")
cat("3. pharma_decision_data (", nrow(pharma_decision_data), " rows) - Drug comparison\n")
cat("4. screening_decision_data (", nrow(screening_decision_data), " rows) - Screening programs\n")
cat("5. minimal_test_data (", nrow(minimal_test_data), " rows) - Basic functionality\n")
cat("6. edge_case_data (", nrow(edge_case_data), " rows) - Edge cases and error testing\n")
