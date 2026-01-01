# CONSORT Example Data Generation
# This script creates sample CONSORT flow chart parameters for different types of clinical trials

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(tibble)
library(dplyr)
library(usethis)

# Example 1: Large cardiovascular outcome trial
consort_cv_trial <- tibble(
    trial_name = "Cardiovascular Outcomes Trial",
    trial_type = "Phase III RCT",
    
    # Enrollment phase
    initial_assessed = 5000,
    not_eligible = 1500,
    not_eligible_reasons = "Age >80 years (n=600), Prior MI <6 months (n=400), Kidney disease (n=300), Other (n=200)",
    
    # Randomization phase  
    randomized = 3500,
    
    # Arm 1: Active treatment
    arm1_label = "ACE Inhibitor + Standard Care",
    arm1_allocated = 1750,
    arm1_received = 1720,
    arm1_lost_followup = 180,
    arm1_analyzed = 1540,
    
    # Arm 2: Control
    arm2_label = "Placebo + Standard Care", 
    arm2_allocated = 1750,
    arm2_received = 1735,
    arm2_lost_followup = 165,
    arm2_analyzed = 1570,
    
    # Post-randomization exclusions
    exclusion_reasons = "Lost to follow-up (n=345), Withdrew consent (n=120), Protocol violations (n=75)"
)

# Example 2: Cancer immunotherapy trial
consort_cancer_trial <- tibble(
    trial_name = "Immunotherapy vs Chemotherapy",
    trial_type = "Phase III Oncology RCT",
    
    # Enrollment phase
    initial_assessed = 1200,
    not_eligible = 350,
    not_eligible_reasons = "ECOG PS >2 (n=150), Prior immunotherapy (n=100), Autoimmune disease (n=100)",
    
    # Randomization phase
    randomized = 850,
    
    # Arm 1: Immunotherapy
    arm1_label = "Anti-PD1 Immunotherapy",
    arm1_allocated = 425,
    arm1_received = 420,
    arm1_lost_followup = 85,
    arm1_analyzed = 335,
    
    # Arm 2: Chemotherapy
    arm2_label = "Standard Chemotherapy",
    arm2_allocated = 425,
    arm2_received = 422,
    arm2_lost_followup = 78,
    arm2_analyzed = 344,
    
    # Post-randomization exclusions
    exclusion_reasons = "Disease progression (n=95), Treatment toxicity (n=68), Patient choice (n=50)"
)

# Example 3: Diabetes prevention study
consort_diabetes_trial <- tibble(
    trial_name = "Diabetes Prevention Trial",
    trial_type = "Lifestyle Intervention RCT",
    
    # Enrollment phase
    initial_assessed = 2000,
    not_eligible = 600,
    not_eligible_reasons = "BMI <25 (n=250), Normal glucose (n=200), Type 2 diabetes (n=150)",
    
    # Randomization phase
    randomized = 1400,
    
    # Arm 1: Lifestyle intervention
    arm1_label = "Intensive Lifestyle",
    arm1_allocated = 700,
    arm1_received = 680,
    arm1_lost_followup = 120,
    arm1_analyzed = 560,
    
    # Arm 2: Standard care
    arm2_label = "Standard Care",
    arm2_allocated = 700,
    arm2_received = 690,
    arm2_lost_followup = 95,
    arm2_analyzed = 595,
    
    # Post-randomization exclusions
    exclusion_reasons = "Moved away (n=80), Lost interest (n=75), Non-compliance (n=60)"
)

# Example 4: Surgical vs medical therapy
consort_surgical_trial <- tibble(
    trial_name = "Surgical vs Medical Treatment",
    trial_type = "Surgical RCT",
    
    # Enrollment phase
    initial_assessed = 800,
    not_eligible = 200,
    not_eligible_reasons = "High surgical risk (n=80), Patient refusal surgery (n=70), Other (n=50)",
    
    # Randomization phase
    randomized = 600,
    
    # Arm 1: Surgery
    arm1_label = "Surgical Treatment",
    arm1_allocated = 300,
    arm1_received = 290,
    arm1_lost_followup = 35,
    arm1_analyzed = 255,
    
    # Arm 2: Medical
    arm2_label = "Medical Treatment",
    arm2_allocated = 300,
    arm2_received = 295,
    arm2_lost_followup = 40,
    arm2_analyzed = 255,
    
    # Post-randomization exclusions
    exclusion_reasons = "Crossover treatment (n=30), Complications (n=25), Lost follow-up (n=20)"
)

# Example 5: Small pilot study
consort_pilot_trial <- tibble(
    trial_name = "Pilot Feasibility Study",
    trial_type = "Phase II Pilot RCT",
    
    # Enrollment phase
    initial_assessed = 150,
    not_eligible = 50,
    not_eligible_reasons = "Screening failures (n=30), Withdrew consent (n=20)",
    
    # Randomization phase
    randomized = 100,
    
    # Arm 1: Experimental
    arm1_label = "Experimental Drug",
    arm1_allocated = 50,
    arm1_received = 48,
    arm1_lost_followup = 8,
    arm1_analyzed = 40,
    
    # Arm 2: Control
    arm2_label = "Placebo",
    arm2_allocated = 50,
    arm2_received = 49,
    arm2_lost_followup = 6,
    arm2_analyzed = 43,
    
    # Post-randomization exclusions
    exclusion_reasons = "Moved (n=8), Side effects (n=4), Other (n=2)"
)

# Combine all examples into a comprehensive dataset
jconsort_examples_data <- bind_rows(
    consort_cv_trial,
    consort_cancer_trial,
    consort_diabetes_trial,
    consort_surgical_trial,
    consort_pilot_trial
)

# Add study characteristics
jconsort_examples_data <- jconsort_examples_data %>%
    mutate(
        study_id = paste0("STUDY_", sprintf("%03d", row_number())),
        duration_months = c(36, 24, 48, 18, 12),
        primary_endpoint = c(
            "Major adverse cardiovascular events",
            "Overall survival",
            "Incidence of type 2 diabetes",
            "Treatment success at 12 months",
            "Safety and tolerability"
        ),
        study_population = c(
            "High-risk cardiovascular patients",
            "Advanced solid tumor patients",
            "Pre-diabetic adults",
            "Symptomatic patients",
            "Treatment-naive patients"
        )
    )

# Save the dataset
use_data_multi_format(jconsort_examples_data, overwrite = TRUE, save_csv = TRUE)

# Documentation for the dataset
# This will be added to the data.R file
