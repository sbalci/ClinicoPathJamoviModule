# Generate Comprehensive Survival Comparison Test Data
# This script creates multiple datasets for testing the comparingsurvival function

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# 1. Basic Survival Comparison Data
# =============================================================================

# Create basic two-group survival comparison data
n_basic <- 300
basic_survival_data <- data.frame(
  # Patient identifiers
  patient_id = sprintf("P%04d", 1:n_basic),
  
  # Basic demographics
  age = round(rnorm(n_basic, mean = 65, sd = 12)),
  sex = factor(sample(c("Male", "Female"), n_basic, replace = TRUE)),
  
  # Treatment groups (main comparison variable)
  treatment = factor(sample(c("Control", "Treatment"), n_basic, replace = TRUE, prob = c(0.5, 0.5))),
  
  # Generate survival times based on treatment effect
  # Treatment group has hazard ratio of 0.7 (30% reduction in hazard)
  baseline_hazard = 0.05,  # monthly hazard rate
  treatment_effect = ifelse(sample(c("Control", "Treatment"), n_basic, replace = TRUE, prob = c(0.5, 0.5)) == "Treatment", 0.7, 1.0)
) %>%
  mutate(
    # Generate exponential survival times with treatment effect
    true_survival_time = rexp(n_basic, rate = baseline_hazard * treatment_effect),
    
    # Add random censoring (administrative censoring at 60 months + random dropout)
    admin_censor_time = 60,
    dropout_time = rexp(n_basic, rate = 0.02),  # 2% monthly dropout rate
    censor_time = pmin(admin_censor_time, dropout_time),
    
    # Observed time and event indicator
    survival_months = pmin(true_survival_time, censor_time),
    death_event = ifelse(true_survival_time <= censor_time, 1, 0),
    
    # Alternative event coding formats for testing
    death_logical = as.logical(death_event),
    death_factor = factor(death_event, levels = c(0, 1), labels = c("Alive", "Dead")),
    
    # Additional variables for subgroup analysis
    stage = factor(sample(c("Early", "Advanced"), n_basic, replace = TRUE, prob = c(0.6, 0.4))),
    biomarker_positive = sample(c(TRUE, FALSE), n_basic, replace = TRUE, prob = c(0.3, 0.7))
  ) %>%
  select(-baseline_hazard, -treatment_effect, -true_survival_time, -admin_censor_time, -dropout_time, -censor_time)

# =============================================================================
# 2. Multi-Group Survival Comparison Data
# =============================================================================

# Create multi-group comparison data (3+ groups)
n_multi <- 400
multi_group_survival_data <- data.frame(
  # Patient identifiers
  patient_id = sprintf("MG%04d", 1:n_multi),
  
  # Demographics
  age = round(rnorm(n_multi, mean = 62, sd = 15)),
  sex = factor(sample(c("Male", "Female"), n_multi, replace = TRUE)),
  
  # Multi-level grouping variables
  risk_group = factor(sample(c("Low", "Intermediate", "High"), n_multi, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
  tumor_grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), n_multi, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
  molecular_subtype = factor(sample(c("Luminal A", "Luminal B", "Triple Negative", "HER2+"), n_multi, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)))
) %>%
  mutate(
    # Generate hazard ratios for different risk groups
    risk_hr = case_when(
      risk_group == "Low" ~ 0.5,
      risk_group == "Intermediate" ~ 1.0,
      risk_group == "High" ~ 2.0
    ),
    
    # Generate hazard ratios for tumor grades
    grade_hr = case_when(
      tumor_grade == "Grade 1" ~ 0.6,
      tumor_grade == "Grade 2" ~ 1.0,
      tumor_grade == "Grade 3" ~ 1.8
    ),
    
    # Generate hazard ratios for molecular subtypes
    molecular_hr = case_when(
      molecular_subtype == "Luminal A" ~ 0.5,
      molecular_subtype == "Luminal B" ~ 0.8,
      molecular_subtype == "Triple Negative" ~ 2.0,
      molecular_subtype == "HER2+" ~ 1.5
    ),
    
    # Combined hazard (multiplicative model)
    combined_hr = risk_hr * grade_hr * molecular_hr,
    
    # Generate survival times
    base_hazard = 0.03,
    survival_months = rexp(n_multi, rate = base_hazard * combined_hr),
    
    # Censoring
    censor_time = pmin(72, rexp(n_multi, rate = 0.015)),  # 72 months max follow-up
    observed_time = pmin(survival_months, censor_time),
    event_occurred = ifelse(survival_months <= censor_time, 1, 0),
    
    # Additional outcome measures
    progression_time = rexp(n_multi, rate = base_hazard * combined_hr * 1.5),  # Progression occurs earlier
    progression_censor = pmin(observed_time, rexp(n_multi, rate = 0.02)),
    pfs_months = pmin(progression_time, progression_censor),
    progression_event = ifelse(progression_time <= progression_censor, 1, 0)
  ) %>%
  select(-risk_hr, -grade_hr, -molecular_hr, -combined_hr, -base_hazard, -survival_months, -censor_time, -progression_time, -progression_censor) %>%
  rename(survival_months = observed_time, death_event = event_occurred)

# =============================================================================
# 3. Biomarker Stratification Data
# =============================================================================

# Create comprehensive biomarker data
n_biomarker <- 350
biomarker_survival_data <- data.frame(
  # Patient identifiers
  patient_id = sprintf("BM%04d", 1:n_biomarker),
  
  # Demographics
  age = round(rnorm(n_biomarker, mean = 68, sd = 10)),
  sex = factor(sample(c("Male", "Female"), n_biomarker, replace = TRUE)),
  race = factor(sample(c("White", "Black", "Hispanic", "Asian", "Other"), n_biomarker, replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.08, 0.02))),
  
  # Molecular biomarkers (binary)
  egfr_mutation = factor(sample(c("Negative", "Positive"), n_biomarker, replace = TRUE, prob = c(0.7, 0.3))),
  kras_mutation = factor(sample(c("Wild Type", "Mutant"), n_biomarker, replace = TRUE, prob = c(0.65, 0.35))),
  p53_mutation = factor(sample(c("Wild Type", "Mutant"), n_biomarker, replace = TRUE, prob = c(0.5, 0.5))),
  msi_status = factor(sample(c("MSS", "MSI-Low", "MSI-High"), n_biomarker, replace = TRUE, prob = c(0.7, 0.2, 0.1))),
  her2_status = factor(sample(c("Negative", "Positive"), n_biomarker, replace = TRUE, prob = c(0.8, 0.2))),
  
  # Continuous biomarkers
  pdl1_expression = round(runif(n_biomarker, min = 0, max = 100), 1),  # PD-L1 TPS %
  tumor_mutational_burden = round(rexp(n_biomarker, rate = 0.1), 1),   # mutations/Mb
  ki67_index = round(rbeta(n_biomarker, 2, 5) * 100, 1),              # Ki-67 % (0-100%)
  
  # Clinical variables
  stage = factor(sample(c("I", "II", "III", "IV"), n_biomarker, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))),
  performance_status = factor(sample(c("0", "1", "2"), n_biomarker, replace = TRUE, prob = c(0.4, 0.5, 0.1))),
  tumor_size = round(rnorm(n_biomarker, mean = 3.5, sd = 2.0), 1)      # cm
) %>%
  mutate(
    # Create composite biomarker groups
    egfr_pdl1_group = case_when(
      egfr_mutation == "Positive" & pdl1_expression >= 50 ~ "EGFR+/PD-L1 High",
      egfr_mutation == "Positive" & pdl1_expression < 50 ~ "EGFR+/PD-L1 Low",
      egfr_mutation == "Negative" & pdl1_expression >= 50 ~ "EGFR-/PD-L1 High",
      TRUE ~ "EGFR-/PD-L1 Low"
    ),
    
    # Categorize continuous biomarkers
    pdl1_category = case_when(
      pdl1_expression < 1 ~ "Negative (<1%)",
      pdl1_expression < 50 ~ "Low (1-49%)",
      TRUE ~ "High (≥50%)"
    ),
    
    tmb_category = case_when(
      tumor_mutational_burden < 5 ~ "TMB-Low",
      tumor_mutational_burden < 20 ~ "TMB-Intermediate",
      TRUE ~ "TMB-High"
    ),
    
    ki67_category = ifelse(ki67_index >= 20, "High", "Low"),
    
    # Generate survival based on biomarker effects
    # Define hazard ratios for different biomarkers
    egfr_hr = ifelse(egfr_mutation == "Positive", 0.7, 1.0),  # EGFR+ better prognosis
    msi_hr = case_when(
      msi_status == "MSI-High" ~ 0.6,  # MSI-High better prognosis
      msi_status == "MSI-Low" ~ 0.9,
      TRUE ~ 1.0
    ),
    her2_hr = ifelse(her2_status == "Positive", 1.3, 1.0),  # HER2+ worse prognosis
    stage_hr = case_when(
      stage == "I" ~ 0.3,
      stage == "II" ~ 0.6,
      stage == "III" ~ 1.0,
      stage == "IV" ~ 2.5
    ),
    
    # PD-L1 continuous effect (higher = better prognosis)
    pdl1_hr = exp(-0.01 * pdl1_expression),
    
    # Combined hazard
    combined_hr = egfr_hr * msi_hr * her2_hr * stage_hr * pdl1_hr,
    
    # Generate survival times
    base_monthly_hazard = 0.04,
    survival_months = rexp(n_biomarker, rate = base_monthly_hazard * combined_hr),
    
    # Apply censoring
    followup_time = pmin(60, rexp(n_biomarker, rate = 0.01)),  # Max 60 months follow-up
    observed_survival = pmin(survival_months, followup_time),
    death_event = ifelse(survival_months <= followup_time, 1, 0),
    
    # Generate progression-free survival
    pfs_base_hazard = base_monthly_hazard * 2,  # Progression occurs earlier
    pfs_time = rexp(n_biomarker, rate = pfs_base_hazard * combined_hr),
    pfs_censor = pmin(observed_survival, rexp(n_biomarker, rate = 0.015)),
    pfs_months = pmin(pfs_time, pfs_censor),
    progression_event = ifelse(pfs_time <= pfs_censor, 1, 0)
  ) %>%
  select(-egfr_hr, -msi_hr, -her2_hr, -stage_hr, -pdl1_hr, -combined_hr, -base_monthly_hazard, 
         -survival_months, -followup_time, -pfs_base_hazard, -pfs_time, -pfs_censor) %>%
  rename(survival_months = observed_survival) %>%
  mutate(
    # Ensure reasonable survival times
    survival_months = pmax(0.1, survival_months),
    pfs_months = pmax(0.1, pfs_months),
    pfs_months = pmin(pfs_months, survival_months)  # PFS cannot exceed OS
  )

# =============================================================================
# 4. Edge Case and Validation Data
# =============================================================================

# Create datasets for testing edge cases
n_edge <- 100

# Dataset with very high event rate
high_event_data <- data.frame(
  patient_id = sprintf("HE%04d", 1:n_edge),
  age = round(rnorm(n_edge, mean = 75, sd = 8)),
  treatment = factor(sample(c("Standard", "Experimental"), n_edge, replace = TRUE)),
  survival_months = rexp(n_edge, rate = 0.2),  # High event rate
  death_event = rbinom(n_edge, 1, 0.9),  # 90% event rate
  sex = factor(sample(c("Male", "Female"), n_edge, replace = TRUE))
)

# Dataset with very low event rate
low_event_data <- data.frame(
  patient_id = sprintf("LE%04d", 1:n_edge),
  age = round(rnorm(n_edge, mean = 55, sd = 10)),
  treatment = factor(sample(c("Control", "Treatment"), n_edge, replace = TRUE)),
  survival_months = rexp(n_edge, rate = 0.01),  # Low event rate
  death_event = rbinom(n_edge, 1, 0.1),  # 10% event rate
  sex = factor(sample(c("Male", "Female"), n_edge, replace = TRUE))
)

# Dataset with unbalanced groups
unbalanced_data <- data.frame(
  patient_id = sprintf("UB%04d", 1:n_edge),
  age = round(rnorm(n_edge, mean = 65, sd = 12)),
  treatment = factor(sample(c("Standard", "New"), n_edge, replace = TRUE, prob = c(0.8, 0.2))),  # 80/20 split
  survival_months = rexp(n_edge, rate = 0.05),
  death_event = rbinom(n_edge, 1, 0.6),
  sex = factor(sample(c("Male", "Female"), n_edge, replace = TRUE))
)

# =============================================================================
# 5. Combined Comprehensive Dataset
# =============================================================================

# Create a comprehensive dataset combining various scenarios
comprehensive_survival_comparison_data <- data.frame(
  # Patient identifiers
  patient_id = sprintf("CS%04d", 1:500),
  
  # Demographics
  age = round(rnorm(500, mean = 65, sd = 12)),
  sex = factor(sample(c("Male", "Female"), 500, replace = TRUE)),
  race = factor(sample(c("White", "Black", "Hispanic", "Asian"), 500, replace = TRUE, prob = c(0.65, 0.2, 0.1, 0.05))),
  
  # Primary treatment comparison
  primary_treatment = factor(sample(c("Surgery", "Chemotherapy", "Radiation", "Immunotherapy"), 500, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2))),
  
  # Secondary grouping variables
  histology = factor(sample(c("Adenocarcinoma", "Squamous Cell", "Other"), 500, replace = TRUE, prob = c(0.6, 0.3, 0.1))),
  grade = factor(sample(c("Low", "Intermediate", "High"), 500, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
  stage = factor(sample(c("I", "II", "III", "IV"), 500, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))),
  
  # Biomarkers
  biomarker_status = factor(sample(c("Negative", "Positive"), 500, replace = TRUE, prob = c(0.7, 0.3))),
  expression_level = round(rnorm(500, mean = 50, sd = 20)),
  mutation_count = rpois(500, lambda = 5),
  
  # Performance status
  ecog_ps = factor(sample(c("0", "1", "2", "3"), 500, replace = TRUE, prob = c(0.4, 0.4, 0.15, 0.05))),
  
  # Comorbidities
  diabetes = factor(sample(c("No", "Yes"), 500, replace = TRUE, prob = c(0.8, 0.2))),
  hypertension = factor(sample(c("No", "Yes"), 500, replace = TRUE, prob = c(0.6, 0.4))),
  heart_disease = factor(sample(c("No", "Yes"), 500, replace = TRUE, prob = c(0.85, 0.15)))
) %>%
  mutate(
    # Create complex hazard model
    treatment_hr = case_when(
      primary_treatment == "Surgery" ~ 0.5,
      primary_treatment == "Immunotherapy" ~ 0.7,
      primary_treatment == "Radiation" ~ 1.0,
      primary_treatment == "Chemotherapy" ~ 1.2
    ),
    
    stage_hr = case_when(
      stage == "I" ~ 0.3,
      stage == "II" ~ 0.6,
      stage == "III" ~ 1.0,
      stage == "IV" ~ 2.0
    ),
    
    grade_hr = case_when(
      grade == "Low" ~ 0.6,
      grade == "Intermediate" ~ 1.0,
      grade == "High" ~ 1.5
    ),
    
    biomarker_hr = ifelse(biomarker_status == "Positive", 0.8, 1.0),
    age_hr = exp(0.02 * (age - 65)),  # 2% increased hazard per year above 65
    
    # Combined hazard
    total_hr = treatment_hr * stage_hr * grade_hr * biomarker_hr * age_hr,
    
    # Generate overall survival
    os_months = rexp(500, rate = 0.03 * total_hr),
    os_censor_time = pmin(72, rexp(500, rate = 0.01)),
    overall_survival_months = pmin(os_months, os_censor_time),
    death_event = ifelse(os_months <= os_censor_time, 1, 0),
    
    # Generate progression-free survival
    pfs_months_raw = rexp(500, rate = 0.05 * total_hr),
    pfs_censor_time = pmin(overall_survival_months, rexp(500, rate = 0.015)),
    progression_free_months = pmin(pfs_months_raw, pfs_censor_time),
    progression_event = ifelse(pfs_months_raw <= pfs_censor_time, 1, 0),
    
    # Create meaningful group comparisons
    risk_stratification = case_when(
      stage %in% c("I", "II") & grade == "Low" ~ "Low Risk",
      stage %in% c("I", "II") & grade %in% c("Intermediate", "High") ~ "Intermediate Risk",
      stage == "III" ~ "High Risk",
      stage == "IV" ~ "Very High Risk"
    ),
    
    treatment_response_group = case_when(
      primary_treatment %in% c("Surgery", "Immunotherapy") & biomarker_status == "Positive" ~ "Good Responders",
      primary_treatment %in% c("Surgery", "Immunotherapy") & biomarker_status == "Negative" ~ "Moderate Responders", 
      primary_treatment %in% c("Chemotherapy", "Radiation") ~ "Standard Responders"
    )
  ) %>%
  select(-treatment_hr, -stage_hr, -grade_hr, -biomarker_hr, -age_hr, -total_hr, 
         -os_months, -os_censor_time, -pfs_months_raw, -pfs_censor_time) %>%
  mutate(
    # Ensure reasonable times
    overall_survival_months = pmax(0.1, overall_survival_months),
    progression_free_months = pmax(0.1, progression_free_months),
    progression_free_months = pmin(progression_free_months, overall_survival_months)
  )

# =============================================================================
# Save all datasets
# =============================================================================

# Save individual datasets
usethis::use_data(basic_survival_data, overwrite = TRUE)
usethis::use_data(multi_group_survival_data, overwrite = TRUE)
usethis::use_data(biomarker_survival_data, overwrite = TRUE)
usethis::use_data(high_event_data, overwrite = TRUE)
usethis::use_data(low_event_data, overwrite = TRUE)
usethis::use_data(unbalanced_data, overwrite = TRUE)
usethis::use_data(comprehensive_survival_comparison_data, overwrite = TRUE)

# =============================================================================
# Create summary documentation
# =============================================================================

cat("Survival Comparison Test Datasets Created Successfully!\n")
cat("======================================================\n\n")

cat("1. basic_survival_data (n=", nrow(basic_survival_data), "):\n")
cat("   - Simple two-group comparison (Control vs Treatment)\n")
cat("   - Event rate:", round(mean(basic_survival_data$death_event) * 100, 1), "%\n")
cat("   - Median follow-up:", round(median(basic_survival_data$survival_months), 1), "months\n\n")

cat("2. multi_group_survival_data (n=", nrow(multi_group_survival_data), "):\n")
cat("   - Multiple grouping variables (risk, grade, molecular subtype)\n")
cat("   - Event rate:", round(mean(multi_group_survival_data$death_event) * 100, 1), "%\n")
cat("   - Includes PFS data\n\n")

cat("3. biomarker_survival_data (n=", nrow(biomarker_survival_data), "):\n")
cat("   - Comprehensive molecular profiling\n")
cat("   - Multiple biomarkers and continuous variables\n")
cat("   - Event rate:", round(mean(biomarker_survival_data$death_event) * 100, 1), "%\n\n")

cat("4. Edge case datasets:\n")
cat("   - high_event_data: 90% event rate\n")
cat("   - low_event_data: 10% event rate\n")
cat("   - unbalanced_data: 80/20 group split\n\n")

cat("5. comprehensive_survival_comparison_data (n=", nrow(comprehensive_survival_comparison_data), "):\n")
cat("   - Complete dataset for all testing scenarios\n")
cat("   - Multiple treatment modalities and risk stratification\n")
cat("   - Event rate:", round(mean(comprehensive_survival_comparison_data$death_event) * 100, 1), "%\n\n")

cat("All datasets include:\n")
cat("- Proper survival time and event variables\n")
cat("- Multiple grouping options for comparisons\n")
cat("- Realistic clinical scenarios\n")
cat("- Edge cases for robust testing\n")