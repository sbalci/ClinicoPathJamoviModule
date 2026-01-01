# Create comprehensive test datasets for groupedforest function
# This script generates various survival datasets suitable for grouped forest plot analysis

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(123)

# 1. Main comprehensive dataset for grouped forest analysis
create_groupedforest_comprehensive_data <- function() {
  n_total <- 200
  
  # Generate patient characteristics
  data.frame(
    patient_id = paste0("P", sprintf("%04d", 1:n_total)),
    
    # Age with realistic distribution
    age = round(rnorm(n_total, mean = 65, sd = 12)),
    
    # Treatment groups (balanced)
    treatment = factor(sample(c("Control", "Treatment"), n_total, replace = TRUE, prob = c(0.5, 0.5))),
    
    # Main grouping variables for forest plot
    biomarker_status = factor(sample(c("Positive", "Negative"), n_total, replace = TRUE, prob = c(0.4, 0.6))),
    tumor_stage = factor(sample(c("Early", "Advanced"), n_total, replace = TRUE, prob = c(0.6, 0.4))),
    gender = factor(sample(c("Male", "Female"), n_total, replace = TRUE, prob = c(0.55, 0.45))),
    age_group = factor(ifelse(rnorm(n_total, 65, 12) >= 65, "≥65 years", "<65 years")),
    
    # Additional clinical characteristics
    performance_status = factor(sample(c("Good", "Poor"), n_total, replace = TRUE, prob = c(0.7, 0.3))),
    comorbidity_score = round(rnorm(n_total, mean = 2, sd = 1.5)),
    
    # Generate realistic survival times
    survival_months = rexp(n_total, rate = 1/20),
    
    # Event indicator (death)
    death_event = rbinom(n_total, 1, prob = 0.65),
    
    # Ensure some censoring
    stringsAsFactors = FALSE
  ) %>%
    # Realistic survival times and events
    dplyr::mutate(
      # Censor some observations
      death_event = ifelse(survival_months > 60, 0, death_event),
      survival_months = pmin(survival_months, 60 + rexp(n_total, rate = 1/12)),
      
      # Convert age to groups
      age_group = factor(ifelse(age >= 65, "≥65 years", "<65 years")),
      
      # Limit comorbidity score
      comorbidity_score = pmax(0, pmin(comorbidity_score, 5))
    )
}

# 2. Simple dataset for basic testing
create_groupedforest_simple_data <- function() {
  n <- 120
  
  data.frame(
    id = paste0("S", 1:n),
    time = rexp(n, rate = 1/20),
    event = rbinom(n, 1, 0.6),
    treatment = factor(rep(c("A", "B"), each = n/2)),
    subgroup = factor(sample(c("Group1", "Group2", "Group3"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

# 3. Multiple subgroups dataset
create_groupedforest_multi_subgroups <- function() {
  n <- 300
  
  # Create realistic survival data with multiple meaningful subgroups
  data.frame(
    patient = paste0("M", sprintf("%03d", 1:n)),
    
    # Survival variables
    time_to_event = pmax(0.5, rweibull(n, shape = 1.2, scale = 18)),
    event_occurred = rbinom(n, 1, 0.55),
    
    # Treatment assignment
    intervention = factor(sample(c("Standard", "Experimental"), n, replace = TRUE)),
    
    # Multiple grouping variables for subgroup analysis
    molecular_subtype = factor(sample(c("Type A", "Type B", "Type C", "Type D"), n, replace = TRUE, 
                                      prob = c(0.3, 0.25, 0.25, 0.2))),
    risk_category = factor(sample(c("Low Risk", "Intermediate Risk", "High Risk"), n, replace = TRUE,
                                  prob = c(0.4, 0.35, 0.25))),
    histologic_grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE,
                                     prob = c(0.3, 0.45, 0.25))),
    
    # Covariates
    patient_age = round(rnorm(n, 62, 15)),
    baseline_score = round(rnorm(n, 50, 20)),
    
    stringsAsFactors = FALSE
  )
}

# 4. Precision medicine dataset
create_groupedforest_precision_medicine <- function() {
  n <- 250
  
  data.frame(
    study_id = paste0("PM", sprintf("%03d", 1:n)),
    
    # Survival endpoints
    progression_free_months = pmax(0.2, rgamma(n, shape = 2, scale = 8)),
    progression_event = rbinom(n, 1, 0.7),
    
    # Treatment arms
    therapy_type = factor(sample(c("Targeted", "Chemotherapy"), n, replace = TRUE)),
    
    # Genomic subtypes for precision medicine
    genomic_variant = factor(sample(c("Mutation Present", "Wild Type", "Amplification", "Deletion"), 
                                    n, replace = TRUE, prob = c(0.35, 0.40, 0.15, 0.10))),
    expression_level = factor(sample(c("High Expression", "Low Expression"), n, replace = TRUE)),
    
    # Clinical characteristics
    disease_stage = factor(sample(c("Stage I-II", "Stage III-IV"), n, replace = TRUE, prob = c(0.3, 0.7))),
    prior_therapy = factor(sample(c("Treatment Naive", "Previously Treated"), n, replace = TRUE, prob = c(0.6, 0.4))),
    
    # Continuous covariates
    age_at_diagnosis = round(rnorm(n, 58, 12)),
    tumor_size = round(rnorm(n, 4.2, 2.1), 1),
    
    stringsAsFactors = FALSE
  )
}

# 5. Biomarker stratification dataset
create_groupedforest_biomarker_data <- function() {
  n <- 180
  
  # Simulate biomarker-driven treatment effects
  data.frame(
    case_id = paste0("BM", sprintf("%03d", 1:n)),
    
    # Time-to-event variables
    overall_survival_months = pmax(1, rlnorm(n, meanlog = log(20), sdlog = 0.8)),
    death_indicator = rbinom(n, 1, 0.45),
    
    # Treatment comparison
    treatment_arm = factor(sample(c("Control", "Biomarker-Targeted"), n, replace = TRUE)),
    
    # Biomarker-based subgroups
    biomarker_level = factor(sample(c("High", "Intermediate", "Low"), n, replace = TRUE,
                                    prob = c(0.3, 0.4, 0.3))),
    pathway_status = factor(sample(c("Activated", "Inhibited"), n, replace = TRUE)),
    resistance_marker = factor(sample(c("Absent", "Present"), n, replace = TRUE, prob = c(0.65, 0.35))),
    
    # Additional stratification factors
    tumor_location = factor(sample(c("Primary", "Metastatic"), n, replace = TRUE, prob = c(0.4, 0.6))),
    
    # Numerical covariates
    baseline_biomarker_value = round(rnorm(n, 100, 25), 1),
    age_at_enrollment = round(rnorm(n, 60, 14)),
    
    stringsAsFactors = FALSE
  )
}

# 6. Clinical trial dataset with interaction effects
create_groupedforest_interaction_data <- function() {
  n <- 320
  
  data.frame(
    trial_id = paste0("CT", sprintf("%03d", 1:n)),
    
    # Primary endpoints
    event_free_survival = pmax(0.5, rweibull(n, shape = 1.5, scale = 15)),
    event_status = rbinom(n, 1, 0.58),
    
    # Treatment assignment
    randomized_treatment = factor(sample(c("Placebo", "Active Treatment"), n, replace = TRUE)),
    
    # Subgroups with potential treatment interactions
    genetic_profile = factor(sample(c("Profile A", "Profile B", "Profile C"), n, replace = TRUE,
                                    prob = c(0.45, 0.35, 0.20))),
    disease_severity = factor(sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE,
                                     prob = c(0.25, 0.50, 0.25))),
    comorbidity_burden = factor(sample(c("None", "Low", "High"), n, replace = TRUE,
                                       prob = c(0.3, 0.5, 0.2))),
    
    # Demographics
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48))),
    ethnicity = factor(sample(c("Caucasian", "Asian", "Hispanic", "Other"), n, replace = TRUE,
                              prob = c(0.6, 0.2, 0.15, 0.05))),
    
    # Continuous predictors
    age_years = round(rnorm(n, 64, 13)),
    baseline_severity_score = round(rnorm(n, 75, 20)),
    
    stringsAsFactors = FALSE
  )
}

# Generate all datasets
message("Creating groupedforest test datasets...")

# Create datasets
groupedforest_comprehensive_data <- create_groupedforest_comprehensive_data()
groupedforest_simple_data <- create_groupedforest_simple_data()
groupedforest_multi_subgroups <- create_groupedforest_multi_subgroups()
groupedforest_precision_medicine <- create_groupedforest_precision_medicine()
groupedforest_biomarker_data <- create_groupedforest_biomarker_data()
groupedforest_interaction_data <- create_groupedforest_interaction_data()

# Save datasets
save(groupedforest_comprehensive_data, file = "data/groupedforest_comprehensive_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_comprehensive_data, "data/groupedforest_comprehensive_data.omv")
  message("✓ Created groupedforest_comprehensive_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_comprehensive_data, "data/groupedforest_comprehensive_data.omv")
  message("✓ Created groupedforest_comprehensive_data.omv")
}
save(groupedforest_simple_data, file = "data/groupedforest_simple_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_simple_data, "data/groupedforest_simple_data.omv")
  message("✓ Created groupedforest_simple_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_simple_data, "data/groupedforest_simple_data.omv")
  message("✓ Created groupedforest_simple_data.omv")
}
save(groupedforest_multi_subgroups, file = "data/groupedforest_multi_subgroups.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_multi_subgroups, "data/groupedforest_multi_subgroups.omv")
  message("✓ Created groupedforest_multi_subgroups.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_multi_subgroups, "data/groupedforest_multi_subgroups.omv")
  message("✓ Created groupedforest_multi_subgroups.omv")
}
save(groupedforest_precision_medicine, file = "data/groupedforest_precision_medicine.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_precision_medicine, "data/groupedforest_precision_medicine.omv")
  message("✓ Created groupedforest_precision_medicine.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_precision_medicine, "data/groupedforest_precision_medicine.omv")
  message("✓ Created groupedforest_precision_medicine.omv")
}
save(groupedforest_biomarker_data, file = "data/groupedforest_biomarker_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_biomarker_data, "data/groupedforest_biomarker_data.omv")
  message("✓ Created groupedforest_biomarker_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_biomarker_data, "data/groupedforest_biomarker_data.omv")
  message("✓ Created groupedforest_biomarker_data.omv")
}
save(groupedforest_interaction_data, file = "data/groupedforest_interaction_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_interaction_data, "data/groupedforest_interaction_data.omv")
  message("✓ Created groupedforest_interaction_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupedforest_interaction_data, "data/groupedforest_interaction_data.omv")
  message("✓ Created groupedforest_interaction_data.omv")
}

# Print dataset summaries
cat("\n=== GROUPEDFOREST TEST DATASETS CREATED ===\n")
cat("1. groupedforest_comprehensive_data:", nrow(groupedforest_comprehensive_data), "rows,", ncol(groupedforest_comprehensive_data), "columns\n")
cat("2. groupedforest_simple_data:", nrow(groupedforest_simple_data), "rows,", ncol(groupedforest_simple_data), "columns\n")
cat("3. groupedforest_multi_subgroups:", nrow(groupedforest_multi_subgroups), "rows,", ncol(groupedforest_multi_subgroups), "columns\n")
cat("4. groupedforest_precision_medicine:", nrow(groupedforest_precision_medicine), "rows,", ncol(groupedforest_precision_medicine), "columns\n")
cat("5. groupedforest_biomarker_data:", nrow(groupedforest_biomarker_data), "rows,", ncol(groupedforest_biomarker_data), "columns\n")
cat("6. groupedforest_interaction_data:", nrow(groupedforest_interaction_data), "rows,", ncol(groupedforest_interaction_data), "columns\n")

cat("\nDatasets saved successfully!\n")
