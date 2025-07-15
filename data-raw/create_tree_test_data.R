#!/usr/bin/env Rscript

# =============================================================================
# Comprehensive Test Data Generation for Enhanced Tree Function
# =============================================================================

cat("Creating comprehensive test datasets for enhanced tree function...\n")

# Load required libraries
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})

set.seed(42)  # For reproducible results

# =============================================================================
# Dataset 1: Cancer Biomarker Diagnosis Dataset
# =============================================================================

cat("Creating cancer biomarker diagnosis dataset...\n")

n_patients <- 500
cancer_biomarkers <- tibble(
  patient_id = sprintf("PAT_%04d", 1:n_patients),
  
  # Continuous biomarkers
  PSA = c(
    rnorm(200, 4, 2),      # Normal range
    rnorm(200, 12, 8),     # Elevated
    rnorm(100, 25, 15)     # Very high
  ),
  age = round(rnorm(n_patients, 65, 10)),
  tumor_size = pmax(0, rnorm(n_patients, 3.5, 2.5)),
  
  # Categorical variables
  grade = sample(c("Low", "Intermediate", "High"), n_patients, 
                 replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  stage = sample(c("I", "II", "III", "IV"), n_patients, 
                 replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
  
  # Outcome variable
  diagnosis = factor(
    c(rep("benign", 200), rep("cancer", 200), rep("cancer", 100)),
    levels = c("benign", "cancer")
  ),
  
  # Cohort variable for train/test split
  cohort = sample(c("discovery", "validation"), n_patients, 
                  replace = TRUE, prob = c(0.7, 0.3)),
  
  # Patient demographics
  sex = sample(c("Male", "Female"), n_patients, 
               replace = TRUE, prob = c(0.85, 0.15)),
  
  # Additional biomarkers
  biopsy_gleason = ifelse(diagnosis == "cancer" & grade == "Low", 
                         sample(6:7, n_patients, replace = TRUE),
                         ifelse(diagnosis == "cancer" & grade == "Intermediate",
                                sample(7:8, n_patients, replace = TRUE),
                                ifelse(diagnosis == "cancer" & grade == "High",
                                       sample(8:10, n_patients, replace = TRUE),
                                       NA_integer_))),
  
  # Spatial coordinates for autocart testing
  x_coord = rnorm(n_patients, 100, 30),
  y_coord = rnorm(n_patients, 100, 30)
)

# Add some realistic missing values
cancer_biomarkers <- cancer_biomarkers %>%
  mutate(
    PSA = ifelse(runif(n_patients) < 0.05, NA, PSA),
    tumor_size = ifelse(runif(n_patients) < 0.1, NA, tumor_size),
    biopsy_gleason = ifelse(runif(n_patients) < 0.15, NA, biopsy_gleason)
  )

# =============================================================================
# Dataset 2: Cardiovascular Risk Assessment Dataset
# =============================================================================

cat("Creating cardiovascular risk assessment dataset...\n")

n_patients_cardio <- 400
cardiovascular_risk <- tibble(
  patient_id = sprintf("CVD_%04d", 1:n_patients_cardio),
  
  # Continuous risk factors
  systolic_bp = rnorm(n_patients_cardio, 140, 25),
  diastolic_bp = rnorm(n_patients_cardio, 90, 15),
  cholesterol = rnorm(n_patients_cardio, 220, 50),
  hdl = rnorm(n_patients_cardio, 45, 15),
  ldl = rnorm(n_patients_cardio, 140, 40),
  triglycerides = rnorm(n_patients_cardio, 150, 60),
  bmi = rnorm(n_patients_cardio, 28, 6),
  age = round(rnorm(n_patients_cardio, 58, 12)),
  
  # Categorical risk factors
  smoking = sample(c("Never", "Former", "Current"), n_patients_cardio, 
                   replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  diabetes = sample(c("No", "Yes"), n_patients_cardio, 
                    replace = TRUE, prob = c(0.8, 0.2)),
  family_history = sample(c("No", "Yes"), n_patients_cardio, 
                          replace = TRUE, prob = c(0.7, 0.3)),
  
  # Outcome - cardiovascular event within 5 years
  cv_event = factor(
    sample(c("No", "Yes"), n_patients_cardio, 
           replace = TRUE, prob = c(0.75, 0.25)),
    levels = c("No", "Yes")
  ),
  
  # Study cohort
  study_cohort = sample(c("training", "testing"), n_patients_cardio, 
                        replace = TRUE, prob = c(0.65, 0.35)),
  
  # Demographics
  sex = sample(c("Male", "Female"), n_patients_cardio, 
               replace = TRUE, prob = c(0.6, 0.4)),
  ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 
                     n_patients_cardio, replace = TRUE, 
                     prob = c(0.65, 0.15, 0.1, 0.07, 0.03)),
  
  # Spatial coordinates
  x_coord = rnorm(n_patients_cardio, 50, 20),
  y_coord = rnorm(n_patients_cardio, 50, 20)
)

# Add missing values
cardiovascular_risk <- cardiovascular_risk %>%
  mutate(
    hdl = ifelse(runif(n_patients_cardio) < 0.08, NA, hdl),
    triglycerides = ifelse(runif(n_patients_cardio) < 0.12, NA, triglycerides),
    family_history = ifelse(runif(n_patients_cardio) < 0.06, NA, family_history)
  )

# =============================================================================
# Dataset 3: Pathology Diagnosis Dataset
# =============================================================================

cat("Creating pathology diagnosis dataset...\n")

n_cases <- 300
pathology_diagnosis <- tibble(
  case_id = sprintf("PATH_%04d", 1:n_cases),
  
  # Histological measurements
  cell_size = rnorm(n_cases, 15, 5),
  nuclear_area = rnorm(n_cases, 8, 3),
  mitotic_count = rpois(n_cases, 3),
  pleomorphism_score = sample(1:3, n_cases, replace = TRUE),
  
  # Immunohistochemistry markers
  ki67_percentage = runif(n_cases, 1, 40),
  p53_positive = sample(c("Negative", "Positive"), n_cases, 
                        replace = TRUE, prob = c(0.6, 0.4)),
  her2_status = sample(c("Negative", "Positive"), n_cases, 
                       replace = TRUE, prob = c(0.8, 0.2)),
  
  # Categorical features
  tumor_type = sample(c("Ductal", "Lobular", "Mixed"), n_cases, 
                      replace = TRUE, prob = c(0.7, 0.2, 0.1)),
  differentiation = sample(c("Well", "Moderate", "Poor"), n_cases, 
                           replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  
  # Outcome
  malignancy = factor(
    sample(c("Benign", "Malignant"), n_cases, 
           replace = TRUE, prob = c(0.4, 0.6)),
    levels = c("Benign", "Malignant")
  ),
  
  # Validation set
  validation_set = sample(c("internal", "external"), n_cases, 
                          replace = TRUE, prob = c(0.8, 0.2)),
  
  # Patient demographics
  patient_age = round(rnorm(n_cases, 55, 15)),
  menopausal_status = sample(c("Pre", "Post"), n_cases, 
                             replace = TRUE, prob = c(0.4, 0.6)),
  
  # Spatial coordinates (tissue microarray)
  x_coord = rnorm(n_cases, 25, 10),
  y_coord = rnorm(n_cases, 25, 10)
)

# Add missing values
pathology_diagnosis <- pathology_diagnosis %>%
  mutate(
    ki67_percentage = ifelse(runif(n_cases) < 0.1, NA, ki67_percentage),
    p53_positive = ifelse(runif(n_cases) < 0.05, NA, p53_positive),
    menopausal_status = ifelse(runif(n_cases) < 0.03, NA, menopausal_status)
  )

# =============================================================================
# Dataset 4: Drug Response Prediction Dataset
# =============================================================================

cat("Creating drug response prediction dataset...\n")

n_patients_drug <- 350
drug_response <- tibble(
  patient_id = sprintf("DRG_%04d", 1:n_patients_drug),
  
  # Genomic biomarkers
  gene_expression_1 = rnorm(n_patients_drug, 100, 30),
  gene_expression_2 = rnorm(n_patients_drug, 80, 25),
  gene_expression_3 = rnorm(n_patients_drug, 120, 40),
  
  # Protein markers
  protein_level_a = rnorm(n_patients_drug, 50, 15),
  protein_level_b = rnorm(n_patients_drug, 75, 20),
  
  # Mutation status
  mutation_status = sample(c("Wild-type", "Mutant"), n_patients_drug, 
                          replace = TRUE, prob = c(0.7, 0.3)),
  
  # Clinical characteristics
  age = round(rnorm(n_patients_drug, 62, 12)),
  performance_status = sample(0:2, n_patients_drug, 
                             replace = TRUE, prob = c(0.5, 0.4, 0.1)),
  prior_treatments = sample(0:3, n_patients_drug, 
                           replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  
  # Categorical features
  tumor_stage = sample(c("II", "III", "IV"), n_patients_drug, 
                       replace = TRUE, prob = c(0.2, 0.4, 0.4)),
  histology = sample(c("Adenocarcinoma", "Squamous", "Other"), 
                     n_patients_drug, replace = TRUE, 
                     prob = c(0.6, 0.25, 0.15)),
  
  # Outcome - drug response
  drug_response = factor(
    sample(c("Non-responder", "Responder"), n_patients_drug, 
           replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("Non-responder", "Responder")
  ),
  
  # Study phase
  study_phase = sample(c("phase1", "phase2"), n_patients_drug, 
                       replace = TRUE, prob = c(0.7, 0.3)),
  
  # Demographics
  sex = sample(c("Male", "Female"), n_patients_drug, 
               replace = TRUE, prob = c(0.55, 0.45)),
  
  # Spatial coordinates
  x_coord = rnorm(n_patients_drug, 75, 25),
  y_coord = rnorm(n_patients_drug, 75, 25)
)

# Add missing values
drug_response <- drug_response %>%
  mutate(
    gene_expression_2 = ifelse(runif(n_patients_drug) < 0.15, NA, gene_expression_2),
    protein_level_b = ifelse(runif(n_patients_drug) < 0.1, NA, protein_level_b),
    prior_treatments = ifelse(runif(n_patients_drug) < 0.07, NA, prior_treatments)
  )

# =============================================================================
# Dataset 5: Pediatric Growth Assessment Dataset
# =============================================================================

cat("Creating pediatric growth assessment dataset...\n")

n_children <- 200
pediatric_growth <- tibble(
  child_id = sprintf("PED_%04d", 1:n_children),
  
  # Growth measurements
  height_cm = rnorm(n_children, 140, 25),
  weight_kg = rnorm(n_children, 35, 12),
  head_circumference = rnorm(n_children, 52, 4),
  
  # Developmental assessments
  age_months = round(rnorm(n_children, 120, 36)),
  motor_score = round(rnorm(n_children, 85, 15)),
  cognitive_score = round(rnorm(n_children, 90, 18)),
  
  # Categorical factors
  sex = sample(c("Male", "Female"), n_children, 
               replace = TRUE, prob = c(0.5, 0.5)),
  birth_weight = sample(c("Normal", "Low", "Very Low"), n_children, 
                        replace = TRUE, prob = c(0.8, 0.15, 0.05)),
  gestational_age = sample(c("Term", "Preterm"), n_children, 
                           replace = TRUE, prob = c(0.85, 0.15)),
  
  # Outcome - growth delay
  growth_delay = factor(
    sample(c("Normal", "Delayed"), n_children, 
           replace = TRUE, prob = c(0.75, 0.25)),
    levels = c("Normal", "Delayed")
  ),
  
  # Study site
  study_site = sample(c("site_A", "site_B"), n_children, 
                      replace = TRUE, prob = c(0.6, 0.4)),
  
  # Additional factors
  maternal_age = round(rnorm(n_children, 28, 6)),
  socioeconomic_status = sample(c("Low", "Medium", "High"), n_children, 
                                replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  
  # Spatial coordinates
  x_coord = rnorm(n_children, 60, 15),
  y_coord = rnorm(n_children, 60, 15)
)

# Add missing values
pediatric_growth <- pediatric_growth %>%
  mutate(
    head_circumference = ifelse(runif(n_children) < 0.08, NA, head_circumference),
    cognitive_score = ifelse(runif(n_children) < 0.12, NA, cognitive_score),
    maternal_age = ifelse(runif(n_children) < 0.05, NA, maternal_age)
  )

# =============================================================================
# Dataset 6: Small Sample Edge Case Dataset
# =============================================================================

cat("Creating small sample edge case dataset...\n")

small_sample_tree <- tibble(
  patient_id = sprintf("SM_%02d", 1:25),
  
  # Simple continuous variables
  biomarker_1 = rnorm(25, 10, 3),
  biomarker_2 = rnorm(25, 15, 5),
  age = round(rnorm(25, 50, 10)),
  
  # Simple categorical variables
  treatment = sample(c("A", "B"), 25, replace = TRUE),
  stage = sample(c("Early", "Advanced"), 25, replace = TRUE),
  
  # Outcome
  outcome = factor(
    sample(c("No", "Yes"), 25, replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("No", "Yes")
  ),
  
  # Validation set
  cohort = sample(c("train", "test"), 25, replace = TRUE, prob = c(0.7, 0.3)),
  
  # Demographics
  sex = sample(c("Male", "Female"), 25, replace = TRUE),
  
  # Spatial coordinates
  x_coord = rnorm(25, 30, 10),
  y_coord = rnorm(25, 30, 10)
)

# =============================================================================
# Save datasets to data/ directory
# =============================================================================

cat("Saving datasets...\n")

# Save as .rda files
usethis::use_data(cancer_biomarkers, overwrite = TRUE)
usethis::use_data(cardiovascular_risk, overwrite = TRUE)  
usethis::use_data(pathology_diagnosis, overwrite = TRUE)
usethis::use_data(drug_response, overwrite = TRUE)
usethis::use_data(pediatric_growth, overwrite = TRUE)
usethis::use_data(small_sample_tree, overwrite = TRUE)

# Save as .csv files for jamovi
write.csv(cancer_biomarkers, "data/cancer_biomarkers.csv", row.names = FALSE)
write.csv(cardiovascular_risk, "data/cardiovascular_risk.csv", row.names = FALSE)
write.csv(pathology_diagnosis, "data/pathology_diagnosis.csv", row.names = FALSE)
write.csv(drug_response, "data/drug_response.csv", row.names = FALSE)
write.csv(pediatric_growth, "data/pediatric_growth.csv", row.names = FALSE)
write.csv(small_sample_tree, "data/small_sample_tree.csv", row.names = FALSE)

# =============================================================================
# Create summary information
# =============================================================================

cat("Creating dataset summary information...\n")

# Create dataset summary
tree_datasets_summary <- tibble(
  Dataset = c("cancer_biomarkers", "cardiovascular_risk", "pathology_diagnosis", 
              "drug_response", "pediatric_growth", "small_sample_tree"),
  N_Patients = c(nrow(cancer_biomarkers), nrow(cardiovascular_risk), 
                 nrow(pathology_diagnosis), nrow(drug_response), 
                 nrow(pediatric_growth), nrow(small_sample_tree)),
  N_Variables = c(ncol(cancer_biomarkers), ncol(cardiovascular_risk), 
                  ncol(pathology_diagnosis), ncol(drug_response), 
                  ncol(pediatric_growth), ncol(small_sample_tree)),
  Description = c("Cancer biomarker diagnosis with PSA, grade, stage",
                  "Cardiovascular risk assessment with traditional risk factors",
                  "Pathology diagnosis with histological measurements",
                  "Drug response prediction with genomic biomarkers",
                  "Pediatric growth assessment with developmental measures",
                  "Small sample dataset for edge case testing"),
  Clinical_Context = c("Oncology - Prostate Cancer", "Cardiology - Risk Assessment",
                       "Pathology - Breast Cancer", "Pharmacogenomics - Drug Response",
                       "Pediatrics - Growth Assessment", "General - Edge Cases"),
  Target_Variable = c("diagnosis", "cv_event", "malignancy", "drug_response", 
                      "growth_delay", "outcome"),
  Spatial_Analysis = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
)

# Save summary
usethis::use_data(tree_datasets_summary, overwrite = TRUE)
write.csv(tree_datasets_summary, "data/tree_datasets_summary.csv", row.names = FALSE)

# =============================================================================
# Create test scenarios documentation
# =============================================================================

tree_test_scenarios <- tibble(
  Scenario = c("Basic Classification", "Biomarker Selection", "Risk Stratification",
               "Spatial Analysis", "Cross-Validation", "Bootstrap Validation", 
               "Model Comparison", "Clinical Interpretation", "Missing Data Handling",
               "Class Imbalance", "Feature Scaling", "Edge Case Testing"),
  Dataset = c("cancer_biomarkers", "drug_response", "cardiovascular_risk",
              "pathology_diagnosis", "cancer_biomarkers", "drug_response",
              "pathology_diagnosis", "cardiovascular_risk", "pediatric_growth",
              "drug_response", "cancer_biomarkers", "small_sample_tree"),
  Analysis_Type = c("Standard Decision Tree", "Feature Importance", "Risk Groups",
                    "Autocart Spatial", "k-fold CV", "Bootstrap CI", 
                    "Algorithm Comparison", "Medical Guidelines", "Imputation",
                    "Class Balancing", "Standardization", "Robustness Testing"),
  Variables = c("PSA, age, grade, stage", "gene_expression_*, protein_level_*",
                "systolic_bp, cholesterol, smoking", "x_coord, y_coord + histology",
                "All variables", "genomic + clinical", "multiple algorithms",
                "risk factors + outcomes", "missing biomarkers", "rare outcomes",
                "mixed scales", "minimal variables"),
  Expected_Result = c("Accurate classification", "Feature ranking", "Risk categories",
                      "Spatial patterns", "CV performance", "Confidence intervals",
                      "Best algorithm", "Clinical guidelines", "Imputed values",
                      "Balanced performance", "Standardized features", "Robust handling")
)

# Save test scenarios
usethis::use_data(tree_test_scenarios, overwrite = TRUE)
write.csv(tree_test_scenarios, "data/tree_test_scenarios.csv", row.names = FALSE)

cat("\n=== TREE TEST DATA GENERATION COMPLETE ===\n")
cat("✅ Total datasets created: 6\n")
cat("✅ Total patients: ", sum(tree_datasets_summary$N_Patients), "\n")
cat("✅ Clinical contexts covered: Oncology, Cardiology, Pathology, Pharmacogenomics, Pediatrics\n")
cat("✅ Decision tree features: FFTrees, spatial analysis, cross-validation, bootstrap\n")
cat("✅ Statistical features: Risk stratification, model comparison, clinical interpretation\n")
cat("✅ Test scenarios documented: 12 comprehensive scenarios\n")

cat("\nDatasets Created:\n")
for (i in 1:nrow(tree_datasets_summary)) {
  cat("- ", tree_datasets_summary$Dataset[i], " (", tree_datasets_summary$N_Patients[i], 
      " patients): ", tree_datasets_summary$Description[i], "\n")
}

cat("\nAll datasets include both .rda and .csv formats\n")
cat("Enhanced tree function ready for comprehensive medical decision tree testing!\n")