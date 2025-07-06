# Create test data for jcomplexupset function
# This script generates datasets suitable for testing Complex UpSet plots

library(tidyverse)

# Create comprehensive UpSet plot test data
set.seed(123)

# Generate sample data with multiple binary variables representing set membership
n_patients <- 200

jcomplexupset_test_data <- data.frame(
  PatientID = 1:n_patients,
  
  # Demographics
  Age = sample(25:85, n_patients, replace = TRUE),
  Sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.45, 0.55)),
  
  # Binary variables for UpSet analysis - Treatment modalities
  Surgery = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.7, 0.3)),
  Chemotherapy = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.6, 0.4)),
  Radiotherapy = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.5, 0.5)),
  Immunotherapy = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
  TargetedTherapy = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.25, 0.75)),
  
  # Binary variables - Biomarkers
  HER2_Positive = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.2, 0.8)),
  ER_Positive = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.7, 0.3)),
  PR_Positive = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.6, 0.4)),
  PDL1_Positive = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
  
  # Binary variables - Complications
  Infection = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.15, 0.85)),
  Bleeding = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.1, 0.9)),
  Nausea = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.4, 0.6)),
  Fatigue = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.6, 0.4)),
  
  # Binary variables - Response indicators
  Complete_Response = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
  Partial_Response = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.4, 0.6)),
  Stable_Disease = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.2, 0.8)),
  Progressive_Disease = sample(c(TRUE, FALSE), n_patients, replace = TRUE, prob = c(0.1, 0.9)),
  
  # Continuous variables for optional weighting
  TumorSize = round(rnorm(n_patients, mean = 3.5, sd = 1.2), 1),
  SurvivalMonths = round(rexp(n_patients, rate = 1/24), 1),
  TreatmentCost = round(rnorm(n_patients, mean = 50000, sd = 15000), 0)
)

# Ensure some logical constraints for realistic data
for (i in 1:n_patients) {
  # Response categories should be mutually exclusive
  responses <- c("Complete_Response", "Partial_Response", "Stable_Disease", "Progressive_Disease")
  if (sum(jcomplexupset_test_data[i, responses]) > 1) {
    # Reset all to FALSE, then randomly assign one
    jcomplexupset_test_data[i, responses] <- FALSE
    jcomplexupset_test_data[i, sample(responses, 1)] <- TRUE
  }
  
  # If no treatment, no response
  if (!any(jcomplexupset_test_data[i, c("Surgery", "Chemotherapy", "Radiotherapy", "Immunotherapy", "TargetedTherapy")])) {
    jcomplexupset_test_data[i, responses] <- FALSE
  }
}

# Create a second dataset for molecular subtyping analysis
molecular_subtype_data <- data.frame(
  SampleID = 1:150,
  
  # Molecular markers
  BRCA1_Mutation = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.05, 0.95)),
  BRCA2_Mutation = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.03, 0.97)),
  TP53_Mutation = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.3, 0.7)),
  PIK3CA_Mutation = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.35, 0.65)),
  KRAS_Mutation = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.25, 0.75)),
  EGFR_Mutation = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.15, 0.85)),
  
  # Pathway alterations
  PI3K_Pathway = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.4, 0.6)),
  WNT_Pathway = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.2, 0.8)),
  RB_Pathway = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.3, 0.7)),
  DNA_Repair = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.25, 0.75)),
  
  # Clinical variables
  Grade = sample(1:3, 150, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  Stage = sample(1:4, 150, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  Age_Group = sample(c("Young", "Middle", "Elderly"), 150, replace = TRUE, prob = c(0.2, 0.5, 0.3))
)

# Create third dataset for diagnostic test combinations
diagnostic_test_data <- data.frame(
  CaseID = 1:180,
  
  # Diagnostic tests
  CT_Scan = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.8, 0.2)),
  MRI = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.6, 0.4)),
  PET_Scan = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.4, 0.6)),
  Ultrasound = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.7, 0.3)),
  Biopsy = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.9, 0.1)),
  
  # Laboratory tests
  CBC = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.95, 0.05)),
  Chemistry_Panel = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.9, 0.1)),
  Tumor_Markers = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.7, 0.3)),
  Genetic_Testing = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.3, 0.7)),
  
  # Specialist consultations
  Oncology = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.8, 0.2)),
  Surgery = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.6, 0.4)),
  Radiology = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.7, 0.3)),
  Pathology = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.85, 0.15)),
  
  # Outcome variables
  Diagnosis_Confirmed = sample(c(TRUE, FALSE), 180, replace = TRUE, prob = c(0.7, 0.3)),
  Time_to_Diagnosis = round(rnorm(180, mean = 14, sd = 7), 0)  # days
)

# Save the datasets
save(jcomplexupset_test_data, file = "data/jcomplexupset_test_data.rda")
save(molecular_subtype_data, file = "data/molecular_subtype_data.rda") 
save(diagnostic_test_data, file = "data/diagnostic_test_data.rda")

# Print summary
cat("Created jcomplexupset test datasets:\n")
cat("- jcomplexupset_test_data:", nrow(jcomplexupset_test_data), "patients with", 
    sum(sapply(jcomplexupset_test_data, is.logical)), "binary variables\n")
cat("- molecular_subtype_data:", nrow(molecular_subtype_data), "samples with", 
    sum(sapply(molecular_subtype_data, is.logical)), "binary variables\n") 
cat("- diagnostic_test_data:", nrow(diagnostic_test_data), "cases with", 
    sum(sapply(diagnostic_test_data, is.logical)), "binary variables\n")