# Create comprehensive test datasets for jjbarstats function
# This script generates multiple datasets to test various scenarios

library(dplyr)

set.seed(20250707)  # For reproducible data generation

# Dataset 1: Basic Medical Study (Treatment vs Control)
medical_study_data <- data.frame(
  patient_id = 1:200,
  treatment_group = sample(c("Control", "Treatment A", "Treatment B"), 200, 
                          replace = TRUE, prob = c(0.4, 0.3, 0.3)),
  response = sample(c("Complete Response", "Partial Response", "No Response"), 200,
                   replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  severity = sample(c("Mild", "Moderate", "Severe"), 200,
                   replace = TRUE, prob = c(0.4, 0.35, 0.25)),
  gender = sample(c("Male", "Female"), 200, replace = TRUE, prob = c(0.48, 0.52)),
  age_group = sample(c("18-30", "31-50", "51-65", "65+"), 200,
                    replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  hospital_site = sample(c("Site A", "Site B", "Site C"), 200,
                        replace = TRUE, prob = c(0.4, 0.35, 0.25))
)

# Dataset 2: Patient Satisfaction Survey
patient_satisfaction_data <- data.frame(
  respondent_id = 1:150,
  satisfaction_level = sample(c("Very Dissatisfied", "Dissatisfied", "Neutral", 
                               "Satisfied", "Very Satisfied"), 150,
                             replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.35, 0.2)),
  service_type = sample(c("Emergency", "Outpatient", "Inpatient", "Surgery"), 150,
                       replace = TRUE, prob = c(0.25, 0.3, 0.25, 0.2)),
  staff_rating = sample(c("Poor", "Fair", "Good", "Excellent"), 150,
                       replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)),
  department = sample(c("Cardiology", "Oncology", "Orthopedics", "Neurology"), 150,
                     replace = TRUE),
  visit_type = sample(c("First Visit", "Follow-up", "Emergency"), 150,
                     replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  insurance_type = sample(c("Private", "Public", "Self-Pay"), 150,
                         replace = TRUE, prob = c(0.5, 0.35, 0.15))
)

# Dataset 3: Clinical Trial Outcomes
clinical_trial_data <- data.frame(
  subject_id = 1:120,
  drug_dosage = sample(c("Low Dose", "Medium Dose", "High Dose", "Placebo"), 120,
                      replace = TRUE),
  primary_outcome = sample(c("Success", "Failure", "Inconclusive"), 120,
                          replace = TRUE, prob = c(0.45, 0.35, 0.2)),
  side_effects = sample(c("None", "Mild", "Moderate", "Severe"), 120,
                       replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05)),
  baseline_condition = sample(c("Excellent", "Good", "Fair", "Poor"), 120,
                             replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1)),
  study_phase = sample(c("Phase I", "Phase II", "Phase III"), 120,
                      replace = TRUE, prob = c(0.25, 0.4, 0.35)),
  geographical_region = sample(c("North America", "Europe", "Asia", "Other"), 120,
                              replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
)

# Dataset 4: Diagnostic Test Results
diagnostic_test_data <- data.frame(
  test_id = 1:180,
  test_result = sample(c("Positive", "Negative", "Indeterminate"), 180,
                      replace = TRUE, prob = c(0.3, 0.6, 0.1)),
  test_method = sample(c("Method A", "Method B", "Method C"), 180,
                      replace = TRUE),
  sample_quality = sample(c("Excellent", "Good", "Adequate", "Poor"), 180,
                         replace = TRUE, prob = c(0.3, 0.4, 0.25, 0.05)),
  laboratory = sample(c("Lab 1", "Lab 2", "Lab 3", "Lab 4"), 180,
                     replace = TRUE),
  urgency_level = sample(c("Routine", "Urgent", "STAT"), 180,
                        replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  technician_experience = sample(c("Novice", "Intermediate", "Expert"), 180,
                                replace = TRUE, prob = c(0.2, 0.5, 0.3))
)

# Dataset 5: Quality Improvement Data
quality_improvement_data <- data.frame(
  record_id = 1:100,
  improvement_category = sample(c("Patient Safety", "Efficiency", "Quality", "Cost"), 100,
                               replace = TRUE),
  implementation_status = sample(c("Not Started", "In Progress", "Completed", "On Hold"), 100,
                                replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1)),
  priority_level = sample(c("Low", "Medium", "High", "Critical"), 100,
                         replace = TRUE, prob = c(0.25, 0.35, 0.3, 0.1)),
  department_involved = sample(c("Medicine", "Surgery", "Nursing", "Administration"), 100,
                              replace = TRUE),
  resource_requirement = sample(c("Minimal", "Moderate", "Substantial"), 100,
                               replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  expected_impact = sample(c("Low", "Medium", "High"), 100,
                          replace = TRUE, prob = c(0.3, 0.4, 0.3))
)

# Convert character variables to factors for proper testing
convert_to_factors <- function(df) {
  df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)
  return(df)
}

medical_study_data <- convert_to_factors(medical_study_data)
patient_satisfaction_data <- convert_to_factors(patient_satisfaction_data)
clinical_trial_data <- convert_to_factors(clinical_trial_data)
diagnostic_test_data <- convert_to_factors(diagnostic_test_data)
quality_improvement_data <- convert_to_factors(quality_improvement_data)

# Save datasets
usethis::use_data(medical_study_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(patient_satisfaction_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(clinical_trial_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(diagnostic_test_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(quality_improvement_data, overwrite = TRUE, internal = FALSE)

# Create summary information
cat("Test datasets created for jjbarstats function:\n")
cat("1. medical_study_data: Medical treatment study (n=200)\n")
cat("2. patient_satisfaction_data: Patient satisfaction survey (n=150)\n")
cat("3. clinical_trial_data: Clinical trial outcomes (n=120)\n")
cat("4. diagnostic_test_data: Diagnostic test results (n=180)\n")
cat("5. quality_improvement_data: Quality improvement tracking (n=100)\n")
cat("\nAll datasets have multiple categorical variables suitable for bar chart analysis.\n")