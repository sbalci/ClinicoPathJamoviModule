# Create comprehensive test datasets for jjbetweenstats function
# This script generates multiple datasets optimized for continuous vs categorical analysis

library(dplyr)

set.seed(20250707)  # For reproducible data generation

# Dataset 1: Clinical Laboratory Results
clinical_lab_data <- data.frame(
  patient_id = 1:300,
  treatment_group = sample(c("Control", "Drug A", "Drug B", "Combination"), 300, 
                          replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
  hemoglobin = rnorm(300, mean = 13.5, sd = 2.1) + 
    ifelse(sample(c("Control", "Drug A", "Drug B", "Combination"), 300, replace = TRUE) == "Drug A", 1.2, 0) +
    ifelse(sample(c("Control", "Drug A", "Drug B", "Combination"), 300, replace = TRUE) == "Drug B", 0.8, 0),
  white_blood_cells = rlnorm(300, meanlog = 2.1, sdlog = 0.4),
  platelet_count = rnorm(300, mean = 250, sd = 80) + 
    ifelse(sample(c("Control", "Drug A", "Drug B", "Combination"), 300, replace = TRUE) == "Combination", -30, 0),
  creatinine = rlnorm(300, meanlog = 0.1, sdlog = 0.3),
  bilirubin = rexp(300, rate = 2),
  albumin = rnorm(300, mean = 4.2, sd = 0.6),
  disease_severity = sample(c("Mild", "Moderate", "Severe"), 300, 
                           replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  age_group = sample(c("18-30", "31-50", "51-70", "70+"), 300,
                    replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  hospital = sample(c("Hospital A", "Hospital B", "Hospital C"), 300,
                   replace = TRUE, prob = c(0.4, 0.35, 0.25))
)

# Dataset 2: Biomarker Expression Study
biomarker_expression_data <- data.frame(
  sample_id = 1:200,
  tissue_type = sample(c("Normal", "Benign", "Malignant"), 200,
                      replace = TRUE, prob = c(0.3, 0.3, 0.4)),
  protein_a_expression = rlnorm(200, meanlog = 3.2, sdlog = 0.8) +
    ifelse(sample(c("Normal", "Benign", "Malignant"), 200, replace = TRUE) == "Malignant", 2.5, 0),
  protein_b_expression = rnorm(200, mean = 45, sd = 15) +
    ifelse(sample(c("Normal", "Benign", "Malignant"), 200, replace = TRUE) == "Benign", 8, 0),
  gene_expression_score = rnorm(200, mean = 100, sd = 25) +
    ifelse(sample(c("Normal", "Benign", "Malignant"), 200, replace = TRUE) == "Malignant", 35, 0),
  cell_proliferation_index = rbeta(200, shape1 = 2, shape2 = 5) * 100,
  apoptosis_rate = rexp(200, rate = 0.1),
  mutation_burden = rpois(200, lambda = 8) +
    ifelse(sample(c("Normal", "Benign", "Malignant"), 200, replace = TRUE) == "Malignant", 15, 0),
  tumor_grade = sample(c("Grade I", "Grade II", "Grade III"), 200,
                      replace = TRUE, prob = c(0.35, 0.4, 0.25)),
  patient_sex = sample(c("Male", "Female"), 200, replace = TRUE, prob = c(0.48, 0.52)),
  study_center = sample(c("Center 1", "Center 2", "Center 3", "Center 4"), 200,
                       replace = TRUE)
)

# Dataset 3: Pharmacokinetics Study
pharmacokinetics_data <- data.frame(
  subject_id = 1:150,
  dose_level = sample(c("Low", "Medium", "High", "Very High"), 150,
                     replace = TRUE),
  peak_concentration = rlnorm(150, meanlog = 2.5, sdlog = 0.6) *
    ifelse(sample(c("Low", "Medium", "High", "Very High"), 150, replace = TRUE) == "Medium", 1.5, 1) *
    ifelse(sample(c("Low", "Medium", "High", "Very High"), 150, replace = TRUE) == "High", 2.2, 1) *
    ifelse(sample(c("Low", "Medium", "High", "Very High"), 150, replace = TRUE) == "Very High", 3.1, 1),
  time_to_peak = rlnorm(150, meanlog = 1.2, sdlog = 0.4),
  half_life = rnorm(150, mean = 8.5, sd = 2.1),
  clearance_rate = rlnorm(150, meanlog = 1.8, sdlog = 0.5),
  volume_distribution = rnorm(150, mean = 65, sd = 18),
  bioavailability = rbeta(150, shape1 = 8, shape2 = 2) * 100,
  metabolite_ratio = rlnorm(150, meanlog = 0.3, sdlog = 0.7),
  formulation = sample(c("Tablet", "Capsule", "Liquid", "Injection"), 150,
                      replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
  genetic_variant = sample(c("Wild Type", "Variant 1", "Variant 2"), 150,
                          replace = TRUE, prob = c(0.6, 0.25, 0.15)),
  food_status = sample(c("Fasted", "Fed"), 150, replace = TRUE)
)

# Dataset 4: Psychological Assessment Data
psychological_assessment_data <- data.frame(
  participant_id = 1:250,
  intervention_group = sample(c("Control", "CBT", "Medication", "Combined"), 250,
                             replace = TRUE),
  depression_score = pmax(0, rnorm(250, mean = 15, sd = 8) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "CBT", -5, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Medication", -3, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Combined", -7, 0)),
  anxiety_score = pmax(0, rnorm(250, mean = 12, sd = 6) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "CBT", -4, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Medication", -2, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Combined", -5, 0)),
  quality_of_life = pmin(100, pmax(0, rnorm(250, mean = 65, sd = 20) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "CBT", 8, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Medication", 5, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Combined", 12, 0))),
  cognitive_score = pmin(30, pmax(0, rnorm(250, mean = 22, sd = 5) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "CBT", 2, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Combined", 3, 0))),
  sleep_quality = pmin(21, pmax(0, rnorm(250, mean = 8, sd = 4) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Medication", 3, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Combined", 4, 0))),
  stress_level = pmax(0, rnorm(250, mean = 18, sd = 7) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "CBT", -6, 0) +
    ifelse(sample(c("Control", "CBT", "Medication", "Combined"), 250, replace = TRUE) == "Combined", -4, 0)),
  baseline_severity = sample(c("Mild", "Moderate", "Severe"), 250,
                            replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  education_level = sample(c("High School", "Bachelor", "Graduate"), 250,
                          replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  session_attendance = sample(c("High", "Medium", "Low"), 250,
                             replace = TRUE, prob = c(0.5, 0.3, 0.2))
)

# Dataset 5: Exercise Physiology Study
exercise_physiology_data <- data.frame(
  athlete_id = 1:180,
  training_regimen = sample(c("Endurance", "Strength", "Mixed", "Control"), 180,
                           replace = TRUE),
  vo2_max = rnorm(180, mean = 45, sd = 12) +
    ifelse(sample(c("Endurance", "Strength", "Mixed", "Control"), 180, replace = TRUE) == "Endurance", 8, 0) +
    ifelse(sample(c("Endurance", "Strength", "Mixed", "Control"), 180, replace = TRUE) == "Mixed", 5, 0),
  max_heart_rate = rnorm(180, mean = 185, sd = 15),
  resting_heart_rate = rnorm(180, mean = 65, sd = 10) +
    ifelse(sample(c("Endurance", "Strength", "Mixed", "Control"), 180, replace = TRUE) == "Endurance", -8, 0),
  lactate_threshold = rnorm(180, mean = 4.2, sd = 1.1) +
    ifelse(sample(c("Endurance", "Strength", "Mixed", "Control"), 180, replace = TRUE) == "Endurance", 0.8, 0),
  muscle_mass = rnorm(180, mean = 32, sd = 8) +
    ifelse(sample(c("Endurance", "Strength", "Mixed", "Control"), 180, replace = TRUE) == "Strength", 6, 0) +
    ifelse(sample(c("Endurance", "Strength", "Mixed", "Control"), 180, replace = TRUE) == "Mixed", 3, 0),
  body_fat_percentage = pmin(35, pmax(5, rnorm(180, mean = 15, sd = 6))),
  flexibility_score = rnorm(180, mean = 25, sd = 8),
  sport_type = sample(c("Cycling", "Running", "Swimming", "Weightlifting"), 180,
                     replace = TRUE),
  experience_level = sample(c("Beginner", "Intermediate", "Advanced"), 180,
                           replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  gender = sample(c("Male", "Female"), 180, replace = TRUE, prob = c(0.55, 0.45))
)

# Convert categorical variables to factors
convert_to_factors <- function(df) {
  factor_cols <- sapply(df, function(x) is.character(x) && !grepl("_id$", names(df)[which(sapply(df, identical, x))]))
  df[factor_cols] <- lapply(df[factor_cols], as.factor)
  return(df)
}

clinical_lab_data <- convert_to_factors(clinical_lab_data)
biomarker_expression_data <- convert_to_factors(biomarker_expression_data)
pharmacokinetics_data <- convert_to_factors(pharmacokinetics_data)
psychological_assessment_data <- convert_to_factors(psychological_assessment_data)
exercise_physiology_data <- convert_to_factors(exercise_physiology_data)

# Save datasets
usethis::use_data(clinical_lab_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(biomarker_expression_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(pharmacokinetics_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(psychological_assessment_data, overwrite = TRUE, internal = FALSE)
usethis::use_data(exercise_physiology_data, overwrite = TRUE, internal = FALSE)

# Create summary information
cat("Test datasets created for jjbetweenstats function:\n")
cat("1. clinical_lab_data: Clinical laboratory results (n=300)\n")
cat("2. biomarker_expression_data: Biomarker expression study (n=200)\n")
cat("3. pharmacokinetics_data: Pharmacokinetics study (n=150)\n")
cat("4. psychological_assessment_data: Psychological assessment (n=250)\n")
cat("5. exercise_physiology_data: Exercise physiology study (n=180)\n")
cat("\nAll datasets optimized for continuous vs categorical variable analysis.\n")