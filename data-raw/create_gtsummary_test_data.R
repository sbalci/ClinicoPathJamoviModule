# Create comprehensive test datasets for gtsummary function
# This script generates datasets specifically designed for testing gtsummary functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(magrittr)

set.seed(456)

# 1. Clinical trial data for publication-ready tables
create_gtsummary_clinical_trial <- function() {
  n <- 300
  
  data.frame(
    # Patient demographics
    patient_id = paste0("PT", sprintf("%03d", 1:n)),
    age = round(rnorm(n, 65, 12)),
    gender = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))),
    race = factor(sample(c("White", "Black", "Hispanic", "Asian", "Other"), n, replace = TRUE,
                        prob = c(0.6, 0.2, 0.1, 0.08, 0.02))),
    education = factor(sample(c("High School", "College", "Graduate"), n, replace = TRUE,
                             prob = c(0.4, 0.4, 0.2)),
                      levels = c("High School", "College", "Graduate"), ordered = TRUE),
    
    # Treatment assignment
    treatment_group = factor(sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE,
                                   prob = c(0.33, 0.33, 0.34))),
    study_site = factor(sample(c("Site 1", "Site 2", "Site 3", "Site 4"), n, replace = TRUE)),
    
    # Baseline characteristics
    bmi = round(rnorm(n, 27, 4.5), 1),
    
    systolic_bp = round(rnorm(n, 135, 20)),
    diastolic_bp = round(rnorm(n, 85, 15)),
    
    diabetes = factor(sample(c("No", "Type 1", "Type 2"), n, replace = TRUE,
                            prob = c(0.7, 0.05, 0.25))),
    
    # Laboratory values
    hemoglobin = round(rnorm(n, 13.5, 2), 1),
    creatinine = round(rnorm(n, 1.1, 0.3), 2),
    cholesterol = round(rnorm(n, 200, 40)),
    
    # Disease characteristics
    disease_stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n, replace = TRUE,
                                 prob = c(0.3, 0.35, 0.25, 0.1)),
                          levels = c("Stage I", "Stage II", "Stage III", "Stage IV"), ordered = TRUE),
    
    tumor_size = round(rnorm(n, 3.2, 1.8), 1),
    lymph_nodes = sample(0:15, n, replace = TRUE, prob = c(0.4, rep(0.6/15, 15))),
    
    # Biomarkers
    biomarker_a = round(rnorm(n, 250, 80), 1),
    biomarker_b = round(rexp(n, rate = 1/100), 1),
    
    # Quality of life scores
    qol_baseline = sample(0:100, n, replace = TRUE),
    pain_score = sample(0:10, n, replace = TRUE),
    
    # Outcomes
    response = factor(sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                            n, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25))),
    
    # Time-to-event data
    follow_up_months = round(runif(n, 6, 36), 1),
    event_occurred = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3))),
    
    # Missing values (realistic pattern)
    missing_indicator = sample(1:10, n, replace = TRUE),
    
    stringsAsFactors = FALSE
  ) %>%
  # Add derived variables
  mutate(
    bmi_category = factor(case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obese"
    ), levels = c("Underweight", "Normal", "Overweight", "Obese"), ordered = TRUE),
    
    hypertension = factor(ifelse(systolic_bp >= 140 | diastolic_bp >= 90, "Yes", "No"))
  ) %>%
  # Introduce realistic missing patterns
  mutate(
    education = ifelse(missing_indicator <= 1, NA, education),
    biomarker_b = ifelse(missing_indicator <= 2, NA, biomarker_b),
    qol_baseline = ifelse(missing_indicator == 1, NA, qol_baseline),
    follow_up_months = ifelse(missing_indicator <= 1, NA, follow_up_months)
  ) %>%
  select(-missing_indicator)
}

# 2. Survey research data for demographic analysis
create_gtsummary_survey_data <- function() {
  n <- 500
  
  data.frame(
    respondent_id = paste0("R", sprintf("%04d", 1:n)),
    survey_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                        n, replace = TRUE),
    
    # Demographics
    age_group = factor(sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), n, replace = TRUE,
                             prob = c(0.15, 0.2, 0.2, 0.2, 0.15, 0.1)),
                      levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), ordered = TRUE),
    
    gender = factor(sample(c("Male", "Female", "Non-binary", "Prefer not to say"), n, replace = TRUE,
                          prob = c(0.48, 0.49, 0.02, 0.01))),
    
    income_level = factor(sample(c("< $25K", "$25K-$50K", "$50K-$75K", "$75K-$100K", "> $100K"), 
                                n, replace = TRUE, prob = c(0.2, 0.25, 0.25, 0.2, 0.1)),
                         levels = c("< $25K", "$25K-$50K", "$50K-$75K", "$75K-$100K", "> $100K"), 
                         ordered = TRUE),
    
    education_level = factor(sample(c("Less than HS", "High School", "Some College", "Bachelor's", "Graduate"), 
                                   n, replace = TRUE, prob = c(0.1, 0.3, 0.25, 0.25, 0.1)),
                            levels = c("Less than HS", "High School", "Some College", "Bachelor's", "Graduate"), 
                            ordered = TRUE),
    
    region = factor(sample(c("Northeast", "Southeast", "Midwest", "West"), n, replace = TRUE,
                          prob = c(0.2, 0.3, 0.25, 0.25))),
    
    urban_rural = factor(sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE,
                               prob = c(0.4, 0.4, 0.2))),
    
    # Survey responses (Likert scales)
    satisfaction_overall = sample(1:7, n, replace = TRUE),
    satisfaction_service = sample(1:7, n, replace = TRUE),
    satisfaction_value = sample(1:7, n, replace = TRUE),
    
    likelihood_recommend = sample(0:10, n, replace = TRUE),
    trust_score = sample(1:5, n, replace = TRUE),
    
    # Behavioral measures
    time_spent_minutes = round(rexp(n, rate = 1/20)),
    pages_visited = sample(1:25, n, replace = TRUE),
    return_visitor = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.4, 0.6))),
    
    # Purchase behavior
    made_purchase = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3))),
    
    
    stringsAsFactors = FALSE
  ) %>%
  # Add derived variables
  mutate(
    purchase_amount = ifelse(made_purchase == "Yes", round(rexp(n, rate = 1/50), 2), 0),
    
    overall_satisfaction = factor(case_when(
      satisfaction_overall <= 2 ~ "Very Dissatisfied",
      satisfaction_overall <= 4 ~ "Dissatisfied", 
      satisfaction_overall == 5 ~ "Neutral",
      satisfaction_overall == 6 ~ "Satisfied",
      satisfaction_overall == 7 ~ "Very Satisfied"
    ), levels = c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied"), 
    ordered = TRUE)
  ) %>%
  # Add realistic missing patterns
  mutate(
    income_level = ifelse(runif(n) < 0.1, NA, income_level),
    purchase_amount = ifelse(made_purchase == "No", NA, purchase_amount)
  )
}

# 3. Laboratory data for medical analysis
create_gtsummary_laboratory_data <- function() {
  n <- 400
  
  data.frame(
    specimen_id = paste0("LAB", sprintf("%04d", 1:n)),
    collection_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                            n, replace = TRUE),
    
    # Patient demographics
    patient_age = round(rnorm(n, 55, 18)),
    patient_sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    
    # Laboratory test categories
    test_type = factor(sample(c("Routine", "Urgent", "Stat"), n, replace = TRUE,
                             prob = c(0.7, 0.2, 0.1))),
    
    department = factor(sample(c("Cardiology", "Oncology", "Endocrinology", "Nephrology", "General"), 
                              n, replace = TRUE, prob = c(0.2, 0.25, 0.2, 0.15, 0.2))),
    
    # Hematology panel
    wbc_count = round(rnorm(n, 7.5, 2.5), 1),
    rbc_count = round(rnorm(n, 4.5, 0.5), 2),
    hemoglobin = round(rnorm(n, 14, 2), 1),
    hematocrit = round(rnorm(n, 42, 6), 1),
    platelet_count = round(rnorm(n, 250, 75)),
    
    # Chemistry panel
    glucose = round(rnorm(n, 95, 25)),
    bun = round(rnorm(n, 15, 8)),
    creatinine = round(rnorm(n, 1.0, 0.4), 2),
    sodium = round(rnorm(n, 140, 5)),
    potassium = round(rnorm(n, 4.0, 0.6), 1),
    chloride = round(rnorm(n, 105, 8)),
    
    # Liver function
    alt = round(rnorm(n, 25, 15)),
    ast = round(rnorm(n, 28, 18)),
    bilirubin_total = round(rnorm(n, 1.0, 0.5), 1),
    albumin = round(rnorm(n, 4.2, 0.6), 1),
    
    # Lipid panel
    total_cholesterol = round(rnorm(n, 200, 40)),
    hdl_cholesterol = round(rnorm(n, 50, 15)),
    ldl_cholesterol = round(rnorm(n, 130, 35)),
    triglycerides = round(rnorm(n, 150, 75)),
    
    
    # Overall assessment
    critical_values = factor(sample(c("None", "One", "Multiple"), n, replace = TRUE,
                                   prob = c(0.85, 0.12, 0.03))),
    
    stringsAsFactors = FALSE
  ) %>%
  # Add derived variables
  mutate(
    glucose_flag = factor(case_when(
      glucose < 70 ~ "Low",
      glucose > 125 ~ "High",
      TRUE ~ "Normal"
    )),
    
    creatinine_flag = factor(case_when(
      creatinine > 1.3 ~ "High",
      TRUE ~ "Normal"
    )),
    
    cholesterol_flag = factor(case_when(
      total_cholesterol > 240 ~ "High",
      total_cholesterol > 200 ~ "Borderline",
      TRUE ~ "Normal"
    ), levels = c("Normal", "Borderline", "High"), ordered = TRUE)
  )
}

# 4. Quality control manufacturing data
create_gtsummary_manufacturing_data <- function() {
  n <- 350
  
  data.frame(
    batch_id = paste0("B", sprintf("%04d", 1:n)),
    production_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                            n, replace = TRUE),
    
    # Manufacturing parameters
    production_line = factor(sample(c("Line A", "Line B", "Line C", "Line D"), n, replace = TRUE)),
    shift = factor(sample(c("Day", "Night", "Weekend"), n, replace = TRUE, prob = c(0.5, 0.4, 0.1))),
    operator_experience = factor(sample(c("Junior", "Experienced", "Senior"), n, replace = TRUE,
                                       prob = c(0.3, 0.5, 0.2)),
                                levels = c("Junior", "Experienced", "Senior"), ordered = TRUE),
    
    # Process parameters
    temperature_setpoint = sample(c(180, 185, 190), n, replace = TRUE),
    pressure_psi = round(rnorm(n, 100, 5), 1),
    ph_level = round(rnorm(n, 7.0, 0.5), 2),
    
    # Quality measurements
    product_weight = round(rnorm(n, 500, 25), 1),
    density = round(rnorm(n, 1.2, 0.08), 3),
    viscosity = round(rnorm(n, 50, 8), 1),
    color_score = sample(1:10, n, replace = TRUE),
    
    # Defect counts
    visual_defects = sample(0:5, n, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.03, 0.01, 0.01)),
    dimensional_defects = sample(0:3, n, replace = TRUE, prob = c(0.7, 0.2, 0.08, 0.02)),
    functional_defects = sample(0:2, n, replace = TRUE, prob = c(0.85, 0.12, 0.03)),
    
    # Quality grades
    overall_grade = factor(sample(c("A", "B", "C", "Reject"), n, replace = TRUE,
                                 prob = c(0.6, 0.25, 0.1, 0.05)),
                          levels = c("A", "B", "C", "Reject"), ordered = TRUE),
    
    # Cost and efficiency
    production_cost = round(rnorm(n, 15, 3), 2),
    cycle_time_minutes = round(rnorm(n, 45, 8), 1),
    yield_percent = round(rnorm(n, 92, 5), 1),
    
    stringsAsFactors = FALSE
  ) %>%
  # Add derived variables
  mutate(
    actual_temperature = round(temperature_setpoint + rnorm(n, 0, 2), 1),
    
    within_specification = factor(case_when(
      visual_defects == 0 & dimensional_defects == 0 & functional_defects == 0 ~ "Yes",
      TRUE ~ "No"
    )),
    
    total_defects = visual_defects + dimensional_defects + functional_defects,
    
    efficiency_score = round(yield_percent * (cycle_time_minutes / 45) * 100, 1),
    
    process_control_flag = factor(case_when(
      abs(actual_temperature - temperature_setpoint) > 3 ~ "Out of Control",
      pressure_psi < 95 | pressure_psi > 105 ~ "Out of Control", 
      ph_level < 6.5 | ph_level > 7.5 ~ "Out of Control",
      TRUE ~ "In Control"
    ))
  )
}

# 5. Cross-tabulation data for categorical analysis
create_gtsummary_cross_data <- function() {
  n <- 600
  
  data.frame(
    subject_id = paste0("S", sprintf("%04d", 1:n)),
    
    # Primary categorical variables for cross-tabulation
    treatment_response = factor(sample(c("Complete", "Partial", "None"), n, replace = TRUE,
                                      prob = c(0.3, 0.4, 0.3))),
    
    drug_dosage = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE,
                               prob = c(0.35, 0.35, 0.3)),
                        levels = c("Low", "Medium", "High"), ordered = TRUE),
    
    side_effects = factor(sample(c("None", "Mild", "Moderate", "Severe"), n, replace = TRUE,
                                prob = c(0.4, 0.35, 0.2, 0.05)),
                         levels = c("None", "Mild", "Moderate", "Severe"), ordered = TRUE),
    
    compliance = factor(sample(c("Poor", "Good", "Excellent"), n, replace = TRUE,
                              prob = c(0.2, 0.5, 0.3)),
                       levels = c("Poor", "Good", "Excellent"), ordered = TRUE),
    
    # Additional grouping variables
    age_category = factor(sample(c("18-30", "31-50", "51-70", ">70"), n, replace = TRUE,
                                prob = c(0.2, 0.35, 0.35, 0.1)),
                         levels = c("18-30", "31-50", "51-70", ">70"), ordered = TRUE),
    
    gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    
    disease_severity = factor(sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE,
                                    prob = c(0.4, 0.45, 0.15)),
                             levels = c("Mild", "Moderate", "Severe"), ordered = TRUE),
    
    prior_treatment = factor(sample(c("None", "One", "Multiple"), n, replace = TRUE,
                                   prob = c(0.5, 0.35, 0.15))),
    
    geographic_region = factor(sample(c("North", "South", "East", "West"), n, replace = TRUE)),
    
    insurance_type = factor(sample(c("Private", "Medicare", "Medicaid", "Uninsured"), n, replace = TRUE,
                                  prob = c(0.6, 0.2, 0.15, 0.05))),
    
    # Outcome measures
    quality_of_life = sample(1:100, n, replace = TRUE),
    satisfaction_score = sample(1:10, n, replace = TRUE),
    
    # Binary outcomes
    hospitalization = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.8, 0.2))),
    treatment_success = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.35, 0.65))),
    
    stringsAsFactors = FALSE
  )
}

# Generate all datasets
message("Creating gtsummary test datasets...")

# Create datasets
gtsummary_clinical_trial <- create_gtsummary_clinical_trial()
gtsummary_survey_data <- create_gtsummary_survey_data()  
gtsummary_laboratory_data <- create_gtsummary_laboratory_data()
gtsummary_manufacturing_data <- create_gtsummary_manufacturing_data()
gtsummary_cross_data <- create_gtsummary_cross_data()

# Save datasets
save(gtsummary_clinical_trial, file = "data/gtsummary_clinical_trial.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_clinical_trial, "data/gtsummary_clinical_trial.omv")
  message("✓ Created gtsummary_clinical_trial.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_clinical_trial, "data/gtsummary_clinical_trial.omv")
  message("✓ Created gtsummary_clinical_trial.omv")
}
save(gtsummary_survey_data, file = "data/gtsummary_survey_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_survey_data, "data/gtsummary_survey_data.omv")
  message("✓ Created gtsummary_survey_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_survey_data, "data/gtsummary_survey_data.omv")
  message("✓ Created gtsummary_survey_data.omv")
}
save(gtsummary_laboratory_data, file = "data/gtsummary_laboratory_data.rda") 

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_laboratory_data, "data/gtsummary_laboratory_data.omv")
  message("✓ Created gtsummary_laboratory_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_laboratory_data, "data/gtsummary_laboratory_data.omv")
  message("✓ Created gtsummary_laboratory_data.omv")
}
save(gtsummary_manufacturing_data, file = "data/gtsummary_manufacturing_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_manufacturing_data, "data/gtsummary_manufacturing_data.omv")
  message("✓ Created gtsummary_manufacturing_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_manufacturing_data, "data/gtsummary_manufacturing_data.omv")
  message("✓ Created gtsummary_manufacturing_data.omv")
}
save(gtsummary_cross_data, file = "data/gtsummary_cross_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_cross_data, "data/gtsummary_cross_data.omv")
  message("✓ Created gtsummary_cross_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(gtsummary_cross_data, "data/gtsummary_cross_data.omv")
  message("✓ Created gtsummary_cross_data.omv")
}

# Print dataset summaries
cat("\n=== GTSUMMARY TEST DATASETS CREATED ===\n")
cat("1. gtsummary_clinical_trial:", nrow(gtsummary_clinical_trial), "rows,", ncol(gtsummary_clinical_trial), "columns\n")
cat("2. gtsummary_survey_data:", nrow(gtsummary_survey_data), "rows,", ncol(gtsummary_survey_data), "columns\n")
cat("3. gtsummary_laboratory_data:", nrow(gtsummary_laboratory_data), "rows,", ncol(gtsummary_laboratory_data), "columns\n")
cat("4. gtsummary_manufacturing_data:", nrow(gtsummary_manufacturing_data), "rows,", ncol(gtsummary_manufacturing_data), "columns\n")
cat("5. gtsummary_cross_data:", nrow(gtsummary_cross_data), "rows,", ncol(gtsummary_cross_data), "columns\n")

cat("\n=== SUGGESTED TESTING SCENARIOS ===\n")

cat("\n1. Clinical Trial Table 1 (gtsummary_clinical_trial):\n")
cat("   Variables: age, gender, bmi, diabetes, disease_stage\n")
cat("   Group by: treatment_group\n")
cat("   Add: Overall column, p-values, sample sizes\n")

cat("\n2. Survey Demographics (gtsummary_survey_data):\n") 
cat("   Variables: age_group, income_level, education_level\n")
cat("   Group by: region\n")
cat("   Add: Percentages, formatting\n")

cat("\n3. Laboratory Reference Ranges (gtsummary_laboratory_data):\n")
cat("   Variables: glucose, creatinine, cholesterol\n") 
cat("   Group by: department\n")
cat("   Add: Statistical tests, range displays\n")

cat("\n4. Cross-tabulation Analysis (gtsummary_cross_data):\n")
cat("   Row variable: treatment_response\n")
cat("   Column variable: drug_dosage\n")
cat("   Table type: Cross table\n")

cat("\n5. Quality Control Summary (gtsummary_manufacturing_data):\n")
cat("   Variables: temperature, pressure, overall_grade\n")
cat("   Group by: shift\n")
cat("   Add: Process control metrics\n")

cat("\nAll datasets include realistic missing data patterns and appropriate variable types!\n")
