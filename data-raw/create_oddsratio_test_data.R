#' @title Test Data Generation for oddsratio Function
#' @description Creates comprehensive test datasets for validating the oddsratio function
#' @author ClinicoPath Development Team

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)
library(MASS)  # For mvrnorm
library(forcats)

# Set seed for reproducibility
set.seed(20250709)

cat("=== Creating Comprehensive Test Data for oddsratio Function ===\n")

# =============================================================================
# 1. BASIC CLINICAL DATASET
# =============================================================================

cat("1. Creating basic clinical dataset...\n")

n_patients <- 500

# Create correlated explanatory variables
correlation_matrix <- matrix(c(
  1.0, 0.3, 0.2, 0.1,
  0.3, 1.0, 0.1, 0.2,
  0.2, 0.1, 1.0, 0.3,
  0.1, 0.2, 0.3, 1.0
), nrow = 4)

continuous_vars <- mvrnorm(n_patients, 
                          mu = c(60, 25, 120, 80), 
                          Sigma = correlation_matrix * c(10, 5, 20, 15))

basic_clinical_data <- tibble(
  patient_id = 1:n_patients,
  
  # Continuous variables
  age = pmax(20, pmin(90, continuous_vars[,1])),
  bmi = pmax(15, pmin(45, continuous_vars[,2])),
  systolic_bp = pmax(90, pmin(200, continuous_vars[,3])),
  tumor_size = pmax(0.5, pmin(15, continuous_vars[,4])),
  
  # Categorical variables
  gender = factor(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.45, 0.55))),
  stage = factor(sample(c("I", "II", "III", "IV"), n_patients, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))),
  histology = factor(sample(c("Adenocarcinoma", "Squamous", "Other"), n_patients, replace = TRUE, prob = c(0.6, 0.3, 0.1))),
  treatment = factor(sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"), n_patients, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))),
  
  # Binary variables
  smoking = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.4, 0.6))),
  comorbidity = factor(sample(c("Present", "Absent"), n_patients, replace = TRUE, prob = c(0.3, 0.7))),
  
  # Create outcome with realistic associations
  mortality_risk = 0.05 + # baseline risk
    0.01 * (age - 60) + # age effect
    0.02 * (bmi > 30) + # obesity effect
    0.03 * as.numeric(stage) + # stage effect
    0.04 * (smoking == "Yes") + # smoking effect
    0.02 * (comorbidity == "Present") + # comorbidity effect
    0.01 * (tumor_size - 5) + # tumor size effect
    rnorm(n_patients, 0, 0.05) # random variation
)

# Convert to binary outcome
basic_clinical_data$mortality <- factor(
  ifelse(basic_clinical_data$mortality_risk > runif(n_patients), "Dead", "Alive"),
  levels = c("Alive", "Dead")
)

# Remove intermediate variable
basic_clinical_data <- basic_clinical_data %>% dplyr::select(-mortality_risk)

cat("   - Basic clinical dataset created: n =", nrow(basic_clinical_data), "\n")

# =============================================================================
# 2. EDGE CASES DATASET
# =============================================================================

cat("2. Creating edge cases dataset...\n")

edge_cases_data <- tibble(
  patient_id = 1:200,
  
  # Extreme values
  age = c(rep(20, 50), rep(90, 50), runif(100, 30, 80)),
  
  # Rare events
  rare_mutation = factor(sample(c("Present", "Absent"), 200, replace = TRUE, prob = c(0.05, 0.95))),
  
  # Highly unbalanced categorical
  rare_histology = factor(sample(c("Common", "Rare"), 200, replace = TRUE, prob = c(0.95, 0.05))),
  
  # Missing data patterns
  biomarker_level = c(rep(NA, 20), runif(180, 0, 100)),
  
  # Perfect predictor (for testing)
  perfect_predictor = factor(rep(c("Good", "Bad"), each = 100)),
  
  # Outcome with known associations
  outcome = factor(
    c(rep("Alive", 95), rep("Dead", 5), # 5% mortality for good predictor
      rep("Alive", 20), rep("Dead", 80)), # 80% mortality for bad predictor
    levels = c("Alive", "Dead")
  )
)

cat("   - Edge cases dataset created: n =", nrow(edge_cases_data), "\n")

# =============================================================================
# 3. EFFECT SIZE VALIDATION DATASET
# =============================================================================

cat("3. Creating effect size validation dataset...\n")

effect_size_data <- tibble(
  patient_id = 1:600,
  
  # No effect (OR = 1.0)
  no_effect = factor(sample(c("Exposed", "Unexposed"), 600, replace = TRUE, prob = c(0.5, 0.5))),
  
  # Small effect (OR = 1.5)
  small_effect = factor(sample(c("Exposed", "Unexposed"), 600, replace = TRUE, prob = c(0.4, 0.6))),
  
  # Medium effect (OR = 2.5)
  medium_effect = factor(sample(c("Exposed", "Unexposed"), 600, replace = TRUE, prob = c(0.3, 0.7))),
  
  # Large effect (OR = 5.0)
  large_effect = factor(sample(c("Exposed", "Unexposed"), 600, replace = TRUE, prob = c(0.2, 0.8))),
  
  # Continuous variable with linear effect
  continuous_predictor = rnorm(600, 50, 15)
)

# Create outcomes with known effect sizes
baseline_risk <- 0.3

effect_size_data$no_effect_outcome <- factor(
  ifelse(runif(600) < baseline_risk, "Event", "No Event"),
  levels = c("No Event", "Event")
)

effect_size_data$small_effect_outcome <- factor(
  ifelse(runif(600) < (baseline_risk * ifelse(effect_size_data$small_effect == "Exposed", 1.5, 1.0)), "Event", "No Event"),
  levels = c("No Event", "Event")
)

effect_size_data$medium_effect_outcome <- factor(
  ifelse(runif(600) < (baseline_risk * ifelse(effect_size_data$medium_effect == "Exposed", 2.5, 1.0)), "Event", "No Event"),
  levels = c("No Event", "Event")
)

effect_size_data$large_effect_outcome <- factor(
  ifelse(runif(600) < (baseline_risk * ifelse(effect_size_data$large_effect == "Exposed", 5.0, 1.0)), "Event", "No Event"),
  levels = c("No Event", "Event")
)

cat("   - Effect size validation dataset created: n =", nrow(effect_size_data), "\n")

# =============================================================================
# 4. CARDIOVASCULAR STUDY DATASET
# =============================================================================

cat("4. Creating cardiovascular study dataset...\n")

cardio_data <- tibble(
  patient_id = 1:400,
  
  # Patient characteristics
  age = pmax(30, pmin(85, rnorm(400, 65, 12))),
  sex = factor(sample(c("Male", "Female"), 400, replace = TRUE, prob = c(0.55, 0.45))),
  
  # Risk factors
  hypertension = factor(sample(c("Yes", "No"), 400, replace = TRUE, prob = c(0.45, 0.55))),
  diabetes = factor(sample(c("Yes", "No"), 400, replace = TRUE, prob = c(0.25, 0.75))),
  dyslipidemia = factor(sample(c("Yes", "No"), 400, replace = TRUE, prob = c(0.35, 0.65))),
  family_history = factor(sample(c("Yes", "No"), 400, replace = TRUE, prob = c(0.3, 0.7))),
  
  # Biomarkers
  cholesterol = pmax(120, pmin(350, rnorm(400, 200, 40))),
  hdl = pmax(20, pmin(100, rnorm(400, 45, 15))),
  crp = pmax(0.1, pmin(20, rlnorm(400, 1, 0.8))),
  
  # Lifestyle factors
  smoking_status = factor(sample(c("Current", "Former", "Never"), 400, replace = TRUE, prob = c(0.2, 0.3, 0.5))),
  exercise = factor(sample(c("Regular", "Occasional", "Sedentary"), 400, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
  
  # Treatment
  statin_use = factor(sample(c("Yes", "No"), 400, replace = TRUE, prob = c(0.4, 0.6))),
  
  # Outcome: Cardiovascular events
  cv_event_risk = 0.08 + # baseline risk
    0.008 * (age - 65) + # age effect
    0.05 * (sex == "Male") + # male sex effect
    0.04 * (hypertension == "Yes") + # hypertension effect
    0.03 * (diabetes == "Yes") + # diabetes effect
    0.02 * (dyslipidemia == "Yes") + # dyslipidemia effect
    0.01 * (family_history == "Yes") + # family history effect
    0.0005 * (cholesterol - 200) + # cholesterol effect
    -0.001 * (hdl - 45) + # HDL effect (protective)
    0.005 * log(crp) + # CRP effect
    0.03 * (smoking_status == "Current") + # smoking effect
    0.015 * (smoking_status == "Former") + # former smoking effect
    -0.02 * (exercise == "Regular") + # exercise effect (protective)
    -0.025 * (statin_use == "Yes") + # statin effect (protective)
    rnorm(400, 0, 0.03) # random variation
)

cardio_data$cv_event <- factor(
  ifelse(cardio_data$cv_event_risk > runif(400), "Event", "No Event"),
  levels = c("No Event", "Event")
)

# Remove intermediate variable
cardio_data <- cardio_data %>% dplyr::select(-cv_event_risk)

cat("   - Cardiovascular study dataset created: n =", nrow(cardio_data), "\n")

# =============================================================================
# 5. ONCOLOGY STUDY DATASET
# =============================================================================

cat("5. Creating oncology study dataset...\n")

oncology_data <- tibble(
  patient_id = 1:350,
  
  # Patient demographics
  age = pmax(18, pmin(90, rnorm(350, 62, 14))),
  gender = factor(sample(c("Male", "Female"), 350, replace = TRUE, prob = c(0.48, 0.52))),
  
  # Tumor characteristics
  tumor_grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), 350, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
  tumor_stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 350, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))),
  histological_type = factor(sample(c("Adenocarcinoma", "Squamous Cell", "Other"), 350, replace = TRUE, prob = c(0.65, 0.25, 0.1))),
  
  # Biomarkers
  her2_status = factor(sample(c("Positive", "Negative"), 350, replace = TRUE, prob = c(0.2, 0.8))),
  er_status = factor(sample(c("Positive", "Negative"), 350, replace = TRUE, prob = c(0.75, 0.25))),
  pr_status = factor(sample(c("Positive", "Negative"), 350, replace = TRUE, prob = c(0.65, 0.35))),
  
  # Treatment variables
  surgery_type = factor(sample(c("Lumpectomy", "Mastectomy", "None"), 350, replace = TRUE, prob = c(0.6, 0.35, 0.05))),
  chemotherapy = factor(sample(c("Yes", "No"), 350, replace = TRUE, prob = c(0.7, 0.3))),
  radiation = factor(sample(c("Yes", "No"), 350, replace = TRUE, prob = c(0.6, 0.4))),
  hormone_therapy = factor(sample(c("Yes", "No"), 350, replace = TRUE, prob = c(0.5, 0.5))),
  
  # Recurrence risk calculation
  recurrence_risk = 0.12 + # baseline risk
    0.005 * (age - 62) + # age effect
    0.03 * (gender == "Male") + # gender effect
    0.02 * as.numeric(tumor_grade) + # grade effect
    0.04 * as.numeric(tumor_stage) + # stage effect
    0.05 * (her2_status == "Positive") + # HER2 effect
    -0.03 * (er_status == "Positive") + # ER effect (protective)
    -0.02 * (pr_status == "Positive") + # PR effect (protective)
    -0.04 * (chemotherapy == "Yes") + # chemotherapy effect (protective)
    -0.03 * (radiation == "Yes") + # radiation effect (protective)
    -0.025 * (hormone_therapy == "Yes") + # hormone therapy effect (protective)
    rnorm(350, 0, 0.04) # random variation
)

oncology_data$recurrence <- factor(
  ifelse(oncology_data$recurrence_risk > runif(350), "Recurrence", "No Recurrence"),
  levels = c("No Recurrence", "Recurrence")
)

# Remove intermediate variable
oncology_data <- oncology_data %>% dplyr::select(-recurrence_risk)

cat("   - Oncology study dataset created: n =", nrow(oncology_data), "\n")

# =============================================================================
# SAVE ALL DATASETS
# =============================================================================

cat("6. Saving all datasets...\n")

# Save individual datasets
save(basic_clinical_data, file = "data/basic_clinical_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(basic_clinical_data, "data/basic_clinical_data.omv")
  message("✓ Created basic_clinical_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(basic_clinical_data, "data/basic_clinical_data.omv")
  message("✓ Created basic_clinical_data.omv")
}
save(edge_cases_data, file = "data/edge_cases_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(edge_cases_data, "data/edge_cases_data.omv")
  message("✓ Created edge_cases_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(edge_cases_data, "data/edge_cases_data.omv")
  message("✓ Created edge_cases_data.omv")
}
save(effect_size_data, file = "data/effect_size_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(effect_size_data, "data/effect_size_data.omv")
  message("✓ Created effect_size_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(effect_size_data, "data/effect_size_data.omv")
  message("✓ Created effect_size_data.omv")
}
save(cardio_data, file = "data/cardio_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(cardio_data, "data/cardio_data.omv")
  message("✓ Created cardio_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(cardio_data, "data/cardio_data.omv")
  message("✓ Created cardio_data.omv")
}
save(oncology_data, file = "data/oncology_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(oncology_data, "data/oncology_data.omv")
  message("✓ Created oncology_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(oncology_data, "data/oncology_data.omv")
  message("✓ Created oncology_data.omv")
}

# Create comprehensive test data list
oddsratio_test_data <- list(
  basic_clinical = basic_clinical_data,
  edge_cases = edge_cases_data,
  effect_size = effect_size_data,
  cardiovascular = cardio_data,
  oncology = oncology_data
)

# Save comprehensive test data
save(oddsratio_test_data, file = "data/oddsratio_test_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(oddsratio_test_data, "data/oddsratio_test_data.omv")
  message("✓ Created oddsratio_test_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(oddsratio_test_data, "data/oddsratio_test_data.omv")
  message("✓ Created oddsratio_test_data.omv")
}

cat("   - All datasets saved successfully!\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== Dataset Summary Statistics ===\n")

datasets <- list(
  "Basic Clinical" = basic_clinical_data,
  "Edge Cases" = edge_cases_data,
  "Effect Size" = effect_size_data,
  "Cardiovascular" = cardio_data,
  "Oncology" = oncology_data
)

for(name in names(datasets)) {
  data <- datasets[[name]]
  cat(sprintf("%-15s: n = %d, variables = %d\n", name, nrow(data), ncol(data)))
  
  # Find binary outcome variables
  binary_vars <- sapply(data, function(x) is.factor(x) && nlevels(x) == 2)
  if(any(binary_vars)) {
    binary_var_names <- names(data)[binary_vars]
    for(var_name in binary_var_names) {
      props <- prop.table(table(data[[var_name]]))
      cat(sprintf("                 %s: %s\n", var_name, 
                  paste(names(props), "=", round(props, 3), collapse = ", ")))
    }
  }
  cat("\n")
}

cat("=== Test Data Creation Complete ===\n")
cat("Total datasets created: 5\n")
cat("Total observations: ", sum(sapply(datasets, nrow)), "\n")
cat("Ready for comprehensive testing of oddsratio function!\n")
