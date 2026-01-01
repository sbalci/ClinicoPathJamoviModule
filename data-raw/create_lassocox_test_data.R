# Test data generation for lassocox function
# Creates realistic clinical datasets for Lasso-Cox regression analysis

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)

# Set seed for reproducibility
set.seed(42)

# Dataset 1: Breast Cancer Genomic Study (High-dimensional scenario)
# Simulates a genomic study with many gene expression variables (p > n)
create_breast_cancer_genomic <- function() {
  n <- 120
  p <- 200
  
  # Patient characteristics
  age <- round(rnorm(n, mean = 58, sd = 12))
  age[age < 25] <- 25
  age[age > 85] <- 85
  
  stage <- factor(sample(1:4, n, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
                  labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  grade <- factor(sample(1:3, n, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
                  labels = c("Grade 1", "Grade 2", "Grade 3"))
  
  er_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.7, 0.3)))
  
  her2_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.2, 0.8)))
  
  # Generate gene expression data (standardized)
  gene_data <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(gene_data) <- paste0("GENE_", sprintf("%03d", 1:p))
  
  # Define true prognostic genes (sparse signal)
  true_effects <- rep(0, p)
  prognostic_genes <- c(1, 5, 12, 25, 47, 89, 156, 198)
  true_effects[prognostic_genes] <- c(0.8, -0.6, 0.5, -0.4, 0.7, -0.5, 0.3, -0.8)
  
  # Add clinical variable effects
  clinical_effects <- 0.3 * (as.numeric(stage) - 1) + 
                     0.4 * (as.numeric(grade) - 1) + 
                     0.2 * (as.numeric(er_status) - 1) + 
                     0.1 * (age - 58) / 12
  
  # Generate survival times with gene expression and clinical effects
  linear_pred <- gene_data %*% true_effects + clinical_effects
  survival_times <- rexp(n, rate = exp(linear_pred * 0.5))
  
  # Generate censoring (administrative censoring at 60 months)
  admin_censor_time <- 60
  censoring_times <- runif(n, min = 36, max = admin_censor_time)
  
  # Observed times and events
  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)
  
  # Create final dataset
  data <- data.frame(
    patient_id = paste0("BC_", sprintf("%03d", 1:n)),
    survival_months = round(observed_time, 1),
    death = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    stage = stage,
    grade = grade,
    er_status = er_status,
    her2_status = her2_status,
    gene_data
  )
  
  # Add some missing values to make it realistic
  missing_indices <- sample(nrow(data), size = round(0.05 * nrow(data)))
  data$age[missing_indices[1:3]] <- NA
  
  return(data)
}

# Dataset 2: Lung Cancer Clinical Trial (Traditional clinical variables)
create_lung_cancer_clinical <- function() {
  n <- 200
  
  # Patient demographics
  age <- round(rnorm(n, mean = 65, sd = 10))
  age[age < 35] <- 35
  age[age > 85] <- 85
  
  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)))
  
  smoking_status <- factor(sample(c("Never", "Former", "Current"), n, replace = TRUE, 
                                  prob = c(0.15, 0.45, 0.4)))
  
  # Disease characteristics
  histology <- factor(sample(c("Adenocarcinoma", "Squamous Cell", "Large Cell", "Other"), 
                             n, replace = TRUE, prob = c(0.5, 0.3, 0.1, 0.1)))
  
  stage <- factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE, 
                          prob = c(0.25, 0.25, 0.3, 0.2)))
  
  tumor_size <- round(rnorm(n, mean = 4.2, sd = 2.5), 1)
  tumor_size[tumor_size < 0.5] <- 0.5
  tumor_size[tumor_size > 15] <- 15
  
  # Performance status (ECOG)
  ecog_ps <- factor(sample(0:3, n, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05)))
  
  # Laboratory values
  hemoglobin <- round(rnorm(n, mean = 12.5, sd = 2.1), 1)
  wbc_count <- round(rnorm(n, mean = 8.2, sd = 3.2), 1)
  platelet_count <- round(rnorm(n, mean = 275, sd = 85))
  creatinine <- round(rnorm(n, mean = 1.1, sd = 0.3), 2)
  
  # Treatment variables
  treatment <- factor(sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"), 
                             n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.25)))
  
  # Generate survival outcome based on prognostic factors
  risk_score <- 0.5 * (as.numeric(stage) - 1) + 
                0.3 * (as.numeric(ecog_ps)) + 
                0.2 * (age - 65) / 10 + 
                0.4 * (as.numeric(smoking_status) == 3) + 
                0.3 * (tumor_size - 4.2) / 2.5 + 
                0.2 * (as.numeric(gender) == 1) +
                -0.1 * (hemoglobin - 12.5) / 2.1
  
  # Generate survival times
  survival_times <- rexp(n, rate = exp(risk_score * 0.4))
  
  # Censoring pattern (mix of administrative and loss to follow-up)
  admin_censor <- 48  # 4 years
  loss_followup <- rexp(n, rate = 0.02)  # Random loss to follow-up
  censoring_times <- pmin(admin_censor, loss_followup)
  
  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)
  
  # Create dataset
  data <- data.frame(
    patient_id = paste0("LC_", sprintf("%03d", 1:n)),
    follow_up_months = round(observed_time, 1),
    progression = factor(event, levels = c(0, 1), labels = c("No", "Yes")),
    age = age,
    gender = gender,
    smoking_status = smoking_status,
    histology = histology,
    stage = stage,
    tumor_size_cm = tumor_size,
    ecog_performance_status = ecog_ps,
    hemoglobin_g_dl = hemoglobin,
    wbc_count_k_ul = wbc_count,
    platelet_count_k_ul = platelet_count,
    creatinine_mg_dl = creatinine,
    treatment_type = treatment
  )
  
  # Add realistic missing values
  missing_indices <- sample(nrow(data), size = round(0.08 * nrow(data)))
  data$tumor_size_cm[missing_indices[1:5]] <- NA
  data$hemoglobin_g_dl[missing_indices[6:10]] <- NA
  data$creatinine_mg_dl[missing_indices[11:13]] <- NA
  
  return(data)
}

# Dataset 3: Cardiovascular Study (Mixed continuous and categorical predictors)
create_cardiovascular_study <- function() {
  n <- 150
  
  # Demographics
  age <- round(rnorm(n, mean = 62, sd = 15))
  age[age < 25] <- 25
  age[age > 95] <- 95
  
  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))
  
  race <- factor(sample(c("White", "Black", "Hispanic", "Asian", "Other"), 
                        n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.08, 0.02)))
  
  # Clinical measurements
  bmi <- round(rnorm(n, mean = 28.5, sd = 6.2), 1)
  bmi[bmi < 16] <- 16
  bmi[bmi > 50] <- 50
  
  systolic_bp <- round(rnorm(n, mean = 140, sd = 25))
  diastolic_bp <- round(rnorm(n, mean = 85, sd = 15))
  
  cholesterol_total <- round(rnorm(n, mean = 220, sd = 45))
  hdl_cholesterol <- round(rnorm(n, mean = 45, sd = 12))
  ldl_cholesterol <- round(rnorm(n, mean = 130, sd = 35))
  
  # Medical history
  diabetes <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)))
  hypertension <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.4, 0.6)))
  smoking <- factor(sample(c("Never", "Former", "Current"), n, replace = TRUE, 
                           prob = c(0.5, 0.35, 0.15)))
  family_history <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.6, 0.4)))
  
  # Medications
  ace_inhibitor <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.5, 0.5)))
  statin <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.4, 0.6)))
  aspirin <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.3, 0.7)))
  
  # Generate cardiovascular events based on established risk factors
  risk_score <- 0.6 * (age - 62) / 15 + 
                0.4 * (as.numeric(gender) == 1) + 
                0.3 * (as.numeric(diabetes) == 2) + 
                0.4 * (as.numeric(hypertension) == 2) + 
                0.3 * (as.numeric(smoking) == 3) + 
                0.2 * (as.numeric(family_history) == 2) + 
                0.2 * (bmi - 28.5) / 6.2 + 
                0.1 * (systolic_bp - 140) / 25 + 
                0.1 * (cholesterol_total - 220) / 45 +
                -0.2 * (as.numeric(ace_inhibitor) == 2) +
                -0.1 * (as.numeric(statin) == 2)
  
  # Time to cardiovascular event
  survival_times <- rweibull(n, shape = 1.2, scale = exp(-risk_score * 0.3) * 36)
  
  # Censoring (mix of end of study and loss to follow-up)
  study_end <- 60  # 5 years
  loss_followup <- rexp(n, rate = 0.015)
  censoring_times <- pmin(study_end, loss_followup)
  
  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)
  
  # Create dataset
  data <- data.frame(
    subject_id = paste0("CVD_", sprintf("%03d", 1:n)),
    time_to_event_months = round(observed_time, 1),
    cv_event = factor(event, levels = c(0, 1), labels = c("No Event", "Event")),
    age_years = age,
    gender = gender,
    race_ethnicity = race,
    bmi_kg_m2 = bmi,
    systolic_bp_mmhg = systolic_bp,
    diastolic_bp_mmhg = diastolic_bp,
    total_cholesterol_mg_dl = cholesterol_total,
    hdl_cholesterol_mg_dl = hdl_cholesterol,
    ldl_cholesterol_mg_dl = ldl_cholesterol,
    diabetes_mellitus = diabetes,
    hypertension = hypertension,
    smoking_status = smoking,
    family_history_cvd = family_history,
    ace_inhibitor_use = ace_inhibitor,
    statin_use = statin,
    aspirin_use = aspirin
  )
  
  # Add missing values
  missing_indices <- sample(nrow(data), size = round(0.06 * nrow(data)))
  data$bmi_kg_m2[missing_indices[1:4]] <- NA
  data$hdl_cholesterol_mg_dl[missing_indices[5:7]] <- NA
  data$ldl_cholesterol_mg_dl[missing_indices[8:9]] <- NA
  
  return(data)
}

# Dataset 4: Small sample with high censoring (challenging scenario)
create_small_cohort_study <- function() {
  n <- 75
  
  # Basic demographics
  age <- round(rnorm(n, mean = 55, sd = 12))
  gender <- factor(sample(c("M", "F"), n, replace = TRUE))
  
  # Clinical variables (fewer variables for small sample)
  biomarker_a <- rnorm(n, mean = 0, sd = 1)
  biomarker_b <- rnorm(n, mean = 0, sd = 1)
  biomarker_c <- rnorm(n, mean = 0, sd = 1)
  
  treatment_group <- factor(sample(c("Control", "Treatment"), n, replace = TRUE))
  severity_score <- round(rnorm(n, mean = 50, sd = 15))
  
  # Generate survival with limited events (high censoring)
  risk_score <- 0.4 * biomarker_a + 0.3 * biomarker_b + 
                0.2 * (as.numeric(treatment_group) - 1) + 
                0.1 * (severity_score - 50) / 15
  
  # Generate times with high censoring rate
  survival_times <- rexp(n, rate = exp(risk_score * 0.3))
  
  # Heavy censoring pattern
  admin_censor <- 24  # Short follow-up
  early_dropout <- rexp(n, rate = 0.05)  # High dropout rate
  censoring_times <- pmin(admin_censor, early_dropout)
  
  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)
  
  data <- data.frame(
    id = paste0("SC_", sprintf("%02d", 1:n)),
    time_months = round(observed_time, 1),
    event_occurred = factor(event, levels = c(0, 1), labels = c("No", "Yes")),
    age = age,
    gender = gender,
    biomarker_a = round(biomarker_a, 3),
    biomarker_b = round(biomarker_b, 3),
    biomarker_c = round(biomarker_c, 3),
    treatment_group = treatment_group,
    severity_score = severity_score
  )
  
  return(data)
}

# Generate all datasets
print("Generating test datasets for lassocox function...")

# Generate datasets
breast_cancer_data <- create_breast_cancer_genomic()
lung_cancer_data <- create_lung_cancer_clinical()
cardiovascular_data <- create_cardiovascular_study()
small_cohort_data <- create_small_cohort_study()

# Display summary information
cat("\nDataset 1: Breast Cancer Genomic Study\n")
cat("Dimensions:", nrow(breast_cancer_data), "x", ncol(breast_cancer_data), "\n")
cat("Events:", sum(breast_cancer_data$death == "Dead"), "/", nrow(breast_cancer_data), 
    "(", round(100 * sum(breast_cancer_data$death == "Dead") / nrow(breast_cancer_data), 1), "%)\n")
cat("Median follow-up:", round(median(breast_cancer_data$survival_months), 1), "months\n")

cat("\nDataset 2: Lung Cancer Clinical Trial\n")
cat("Dimensions:", nrow(lung_cancer_data), "x", ncol(lung_cancer_data), "\n")
cat("Events:", sum(lung_cancer_data$progression == "Yes"), "/", nrow(lung_cancer_data), 
    "(", round(100 * sum(lung_cancer_data$progression == "Yes") / nrow(lung_cancer_data), 1), "%)\n")
cat("Median follow-up:", round(median(lung_cancer_data$follow_up_months), 1), "months\n")

cat("\nDataset 3: Cardiovascular Study\n")
cat("Dimensions:", nrow(cardiovascular_data), "x", ncol(cardiovascular_data), "\n")
cat("Events:", sum(cardiovascular_data$cv_event == "Event"), "/", nrow(cardiovascular_data), 
    "(", round(100 * sum(cardiovascular_data$cv_event == "Event") / nrow(cardiovascular_data), 1), "%)\n")
cat("Median follow-up:", round(median(cardiovascular_data$time_to_event_months), 1), "months\n")

cat("\nDataset 4: Small Cohort Study\n")
cat("Dimensions:", nrow(small_cohort_data), "x", ncol(small_cohort_data), "\n")
cat("Events:", sum(small_cohort_data$event_occurred == "Yes"), "/", nrow(small_cohort_data), 
    "(", round(100 * sum(small_cohort_data$event_occurred == "Yes") / nrow(small_cohort_data), 1), "%)\n")
cat("Median follow-up:", round(median(small_cohort_data$time_months), 1), "months\n")

# Save datasets
save(breast_cancer_data, file = "data/lassocox_breast_cancer.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(breast_cancer_data, "data/lassocox_breast_cancer.omv")
  message("✓ Created lassocox_breast_cancer.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(breast_cancer_data, "data/lassocox_breast_cancer.omv")
  message("✓ Created lassocox_breast_cancer.omv")
}
save(lung_cancer_data, file = "data/lassocox_lung_cancer.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(lung_cancer_data, "data/lassocox_lung_cancer.omv")
  message("✓ Created lassocox_lung_cancer.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(lung_cancer_data, "data/lassocox_lung_cancer.omv")
  message("✓ Created lassocox_lung_cancer.omv")
}
save(cardiovascular_data, file = "data/lassocox_cardiovascular.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(cardiovascular_data, "data/lassocox_cardiovascular.omv")
  message("✓ Created lassocox_cardiovascular.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(cardiovascular_data, "data/lassocox_cardiovascular.omv")
  message("✓ Created lassocox_cardiovascular.omv")
}
save(small_cohort_data, file = "data/lassocox_small_cohort.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(small_cohort_data, "data/lassocox_small_cohort.omv")
  message("✓ Created lassocox_small_cohort.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(small_cohort_data, "data/lassocox_small_cohort.omv")
  message("✓ Created lassocox_small_cohort.omv")
}

# Also save as CSV for easy inspection
write.csv(breast_cancer_data, "data-raw/lassocox_breast_cancer.csv", row.names = FALSE)
write.csv(lung_cancer_data, "data-raw/lassocox_lung_cancer.csv", row.names = FALSE)
write.csv(cardiovascular_data, "data-raw/lassocox_cardiovascular.csv", row.names = FALSE)
write.csv(small_cohort_data, "data-raw/lassocox_small_cohort.csv", row.names = FALSE)

cat("\nTest datasets created successfully!\n")
cat("Saved as .rda files in data/ directory and .csv files in data-raw/ directory\n")

# Clinical interpretation guide
cat("\n" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("CLINICAL INTERPRETATION GUIDE FOR LASSOCOX TEST DATA\n")
cat(paste(rep("=", 60), collapse = "") %+% "\n")

cat("\n1. BREAST CANCER GENOMIC STUDY (High-dimensional scenario)\n")
cat("   Purpose: Gene expression profiling for prognosis\n")
cat("   Challenge: p >> n (200 genes, 120 patients)\n")
cat("   Key features: Sparse signal, clinical + genomic variables\n")
cat("   Use case: Identify prognostic gene signature\n")

cat("\n2. LUNG CANCER CLINICAL TRIAL (Traditional clinical variables)\n")
cat("   Purpose: Identify clinical prognostic factors\n")
cat("   Challenge: Mixed variable types, realistic missing data\n")
cat("   Key features: Established prognostic factors, treatment effects\n")
cat("   Use case: Build clinical prognostic model\n")

cat("\n3. CARDIOVASCULAR STUDY (Risk factor analysis)\n")
cat("   Purpose: Cardiovascular risk prediction\n")
cat("   Challenge: Multiple correlated risk factors\n")
cat("   Key features: Known CVD risk factors, medication effects\n")
cat("   Use case: Risk stratification model development\n")

cat("\n4. SMALL COHORT STUDY (Limited sample scenario)\n")
cat("   Purpose: Biomarker validation in small cohort\n")
cat("   Challenge: Small n, high censoring, limited power\n")
cat("   Key features: Few variables, challenging for variable selection\n")
cat("   Use case: Test robustness with minimal data\n")

cat("\nThese datasets provide comprehensive scenarios for testing lassocox:\n")
cat("• High-dimensional genomic data (p >> n)\n")
cat("• Traditional clinical variables with mixed types\n")
cat("• Correlated predictors and established risk factors\n")
cat("• Small sample sizes with high censoring\n")
cat("• Realistic missing data patterns\n")
cat("• Clinically meaningful effect sizes\n")
