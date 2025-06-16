# Test Datasets for Decision Panel Optimization Module
# Educational and demonstration purposes

# Load required libraries
library(dplyr)
library(forcats)

# Set seed for reproducibility
set.seed(42)

# ============================================================================
# DATASET 1: COVID-19 SCREENING
# ============================================================================

create_covid_data <- function(n = 1000, prevalence = 0.15) {
  # True disease status
  disease <- rbinom(n, 1, prevalence)

  # Rapid Antigen Test (RAT)
  # Sensitivity: 65%, Specificity: 98%
  rat_prob <- ifelse(disease == 1, 0.65, 0.02)
  rat_result <- rbinom(n, 1, rat_prob)

  # PCR Test
  # Sensitivity: 95%, Specificity: 99%
  pcr_prob <- ifelse(disease == 1, 0.95, 0.01)
  pcr_result <- rbinom(n, 1, pcr_prob)

  # Chest CT
  # Sensitivity: 90%, Specificity: 85%
  ct_prob <- ifelse(disease == 1, 0.90, 0.15)
  ct_result <- rbinom(n, 1, ct_prob)

  # Clinical symptoms score (0-10)
  # Higher in disease
  symptoms <- ifelse(disease == 1,
                     pmin(10, round(rnorm(sum(disease == 1), 7, 2))),
                     pmax(0, round(rnorm(sum(disease == 0), 3, 2))))

  # Create dataset
  covid_data <- data.frame(
    patient_id = 1:n,
    rapid_antigen = factor(rat_result, levels = c(0, 1),
                          labels = c("Negative", "Positive")),
    pcr = factor(pcr_result, levels = c(0, 1),
                labels = c("Negative", "Positive")),
    chest_ct = factor(ct_result, levels = c(0, 1),
                     labels = c("Normal", "Abnormal")),
    symptom_score = symptoms,
    covid_status = factor(disease, levels = c(0, 1),
                         labels = c("Negative", "Positive")),
    age = round(rnorm(n, 45, 15)),
    risk_group = factor(sample(c("Low", "Medium", "High"), n,
                              replace = TRUE, prob = c(0.6, 0.3, 0.1)))
  )

  # Add some missing values realistically
  # PCR might be missing if rapid test is negative
  missing_pcr <- which(covid_data$rapid_antigen == "Negative" &
                      runif(n) < 0.3)
  covid_data$pcr[missing_pcr] <- NA

  return(covid_data)
}

# Generate COVID dataset
covid_screening_data <- create_covid_data(n = 1000, prevalence = 0.15)

# ============================================================================
# DATASET 2: BREAST CANCER SCREENING
# ============================================================================

create_breast_cancer_data <- function(n = 2000, prevalence = 0.005) {
  # True disease status (low prevalence for screening)
  disease <- rbinom(n, 1, prevalence)

  # Clinical Breast Exam (CBE)
  # Sensitivity: 54%, Specificity: 94%
  cbe_prob <- ifelse(disease == 1, 0.54, 0.06)
  cbe_result <- rbinom(n, 1, cbe_prob)

  # Mammography
  # Sensitivity: 85%, Specificity: 95%
  mammo_prob <- ifelse(disease == 1, 0.85, 0.05)
  mammo_result <- rbinom(n, 1, mammo_prob)

  # Ultrasound
  # Sensitivity: 80%, Specificity: 90%
  us_prob <- ifelse(disease == 1, 0.80, 0.10)
  us_result <- rbinom(n, 1, us_prob)

  # MRI (for high-risk patients)
  # Sensitivity: 95%, Specificity: 85%
  mri_prob <- ifelse(disease == 1, 0.95, 0.15)
  mri_result <- rbinom(n, 1, mri_prob)

  # Create risk factors
  age <- round(rnorm(n, 55, 10))
  age[age < 40] <- 40
  age[age > 75] <- 75

  family_history <- rbinom(n, 1, 0.15)
  brca_status <- rbinom(n, 1, 0.02)

  # Create dataset
  breast_cancer_data <- data.frame(
    patient_id = 1:n,
    clinical_exam = factor(cbe_result, levels = c(0, 1),
                          labels = c("Normal", "Abnormal")),
    mammography = factor(mammo_result, levels = c(0, 1),
                        labels = c("BIRADS 1-2", "BIRADS 3-5")),
    ultrasound = factor(us_result, levels = c(0, 1),
                       labels = c("Normal", "Suspicious")),
    mri = factor(mri_result, levels = c(0, 1),
                labels = c("Normal", "Suspicious")),
    cancer_status = factor(disease, levels = c(0, 1),
                          labels = c("No Cancer", "Cancer")),
    age = age,
    family_history = factor(family_history, levels = c(0, 1),
                           labels = c("No", "Yes")),
    brca_mutation = factor(brca_status, levels = c(0, 1),
                          labels = c("Negative", "Positive")),
    breast_density = factor(sample(c("A", "B", "C", "D"), n,
                                  replace = TRUE,
                                  prob = c(0.1, 0.4, 0.4, 0.1)))
  )

  # MRI typically only done for high-risk
  low_risk_idx <- which(breast_cancer_data$family_history == "No" &
                       breast_cancer_data$brca_mutation == "Negative")
  breast_cancer_data$mri[sample(low_risk_idx,
                               length(low_risk_idx) * 0.9)] <- NA

  return(breast_cancer_data)
}

# Generate breast cancer dataset
breast_cancer_data <- create_breast_cancer_data(n = 2000, prevalence = 0.005)

# ============================================================================
# DATASET 3: TUBERCULOSIS DIAGNOSIS
# ============================================================================

create_tb_data <- function(n = 1500, prevalence = 0.20) {
  # True disease status (high prevalence in TB clinic)
  disease <- rbinom(n, 1, prevalence)

  # Symptom screening (cough > 2 weeks, fever, weight loss, night sweats)
  # Sensitivity: 80%, Specificity: 60%
  symptom_prob <- ifelse(disease == 1, 0.80, 0.40)
  symptom_result <- rbinom(n, 1, symptom_prob)

  # Sputum smear microscopy
  # Sensitivity: 60%, Specificity: 98%
  smear_prob <- ifelse(disease == 1, 0.60, 0.02)
  smear_result <- rbinom(n, 1, smear_prob)

  # GeneXpert MTB/RIF
  # Sensitivity: 88%, Specificity: 98%
  genexpert_prob <- ifelse(disease == 1, 0.88, 0.02)
  genexpert_result <- rbinom(n, 1, genexpert_prob)

  # Culture (gold standard-ish)
  # Sensitivity: 95%, Specificity: 100%
  culture_prob <- ifelse(disease == 1, 0.95, 0.00)
  culture_result <- rbinom(n, 1, culture_prob)

  # Chest X-ray
  # Sensitivity: 85%, Specificity: 75%
  cxr_prob <- ifelse(disease == 1, 0.85, 0.25)
  cxr_result <- rbinom(n, 1, cxr_prob)

  # HIV status affects presentation
  hiv_status <- rbinom(n, 1, 0.25)

  # Create dataset
  tb_data <- data.frame(
    patient_id = 1:n,
    symptoms = factor(symptom_result, levels = c(0, 1),
                     labels = c("No", "Yes")),
    sputum_smear = factor(smear_result, levels = c(0, 1),
                         labels = c("Negative", "Positive")),
    genexpert = factor(genexpert_result, levels = c(0, 1),
                      labels = c("MTB not detected", "MTB detected")),
    culture = factor(culture_result, levels = c(0, 1),
                    labels = c("Negative", "Positive")),
    chest_xray = factor(cxr_result, levels = c(0, 1),
                       labels = c("Normal", "Abnormal")),
    tb_status = factor(disease, levels = c(0, 1),
                      labels = c("No TB", "TB")),
    hiv_status = factor(hiv_status, levels = c(0, 1),
                       labels = c("Negative", "Positive")),
    age = round(rnorm(n, 35, 15)),
    contact_history = factor(rbinom(n, 1, 0.30), levels = c(0, 1),
                           labels = c("No", "Yes"))
  )

  # Culture takes time, might not be done for all
  no_culture_idx <- which(tb_data$genexpert == "MTB not detected" &
                         tb_data$symptoms == "No" &
                         runif(n) < 0.4)
  tb_data$culture[no_culture_idx] <- NA

  return(tb_data)
}

# Generate TB dataset
tb_diagnosis_data <- create_tb_data(n = 1500, prevalence = 0.20)

# ============================================================================
# DATASET 4: MYOCARDIAL INFARCTION RULE-OUT
# ============================================================================

create_mi_data <- function(n = 800, prevalence = 0.10) {
  # True disease status
  disease <- rbinom(n, 1, prevalence)

  # ECG changes
  # Sensitivity: 55%, Specificity: 95%
  ecg_prob <- ifelse(disease == 1, 0.55, 0.05)
  ecg_result <- rbinom(n, 1, ecg_prob)

  # Initial troponin
  # Sensitivity: 85%, Specificity: 95%
  trop1_prob <- ifelse(disease == 1, 0.85, 0.05)
  trop1_result <- rbinom(n, 1, trop1_prob)

  # 3-hour troponin
  # Sensitivity: 98%, Specificity: 95%
  trop3_prob <- ifelse(disease == 1, 0.98, 0.05)
  trop3_result <- rbinom(n, 1, trop3_prob)

  # Make sure 3-hour is at least as positive as initial
  trop3_result <- pmax(trop1_result, trop3_result)

  # CT Angiography
  # Sensitivity: 95%, Specificity: 90%
  cta_prob <- ifelse(disease == 1, 0.95, 0.10)
  cta_result <- rbinom(n, 1, cta_prob)

  # Clinical risk score components
  age <- round(rnorm(n, 60, 15))
  age[age < 30] <- 30
  age[age > 90] <- 90

  # Create empty character vector to store results
  chest_pain_type <- character(n)

  # Assign chest pain types individually based on disease status
  for (i in seq_len(n)) {
    probs <- if (disease[i] == 1) c(0.6, 0.3, 0.1) else c(0.1, 0.3, 0.6)
    chest_pain_type[i] <- sample(c("Typical", "Atypical", "Non-cardiac"), 1, prob = probs)
  }

  # Create dataset
  mi_data <- data.frame(
    patient_id = 1:n,
    ecg = factor(ecg_result, levels = c(0, 1),
                labels = c("Normal", "Ischemic changes")),
    troponin_initial = factor(trop1_result, levels = c(0, 1),
                             labels = c("Normal", "Elevated")),
    troponin_3hr = factor(trop3_result, levels = c(0, 1),
                         labels = c("Normal", "Elevated")),
    ct_angiography = factor(cta_result, levels = c(0, 1),
                           labels = c("No stenosis", "Significant stenosis")),
    mi_status = factor(disease, levels = c(0, 1),
                      labels = c("No MI", "MI")),
    age = age,
    chest_pain = chest_pain_type,
    diabetes = factor(rbinom(n, 1, 0.25), levels = c(0, 1),
                     labels = c("No", "Yes")),
    smoking = factor(rbinom(n, 1, 0.30), levels = c(0, 1),
                    labels = c("No", "Yes")),
    prior_cad = factor(rbinom(n, 1, 0.20), levels = c(0, 1),
                      labels = c("No", "Yes"))
  )

  # CTA typically only for intermediate risk
  low_risk_idx <- which(mi_data$chest_pain == "Non-cardiac" &
                       mi_data$ecg == "Normal" &
                       mi_data$troponin_initial == "Normal")
  mi_data$ct_angiography[sample(low_risk_idx,
                               length(low_risk_idx) * 0.8)] <- NA

  return(mi_data)
}

# Generate MI dataset
mi_ruleout_data <- create_mi_data(n = 800, prevalence = 0.10)

# ============================================================================
# DATASET 5: THYROID NODULE EVALUATION
# ============================================================================

create_thyroid_data <- function(n = 600, prevalence = 0.05) {
  # True disease status (thyroid cancer)
  disease <- rbinom(n, 1, prevalence)

  # Ultrasound features (TI-RADS)
  # Sensitivity: 90%, Specificity: 70%
  us_prob <- ifelse(disease == 1, 0.90, 0.30)
  us_result <- rbinom(n, 1, us_prob)

  # Fine Needle Aspiration (FNA) cytology
  # Sensitivity: 95%, Specificity: 98%
  fna_prob <- ifelse(disease == 1, 0.95, 0.02)
  fna_result <- rbinom(n, 1, fna_prob)

  # Molecular testing (ThyroSeq/Afirma)
  # Sensitivity: 91%, Specificity: 85%
  molecular_prob <- ifelse(disease == 1, 0.91, 0.15)
  molecular_result <- rbinom(n, 1, molecular_prob)

  # Thyroglobulin levels
  # Sensitivity: 70%, Specificity: 80%
  tg_prob <- ifelse(disease == 1, 0.70, 0.20)
  tg_result <- rbinom(n, 1, tg_prob)

  # Create dataset
  thyroid_data <- data.frame(
    patient_id = 1:n,
    ultrasound = factor(us_result, levels = c(0, 1),
                       labels = c("TI-RADS 1-3", "TI-RADS 4-5")),
    fna_cytology = factor(fna_result, levels = c(0, 1),
                         labels = c("Benign/Indeterminate", "Suspicious/Malignant")),
    molecular_test = factor(molecular_result, levels = c(0, 1),
                           labels = c("Benign", "Suspicious")),
    thyroglobulin = factor(tg_result, levels = c(0, 1),
                          labels = c("Normal", "Elevated")),
    cancer_status = factor(disease, levels = c(0, 1),
                          labels = c("Benign", "Malignant")),
    nodule_size = round(rlnorm(n, log(15), 0.5)),
    age = round(rnorm(n, 50, 15)),
    gender = factor(sample(c("Female", "Male"), n,
                          replace = TRUE, prob = c(0.75, 0.25))),
    radiation_history = factor(rbinom(n, 1, 0.05), levels = c(0, 1),
                             labels = c("No", "Yes"))
  )

  # Molecular testing only for indeterminate FNA
  molecular_not_done <- which(thyroid_data$fna_cytology != "Benign/Indeterminate" |
                             runif(n) < 0.5)
  thyroid_data$molecular_test[molecular_not_done] <- NA

  return(thyroid_data)
}

# Generate thyroid dataset
thyroid_nodule_data <- create_thyroid_data(n = 600, prevalence = 0.05)

# ============================================================================
# SAVE ALL DATASETS
# ============================================================================

# Save as RData file
save(covid_screening_data,
     breast_cancer_data,
     tb_diagnosis_data,
     mi_ruleout_data,
     thyroid_nodule_data,
     file = "./data/decision_panel_test_data.RData")

# Save as CSV files for external use
write.csv(covid_screening_data, "./data/covid_screening_data.csv", row.names = FALSE)
write.csv(breast_cancer_data, "./data/breast_cancer_data.csv", row.names = FALSE)
write.csv(tb_diagnosis_data, "./data/tb_diagnosis_data.csv", row.names = FALSE)
write.csv(mi_ruleout_data, "./data/mi_ruleout_data.csv", row.names = FALSE)
write.csv(thyroid_nodule_data, "./data/thyroid_nodule_data.csv", row.names = FALSE)

# ============================================================================
# DATASET SUMMARIES
# ============================================================================

summarize_dataset <- function(data, disease_col, test_cols) {
  cat("\nDataset Summary:\n")
  cat("Total observations:", nrow(data), "\n")
  cat("Disease prevalence:",
      mean(data[[disease_col]] == levels(data[[disease_col]])[2]), "\n")
  cat("\nTest performance:\n")

  for (test in test_cols) {
    if (any(!is.na(data[[test]]))) {
      tab <- table(data[[test]], data[[disease_col]], useNA = "no")
      if (nrow(tab) == 2 && ncol(tab) == 2) {
        sens <- tab[2,2] / sum(tab[,2])
        spec <- tab[1,1] / sum(tab[,1])
        cat(sprintf("  %s: Sens=%.1f%%, Spec=%.1f%%\n",
                    test, sens*100, spec*100))
      }
    }
  }
}

# Print summaries
cat("=== COVID-19 SCREENING DATA ===")
summarize_dataset(covid_screening_data, "covid_status",
                 c("rapid_antigen", "pcr", "chest_ct"))

cat("\n=== BREAST CANCER SCREENING DATA ===")
summarize_dataset(breast_cancer_data, "cancer_status",
                 c("clinical_exam", "mammography", "ultrasound", "mri"))

cat("\n=== TUBERCULOSIS DIAGNOSIS DATA ===")
summarize_dataset(tb_diagnosis_data, "tb_status",
                 c("symptoms", "sputum_smear", "genexpert", "culture", "chest_xray"))

cat("\n=== MYOCARDIAL INFARCTION DATA ===")
summarize_dataset(mi_ruleout_data, "mi_status",
                 c("ecg", "troponin_initial", "troponin_3hr", "ct_angiography"))

cat("\n=== THYROID NODULE DATA ===")
summarize_dataset(thyroid_nodule_data, "cancer_status",
                 c("ultrasound", "fna_cytology", "molecular_test", "thyroglobulin"))
