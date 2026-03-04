# ═══════════════════════════════════════════════════════════
# Test Data Generation: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the enhancedROC function
# (Clinical ROC Analysis with Diagnostic Metrics and Optimal Cutoffs)
#
# Generated datasets cover:
# 1. Single biomarker ROC analysis
# 2. Comparative ROC analysis (multiple biomarkers)
# 3. Imbalanced data (rare disease)
# 4. Multi-class ROC analysis
# 5. Calibration assessment
# 6. Screening context (high sensitivity)
# 7. Confirmatory testing (high specificity)
# 8. Small dataset for quick testing
#
# Each dataset is saved in 4 formats: RDA, CSV, XLSX, OMV

library(dplyr)
library(here)
library(writexl)

# Set seed for reproducibility
set.seed(42)

# Helper function to generate correlated predictors
generate_correlated_predictors <- function(n, outcome, base_auc = 0.75, n_predictors = 3) {
  # Generate predictors with different AUC levels
  predictors <- matrix(nrow = n, ncol = n_predictors)

  for (i in 1:n_predictors) {
    # Adjust AUC for each predictor
    auc_target <- base_auc + (i - 1) * 0.05

    # Generate predictor based on outcome with controlled AUC
    signal_strength <- qnorm(auc_target) * sqrt(2)

    for (j in 1:n) {
      if (outcome[j] == 1) {
        predictors[j, i] <- rnorm(1, signal_strength, 1)
      } else {
        predictors[j, i] <- rnorm(1, -signal_strength, 1)
      }
    }
  }

  # Add correlation between predictors
  correlation_matrix <- matrix(0.3, nrow = n_predictors, ncol = n_predictors)
  diag(correlation_matrix) <- 1

  as.data.frame(predictors)
}

# ═══════════════════════════════════════════════════════════
# Dataset 1: enhancedroc_biomarker
# ═══════════════════════════════════════════════════════════
# Single biomarker ROC analysis with moderate discrimination
# Use case: Standard biomarker validation
# n = 300, prevalence = 30%

n <- 300
prevalence <- 0.30

enhancedroc_biomarker <- data.frame(
  patient_id = sprintf("BIO-%03d", 1:n),
  age = round(rnorm(n, 55, 12)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  disease_status = rbinom(n, 1, prevalence)
)

# Adjust age range
enhancedroc_biomarker$age <- pmax(25, pmin(enhancedroc_biomarker$age, 85))

# Generate biomarker with good discrimination (AUC ~ 0.80)
enhancedroc_biomarker$biomarker1 <- with(enhancedroc_biomarker, {
  ifelse(disease_status == 1,
         rnorm(n, 25, 8),  # Diseased: higher values
         rnorm(n, 15, 6))  # Healthy: lower values
})

# Ensure no negative biomarker values
enhancedroc_biomarker$biomarker1 <- pmax(0, enhancedroc_biomarker$biomarker1)

# Add second biomarker with moderate discrimination (AUC ~ 0.70)
enhancedroc_biomarker$biomarker2 <- with(enhancedroc_biomarker, {
  ifelse(disease_status == 1,
         rnorm(n, 120, 30),
         rnorm(n, 95, 28))
})

# Add third biomarker with lower discrimination (AUC ~ 0.65)
enhancedroc_biomarker$biomarker3 <- with(enhancedroc_biomarker, {
  ifelse(disease_status == 1,
         rnorm(n, 8.5, 2.5),
         rnorm(n, 7.2, 2.3))
})

# Add clinical risk score (composite)
enhancedroc_biomarker$clinical_risk_score <- with(enhancedroc_biomarker, {
  0.4 * biomarker1 + 0.3 * (biomarker2/10) + 0.3 * biomarker3 +
    rnorm(n, 0, 2)
})

# Convert disease status to factor
enhancedroc_biomarker$disease_status <- factor(
  enhancedroc_biomarker$disease_status,
  levels = c(0, 1),
  labels = c("Healthy", "Disease")
)

# Save in all formats
save(enhancedroc_biomarker, file = here::here("data", "enhancedroc_biomarker.rda"))
write.csv(enhancedroc_biomarker, file = here::here("data", "enhancedroc_biomarker.csv"), row.names = FALSE)
write_xlsx(enhancedroc_biomarker, path = here::here("data", "enhancedroc_biomarker.xlsx"))
jmvReadWrite::write_omv(enhancedroc_biomarker, here::here("data", "enhancedroc_biomarker.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 2: enhancedroc_comparative
# ═══════════════════════════════════════════════════════════
# Multiple biomarkers for comparative ROC analysis
# Use case: Compare diagnostic performance of different tests
# n = 400, prevalence = 25%

n <- 400
prevalence <- 0.25

enhancedroc_comparative <- data.frame(
  patient_id = sprintf("COMP-%03d", 1:n),
  age = round(rnorm(n, 58, 14)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))),
  cancer_status = rbinom(n, 1, prevalence),
  tumor_stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE,
                              prob = c(0.3, 0.3, 0.25, 0.15)),
                       levels = c("I", "II", "III", "IV"))
)

enhancedroc_comparative$age <- pmax(30, pmin(enhancedroc_comparative$age, 85))

# Generate established biomarker (good performance, AUC ~ 0.82)
enhancedroc_comparative$established_marker <- with(enhancedroc_comparative, {
  ifelse(cancer_status == 1,
         rnorm(n, 85, 20),
         rnorm(n, 45, 18))
})

# Generate novel biomarker 1 (better performance, AUC ~ 0.87)
enhancedroc_comparative$novel_marker1 <- with(enhancedroc_comparative, {
  ifelse(cancer_status == 1,
         rnorm(n, 220, 45),
         rnorm(n, 110, 35))
})

# Generate novel biomarker 2 (similar to established, AUC ~ 0.83)
enhancedroc_comparative$novel_marker2 <- with(enhancedroc_comparative, {
  ifelse(cancer_status == 1,
         rnorm(n, 15.5, 4.2),
         rnorm(n, 8.3, 3.5))
})

# Generate imaging biomarker (moderate performance, AUC ~ 0.75)
enhancedroc_comparative$imaging_score <- with(enhancedroc_comparative, {
  ifelse(cancer_status == 1,
         rnorm(n, 72, 18),
         rnorm(n, 52, 16))
})

# Generate genetic risk score (excellent performance, AUC ~ 0.90)
enhancedroc_comparative$genetic_risk_score <- with(enhancedroc_comparative, {
  ifelse(cancer_status == 1,
         rnorm(n, 0.75, 0.15),
         rnorm(n, 0.35, 0.12))
})

# Ensure biomarkers are non-negative
enhancedroc_comparative$established_marker <- pmax(0, enhancedroc_comparative$established_marker)
enhancedroc_comparative$novel_marker1 <- pmax(0, enhancedroc_comparative$novel_marker1)
enhancedroc_comparative$novel_marker2 <- pmax(0, enhancedroc_comparative$novel_marker2)
enhancedroc_comparative$imaging_score <- pmax(0, pmin(100, enhancedroc_comparative$imaging_score))
enhancedroc_comparative$genetic_risk_score <- pmax(0, pmin(1, enhancedroc_comparative$genetic_risk_score))

# Convert cancer status to factor
enhancedroc_comparative$cancer_status <- factor(
  enhancedroc_comparative$cancer_status,
  levels = c(0, 1),
  labels = c("No Cancer", "Cancer")
)

# Save in all formats
save(enhancedroc_comparative, file = here::here("data", "enhancedroc_comparative.rda"))
write.csv(enhancedroc_comparative, file = here::here("data", "enhancedroc_comparative.csv"), row.names = FALSE)
write_xlsx(enhancedroc_comparative, path = here::here("data", "enhancedroc_comparative.xlsx"))
jmvReadWrite::write_omv(enhancedroc_comparative, here::here("data", "enhancedroc_comparative.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 3: enhancedroc_imbalanced
# ═══════════════════════════════════════════════════════════
# Imbalanced data (rare disease) - test class imbalance detection
# Use case: Rare disease biomarker validation
# n = 500, prevalence = 5%

n <- 500
prevalence <- 0.05  # Rare disease (5%)

enhancedroc_imbalanced <- data.frame(
  patient_id = sprintf("RARE-%03d", 1:n),
  age = round(rnorm(n, 45, 15)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  rare_disease = rbinom(n, 1, prevalence),
  risk_factor = factor(sample(c("Low", "Moderate", "High"), n, replace = TRUE,
                              prob = c(0.6, 0.3, 0.1)),
                       levels = c("Low", "Moderate", "High"))
)

enhancedroc_imbalanced$age <- pmax(18, pmin(enhancedroc_imbalanced$age, 75))

# Generate screening biomarker with excellent sensitivity (AUC ~ 0.88)
enhancedroc_imbalanced$screening_marker <- with(enhancedroc_imbalanced, {
  ifelse(rare_disease == 1,
         rnorm(n, 145, 25),  # Wide separation for high sensitivity
         rnorm(n, 75, 22))
})

# Generate confirmatory marker with excellent specificity (AUC ~ 0.85)
enhancedroc_imbalanced$confirmatory_marker <- with(enhancedroc_imbalanced, {
  ifelse(rare_disease == 1,
         rnorm(n, 28, 6),
         rnorm(n, 15, 4))  # Tighter distribution in non-diseased
})

# Generate combined risk score
enhancedroc_imbalanced$combined_risk <- with(enhancedroc_imbalanced, {
  0.6 * (screening_marker/10) + 0.4 * confirmatory_marker + rnorm(n, 0, 3)
})

# Ensure non-negative values
enhancedroc_imbalanced$screening_marker <- pmax(0, enhancedroc_imbalanced$screening_marker)
enhancedroc_imbalanced$confirmatory_marker <- pmax(0, enhancedroc_imbalanced$confirmatory_marker)

# Convert disease status to factor
enhancedroc_imbalanced$rare_disease <- factor(
  enhancedroc_imbalanced$rare_disease,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# Save in all formats
save(enhancedroc_imbalanced, file = here::here("data", "enhancedroc_imbalanced.rda"))
write.csv(enhancedroc_imbalanced, file = here::here("data", "enhancedroc_imbalanced.csv"), row.names = FALSE)
write_xlsx(enhancedroc_imbalanced, path = here::here("data", "enhancedroc_imbalanced.xlsx"))
jmvReadWrite::write_omv(enhancedroc_imbalanced, here::here("data", "enhancedroc_imbalanced.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 4: enhancedroc_multiclass
# ═══════════════════════════════════════════════════════════
# Multi-class outcome for multi-class ROC analysis
# Use case: Disease severity classification (Normal, Mild, Moderate, Severe)
# n = 350

n <- 350

enhancedroc_multiclass <- data.frame(
  patient_id = sprintf("MULTI-%03d", 1:n),
  age = round(rnorm(n, 52, 16)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE))
)

enhancedroc_multiclass$age <- pmax(20, pmin(enhancedroc_multiclass$age, 80))

# Generate disease severity (4 levels)
enhancedroc_multiclass$disease_severity <- sample(
  c("Normal", "Mild", "Moderate", "Severe"),
  n, replace = TRUE,
  prob = c(0.35, 0.30, 0.25, 0.10)
)

# Convert to ordered factor
enhancedroc_multiclass$disease_severity <- factor(
  enhancedroc_multiclass$disease_severity,
  levels = c("Normal", "Mild", "Moderate", "Severe"),
  ordered = TRUE
)

# Generate biomarker that increases with severity
enhancedroc_multiclass$biomarker_A <- with(enhancedroc_multiclass, {
  case_when(
    disease_severity == "Normal" ~ rnorm(n, 45, 12),
    disease_severity == "Mild" ~ rnorm(n, 65, 14),
    disease_severity == "Moderate" ~ rnorm(n, 95, 16),
    disease_severity == "Severe" ~ rnorm(n, 135, 20)
  )
})

# Generate second biomarker with different pattern
enhancedroc_multiclass$biomarker_B <- with(enhancedroc_multiclass, {
  case_when(
    disease_severity == "Normal" ~ rnorm(n, 12, 4),
    disease_severity == "Mild" ~ rnorm(n, 18, 5),
    disease_severity == "Moderate" ~ rnorm(n, 28, 6),
    disease_severity == "Severe" ~ rnorm(n, 42, 8)
  )
})

# Generate imaging score
enhancedroc_multiclass$imaging_severity_score <- with(enhancedroc_multiclass, {
  case_when(
    disease_severity == "Normal" ~ rnorm(n, 15, 8),
    disease_severity == "Mild" ~ rnorm(n, 35, 10),
    disease_severity == "Moderate" ~ rnorm(n, 62, 12),
    disease_severity == "Severe" ~ rnorm(n, 88, 14)
  )
})

# Ensure non-negative and bounded values
enhancedroc_multiclass$biomarker_A <- pmax(0, enhancedroc_multiclass$biomarker_A)
enhancedroc_multiclass$biomarker_B <- pmax(0, enhancedroc_multiclass$biomarker_B)
enhancedroc_multiclass$imaging_severity_score <- pmax(0, pmin(100, enhancedroc_multiclass$imaging_severity_score))

# Save in all formats
save(enhancedroc_multiclass, file = here::here("data", "enhancedroc_multiclass.rda"))
write.csv(enhancedroc_multiclass, file = here::here("data", "enhancedroc_multiclass.csv"), row.names = FALSE)
write_xlsx(enhancedroc_multiclass, path = here::here("data", "enhancedroc_multiclass.xlsx"))
jmvReadWrite::write_omv(enhancedroc_multiclass, here::here("data", "enhancedroc_multiclass.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 5: enhancedroc_calibration
# ═══════════════════════════════════════════════════════════
# Data for calibration assessment with predicted probabilities
# Use case: Clinical prediction model validation
# n = 300

n <- 300
prevalence <- 0.35

enhancedroc_calibration <- data.frame(
  patient_id = sprintf("CAL-%03d", 1:n),
  age = round(rnorm(n, 60, 13)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48))),
  outcome = rbinom(n, 1, prevalence),
  comorbidity_count = sample(0:5, n, replace = TRUE, prob = c(0.2, 0.3, 0.25, 0.15, 0.07, 0.03))
)

enhancedroc_calibration$age <- pmax(25, pmin(enhancedroc_calibration$age, 90))

# Generate predictors
enhancedroc_calibration$predictor1 <- with(enhancedroc_calibration, {
  ifelse(outcome == 1,
         rnorm(n, 18, 5),
         rnorm(n, 12, 4.5))
})

enhancedroc_calibration$predictor2 <- with(enhancedroc_calibration, {
  ifelse(outcome == 1,
         rnorm(n, 135, 35),
         rnorm(n, 95, 30))
})

# Generate predicted probabilities (for calibration plot)
# Add miscalibration by design
enhancedroc_calibration$predicted_prob <- with(enhancedroc_calibration, {
  # Logistic-like transformation
  logit <- -3 + 0.08 * predictor1 + 0.015 * predictor2 +
           0.02 * age + 0.2 * comorbidity_count

  # Add calibration slope different from 1 (mild miscalibration)
  logit_miscalibrated <- 0.5 + 0.8 * logit  # Calibration slope = 0.8, intercept = 0.5

  prob <- 1 / (1 + exp(-logit_miscalibrated))
  pmax(0.01, pmin(0.99, prob))  # Bound probabilities
})

# Generate linear predictor score
enhancedroc_calibration$risk_score <- with(enhancedroc_calibration, {
  0.4 * predictor1 + 0.3 * (predictor2/10) + 0.2 * (age/10) +
    0.1 * comorbidity_count + rnorm(n, 0, 2)
})

# Ensure non-negative predictors
enhancedroc_calibration$predictor1 <- pmax(0, enhancedroc_calibration$predictor1)
enhancedroc_calibration$predictor2 <- pmax(0, enhancedroc_calibration$predictor2)

# Convert outcome to factor
enhancedroc_calibration$outcome <- factor(
  enhancedroc_calibration$outcome,
  levels = c(0, 1),
  labels = c("No Event", "Event")
)

# Save in all formats
save(enhancedroc_calibration, file = here::here("data", "enhancedroc_calibration.rda"))
write.csv(enhancedroc_calibration, file = here::here("data", "enhancedroc_calibration.csv"), row.names = FALSE)
write_xlsx(enhancedroc_calibration, path = here::here("data", "enhancedroc_calibration.xlsx"))
jmvReadWrite::write_omv(enhancedroc_calibration, here::here("data", "enhancedroc_calibration.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 6: enhancedroc_screening
# ═══════════════════════════════════════════════════════════
# Screening context - need high sensitivity
# Use case: Population screening for early disease detection
# n = 600, prevalence = 8%

n <- 600
prevalence <- 0.08

enhancedroc_screening <- data.frame(
  patient_id = sprintf("SCR-%03d", 1:n),
  age = round(rnorm(n, 50, 12)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  screening_indication = rbinom(n, 1, prevalence),
  family_history = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.85, 0.15)))
)

enhancedroc_screening$age <- pmax(40, pmin(enhancedroc_screening$age, 75))

# Generate sensitive screening marker (high sensitivity, moderate specificity)
# AUC ~ 0.85, but optimized for sensitivity
enhancedroc_screening$sensitive_marker <- with(enhancedroc_screening, {
  ifelse(screening_indication == 1,
         rnorm(n, 92, 18),  # Diseased: higher mean
         rnorm(n, 55, 20))  # Non-diseased: wider distribution (lower specificity)
})

# Generate quantitative imaging marker
enhancedroc_screening$imaging_marker <- with(enhancedroc_screening, {
  ifelse(screening_indication == 1,
         rnorm(n, 68, 14),
         rnorm(n, 42, 16))
})

# Generate panel score (combination)
enhancedroc_screening$panel_score <- with(enhancedroc_screening, {
  0.7 * (sensitive_marker/10) + 0.3 * (imaging_marker/10) + rnorm(n, 0, 1.5)
})

# Ensure non-negative values
enhancedroc_screening$sensitive_marker <- pmax(0, enhancedroc_screening$sensitive_marker)
enhancedroc_screening$imaging_marker <- pmax(0, pmin(100, enhancedroc_screening$imaging_marker))

# Convert indication to factor
enhancedroc_screening$screening_indication <- factor(
  enhancedroc_screening$screening_indication,
  levels = c(0, 1),
  labels = c("Screen Negative", "Screen Positive")
)

# Save in all formats
save(enhancedroc_screening, file = here::here("data", "enhancedroc_screening.rda"))
write.csv(enhancedroc_screening, file = here::here("data", "enhancedroc_screening.csv"), row.names = FALSE)
write_xlsx(enhancedroc_screening, path = here::here("data", "enhancedroc_screening.xlsx"))
jmvReadWrite::write_omv(enhancedroc_screening, here::here("data", "enhancedroc_screening.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 7: enhancedroc_confirmatory
# ═══════════════════════════════════════════════════════════
# Confirmatory testing - need high specificity
# Use case: Confirmatory diagnosis after positive screening
# n = 250, prevalence = 45%

n <- 250
prevalence <- 0.45  # Enriched sample (post-screening)

enhancedroc_confirmatory <- data.frame(
  patient_id = sprintf("CONF-%03d", 1:n),
  age = round(rnorm(n, 58, 11)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))),
  confirmed_diagnosis = rbinom(n, 1, prevalence),
  clinical_suspicion = factor(sample(c("Low", "Moderate", "High"), n, replace = TRUE,
                                     prob = c(0.2, 0.4, 0.4)),
                              levels = c("Low", "Moderate", "High"))
)

enhancedroc_confirmatory$age <- pmax(35, pmin(enhancedroc_confirmatory$age, 85))

# Generate specific confirmatory marker (moderate sensitivity, high specificity)
# AUC ~ 0.83, but optimized for specificity
enhancedroc_confirmatory$specific_marker <- with(enhancedroc_confirmatory, {
  ifelse(confirmed_diagnosis == 1,
         rnorm(n, 155, 40),  # Diseased: wider distribution (lower sensitivity)
         rnorm(n, 65, 22))   # Non-diseased: tight distribution (high specificity)
})

# Generate pathology score
enhancedroc_confirmatory$pathology_score <- with(enhancedroc_confirmatory, {
  ifelse(confirmed_diagnosis == 1,
         rnorm(n, 7.8, 1.8),
         rnorm(n, 4.2, 1.2))
})

# Generate histological grade (0-10 scale)
enhancedroc_confirmatory$histological_grade <- with(enhancedroc_confirmatory, {
  ifelse(confirmed_diagnosis == 1,
         pmin(10, pmax(0, round(rnorm(n, 6.5, 2.2)))),
         pmin(10, pmax(0, round(rnorm(n, 2.8, 1.5)))))
})

# Generate molecular signature score
enhancedroc_confirmatory$molecular_signature <- with(enhancedroc_confirmatory, {
  ifelse(confirmed_diagnosis == 1,
         rnorm(n, 0.72, 0.16),
         rnorm(n, 0.28, 0.12))
})

# Ensure values in valid ranges
enhancedroc_confirmatory$specific_marker <- pmax(0, enhancedroc_confirmatory$specific_marker)
enhancedroc_confirmatory$pathology_score <- pmax(0, pmin(10, enhancedroc_confirmatory$pathology_score))
enhancedroc_confirmatory$molecular_signature <- pmax(0, pmin(1, enhancedroc_confirmatory$molecular_signature))

# Convert diagnosis to factor
enhancedroc_confirmatory$confirmed_diagnosis <- factor(
  enhancedroc_confirmatory$confirmed_diagnosis,
  levels = c(0, 1),
  labels = c("Not Confirmed", "Confirmed")
)

# Save in all formats
save(enhancedroc_confirmatory, file = here::here("data", "enhancedroc_confirmatory.rda"))
write.csv(enhancedroc_confirmatory, file = here::here("data", "enhancedroc_confirmatory.csv"), row.names = FALSE)
write_xlsx(enhancedroc_confirmatory, path = here::here("data", "enhancedroc_confirmatory.xlsx"))
jmvReadWrite::write_omv(enhancedroc_confirmatory, here::here("data", "enhancedroc_confirmatory.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 8: enhancedroc_small
# ═══════════════════════════════════════════════════════════
# Small dataset for quick testing
# Use case: Fast unit tests
# n = 60

n <- 60
prevalence <- 0.30

enhancedroc_small <- data.frame(
  patient_id = sprintf("SM-%02d", 1:n),
  age = round(runif(n, 30, 75)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  disease = rbinom(n, 1, prevalence)
)

# Generate simple biomarker
enhancedroc_small$marker <- with(enhancedroc_small, {
  ifelse(disease == 1,
         rnorm(n, 75, 20),
         rnorm(n, 45, 18))
})

enhancedroc_small$marker <- pmax(0, enhancedroc_small$marker)

# Convert disease to factor
enhancedroc_small$disease <- factor(
  enhancedroc_small$disease,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# Save in all formats
save(enhancedroc_small, file = here::here("data", "enhancedroc_small.rda"))
write.csv(enhancedroc_small, file = here::here("data", "enhancedroc_small.csv"), row.names = FALSE)
write_xlsx(enhancedroc_small, path = here::here("data", "enhancedroc_small.xlsx"))
jmvReadWrite::write_omv(enhancedroc_small, here::here("data", "enhancedroc_small.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 9: enhancedroc_validation
# ═══════════════════════════════════════════════════════════
# Data for internal validation, CROC, convex hull, and
# probability-scale predictors for advanced calibration
# Use case: Prediction model validation & advanced ROC methods
# n = 350

n <- 350
prevalence <- 0.30

enhancedroc_validation <- data.frame(
  patient_id = sprintf("VAL-%03d", 1:n),
  age = round(rnorm(n, 62, 11)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))),
  outcome = rbinom(n, 1, prevalence)
)

enhancedroc_validation$age <- pmax(30, pmin(enhancedroc_validation$age, 85))

# Well-calibrated probability predictor (AUC ~ 0.80)
enhancedroc_validation$model_prob <- with(enhancedroc_validation, {
  logit <- -2 + 0.04 * age + 1.5 * outcome + rnorm(n, 0, 0.8)
  prob <- 1 / (1 + exp(-logit))
  pmax(0.01, pmin(0.99, prob))
})

# Poorly-calibrated probability predictor (miscalibrated)
enhancedroc_validation$miscalibrated_prob <- with(enhancedroc_validation, {
  logit <- -1 + 0.03 * age + 2.0 * outcome + rnorm(n, 0, 1.0)
  prob <- 1 / (1 + exp(-0.4 * logit))  # Flattened calibration
  pmax(0.01, pmin(0.99, prob))
})

# Continuous biomarker (for CROC / convex hull)
enhancedroc_validation$biomarker <- with(enhancedroc_validation, {
  ifelse(outcome == 1,
         rnorm(n, 80, 18),
         rnorm(n, 50, 15))
})
enhancedroc_validation$biomarker <- pmax(0, enhancedroc_validation$biomarker)

# Clinical risk score (composite)
enhancedroc_validation$risk_score <- with(enhancedroc_validation, {
  0.5 * (biomarker / 10) + 0.3 * model_prob * 100 + 0.2 * (age / 10) + rnorm(n, 0, 2)
})

# Convert outcome to factor
enhancedroc_validation$outcome <- factor(
  enhancedroc_validation$outcome,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# Save in all formats
save(enhancedroc_validation, file = here::here("data", "enhancedroc_validation.rda"))
write.csv(enhancedroc_validation, file = here::here("data", "enhancedroc_validation.csv"), row.names = FALSE)
write_xlsx(enhancedroc_validation, path = here::here("data", "enhancedroc_validation.xlsx"))
jmvReadWrite::write_omv(enhancedroc_validation, here::here("data", "enhancedroc_validation.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 10: enhancedroc_tiedscores
# ═══════════════════════════════════════════════════════════
# Data with many tied predictor values for tied score handling tests
# Use case: Ordinal scores, Likert scales, rounded lab values
# n = 200

n <- 200
prevalence <- 0.35

enhancedroc_tiedscores <- data.frame(
  patient_id = sprintf("TIED-%03d", 1:n),
  disease = rbinom(n, 1, prevalence)
)

# Ordinal score (1-10 with many ties)
enhancedroc_tiedscores$ordinal_score <- with(enhancedroc_tiedscores, {
  ifelse(disease == 1,
         sample(1:10, n, replace = TRUE, prob = c(0.02, 0.03, 0.05, 0.08, 0.10, 0.12, 0.15, 0.18, 0.15, 0.12)),
         sample(1:10, n, replace = TRUE, prob = c(0.15, 0.18, 0.15, 0.12, 0.12, 0.10, 0.08, 0.05, 0.03, 0.02)))
})

# Rounded lab value (many ties from rounding)
enhancedroc_tiedscores$rounded_lab <- with(enhancedroc_tiedscores, {
  raw <- ifelse(disease == 1, rnorm(n, 8.5, 2.0), rnorm(n, 5.5, 1.8))
  round(raw)  # Integer rounding creates ties
})

# Likert-type composite (summed ordinal items, limited range)
enhancedroc_tiedscores$composite_score <- with(enhancedroc_tiedscores, {
  item1 <- ifelse(disease == 1, sample(1:5, n, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.30, 0.35)),
                                sample(1:5, n, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.10, 0.05)))
  item2 <- ifelse(disease == 1, sample(1:5, n, replace = TRUE, prob = c(0.08, 0.12, 0.20, 0.28, 0.32)),
                                sample(1:5, n, replace = TRUE, prob = c(0.32, 0.28, 0.20, 0.12, 0.08)))
  item1 + item2  # Range 2-10
})

# Convert disease to factor
enhancedroc_tiedscores$disease <- factor(
  enhancedroc_tiedscores$disease,
  levels = c(0, 1),
  labels = c("No Disease", "Disease")
)

# Save in all formats
save(enhancedroc_tiedscores, file = here::here("data", "enhancedroc_tiedscores.rda"))
write.csv(enhancedroc_tiedscores, file = here::here("data", "enhancedroc_tiedscores.csv"), row.names = FALSE)
write_xlsx(enhancedroc_tiedscores, path = here::here("data", "enhancedroc_tiedscores.xlsx"))
jmvReadWrite::write_omv(enhancedroc_tiedscores, here::here("data", "enhancedroc_tiedscores.omv"))

# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete: enhancedROC\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("Generated 10 datasets with 4 formats each (40 files total):\n")
cat("\n")
cat(" 1. enhancedroc_biomarker      (n=300) - Single biomarker ROC (AUC ~0.80)\n")
cat(" 2. enhancedroc_comparative    (n=400) - Multiple biomarkers for comparison\n")
cat(" 3. enhancedroc_imbalanced     (n=500) - Rare disease (5% prevalence)\n")
cat(" 4. enhancedroc_multiclass     (n=350) - Multi-class severity (4 levels)\n")
cat(" 5. enhancedroc_calibration    (n=300) - Calibration assessment data\n")
cat(" 6. enhancedroc_screening      (n=600) - Screening context (high sensitivity)\n")
cat(" 7. enhancedroc_confirmatory   (n=250) - Confirmatory testing (high specificity)\n")
cat(" 8. enhancedroc_small          (n=60)  - Quick testing dataset\n")
cat(" 9. enhancedroc_validation     (n=350) - Validation / CROC / convex hull\n")
cat("10. enhancedroc_tiedscores     (n=200) - Tied scores for handling methods\n")
cat("\n")
cat("Formats: RDA, CSV, XLSX, OMV\n")
cat("Total patients across all datasets: 3,310\n")
cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
