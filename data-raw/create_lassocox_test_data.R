# ===============================================================
# Test Data Generation: lassocox (Lasso-Cox Regression)
# ===============================================================
#
# This script generates realistic test data for the lassocox jamovi function.
# It creates 6 clinically realistic datasets covering diverse scenarios:
#
#   1. Breast cancer clinicopathological study (n=250, 20 predictors)
#   2. Lung cancer clinical trial (n=200, 14 predictors)
#   3. Cardiovascular risk factor study (n=150, 18 predictors)
#   4. Small cohort with high censoring (n=75, 8 predictors)
#   5. High-dimensional genomic data (n=80, 50 gene features)
#   6. Multicollinearity scenario (n=180, 12 correlated predictors)
#
# Generated: 2026-03-08
# Seed: 42
# Formats: .rda (data/), .omv + .csv (data-raw/non-rda/)
# ===============================================================

# Load helper for multi-format saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)

set.seed(42)

# -------------------------------------------------------------------
# Dataset 1: Breast Cancer Clinicopathological Study
# Purpose: Identify prognostic factors from standard pathology data
# -------------------------------------------------------------------
create_breast_cancer_clinical <- function() {
  n <- 250

  # Demographics
  age <- round(rnorm(n, mean = 56, sd = 12))
  age <- pmax(28, pmin(age, 85))

  menopausal_status <- factor(ifelse(age >= 50,
    sample(c("Pre", "Post"), n, replace = TRUE, prob = c(0.15, 0.85)),
    sample(c("Pre", "Post"), n, replace = TRUE, prob = c(0.80, 0.20))))

  # Tumor characteristics
  tumor_size_cm <- round(rgamma(n, shape = 2.5, rate = 1.2), 1)
  tumor_size_cm <- pmax(0.3, pmin(tumor_size_cm, 12))

  grade <- factor(sample(1:3, n, replace = TRUE, prob = c(0.20, 0.45, 0.35)),
                  labels = c("Grade 1", "Grade 2", "Grade 3"))

  lymph_nodes_positive <- rpois(n, lambda = 1.8)
  lymph_nodes_positive <- pmin(lymph_nodes_positive, 20)

  lymph_nodes_examined <- lymph_nodes_positive + rpois(n, lambda = 8)
  lymph_nodes_examined <- pmax(lymph_nodes_examined, lymph_nodes_positive + 1)

  stage <- factor(ifelse(tumor_size_cm <= 2 & lymph_nodes_positive == 0, "I",
                  ifelse(tumor_size_cm <= 5 & lymph_nodes_positive <= 3, "II",
                  ifelse(lymph_nodes_positive <= 9, "III", "IV"))),
                  levels = c("I", "II", "III", "IV"))

  # Biomarker panel
  er_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.72, 0.28)))
  pr_status <- factor(ifelse(er_status == "Positive",
    sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.80, 0.20)),
    sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.10, 0.90))))
  her2_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.18, 0.82)))
  ki67_percent <- round(ifelse(grade == "Grade 1", rnorm(n, 8, 4),
                        ifelse(grade == "Grade 2", rnorm(n, 20, 8),
                               rnorm(n, 40, 12))), 1)
  ki67_percent <- pmax(1, pmin(ki67_percent, 95))

  # Histological features
  histology <- factor(sample(c("IDC", "ILC", "Mixed", "Other"), n, replace = TRUE,
                             prob = c(0.70, 0.15, 0.10, 0.05)))
  lvi <- factor(sample(c("Absent", "Present"), n, replace = TRUE,
                       prob = c(0.55, 0.45)))
  margin_status <- factor(sample(c("Negative", "Close", "Positive"), n, replace = TRUE,
                                 prob = c(0.75, 0.15, 0.10)))

  # Treatment
  surgery_type <- factor(sample(c("Lumpectomy", "Mastectomy"), n, replace = TRUE,
                                prob = c(0.55, 0.45)))
  chemotherapy <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.35, 0.65)))
  radiation <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.30, 0.70)))

  # Lab values
  albumin <- round(rnorm(n, 4.0, 0.4), 1)
  albumin <- pmax(2.5, pmin(albumin, 5.2))
  hemoglobin <- round(rnorm(n, 12.8, 1.5), 1)

  # Generate survival with known effects
  risk_score <- 0.35 * (as.numeric(grade) - 1) +
                0.40 * log1p(lymph_nodes_positive) +
                0.25 * (tumor_size_cm - 2) / 2 +
                0.30 * (ki67_percent - 20) / 15 +
                0.20 * (as.numeric(her2_status) == 1) +
               -0.25 * (as.numeric(er_status) == 1) +
                0.15 * (as.numeric(lvi) == 2) +
                0.10 * (age - 56) / 12 +
               -0.10 * (albumin - 4.0) / 0.4

  baseline_hazard <- 0.008
  survival_times <- rweibull(n, shape = 1.1,
                             scale = (1 / baseline_hazard) * exp(-risk_score * 0.4))

  # Censoring: 60 month follow-up with some loss
  admin_censor <- 60
  loss_followup <- rexp(n, rate = 0.012)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time <- pmax(observed_time, 0.1)
  event <- as.numeric(survival_times <= censoring_times)

  data <- data.frame(
    patient_id = paste0("BC_", sprintf("%03d", 1:n)),
    survival_months = observed_time,
    death = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    menopausal_status = menopausal_status,
    tumor_size_cm = tumor_size_cm,
    grade = grade,
    stage = stage,
    lymph_nodes_positive = lymph_nodes_positive,
    lymph_nodes_examined = lymph_nodes_examined,
    er_status = er_status,
    pr_status = pr_status,
    her2_status = her2_status,
    ki67_percent = ki67_percent,
    histology = histology,
    lvi = lvi,
    margin_status = margin_status,
    surgery_type = surgery_type,
    chemotherapy = chemotherapy,
    radiation = radiation,
    albumin = albumin,
    hemoglobin = hemoglobin
  )

  # Realistic missing (~3%)
  set.seed(99)
  data$ki67_percent[sample(n, 5)] <- NA
  data$albumin[sample(n, 3)] <- NA

  return(data)
}


# -------------------------------------------------------------------
# Dataset 2: Lung Cancer Clinical Trial
# Purpose: Traditional clinical variable selection
# -------------------------------------------------------------------
create_lung_cancer_clinical <- function() {
  n <- 200

  age <- round(rnorm(n, mean = 65, sd = 10))
  age[age < 35] <- 35
  age[age > 85] <- 85

  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)))

  smoking_status <- factor(sample(c("Never", "Former", "Current"), n, replace = TRUE,
                                  prob = c(0.15, 0.45, 0.4)))

  histology <- factor(sample(c("Adenocarcinoma", "Squamous Cell", "Large Cell", "Other"),
                             n, replace = TRUE, prob = c(0.5, 0.3, 0.1, 0.1)))

  stage <- factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE,
                          prob = c(0.25, 0.25, 0.3, 0.2)))

  tumor_size <- round(rnorm(n, mean = 4.2, sd = 2.5), 1)
  tumor_size[tumor_size < 0.5] <- 0.5
  tumor_size[tumor_size > 15] <- 15

  ecog_ps <- factor(sample(0:3, n, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05)))

  hemoglobin <- round(rnorm(n, mean = 12.5, sd = 2.1), 1)
  wbc_count <- round(rnorm(n, mean = 8.2, sd = 3.2), 1)
  platelet_count <- round(rnorm(n, mean = 275, sd = 85))
  creatinine <- round(rnorm(n, mean = 1.1, sd = 0.3), 2)

  treatment <- factor(sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"),
                             n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.25)))

  risk_score <- 0.5 * (as.numeric(stage) - 1) +
                0.3 * (as.numeric(ecog_ps)) +
                0.2 * (age - 65) / 10 +
                0.4 * (as.numeric(smoking_status) == 3) +
                0.3 * (tumor_size - 4.2) / 2.5 +
                0.2 * (as.numeric(gender) == 1) +
               -0.1 * (hemoglobin - 12.5) / 2.1

  survival_times <- rexp(n, rate = exp(risk_score * 0.4))

  admin_censor <- 48
  loss_followup <- rexp(n, rate = 0.02)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)

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


# -------------------------------------------------------------------
# Dataset 3: Cardiovascular Risk Factor Study
# Purpose: Correlated risk factors, medication confounders
# -------------------------------------------------------------------
create_cardiovascular_study <- function() {
  n <- 150

  age <- round(rnorm(n, mean = 62, sd = 15))
  age[age < 25] <- 25
  age[age > 95] <- 95

  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))

  race <- factor(sample(c("White", "Black", "Hispanic", "Asian", "Other"),
                        n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.08, 0.02)))

  bmi <- round(rnorm(n, mean = 28.5, sd = 6.2), 1)
  bmi[bmi < 16] <- 16
  bmi[bmi > 50] <- 50

  systolic_bp <- round(rnorm(n, mean = 140, sd = 25))
  diastolic_bp <- round(rnorm(n, mean = 85, sd = 15))

  cholesterol_total <- round(rnorm(n, mean = 220, sd = 45))
  hdl_cholesterol <- round(rnorm(n, mean = 45, sd = 12))
  ldl_cholesterol <- round(rnorm(n, mean = 130, sd = 35))

  diabetes <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)))
  hypertension <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.4, 0.6)))
  smoking <- factor(sample(c("Never", "Former", "Current"), n, replace = TRUE,
                           prob = c(0.5, 0.35, 0.15)))
  family_history <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.6, 0.4)))

  ace_inhibitor <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.5, 0.5)))
  statin <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.4, 0.6)))
  aspirin <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.3, 0.7)))

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

  survival_times <- rweibull(n, shape = 1.2, scale = exp(-risk_score * 0.3) * 36)

  study_end <- 60
  loss_followup <- rexp(n, rate = 0.015)
  censoring_times <- pmin(study_end, loss_followup)

  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)

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

  missing_indices <- sample(nrow(data), size = round(0.06 * nrow(data)))
  data$bmi_kg_m2[missing_indices[1:4]] <- NA
  data$hdl_cholesterol_mg_dl[missing_indices[5:7]] <- NA
  data$ldl_cholesterol_mg_dl[missing_indices[8:9]] <- NA

  return(data)
}


# -------------------------------------------------------------------
# Dataset 4: Small Cohort with High Censoring
# Purpose: Stress-test with limited events and small sample
# -------------------------------------------------------------------
create_small_cohort_study <- function() {
  n <- 75

  age <- round(rnorm(n, mean = 55, sd = 12))
  gender <- factor(sample(c("M", "F"), n, replace = TRUE))

  biomarker_a <- rnorm(n, mean = 0, sd = 1)
  biomarker_b <- rnorm(n, mean = 0, sd = 1)
  biomarker_c <- rnorm(n, mean = 0, sd = 1)

  treatment_group <- factor(sample(c("Control", "Treatment"), n, replace = TRUE))
  severity_score <- round(rnorm(n, mean = 50, sd = 15))

  risk_score <- 0.4 * biomarker_a + 0.3 * biomarker_b +
                0.2 * (as.numeric(treatment_group) - 1) +
                0.1 * (severity_score - 50) / 15

  survival_times <- rexp(n, rate = exp(risk_score * 0.3))

  admin_censor <- 24
  early_dropout <- rexp(n, rate = 0.05)
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


# -------------------------------------------------------------------
# Dataset 5: High-Dimensional Genomic Data (p > n)
# Purpose: Test LASSO variable selection when predictors >> samples
# -------------------------------------------------------------------
create_genomic_highdim <- function() {
  n <- 80
  p <- 50  # 50 gene expression features

  # Base gene expression matrix (correlated blocks)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("gene_", sprintf("%02d", 1:p))

  # Add correlation structure: genes 1-5 are pathway A, 6-10 pathway B
  for (i in 2:5) X[, i] <- X[, 1] * 0.6 + rnorm(n) * 0.8
  for (i in 7:10) X[, i] <- X[, 6] * 0.5 + rnorm(n) * 0.85

  # Sparse true effects: only 6 genes truly prognostic
  true_coef <- rep(0, p)
  true_coef[c(1, 6, 15, 22, 30, 45)] <- c(0.7, -0.5, 0.4, -0.6, 0.3, -0.35)

  linear_pred <- X %*% true_coef

  # Demographics
  age <- round(rnorm(n, mean = 60, sd = 10))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  tumor_stage <- factor(sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.55, 0.45)))

  # Survival driven by genomic risk + clinical factors
  combined_risk <- linear_pred + 0.3 * (as.numeric(tumor_stage) - 1) + 0.1 * (age - 60) / 10
  survival_times <- rexp(n, rate = exp(combined_risk * 0.35))

  admin_censor <- 36
  loss_followup <- rexp(n, rate = 0.025)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)

  # Build data frame
  clinical <- data.frame(
    sample_id = paste0("GEN_", sprintf("%03d", 1:n)),
    os_months = round(observed_time, 2),
    vital_status = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    sex = sex,
    tumor_stage = tumor_stage
  )

  gene_df <- as.data.frame(round(X, 4))

  data <- cbind(clinical, gene_df)
  return(data)
}


# -------------------------------------------------------------------
# Dataset 6: Multicollinearity Scenario
# Purpose: Test LASSO with highly correlated predictors
# -------------------------------------------------------------------
create_multicollinearity_study <- function() {
  n <- 180

  # Create a latent "inflammation" factor and derive correlated biomarkers
  inflammation <- rnorm(n)
  crp <- round(exp(1.5 + 0.8 * inflammation + rnorm(n, 0, 0.3)), 1)  # C-reactive protein
  esr <- round(20 + 15 * inflammation + rnorm(n, 0, 5))                # ESR
  il6 <- round(5 + 4 * inflammation + rnorm(n, 0, 1.5), 1)            # IL-6
  ferritin <- round(150 + 80 * inflammation + rnorm(n, 0, 30))         # Ferritin

  # Create a latent "nutritional status" factor
  nutrition <- rnorm(n)
  albumin <- round(4.0 + 0.3 * nutrition + rnorm(n, 0, 0.15), 1)
  prealbumin <- round(25 + 5 * nutrition + rnorm(n, 0, 2), 1)
  bmi <- round(25 + 4 * nutrition + rnorm(n, 0, 2), 1)
  weight_loss_pct <- round(pmax(0, 5 - 3 * nutrition + rnorm(n, 0, 2)), 1)

  # Clinical
  age <- round(rnorm(n, 65, 11))
  age <- pmax(30, pmin(age, 90))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48)))
  ecog <- factor(sample(0:2, n, replace = TRUE, prob = c(0.45, 0.40, 0.15)))
  comorbidity_index <- rpois(n, lambda = 2)

  # Survival driven by BOTH latent factors + age
  risk_score <- 0.5 * inflammation +
               -0.4 * nutrition +
                0.3 * (age - 65) / 11 +
                0.2 * as.numeric(ecog)

  survival_times <- rweibull(n, shape = 1.15,
                             scale = exp(-risk_score * 0.35) * 30)

  admin_censor <- 48
  loss_followup <- rexp(n, rate = 0.018)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time <- pmax(observed_time, 0.1)
  event <- as.numeric(survival_times <= censoring_times)

  data <- data.frame(
    patient_id = paste0("MC_", sprintf("%03d", 1:n)),
    survival_months = observed_time,
    death = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    sex = sex,
    ecog_ps = ecog,
    comorbidity_index = comorbidity_index,
    crp_mg_l = crp,
    esr_mm_hr = esr,
    il6_pg_ml = il6,
    ferritin_ng_ml = ferritin,
    albumin_g_dl = albumin,
    prealbumin_mg_dl = prealbumin,
    bmi = bmi,
    weight_loss_pct = weight_loss_pct
  )

  # 4% missing
  set.seed(77)
  data$il6_pg_ml[sample(n, 5)] <- NA
  data$prealbumin_mg_dl[sample(n, 3)] <- NA

  return(data)
}


# ===============================================================
# Generate all datasets
# ===============================================================
message("Generating test datasets for lassocox function...")
message(paste(rep("=", 60), collapse = ""))

lassocox_breast_cancer  <- create_breast_cancer_clinical()
lassocox_lung_cancer    <- create_lung_cancer_clinical()
lassocox_cardiovascular <- create_cardiovascular_study()
lassocox_small_cohort   <- create_small_cohort_study()
lassocox_genomic        <- create_genomic_highdim()
lassocox_multicollinear <- create_multicollinearity_study()

# ---------------------------------------------------------------
# Summary statistics
# ---------------------------------------------------------------
datasets <- list(
  "Breast Cancer Clinicopathological" = list(
    data = lassocox_breast_cancer,
    time = "survival_months", event = "death", event_level = "Dead"
  ),
  "Lung Cancer Clinical Trial" = list(
    data = lassocox_lung_cancer,
    time = "follow_up_months", event = "progression", event_level = "Yes"
  ),
  "Cardiovascular Risk Study" = list(
    data = lassocox_cardiovascular,
    time = "time_to_event_months", event = "cv_event", event_level = "Event"
  ),
  "Small Cohort (High Censoring)" = list(
    data = lassocox_small_cohort,
    time = "time_months", event = "event_occurred", event_level = "Yes"
  ),
  "High-Dimensional Genomic" = list(
    data = lassocox_genomic,
    time = "os_months", event = "vital_status", event_level = "Dead"
  ),
  "Multicollinearity Scenario" = list(
    data = lassocox_multicollinear,
    time = "survival_months", event = "death", event_level = "Dead"
  )
)

for (name in names(datasets)) {
  d <- datasets[[name]]
  n_events <- sum(d$data[[d$event]] == d$event_level)
  cat(sprintf("\n%s\n  Dimensions: %d x %d | Events: %d/%d (%.1f%%) | Median FU: %.1f months\n",
              name, nrow(d$data), ncol(d$data),
              n_events, nrow(d$data),
              100 * n_events / nrow(d$data),
              median(d$data[[d$time]])))
}

# ---------------------------------------------------------------
# Save datasets in all formats
# ---------------------------------------------------------------

# .rda files go into data/
save_data_multi_format(lassocox_breast_cancer,  "lassocox_breast_cancer",  save_csv = FALSE)
save_data_multi_format(lassocox_lung_cancer,    "lassocox_lung_cancer",    save_csv = FALSE)
save_data_multi_format(lassocox_cardiovascular, "lassocox_cardiovascular", save_csv = FALSE)
save_data_multi_format(lassocox_small_cohort,   "lassocox_small_cohort",   save_csv = FALSE)
save_data_multi_format(lassocox_genomic,        "lassocox_genomic",        save_csv = FALSE)
save_data_multi_format(lassocox_multicollinear, "lassocox_multicollinear", save_csv = FALSE)

# .csv and .omv files go into data-raw/non-rda/
dir.create("data-raw/non-rda", showWarnings = FALSE, recursive = TRUE)

write.csv(lassocox_breast_cancer,  "data-raw/non-rda/lassocox_breast_cancer.csv",  row.names = FALSE)
write.csv(lassocox_lung_cancer,    "data-raw/non-rda/lassocox_lung_cancer.csv",    row.names = FALSE)
write.csv(lassocox_cardiovascular, "data-raw/non-rda/lassocox_cardiovascular.csv", row.names = FALSE)
write.csv(lassocox_small_cohort,   "data-raw/non-rda/lassocox_small_cohort.csv",   row.names = FALSE)
write.csv(lassocox_genomic,        "data-raw/non-rda/lassocox_genomic.csv",        row.names = FALSE)
write.csv(lassocox_multicollinear, "data-raw/non-rda/lassocox_multicollinear.csv", row.names = FALSE)

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(lassocox_breast_cancer,  "data-raw/non-rda/lassocox_breast_cancer.omv")
  jmvReadWrite::write_omv(lassocox_lung_cancer,    "data-raw/non-rda/lassocox_lung_cancer.omv")
  jmvReadWrite::write_omv(lassocox_cardiovascular, "data-raw/non-rda/lassocox_cardiovascular.omv")
  jmvReadWrite::write_omv(lassocox_small_cohort,   "data-raw/non-rda/lassocox_small_cohort.omv")
  jmvReadWrite::write_omv(lassocox_genomic,        "data-raw/non-rda/lassocox_genomic.omv")
  jmvReadWrite::write_omv(lassocox_multicollinear, "data-raw/non-rda/lassocox_multicollinear.omv")
  message("Created .omv files in data-raw/non-rda/")
}

# ---------------------------------------------------------------
# Clinical interpretation guide
# ---------------------------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("CLINICAL INTERPRETATION GUIDE FOR LASSOCOX TEST DATA\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n1. BREAST CANCER CLINICOPATHOLOGICAL STUDY (n=250)")
cat("\n   Standard pathology variables; tests grade/stage/biomarker selection\n")

cat("\n2. LUNG CANCER CLINICAL TRIAL (n=200)")
cat("\n   Mixed variable types; established prognostic factors with missing data\n")

cat("\n3. CARDIOVASCULAR RISK STUDY (n=150)")
cat("\n   Multiple correlated risk factors and medication confounders\n")

cat("\n4. SMALL COHORT (n=75)")
cat("\n   High censoring rate; tests robustness with limited events\n")

cat("\n5. HIGH-DIMENSIONAL GENOMIC (n=80, p=50)")
cat("\n   Gene expression features with correlated pathway blocks (p > n territory)\n")

cat("\n6. MULTICOLLINEARITY SCENARIO (n=180)")
cat("\n   Inflammation and nutrition biomarkers with strong latent correlations\n")
cat("   Tests LASSO's ability to select one representative per correlated group\n")

cat("\nTest datasets created successfully!\n")
