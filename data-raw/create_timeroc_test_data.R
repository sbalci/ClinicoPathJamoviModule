#!/usr/bin/env Rscript
# =============================================================================
# Comprehensive Test Data Generation for TimeROC Function
# =============================================================================
# 
# This script generates multiple test datasets for the timeroc function
# covering various clinical scenarios, biomarker types, and edge cases
#
# Author: ClinicoPath Development Team
# Date: 2024
# =============================================================================

# Load required libraries
library(dplyr)
library(survival)

# Set seed for reproducibility
set.seed(54321)

# =============================================================================
# Dataset 1: Basic Cancer Biomarker Study
# =============================================================================

create_cancer_biomarker_data <- function(n = 300) {
  # Simulate a cancer biomarker study with time-dependent outcomes
  
  # Generate patient demographics
  age <- round(rnorm(n, 65, 12))
  age <- pmax(30, pmin(90, age))  # Realistic age range
  
  sex <- sample(c("Male", "Female"), n, replace = TRUE)
  stage <- sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
  
  # Generate biomarker values (e.g., tumor marker, genetic score)
  # Higher values associated with worse prognosis
  biomarker_base <- numeric(n)
  biomarker_base[stage == "I"] <- rnorm(sum(stage == "I"), 2, 0.8)
  biomarker_base[stage == "II"] <- rnorm(sum(stage == "II"), 3, 0.9)
  biomarker_base[stage == "III"] <- rnorm(sum(stage == "III"), 4.5, 1.1)
  biomarker_base[stage == "IV"] <- rnorm(sum(stage == "IV"), 6, 1.3)
  
  # Add some noise and ensure positive values
  biomarker <- pmax(0.1, biomarker_base + rnorm(n, 0, 0.3))
  
  # Generate survival times based on biomarker and other factors
  # Higher biomarker = higher hazard = shorter survival
  hazard_ratio_biomarker <- exp(0.3 * scale(biomarker)[,1])
  hazard_ratio_age <- exp(0.02 * (age - 65))
  hazard_ratio_stage <- case_when(
    stage == "I" ~ 1.0,
    stage == "II" ~ 1.5,
    stage == "III" ~ 2.5,
    stage == "IV" ~ 4.0,
    TRUE ~ 1.0
  )
  
  # Combined hazard
  lambda <- 0.05 * hazard_ratio_biomarker * hazard_ratio_age * hazard_ratio_stage
  
  # Generate survival times (exponential distribution)
  survival_months <- round(rexp(n, lambda), 1)
  
  # Generate censoring (administrative censoring at 60 months + random dropout)
  admin_censor <- 60
  dropout_months <- rexp(n, 0.02)  # Dropout rate
  observed_time <- pmin(survival_months, admin_censor, dropout_months)
  
  # Event indicator (1 = death, 0 = censored)
  death_indicator <- as.numeric(survival_months <= pmin(admin_censor, dropout_months))
  
  # Create final dataset
  cancer_data <- data.frame(
    patient_id = paste0("CA_", sprintf("%03d", 1:n)),
    age = age,
    sex = sex,
    cancer_stage = stage,
    tumor_biomarker = round(biomarker, 3),
    follow_up_months = round(observed_time, 1),
    death_event = death_indicator,
    treatment_type = sample(c("Surgery", "Surgery+Chemo", "Surgery+Radio", "Palliative"), 
                           n, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
    hospital_center = sample(paste0("Center_", LETTERS[1:5]), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(cancer_data)
}

# =============================================================================
# Dataset 2: Cardiovascular Risk Prediction
# =============================================================================

create_cardiovascular_risk_data <- function(n = 400) {
  # Simulate cardiovascular risk prediction study
  
  # Demographics
  age <- round(rnorm(n, 58, 15))
  age <- pmax(30, pmin(85, age))
  
  sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4))
  
  # Risk factors
  diabetes <- rbinom(n, 1, 0.25)
  hypertension <- rbinom(n, 1, 0.4)
  smoking <- rbinom(n, 1, 0.3)
  
  # Biomarkers (e.g., troponin, CRP, BNP)
  # Log-normal distribution for realistic biomarker values
  troponin_log <- rnorm(n, -1, 1.2)
  troponin <- round(exp(troponin_log), 3)
  
  crp_log <- rnorm(n, 0.5, 0.8)
  crp <- round(exp(crp_log), 2)
  
  # Combined risk score
  risk_score <- 0.05 * age + 0.3 * (sex == "Male") + 0.4 * diabetes + 
                0.3 * hypertension + 0.25 * smoking + 0.2 * log(troponin) + 0.15 * log(crp)
  
  # Time to cardiovascular event
  hazard <- exp(-2 + 0.8 * scale(risk_score)[,1])
  event_time_months <- round(rexp(n, hazard), 1)
  
  # Administrative censoring at 36 months
  admin_censor <- 36
  observed_time <- pmin(event_time_months, admin_censor)
  cv_event <- as.numeric(event_time_months <= admin_censor)
  
  cardiovascular_data <- data.frame(
    patient_id = paste0("CV_", sprintf("%04d", 1:n)),
    age = age,
    sex = sex,
    diabetes = diabetes,
    hypertension = hypertension,
    smoking_status = smoking,
    troponin_level = troponin,
    crp_level = crp,
    risk_score = round(risk_score, 3),
    follow_up_months = round(observed_time, 1),
    cv_event = cv_event,
    event_type = ifelse(cv_event == 1, 
                       sample(c("MI", "Stroke", "CHF", "Death"), sum(cv_event), replace = TRUE),
                       "None"),
    study_site = sample(c("Site_A", "Site_B", "Site_C", "Site_D"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(cardiovascular_data)
}

# =============================================================================
# Dataset 3: Multi-Biomarker Comparison Study
# =============================================================================

create_multi_biomarker_data <- function(n = 250) {
  # Study comparing multiple biomarkers for same outcome
  
  # Generate correlated biomarkers with different predictive abilities
  # Biomarker 1: Excellent predictor (AUC ~0.85)
  # Biomarker 2: Good predictor (AUC ~0.75) 
  # Biomarker 3: Fair predictor (AUC ~0.65)
  
  # Generate latent risk factor
  true_risk <- rnorm(n, 0, 1)
  
  # Generate biomarkers with different signal-to-noise ratios
  biomarker_1 <- true_risk * 1.5 + rnorm(n, 0, 0.5)  # Strong signal
  biomarker_2 <- true_risk * 1.0 + rnorm(n, 0, 0.8)  # Moderate signal
  biomarker_3 <- true_risk * 0.6 + rnorm(n, 0, 1.2)  # Weak signal
  
  # Add realistic scales and ensure positive values
  biomarker_1 <- round(pmax(0.01, exp(biomarker_1 * 0.3 + 2)), 3)
  biomarker_2 <- round(pmax(0.1, biomarker_2 * 2 + 10), 2)
  biomarker_3 <- round(pmax(1, biomarker_3 * 5 + 50), 1)
  
  # Time to event based on true risk
  hazard <- exp(-1.5 + 0.6 * true_risk)
  event_time_months <- round(rexp(n, hazard), 1)
  
  # Censoring
  admin_censor <- 48
  observed_time <- pmin(event_time_months, admin_censor)
  event_occurred <- as.numeric(event_time_months <= admin_censor)
  
  multi_biomarker_data <- data.frame(
    subject_id = paste0("MB_", sprintf("%03d", 1:n)),
    age_years = round(rnorm(n, 62, 10)),
    gender = sample(c("M", "F"), n, replace = TRUE),
    biomarker_alpha = biomarker_1,     # Best predictor
    biomarker_beta = biomarker_2,      # Moderate predictor  
    biomarker_gamma = biomarker_3,     # Weak predictor
    composite_score = round((scale(biomarker_1)[,1] + scale(biomarker_2)[,1]) / 2, 3),
    follow_up_months = round(observed_time, 1),
    primary_event = event_occurred,
    cohort = sample(c("Training", "Validation"), n, replace = TRUE, prob = c(0.7, 0.3)),
    enrollment_year = sample(2018:2022, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(multi_biomarker_data)
}

# =============================================================================
# Dataset 4: Time-Dependent Biomarker Study (Landmark Analysis)
# =============================================================================

create_landmark_biomarker_data <- function(n = 200) {
  # Study where biomarker changes over time
  
  # Baseline characteristics
  age <- round(rnorm(n, 60, 12))
  
  # Generate baseline biomarker
  baseline_biomarker <- rlnorm(n, 1, 0.8)
  
  # Simulate biomarker change over time (some increase, some decrease)
  biomarker_trend <- sample(c(-1, 0, 1), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
  
  # 6-month biomarker value
  month6_biomarker <- baseline_biomarker * exp(biomarker_trend * 0.3 + rnorm(n, 0, 0.2))
  
  # Event probability based on 6-month biomarker value
  risk_prob <- plogis(-2 + 0.5 * log(month6_biomarker))
  
  # Time to event (conditional on surviving to 6 months)
  # Everyone starts at 6 months (landmark time)
  additional_survival <- rexp(n, -log(1 - risk_prob) / 30)  # Average 30 months if at risk
  total_time <- 6 + additional_survival
  
  # Censoring
  admin_censor <- 60
  observed_time <- pmin(total_time, admin_censor)
  landmark_event <- as.numeric(total_time <= admin_censor)
  
  landmark_data <- data.frame(
    patient_id = paste0("LM_", sprintf("%03d", 1:n)),
    age = age,
    baseline_biomarker = round(baseline_biomarker, 3),
    month6_biomarker = round(month6_biomarker, 3),
    biomarker_change = round(log(month6_biomarker / baseline_biomarker), 3),
    total_follow_up_months = round(observed_time, 1),
    landmark_eligible = TRUE,  # All patients survived to 6 months
    post_landmark_event = landmark_event,
    response_status = sample(c("Complete", "Partial", "Stable", "Progressive"), 
                           n, replace = TRUE, prob = c(0.2, 0.3, 0.35, 0.15)),
    treatment_arm = sample(c("Experimental", "Standard"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(landmark_data)
}

# =============================================================================
# Dataset 5: Edge Cases and Quality Testing Data
# =============================================================================

create_edge_case_data <- function(n = 150) {
  # Dataset with various edge cases for robust testing
  
  base_data <- data.frame(
    id = paste0("EC_", sprintf("%03d", 1:n)),
    age = round(rnorm(n, 55, 15)),
    stringsAsFactors = FALSE
  )
  
  # Create different scenarios
  scenarios <- sample(1:5, n, replace = TRUE)
  
  base_data$scenario <- scenarios
  base_data$biomarker <- NA
  base_data$time_months <- NA
  base_data$event_status <- NA
  
  for (i in 1:n) {
    scenario <- scenarios[i]
    
    if (scenario == 1) {
      # Normal case
      base_data$biomarker[i] <- rlnorm(1, 1, 0.5)
      base_data$time_months[i] <- rexp(1, 0.02)
      base_data$event_status[i] <- rbinom(1, 1, 0.3)
      
    } else if (scenario == 2) {
      # Very high biomarker values
      base_data$biomarker[i] <- rlnorm(1, 3, 0.3)  # Very high
      base_data$time_months[i] <- rexp(1, 0.1)     # Short survival
      base_data$event_status[i] <- rbinom(1, 1, 0.8)
      
    } else if (scenario == 3) {
      # Very low biomarker values
      base_data$biomarker[i] <- rlnorm(1, -1, 0.2)  # Very low
      base_data$time_months[i] <- rexp(1, 0.01)     # Long survival
      base_data$event_status[i] <- rbinom(1, 1, 0.1)
      
    } else if (scenario == 4) {
      # Extreme outliers
      base_data$biomarker[i] <- ifelse(runif(1) < 0.1, 1000, rlnorm(1, 1, 0.5))
      base_data$time_months[i] <- rexp(1, 0.02)
      base_data$event_status[i] <- rbinom(1, 1, 0.3)
      
    } else if (scenario == 5) {
      # Very long follow-up times
      base_data$biomarker[i] <- rlnorm(1, 0.5, 0.4)
      base_data$time_months[i] <- rexp(1, 0.005)  # Very long times
      base_data$event_status[i] <- rbinom(1, 1, 0.15)
    }
  }
  
  # Round values
  base_data$biomarker <- round(base_data$biomarker, 4)
  base_data$time_months <- round(base_data$time_months, 1)
  
  # Add some missing values
  missing_indices <- sample(1:n, size = round(n * 0.05))
  base_data$biomarker[missing_indices] <- NA
  
  # Add problematic timepoints for testing
  base_data$test_timepoints <- paste0(sample(c("6, 12, 18", "3, 9, 15", "12, 24, 36", "1, 6, 12"), 
                                           n, replace = TRUE))
  
  return(base_data)
}

# =============================================================================
# Dataset 6: Competing Risks Scenario
# =============================================================================

create_competing_risks_data <- function(n = 180) {
  # Study with competing risks (death from disease vs other causes)
  
  age <- round(rnorm(n, 70, 10))
  
  # Comorbidity score (affects competing risk)
  comorbidity <- rpois(n, 2)
  
  # Biomarker
  disease_biomarker <- rlnorm(n, 1.5, 0.6)
  
  # Risk of disease-specific death
  disease_hazard <- exp(-3 + 0.4 * log(disease_biomarker) + 0.02 * age)
  disease_time <- rexp(n, disease_hazard)
  
  # Risk of competing death (other causes)
  competing_hazard <- exp(-4 + 0.05 * age + 0.2 * comorbidity)
  competing_time <- rexp(n, competing_hazard)
  
  # Observed time and cause
  observed_time <- pmin(disease_time, competing_time, 48)  # 48 month admin censor
  
  cause <- case_when(
    disease_time <= competing_time & disease_time <= 48 ~ 1,    # Disease death
    competing_time < disease_time & competing_time <= 48 ~ 2,   # Competing death
    TRUE ~ 0  # Censored
  )
  
  competing_data <- data.frame(
    patient_id = paste0("CR_", sprintf("%03d", 1:n)),
    age = age,
    comorbidity_score = comorbidity,
    disease_biomarker = round(disease_biomarker, 3),
    time_to_event_months = round(observed_time, 1),
    event_type = factor(cause, levels = 0:2, 
                       labels = c("Censored", "Disease_Death", "Other_Death")),
    disease_death = as.numeric(cause == 1),  # For primary analysis
    any_death = as.numeric(cause %in% 1:2),
    performance_status = sample(0:2, n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    prior_treatment = rbinom(n, 1, 0.6),
    stringsAsFactors = FALSE
  )
  
  return(competing_data)
}

# =============================================================================
# Generate All Datasets
# =============================================================================

cat("Generating comprehensive timeroc test datasets...\n")

# Generate all datasets
timeroc_cancer_biomarker <- create_cancer_biomarker_data(300)
timeroc_cardiovascular_risk <- create_cardiovascular_risk_data(400)
timeroc_multi_biomarker <- create_multi_biomarker_data(250)
timeroc_landmark_biomarker <- create_landmark_biomarker_data(200)
timeroc_edge_cases <- create_edge_case_data(150)
timeroc_competing_risks <- create_competing_risks_data(180)

# =============================================================================
# Save Datasets
# =============================================================================

# Ensure data directory exists
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Save as both CSV and RDA files
datasets <- list(
  timeroc_cancer_biomarker = timeroc_cancer_biomarker,
  timeroc_cardiovascular_risk = timeroc_cardiovascular_risk,
  timeroc_multi_biomarker = timeroc_multi_biomarker,
  timeroc_landmark_biomarker = timeroc_landmark_biomarker,
  timeroc_edge_cases = timeroc_edge_cases,
  timeroc_competing_risks = timeroc_competing_risks
)

# Save CSV files
for (name in names(datasets)) {
  write.csv(datasets[[name]], 
           file = paste0("data/", name, ".csv"), 
           row.names = FALSE)
  cat("Saved:", paste0("data/", name, ".csv"), "\n")
}

# Save RDA files  
for (name in names(datasets)) {
  data_obj <- datasets[[name]]
  assign(name, data_obj)
  save(list = name, file = paste0("data/", name, ".rda"))
  cat("Saved:", paste0("data/", name, ".rda"), "\n")
}

# =============================================================================
# Create Dataset Summary
# =============================================================================

summary_stats <- data.frame(
  Dataset = names(datasets),
  Observations = sapply(datasets, nrow),
  Description = c(
    "Cancer biomarker study with tumor marker and survival outcomes",
    "Cardiovascular risk prediction with troponin and clinical events", 
    "Multi-biomarker comparison study with different predictive abilities",
    "Landmark biomarker analysis with 6-month conditional survival",
    "Edge cases and quality testing scenarios with various challenges",
    "Competing risks study with disease-specific and other-cause mortality"
  ),
  Key_Features = c(
    "Cancer staging, tumor biomarker, 60-month follow-up",
    "CV risk factors, troponin/CRP, 36-month follow-up",
    "3 biomarkers with AUC ~0.85, 0.75, 0.65",
    "Landmark analysis, biomarker change over time",
    "Outliers, missing data, extreme values",
    "Competing events, comorbidity effects"
  ),
  Primary_Endpoint = c(
    "death_event",
    "cv_event", 
    "primary_event",
    "post_landmark_event",
    "event_status",
    "disease_death"
  ),
  Biomarker_Variable = c(
    "tumor_biomarker",
    "troponin_level or risk_score",
    "biomarker_alpha, biomarker_beta, biomarker_gamma",
    "month6_biomarker",
    "biomarker",
    "disease_biomarker"
  ),
  stringsAsFactors = FALSE
)

write.csv(summary_stats, "data/timeroc_datasets_summary.csv", row.names = FALSE)
save(summary_stats, file = "data/timeroc_datasets_summary.rda")

# =============================================================================
# Create Test Scenarios Documentation
# =============================================================================

test_scenarios <- data.frame(
  Scenario = c(
    "Basic Time-Dependent ROC",
    "Method Comparison", 
    "Bootstrap Confidence Intervals",
    "Optimal Cutoff Calculation",
    "Multi-Biomarker Comparison",
    "Landmark Analysis",
    "Edge Case Handling",
    "Competing Risks Analysis",
    "Clinical Interpretation",
    "Publication-Ready Plots"
  ),
  Dataset = c(
    "timeroc_cancer_biomarker",
    "timeroc_cardiovascular_risk",
    "timeroc_cancer_biomarker", 
    "timeroc_multi_biomarker",
    "timeroc_multi_biomarker",
    "timeroc_landmark_biomarker",
    "timeroc_edge_cases",
    "timeroc_competing_risks",
    "timeroc_cardiovascular_risk",
    "timeroc_cancer_biomarker"
  ),
  Expected_Result = c(
    "Time-specific AUC values with 95% CI at 12, 36, 60 months",
    "Comparison of incident vs cumulative methods",
    "Bootstrap CIs for more robust inference",
    "Youden-optimal cutoffs with sensitivity/specificity",
    "Ranking of biomarkers by predictive performance",
    "6-month landmark analysis with conditional survival",
    "Robust handling of outliers and missing data",
    "Analysis focused on disease-specific mortality",
    "Clinical utility assessment and recommendations",
    "High-quality ROC curves and AUC plots over time"
  ),
  timepoints = c(
    "12, 36, 60",
    "6, 18, 36",
    "12, 24, 48",
    "6, 12, 24",
    "6, 12, 18",
    "12, 24, 36",
    "6, 12, 18",
    "6, 24, 48",
    "12, 24, 36",
    "12, 36, 60"
  ),
  stringsAsFactors = FALSE
)

write.csv(test_scenarios, "data/timeroc_test_scenarios.csv", row.names = FALSE)
save(test_scenarios, file = "data/timeroc_test_scenarios.rda")

# =============================================================================
# Summary Report
# =============================================================================

cat("\n=== TimeROC Test Data Generation Complete ===\n")
cat("Total datasets created:", length(datasets), "\n")
cat("Total observations:", sum(sapply(datasets, nrow)), "\n")
cat("\nDatasets saved in both CSV and RDA formats:\n")
for (name in names(datasets)) {
  n_obs <- nrow(datasets[[name]])
  n_events <- if("death_event" %in% names(datasets[[name]])) sum(datasets[[name]]$death_event) else
              if("cv_event" %in% names(datasets[[name]])) sum(datasets[[name]]$cv_event) else
              if("primary_event" %in% names(datasets[[name]])) sum(datasets[[name]]$primary_event) else
              if("post_landmark_event" %in% names(datasets[[name]])) sum(datasets[[name]]$post_landmark_event) else
              if("event_status" %in% names(datasets[[name]])) sum(datasets[[name]]$event_status, na.rm = TRUE) else
              if("disease_death" %in% names(datasets[[name]])) sum(datasets[[name]]$disease_death) else "N/A"
  cat(sprintf("- %s (%d obs, %s events)\n", name, n_obs, n_events))
}

cat("\nAdditional files created:\n")
cat("- timeroc_datasets_summary: Overview of all datasets\n") 
cat("- timeroc_test_scenarios: Testing scenarios and expected results\n")

cat("\n=== Data Features Summary ===\n")
cat("✓ Cancer biomarker prediction scenarios\n")
cat("✓ Cardiovascular risk assessment data\n") 
cat("✓ Multi-biomarker comparison studies\n")
cat("✓ Landmark analysis test cases\n")
cat("✓ Edge cases and robustness testing\n")
cat("✓ Competing risks scenarios\n")
cat("✓ Various timepoint specifications\n")
cat("✓ Realistic clinical data patterns\n")
cat("✓ Quality issues for error handling validation\n")

cat("\nReady for timeroc function testing and validation!\n")