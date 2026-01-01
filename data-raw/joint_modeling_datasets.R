# Joint Modeling Example Datasets
# These datasets are designed for testing and demonstrating joint longitudinal-survival models

# Load required packages
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)

set.seed(12345)  # For reproducibility

# ============================================================================
# Dataset 1: PSA Trajectory and Prostate Cancer Survival
# ============================================================================

# Function to generate PSA trajectory data
generate_psa_data <- function(n_patients = 200, avg_visits = 6, max_followup = 60) {
  
  psa_data <- data.frame()
  
  for (i in 1:n_patients) {
    # Patient characteristics
    age <- round(rnorm(1, 70, 8))
    age <- pmax(50, pmin(90, age))  # Constrain age
    
    stage <- sample(c("T1", "T2", "T3", "T4"), 1, 
                   prob = c(0.3, 0.4, 0.2, 0.1))
    stage_num <- as.numeric(factor(stage, levels = c("T1", "T2", "T3", "T4")))
    
    gleason <- sample(6:10, 1, prob = c(0.2, 0.3, 0.25, 0.15, 0.1))
    
    # Number of visits for this patient
    n_visits <- rpois(1, avg_visits) + 1
    n_visits <- pmax(2, pmin(12, n_visits))  # At least 2, max 12 visits
    
    # Visit times (irregular intervals)
    visit_times <- c(0, sort(runif(n_visits - 1, 0.5, max_followup * 0.8)))
    
    # True PSA trajectory parameters (individual-specific)
    psa_intercept <- rnorm(1, 8 + 2 * (stage_num - 1) + 0.5 * (gleason - 6), 2)
    psa_intercept <- pmax(0.1, psa_intercept)
    
    psa_slope <- rnorm(1, 0.05 + 0.02 * (stage_num - 1) + 0.01 * (gleason - 6), 0.02)
    
    # Generate PSA values with measurement error
    psa_true <- psa_intercept + psa_slope * visit_times
    psa_observed <- pmax(0.01, psa_true + rnorm(n_visits, 0, 0.3))
    
    # Survival outcome - influenced by PSA trajectory
    hazard_base <- 0.001 * exp(0.3 * (stage_num - 1) + 0.2 * (gleason - 6) - 0.02 * (age - 70))
    
    # Time-varying effect of PSA (current value association)
    for (j in 1:length(visit_times)) {
      # PSA influences hazard
      current_psa <- psa_observed[j]
      hazard_multiplier <- exp(0.1 * log(current_psa))
      
      # Simulate survival time
      if (j == 1) {
        cumulative_hazard <- 0
      }
      
      if (j < length(visit_times)) {
        interval_length <- visit_times[j + 1] - visit_times[j]
      } else {
        interval_length <- max_followup - visit_times[j]
      }
      
      cumulative_hazard <- cumulative_hazard + hazard_base * hazard_multiplier * interval_length
    }
    
    # Generate survival time
    survival_time <- rexp(1, hazard_base * exp(0.1 * mean(log(psa_observed))))
    survival_time <- pmin(survival_time, max_followup)
    event_status <- ifelse(survival_time < max_followup, 1, 0)
    
    # Create patient data
    patient_data <- data.frame(
      patient_id = paste0("PSA_", sprintf("%03d", i)),
      age = age,
      stage = stage,
      gleason_score = gleason,
      visit_time = visit_times,
      psa_level = round(psa_observed, 2),
      survival_time = round(survival_time, 1),
      death_status = event_status
    )
    
    psa_data <- rbind(psa_data, patient_data)
  }
  
  return(psa_data)
}

# Generate PSA dataset
psa_joint_data <- generate_psa_data(n_patients = 200, avg_visits = 6, max_followup = 60)

# Add some missing values to make it realistic
n_missing <- round(0.05 * nrow(psa_joint_data))
missing_indices <- sample(nrow(psa_joint_data), n_missing)
psa_joint_data$psa_level[missing_indices] <- NA

# Remove rows with missing PSA (complete case analysis)
psa_joint_data <- psa_joint_data[!is.na(psa_joint_data$psa_level), ]

# ============================================================================
# Dataset 2: CD4 Count and AIDS Progression
# ============================================================================

generate_cd4_data <- function(n_patients = 180, avg_visits = 8, max_followup = 48) {
  
  cd4_data <- data.frame()
  
  for (i in 1:n_patients) {
    # Patient characteristics
    age <- round(rnorm(1, 40, 12))
    age <- pmax(18, pmin(70, age))
    
    baseline_viral_load <- rlnorm(1, 10, 1.5)  # Log-normal viral load
    art_adherence <- rbinom(1, 1, 0.7)  # 70% good adherence
    
    # Number of visits
    n_visits <- rpois(1, avg_visits) + 1
    n_visits <- pmax(3, pmin(15, n_visits))
    
    # Visit times (more regular for HIV monitoring)
    visit_times <- c(0, cumsum(rexp(n_visits - 1, 1/6)))  # Average 6-month intervals
    visit_times <- visit_times[visit_times <= max_followup]
    n_visits <- length(visit_times)
    
    # CD4 trajectory parameters
    cd4_intercept <- rnorm(1, 400 - 50 * (1 - art_adherence), 100)
    cd4_intercept <- pmax(50, cd4_intercept)
    
    # Slope depends on treatment adherence
    cd4_slope <- rnorm(1, 20 * art_adherence - 5 * (1 - art_adherence), 5)
    
    # Generate CD4 counts
    cd4_true <- cd4_intercept + cd4_slope * visit_times
    cd4_observed <- pmax(10, cd4_true + rnorm(n_visits, 0, 30))
    
    # AIDS/Death outcome
    hazard_base <- 0.002 * exp(-0.01 * (age - 40) + 0.5 * (1 - art_adherence))
    
    # Current CD4 count affects hazard
    avg_cd4 <- mean(cd4_observed)
    hazard_multiplier <- exp(-0.005 * avg_cd4)  # Lower CD4 = higher hazard
    
    survival_time <- rexp(1, hazard_base * hazard_multiplier)
    survival_time <- pmin(survival_time, max_followup)
    event_status <- ifelse(survival_time < max_followup, 1, 0)
    
    # Create patient data
    patient_data <- data.frame(
      patient_id = paste0("HIV_", sprintf("%03d", i)),
      age = age,
      baseline_viral_load = round(baseline_viral_load, 0),
      art_adherence = factor(art_adherence, levels = c(0, 1), labels = c("Poor", "Good")),
      visit_time = round(visit_times, 1),
      cd4_count = round(cd4_observed, 0),
      survival_time = round(survival_time, 1),
      aids_death_status = event_status
    )
    
    cd4_data <- rbind(cd4_data, patient_data)
  }
  
  return(cd4_data)
}

# Generate CD4 dataset
cd4_joint_data <- generate_cd4_data(n_patients = 180, avg_visits = 8, max_followup = 48)

# ============================================================================
# Dataset 3: Kidney Function Decline and ESRD/Death
# ============================================================================

generate_kidney_data <- function(n_patients = 150, avg_visits = 10, max_followup = 72) {
  
  kidney_data <- data.frame()
  
  for (i in 1:n_patients) {
    # Patient characteristics
    age <- round(rnorm(1, 65, 15))
    age <- pmax(30, pmin(90, age))
    
    diabetes <- rbinom(1, 1, 0.4)  # 40% diabetic
    hypertension <- rbinom(1, 1, 0.6)  # 60% hypertensive
    baseline_proteinuria <- rgamma(1, 2, 2)  # g/day
    
    # Number of visits
    n_visits <- rpois(1, avg_visits) + 2
    n_visits <- pmax(3, pmin(18, n_visits))
    
    # Visit times
    visit_times <- c(0, cumsum(rexp(n_visits - 1, 1/6)))  # Average 6-month intervals
    visit_times <- visit_times[visit_times <= max_followup]
    n_visits <- length(visit_times)
    
    # eGFR trajectory (decline over time)
    egfr_intercept <- rnorm(1, 50 - 5 * diabetes - 3 * hypertension - 2 * baseline_proteinuria, 10)
    egfr_intercept <- pmax(15, pmin(120, egfr_intercept))
    
    # Rate of decline
    egfr_slope <- rnorm(1, -1.5 - 0.5 * diabetes - 0.3 * hypertension - 0.2 * baseline_proteinuria, 0.3)
    egfr_slope <- pmin(egfr_slope, 0)  # Can only decline or stay stable
    
    # Generate eGFR values
    egfr_true <- egfr_intercept + egfr_slope * visit_times
    egfr_observed <- pmax(5, egfr_true + rnorm(n_visits, 0, 3))
    
    # ESRD/Death outcome (ESRD when eGFR < 15)
    min_egfr <- min(egfr_observed)
    
    # Hazard depends on kidney function trajectory
    hazard_base <- 0.001 * exp(0.02 * (age - 65) + 0.3 * diabetes + 0.2 * hypertension)
    
    # eGFR affects hazard
    avg_egfr <- mean(egfr_observed)
    hazard_multiplier <- exp(-0.05 * avg_egfr)  # Lower eGFR = higher hazard
    
    # Competing risks: ESRD vs Death
    survival_time_esrd <- rexp(1, hazard_base * hazard_multiplier)
    survival_time_death <- rexp(1, hazard_base * 0.5)  # Lower death hazard
    
    # First event
    survival_time <- pmin(survival_time_esrd, survival_time_death, max_followup)
    
    if (survival_time == survival_time_esrd && survival_time < max_followup) {
      event_status <- 1  # ESRD
    } else if (survival_time == survival_time_death && survival_time < max_followup) {
      event_status <- 2  # Death
    } else {
      event_status <- 0  # Censored
    }
    
    # Create patient data
    patient_data <- data.frame(
      patient_id = paste0("CKD_", sprintf("%03d", i)),
      age = age,
      diabetes = factor(diabetes, levels = c(0, 1), labels = c("No", "Yes")),
      hypertension = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")),
      baseline_proteinuria = round(baseline_proteinuria, 1),
      visit_time = round(visit_times, 1),
      egfr = round(egfr_observed, 1),
      survival_time = round(survival_time, 1),
      esrd_death_status = event_status
    )
    
    kidney_data <- rbind(kidney_data, patient_data)
  }
  
  return(kidney_data)
}

# Generate kidney function dataset
kidney_joint_data <- generate_kidney_data(n_patients = 150, avg_visits = 10, max_followup = 72)

# ============================================================================
# Dataset 4: Cardiac Biomarkers and Heart Failure Events
# ============================================================================

generate_cardiac_data <- function(n_patients = 120, avg_visits = 7, max_followup = 36) {
  
  cardiac_data <- data.frame()
  
  for (i in 1:n_patients) {
    # Patient characteristics
    age <- round(rnorm(1, 68, 12))
    age <- pmax(45, pmin(85, age))
    
    nyha_class <- sample(1:4, 1, prob = c(0.3, 0.4, 0.2, 0.1))
    ef_baseline <- rnorm(1, 45 - 5 * (nyha_class - 1), 10)
    ef_baseline <- pmax(15, pmin(65, ef_baseline))
    
    # Number of visits
    n_visits <- rpois(1, avg_visits) + 2
    n_visits <- pmax(3, pmin(12, n_visits))
    
    # Visit times
    visit_times <- c(0, cumsum(rexp(n_visits - 1, 1/4)))  # Average 4-month intervals
    visit_times <- visit_times[visit_times <= max_followup]
    n_visits <- length(visit_times)
    
    # NT-proBNP trajectory (biomarker of heart failure)
    bnp_intercept <- rlnorm(1, 6 + 0.3 * (nyha_class - 1), 0.8)
    
    # Slope depends on heart failure progression
    bnp_slope <- rnorm(1, 0.02 * nyha_class, 0.01)
    
    # Generate NT-proBNP values (log scale then transform)
    log_bnp_true <- log(bnp_intercept) + bnp_slope * visit_times
    bnp_observed <- exp(log_bnp_true + rnorm(n_visits, 0, 0.3))
    bnp_observed <- pmax(50, bnp_observed)  # Minimum detectable level
    
    # Heart failure hospitalization/death
    hazard_base <- 0.003 * exp(0.02 * (age - 68) + 0.2 * (nyha_class - 1))
    
    # NT-proBNP affects hazard
    avg_log_bnp <- mean(log(bnp_observed))
    hazard_multiplier <- exp(0.3 * (avg_log_bnp - 6))
    
    survival_time <- rexp(1, hazard_base * hazard_multiplier)
    survival_time <- pmin(survival_time, max_followup)
    event_status <- ifelse(survival_time < max_followup, 1, 0)
    
    # Create patient data
    patient_data <- data.frame(
      patient_id = paste0("HF_", sprintf("%03d", i)),
      age = age,
      nyha_class = factor(nyha_class, levels = 1:4, labels = paste("Class", 1:4)),
      baseline_ef = round(ef_baseline, 0),
      visit_time = round(visit_times, 1),
      nt_probnp = round(bnp_observed, 0),
      survival_time = round(survival_time, 1),
      hf_event_status = event_status
    )
    
    cardiac_data <- rbind(cardiac_data, patient_data)
  }
  
  return(cardiac_data)
}

# Generate cardiac biomarker dataset
cardiac_joint_data <- generate_cardiac_data(n_patients = 120, avg_visits = 7, max_followup = 36)

# ============================================================================
# Dataset 5: Tumor Marker and Cancer Progression (Simple Example)
# ============================================================================

generate_simple_cancer_data <- function(n_patients = 100, avg_visits = 5, max_followup = 24) {
  
  cancer_data <- data.frame()
  
  for (i in 1:n_patients) {
    # Patient characteristics
    age <- round(rnorm(1, 60, 12))
    age <- pmax(30, pmin(80, age))
    
    treatment <- sample(c("Standard", "Experimental"), 1, prob = c(0.5, 0.5))
    treatment_effect <- ifelse(treatment == "Experimental", -0.1, 0)
    
    # Number of visits
    n_visits <- rpois(1, avg_visits) + 2
    n_visits <- pmax(3, pmin(10, n_visits))
    
    # Visit times
    visit_times <- c(0, cumsum(rexp(n_visits - 1, 1/4)))  # Average 4-month intervals
    visit_times <- visit_times[visit_times <= max_followup]
    n_visits <- length(visit_times)
    
    # Tumor marker trajectory (CA-125 like)
    marker_intercept <- rlnorm(1, 4, 0.6)  # Starting around 50 units
    marker_slope <- rnorm(1, 0.05 + treatment_effect, 0.02)
    
    # Generate marker values
    log_marker_true <- log(marker_intercept) + marker_slope * visit_times
    marker_observed <- exp(log_marker_true + rnorm(n_visits, 0, 0.2))
    marker_observed <- pmax(5, marker_observed)
    
    # Progression/death outcome
    hazard_base <- 0.005 * exp(0.01 * (age - 60))
    
    # Marker level affects hazard
    avg_log_marker <- mean(log(marker_observed))
    hazard_multiplier <- exp(0.4 * (avg_log_marker - 4))
    
    survival_time <- rexp(1, hazard_base * hazard_multiplier)
    survival_time <- pmin(survival_time, max_followup)
    event_status <- ifelse(survival_time < max_followup, 1, 0)
    
    # Create patient data
    patient_data <- data.frame(
      patient_id = paste0("CA_", sprintf("%03d", i)),
      age = age,
      treatment = treatment,
      visit_time = round(visit_times, 1),
      tumor_marker = round(marker_observed, 1),
      survival_time = round(survival_time, 1),
      progression_status = event_status
    )
    
    cancer_data <- rbind(cancer_data, patient_data)
  }
  
  return(cancer_data)
}

# Generate simple cancer dataset
simple_cancer_data <- generate_simple_cancer_data(n_patients = 100, avg_visits = 5, max_followup = 24)

# ============================================================================
# Save all datasets
# ============================================================================

# Create data summaries
create_data_summary <- function(data, patient_col, time_col, biomarker_col, survival_col, event_col) {
  cat("\n=== Dataset Summary ===\n")
  cat("Total observations:", nrow(data), "\n")
  cat("Unique patients:", length(unique(data[[patient_col]])), "\n")
  cat("Average visits per patient:", round(nrow(data) / length(unique(data[[patient_col]])), 1), "\n")
  cat("Follow-up range:", range(data[[time_col]])[1], "-", range(data[[time_col]])[2], "\n")
  cat("Biomarker range:", round(range(data[[biomarker_col]], na.rm = TRUE), 2), "\n")
  cat("Event rate:", round(mean(data[!duplicated(data[[patient_col]]), event_col]) * 100, 1), "%\n")
  cat("Median survival time:", round(median(data[!duplicated(data[[patient_col]]), survival_col]), 1), "\n")
}

# Print summaries
cat("PSA Prostate Cancer Data:")
create_data_summary(psa_joint_data, "patient_id", "visit_time", "psa_level", "survival_time", "death_status")

cat("\nCD4 HIV Data:")
create_data_summary(cd4_joint_data, "patient_id", "visit_time", "cd4_count", "survival_time", "aids_death_status")

cat("\nKidney Function Data:")
create_data_summary(kidney_joint_data, "patient_id", "visit_time", "egfr", "survival_time", "esrd_death_status")

cat("\nCardiac Biomarker Data:")
create_data_summary(cardiac_joint_data, "patient_id", "visit_time", "nt_probnp", "survival_time", "hf_event_status")

cat("\nSimple Cancer Data:")
create_data_summary(simple_cancer_data, "patient_id", "visit_time", "tumor_marker", "survival_time", "progression_status")

# Save datasets
use_data_multi_format(psa_joint_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(cd4_joint_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(kidney_joint_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(cardiac_joint_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(simple_cancer_data, overwrite = TRUE, save_csv = TRUE)

cat("\n✅ All joint modeling datasets created and saved!\n")
