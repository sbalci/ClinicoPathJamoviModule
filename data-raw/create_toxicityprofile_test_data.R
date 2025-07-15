#!/usr/bin/env Rscript

# =============================================================================
# Comprehensive Test Data Generation for Enhanced toxicityprofile Function
# =============================================================================

cat("Creating comprehensive test datasets for enhanced toxicityprofile function...\n")

# Load required libraries
suppressMessages({
  library(dplyr)
  library(lubridate)
})

set.seed(123)  # For reproducible results

# =============================================================================
# Dataset 1: Oncology Clinical Trial Safety Data
# =============================================================================

cat("Creating oncology clinical trial safety dataset...\n")

n_patients <- 250
n_treatment_arms <- 3
treatment_arms <- c("Control", "Treatment A", "Treatment B")

# Common adverse events in oncology trials with expected frequencies
common_aes <- c(
  "Fatigue", "Nausea", "Vomiting", "Diarrhea", "Constipation", "Decreased appetite",
  "Anemia", "Neutropenia", "Thrombocytopenia", "Alopecia", "Peripheral neuropathy",
  "Rash", "Mucositis", "Fever", "Infection", "Dehydration", "Hyponatremia",
  "Elevated ALT", "Elevated AST", "Pneumonitis", "Hypertension", "Proteinuria"
)

# System organ classes according to MedDRA
soc_mapping <- c(
  "Fatigue" = "General disorders",
  "Nausea" = "Gastrointestinal disorders", 
  "Vomiting" = "Gastrointestinal disorders",
  "Diarrhea" = "Gastrointestinal disorders",
  "Constipation" = "Gastrointestinal disorders",
  "Decreased appetite" = "Metabolism and nutrition disorders",
  "Anemia" = "Blood and lymphatic system disorders",
  "Neutropenia" = "Blood and lymphatic system disorders", 
  "Thrombocytopenia" = "Blood and lymphatic system disorders",
  "Alopecia" = "Skin and subcutaneous tissue disorders",
  "Peripheral neuropathy" = "Nervous system disorders",
  "Rash" = "Skin and subcutaneous tissue disorders",
  "Mucositis" = "Gastrointestinal disorders",
  "Fever" = "General disorders",
  "Infection" = "Infections and infestations",
  "Dehydration" = "Metabolism and nutrition disorders",
  "Hyponatremia" = "Metabolism and nutrition disorders",
  "Elevated ALT" = "Investigations",
  "Elevated AST" = "Investigations",
  "Pneumonitis" = "Respiratory disorders",
  "Hypertension" = "Vascular disorders",
  "Proteinuria" = "Renal and urinary disorders"
)

# Expected incidence rates by treatment arm (Control, Treatment A, Treatment B)
ae_incidence_rates <- list(
  "Fatigue" = c(0.60, 0.70, 0.75),
  "Nausea" = c(0.30, 0.50, 0.60),
  "Vomiting" = c(0.15, 0.25, 0.35),
  "Diarrhea" = c(0.20, 0.40, 0.50),
  "Constipation" = c(0.25, 0.30, 0.35),
  "Decreased appetite" = c(0.35, 0.45, 0.55),
  "Anemia" = c(0.25, 0.45, 0.50),
  "Neutropenia" = c(0.15, 0.35, 0.45),
  "Thrombocytopenia" = c(0.10, 0.25, 0.35),
  "Alopecia" = c(0.05, 0.60, 0.70),
  "Peripheral neuropathy" = c(0.05, 0.30, 0.40),
  "Rash" = c(0.20, 0.35, 0.45),
  "Mucositis" = c(0.10, 0.25, 0.35),
  "Fever" = c(0.15, 0.20, 0.25),
  "Infection" = c(0.10, 0.15, 0.20),
  "Dehydration" = c(0.08, 0.12, 0.18),
  "Hyponatremia" = c(0.05, 0.10, 0.15),
  "Elevated ALT" = c(0.12, 0.25, 0.30),
  "Elevated AST" = c(0.10, 0.20, 0.25),
  "Pneumonitis" = c(0.02, 0.05, 0.08),
  "Hypertension" = c(0.15, 0.20, 0.25),
  "Proteinuria" = c(0.08, 0.15, 0.20)
)

# Generate patient data
patients <- data.frame(
  patient_id = sprintf("PT_%03d", 1:n_patients),
  treatment_arm = sample(treatment_arms, n_patients, replace = TRUE),
  age = round(rnorm(n_patients, 60, 12)),
  sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.55, 0.45)),
  study_start_date = as.Date("2023-01-01") + sample(0:180, n_patients, replace = TRUE),
  stringsAsFactors = FALSE
)

# Generate adverse events
toxicityprofile_oncology_trial <- data.frame()

for (i in 1:n_patients) {
  patient <- patients[i, ]
  treatment_idx <- which(treatment_arms == patient$treatment_arm)
  
  # Generate adverse events for this patient
  for (ae in common_aes) {
    incidence_rate <- ae_incidence_rates[[ae]][treatment_idx]
    
    # Determine if patient experiences this AE
    if (runif(1) < incidence_rate) {
      # Multiple events possible for same AE
      n_events <- sample(1:3, 1, prob = c(0.7, 0.25, 0.05))
      
      for (event_num in 1:n_events) {
        # Generate grade based on AE type and treatment
        if (ae %in% c("Alopecia", "Rash")) {
          # Mostly grade 1-2 events
          grade <- sample(1:3, 1, prob = c(0.6, 0.35, 0.05))
        } else if (ae %in% c("Neutropenia", "Thrombocytopenia", "Anemia")) {
          # Hematologic AEs can be higher grade
          grade <- sample(1:4, 1, prob = c(0.3, 0.35, 0.25, 0.1))
        } else if (ae %in% c("Pneumonitis", "Infection")) {
          # Serious AEs
          grade <- sample(2:5, 1, prob = c(0.4, 0.35, 0.2, 0.05))
        } else {
          # General distribution
          grade <- sample(1:4, 1, prob = c(0.45, 0.35, 0.15, 0.05))
        }
        
        # Time to event (days from study start)
        time_to_event <- round(rexp(1, rate = 0.01) + 1)
        if (time_to_event > 365) time_to_event <- 365
        
        # Event date
        event_date <- patient$study_start_date + time_to_event
        
        # Add to dataset
        toxicityprofile_oncology_trial <- rbind(toxicityprofile_oncology_trial, data.frame(
          patient_id = patient$patient_id,
          treatment_group = patient$treatment_arm,
          adverse_event = ae,
          toxicity_grade = grade,
          system_organ_class = soc_mapping[ae],
          time_to_event = time_to_event,
          event_date = event_date,
          patient_age = patient$age,
          patient_sex = patient$sex,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Ensure factors are properly set
toxicityprofile_oncology_trial$treatment_group <- as.factor(toxicityprofile_oncology_trial$treatment_group)
toxicityprofile_oncology_trial$adverse_event <- as.factor(toxicityprofile_oncology_trial$adverse_event)
toxicityprofile_oncology_trial$system_organ_class <- as.factor(toxicityprofile_oncology_trial$system_organ_class)
toxicityprofile_oncology_trial$patient_sex <- as.factor(toxicityprofile_oncology_trial$patient_sex)

cat("✅ Oncology trial dataset:", nrow(toxicityprofile_oncology_trial), "events,", 
    length(unique(toxicityprofile_oncology_trial$patient_id)), "patients\n")

# =============================================================================
# Dataset 2: Immunotherapy Safety Profile
# =============================================================================

cat("Creating immunotherapy safety profile dataset...\n")

n_patients_immuno <- 180
immuno_aes <- c(
  "Fatigue", "Diarrhea", "Colitis", "Pneumonitis", "Hepatitis", "Endocrinopathy",
  "Thyroiditis", "Adrenal insufficiency", "Diabetes mellitus", "Hypophysitis",
  "Rash", "Pruritus", "Vitiligo", "Severe skin reaction", "Arthralgia", "Myalgia",
  "Nephritis", "Neurological toxicity", "Myocarditis", "Uveitis"
)

immuno_soc_mapping <- c(
  "Fatigue" = "General disorders",
  "Diarrhea" = "Gastrointestinal disorders", 
  "Colitis" = "Gastrointestinal disorders",
  "Pneumonitis" = "Respiratory disorders",
  "Hepatitis" = "Hepatobiliary disorders",
  "Endocrinopathy" = "Endocrine disorders",
  "Thyroiditis" = "Endocrine disorders",
  "Adrenal insufficiency" = "Endocrine disorders",
  "Diabetes mellitus" = "Endocrine disorders",
  "Hypophysitis" = "Endocrine disorders",
  "Rash" = "Skin and subcutaneous tissue disorders",
  "Pruritus" = "Skin and subcutaneous tissue disorders",
  "Vitiligo" = "Skin and subcutaneous tissue disorders",
  "Severe skin reaction" = "Skin and subcutaneous tissue disorders",
  "Arthralgia" = "Musculoskeletal disorders",
  "Myalgia" = "Musculoskeletal disorders",
  "Nephritis" = "Renal and urinary disorders",
  "Neurological toxicity" = "Nervous system disorders",
  "Myocarditis" = "Cardiac disorders",
  "Uveitis" = "Eye disorders"
)

# Different incidence pattern for immunotherapy
immuno_incidence_rates <- list(
  "Fatigue" = c(0.50, 0.65),
  "Diarrhea" = c(0.25, 0.35),
  "Colitis" = c(0.05, 0.15),
  "Pneumonitis" = c(0.03, 0.12),
  "Hepatitis" = c(0.02, 0.08),
  "Endocrinopathy" = c(0.08, 0.18),
  "Thyroiditis" = c(0.06, 0.12),
  "Adrenal insufficiency" = c(0.02, 0.06),
  "Diabetes mellitus" = c(0.01, 0.04),
  "Hypophysitis" = c(0.01, 0.05),
  "Rash" = c(0.30, 0.45),
  "Pruritus" = c(0.25, 0.35),
  "Vitiligo" = c(0.05, 0.15),
  "Severe skin reaction" = c(0.02, 0.08),
  "Arthralgia" = c(0.15, 0.25),
  "Myalgia" = c(0.12, 0.20),
  "Nephritis" = c(0.01, 0.05),
  "Neurological toxicity" = c(0.02, 0.06),
  "Myocarditis" = c(0.005, 0.02),
  "Uveitis" = c(0.01, 0.03)
)

# Generate immunotherapy data
immuno_patients <- data.frame(
  patient_id = sprintf("IMM_%03d", 1:n_patients_immuno),
  treatment_group = sample(c("Monotherapy", "Combination"), n_patients_immuno, replace = TRUE),
  age = round(rnorm(n_patients_immuno, 58, 15)),
  sex = sample(c("Male", "Female"), n_patients_immuno, replace = TRUE),
  stringsAsFactors = FALSE
)

toxicityprofile_immunotherapy <- data.frame()

for (i in 1:n_patients_immuno) {
  patient <- immuno_patients[i, ]
  treatment_idx <- ifelse(patient$treatment_group == "Monotherapy", 1, 2)
  
  for (ae in immuno_aes) {
    incidence_rate <- immuno_incidence_rates[[ae]][treatment_idx]
    
    if (runif(1) < incidence_rate) {
      # Immunotherapy AEs often have specific grade patterns
      if (ae %in% c("Colitis", "Pneumonitis", "Hepatitis", "Myocarditis")) {
        # Serious immune-related AEs
        grade <- sample(2:5, 1, prob = c(0.3, 0.4, 0.25, 0.05))
      } else if (ae %in% c("Rash", "Pruritus", "Vitiligo")) {
        # Skin toxicities mostly grade 1-2
        grade <- sample(1:3, 1, prob = c(0.6, 0.35, 0.05))
      } else if (ae %in% c("Endocrinopathy", "Thyroiditis", "Adrenal insufficiency")) {
        # Endocrine toxicities
        grade <- sample(1:4, 1, prob = c(0.4, 0.35, 0.2, 0.05))
      } else {
        # General distribution
        grade <- sample(1:4, 1, prob = c(0.5, 0.3, 0.15, 0.05))
      }
      
      # Time to event - immunotherapy AEs often occur later
      time_to_event <- round(rweibull(1, shape = 1.5, scale = 80) + 7)
      if (time_to_event > 365) time_to_event <- 365
      
      toxicityprofile_immunotherapy <- rbind(toxicityprofile_immunotherapy, data.frame(
        patient_id = patient$patient_id,
        treatment_group = patient$treatment_group,
        adverse_event = ae,
        toxicity_grade = grade,
        system_organ_class = immuno_soc_mapping[ae],
        time_to_event = time_to_event,
        patient_age = patient$age,
        patient_sex = patient$sex,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Ensure factors are properly set
toxicityprofile_immunotherapy$treatment_group <- as.factor(toxicityprofile_immunotherapy$treatment_group)
toxicityprofile_immunotherapy$adverse_event <- as.factor(toxicityprofile_immunotherapy$adverse_event)
toxicityprofile_immunotherapy$system_organ_class <- as.factor(toxicityprofile_immunotherapy$system_organ_class)
toxicityprofile_immunotherapy$patient_sex <- as.factor(toxicityprofile_immunotherapy$patient_sex)

cat("✅ Immunotherapy dataset:", nrow(toxicityprofile_immunotherapy), "events,", 
    length(unique(toxicityprofile_immunotherapy$patient_id)), "patients\n")

# =============================================================================
# Dataset 3: Targeted Therapy Safety Profile
# =============================================================================

cat("Creating targeted therapy safety profile dataset...\n")

n_patients_targeted <- 200
targeted_aes <- c(
  "Diarrhea", "Rash", "Acneiform rash", "Paronychia", "Dry skin", "Mucositis",
  "Fatigue", "Decreased appetite", "Nausea", "Vomiting", "Hand-foot syndrome",
  "Hypertension", "Proteinuria", "Bleeding", "Thrombosis", "QT prolongation",
  "Elevated transaminases", "Hyperbilirubinemia", "Pneumonitis", "ILD",
  "Peripheral edema", "Pleural effusion", "Muscle spasms", "Arthralgia"
)

targeted_soc_mapping <- c(
  "Diarrhea" = "Gastrointestinal disorders",
  "Rash" = "Skin and subcutaneous tissue disorders",
  "Acneiform rash" = "Skin and subcutaneous tissue disorders",
  "Paronychia" = "Skin and subcutaneous tissue disorders",
  "Dry skin" = "Skin and subcutaneous tissue disorders",
  "Mucositis" = "Gastrointestinal disorders",
  "Fatigue" = "General disorders",
  "Decreased appetite" = "Metabolism and nutrition disorders",
  "Nausea" = "Gastrointestinal disorders",
  "Vomiting" = "Gastrointestinal disorders",
  "Hand-foot syndrome" = "Skin and subcutaneous tissue disorders",
  "Hypertension" = "Vascular disorders",
  "Proteinuria" = "Renal and urinary disorders",
  "Bleeding" = "Vascular disorders",
  "Thrombosis" = "Vascular disorders",
  "QT prolongation" = "Cardiac disorders",
  "Elevated transaminases" = "Investigations",
  "Hyperbilirubinemia" = "Hepatobiliary disorders",
  "Pneumonitis" = "Respiratory disorders",
  "ILD" = "Respiratory disorders",
  "Peripheral edema" = "General disorders",
  "Pleural effusion" = "Respiratory disorders",
  "Muscle spasms" = "Musculoskeletal disorders",
  "Arthralgia" = "Musculoskeletal disorders"
)

# Generate targeted therapy data
targeted_patients <- data.frame(
  patient_id = sprintf("TRG_%03d", 1:n_patients_targeted),
  treatment_group = sample(c("Monotherapy", "Combination"), n_patients_targeted, replace = TRUE),
  age = round(rnorm(n_patients_targeted, 62, 10)),
  sex = sample(c("Male", "Female"), n_patients_targeted, replace = TRUE),
  stringsAsFactors = FALSE
)

toxicityprofile_targeted_therapy <- data.frame()

# Targeted therapy specific incidence rates
targeted_incidence_rates <- list(
  "Diarrhea" = c(0.40, 0.55),
  "Rash" = c(0.60, 0.75),
  "Acneiform rash" = c(0.30, 0.45),
  "Paronychia" = c(0.15, 0.25),
  "Dry skin" = c(0.25, 0.35),
  "Mucositis" = c(0.20, 0.30),
  "Fatigue" = c(0.45, 0.55),
  "Decreased appetite" = c(0.30, 0.40),
  "Nausea" = c(0.25, 0.35),
  "Vomiting" = c(0.15, 0.25),
  "Hand-foot syndrome" = c(0.20, 0.35),
  "Hypertension" = c(0.25, 0.40),
  "Proteinuria" = c(0.15, 0.25),
  "Bleeding" = c(0.10, 0.20),
  "Thrombosis" = c(0.05, 0.12),
  "QT prolongation" = c(0.08, 0.15),
  "Elevated transaminases" = c(0.18, 0.28),
  "Hyperbilirubinemia" = c(0.05, 0.12),
  "Pneumonitis" = c(0.03, 0.08),
  "ILD" = c(0.02, 0.05),
  "Peripheral edema" = c(0.15, 0.25),
  "Pleural effusion" = c(0.08, 0.15),
  "Muscle spasms" = c(0.10, 0.18),
  "Arthralgia" = c(0.12, 0.20)
)

for (i in 1:n_patients_targeted) {
  patient <- targeted_patients[i, ]
  treatment_idx <- ifelse(patient$treatment_group == "Monotherapy", 1, 2)
  
  for (ae in targeted_aes) {
    incidence_rate <- targeted_incidence_rates[[ae]][treatment_idx]
    
    if (runif(1) < incidence_rate) {
      # Targeted therapy specific grading
      if (ae %in% c("Rash", "Acneiform rash", "Paronychia", "Dry skin")) {
        # Skin toxicities in targeted therapy
        grade <- sample(1:3, 1, prob = c(0.5, 0.4, 0.1))
      } else if (ae %in% c("Hypertension", "Proteinuria", "QT prolongation")) {
        # Cardiovascular/renal toxicities
        grade <- sample(1:4, 1, prob = c(0.4, 0.35, 0.2, 0.05))
      } else if (ae %in% c("Bleeding", "Thrombosis", "Pneumonitis", "ILD")) {
        # Serious targeted therapy AEs
        grade <- sample(2:5, 1, prob = c(0.35, 0.4, 0.2, 0.05))
      } else {
        # General distribution
        grade <- sample(1:4, 1, prob = c(0.45, 0.35, 0.15, 0.05))
      }
      
      # Time to event - targeted therapy AEs often occur early
      time_to_event <- round(rweibull(1, shape = 2, scale = 30) + 1)
      if (time_to_event > 365) time_to_event <- 365
      
      toxicityprofile_targeted_therapy <- rbind(toxicityprofile_targeted_therapy, data.frame(
        patient_id = patient$patient_id,
        treatment_group = patient$treatment_group,
        adverse_event = ae,
        toxicity_grade = grade,
        system_organ_class = targeted_soc_mapping[ae],
        time_to_event = time_to_event,
        patient_age = patient$age,
        patient_sex = patient$sex,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Ensure factors are properly set
toxicityprofile_targeted_therapy$treatment_group <- as.factor(toxicityprofile_targeted_therapy$treatment_group)
toxicityprofile_targeted_therapy$adverse_event <- as.factor(toxicityprofile_targeted_therapy$adverse_event)
toxicityprofile_targeted_therapy$system_organ_class <- as.factor(toxicityprofile_targeted_therapy$system_organ_class)
toxicityprofile_targeted_therapy$patient_sex <- as.factor(toxicityprofile_targeted_therapy$patient_sex)

cat("✅ Targeted therapy dataset:", nrow(toxicityprofile_targeted_therapy), "events,", 
    length(unique(toxicityprofile_targeted_therapy$patient_id)), "patients\n")

# =============================================================================
# Dataset 4: Dose Escalation Study
# =============================================================================

cat("Creating dose escalation study dataset...\n")

n_patients_dose <- 150
dose_levels <- c("Dose Level 1", "Dose Level 2", "Dose Level 3", "Dose Level 4", "Dose Level 5")

dose_escalation_aes <- c(
  "Fatigue", "Nausea", "Vomiting", "Diarrhea", "Anorexia", "Headache",
  "Dizziness", "Insomnia", "Anxiety", "Rash", "Pruritus", "Dry mouth",
  "Constipation", "Abdominal pain", "Dyspepsia", "Peripheral neuropathy",
  "Muscle weakness", "Arthralgia", "Back pain", "Hypertension"
)

# Generate dose escalation data
dose_patients <- data.frame(
  patient_id = sprintf("DSE_%03d", 1:n_patients_dose),
  dose_level = sample(dose_levels, n_patients_dose, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
  age = round(rnorm(n_patients_dose, 55, 12)),
  sex = sample(c("Male", "Female"), n_patients_dose, replace = TRUE),
  stringsAsFactors = FALSE
)

toxicityprofile_dose_escalation <- data.frame()

# Dose-dependent incidence rates
dose_multipliers <- c(1.0, 1.2, 1.5, 1.8, 2.2)  # Increasing with dose
base_incidence <- 0.15

for (i in 1:n_patients_dose) {
  patient <- dose_patients[i, ]
  dose_idx <- which(dose_levels == patient$dose_level)
  
  for (ae in dose_escalation_aes) {
    # Dose-dependent incidence
    incidence_rate <- base_incidence * dose_multipliers[dose_idx]
    if (incidence_rate > 0.8) incidence_rate <- 0.8
    
    if (runif(1) < incidence_rate) {
      # Dose-dependent grade severity
      grade_probs <- c(0.6, 0.25, 0.1, 0.05) * (1 + (dose_idx - 1) * 0.1)
      grade_probs <- grade_probs / sum(grade_probs)
      
      grade <- sample(1:4, 1, prob = grade_probs)
      
      # Time to event
      time_to_event <- round(rexp(1, rate = 0.02) + 1)
      if (time_to_event > 180) time_to_event <- 180
      
      toxicityprofile_dose_escalation <- rbind(toxicityprofile_dose_escalation, data.frame(
        patient_id = patient$patient_id,
        treatment_group = patient$dose_level,
        adverse_event = ae,
        toxicity_grade = grade,
        system_organ_class = "General disorders",
        time_to_event = time_to_event,
        patient_age = patient$age,
        patient_sex = patient$sex,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Ensure factors are properly set
toxicityprofile_dose_escalation$treatment_group <- as.factor(toxicityprofile_dose_escalation$treatment_group)
toxicityprofile_dose_escalation$adverse_event <- as.factor(toxicityprofile_dose_escalation$adverse_event)
toxicityprofile_dose_escalation$system_organ_class <- as.factor(toxicityprofile_dose_escalation$system_organ_class)
toxicityprofile_dose_escalation$patient_sex <- as.factor(toxicityprofile_dose_escalation$patient_sex)

cat("✅ Dose escalation dataset:", nrow(toxicityprofile_dose_escalation), "events,", 
    length(unique(toxicityprofile_dose_escalation$patient_id)), "patients\n")

# =============================================================================
# Dataset 5: Pediatric Safety Study
# =============================================================================

cat("Creating pediatric safety study dataset...\n")

n_patients_pediatric <- 120
pediatric_aes <- c(
  "Fatigue", "Nausea", "Vomiting", "Diarrhea", "Fever", "Headache",
  "Irritability", "Sleep disturbance", "Decreased appetite", "Weight loss",
  "Rash", "Injection site reaction", "Upper respiratory infection", "Cough",
  "Abdominal pain", "Mood changes", "Attention difficulties", "Hyperactivity"
)

pediatric_patients <- data.frame(
  patient_id = sprintf("PED_%03d", 1:n_patients_pediatric),
  treatment_group = sample(c("Active", "Placebo"), n_patients_pediatric, replace = TRUE),
  age = round(runif(n_patients_pediatric, 6, 17)),
  sex = sample(c("Male", "Female"), n_patients_pediatric, replace = TRUE),
  stringsAsFactors = FALSE
)

toxicityprofile_pediatric <- data.frame()

# Pediatric-specific incidence rates
pediatric_incidence_rates <- list(
  "Fatigue" = c(0.15, 0.25),
  "Nausea" = c(0.10, 0.20),
  "Vomiting" = c(0.08, 0.15),
  "Diarrhea" = c(0.12, 0.18),
  "Fever" = c(0.15, 0.20),
  "Headache" = c(0.18, 0.25),
  "Irritability" = c(0.20, 0.35),
  "Sleep disturbance" = c(0.15, 0.28),
  "Decreased appetite" = c(0.12, 0.22),
  "Weight loss" = c(0.05, 0.12),
  "Rash" = c(0.10, 0.15),
  "Injection site reaction" = c(0.25, 0.30),
  "Upper respiratory infection" = c(0.20, 0.25),
  "Cough" = c(0.15, 0.22),
  "Abdominal pain" = c(0.08, 0.15),
  "Mood changes" = c(0.10, 0.18),
  "Attention difficulties" = c(0.08, 0.15),
  "Hyperactivity" = c(0.05, 0.12)
)

for (i in 1:n_patients_pediatric) {
  patient <- pediatric_patients[i, ]
  treatment_idx <- ifelse(patient$treatment_group == "Placebo", 1, 2)
  
  for (ae in pediatric_aes) {
    incidence_rate <- pediatric_incidence_rates[[ae]][treatment_idx]
    
    if (runif(1) < incidence_rate) {
      # Pediatric AEs are typically milder
      grade <- sample(1:3, 1, prob = c(0.7, 0.25, 0.05))
      
      # Time to event
      time_to_event <- round(rexp(1, rate = 0.03) + 1)
      if (time_to_event > 90) time_to_event <- 90
      
      toxicityprofile_pediatric <- rbind(toxicityprofile_pediatric, data.frame(
        patient_id = patient$patient_id,
        treatment_group = patient$treatment_group,
        adverse_event = ae,
        toxicity_grade = grade,
        system_organ_class = "General disorders",
        time_to_event = time_to_event,
        patient_age = patient$age,
        patient_sex = patient$sex,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Ensure factors are properly set
toxicityprofile_pediatric$treatment_group <- as.factor(toxicityprofile_pediatric$treatment_group)
toxicityprofile_pediatric$adverse_event <- as.factor(toxicityprofile_pediatric$adverse_event)
toxicityprofile_pediatric$system_organ_class <- as.factor(toxicityprofile_pediatric$system_organ_class)
toxicityprofile_pediatric$patient_sex <- as.factor(toxicityprofile_pediatric$patient_sex)

cat("✅ Pediatric dataset:", nrow(toxicityprofile_pediatric), "events,", 
    length(unique(toxicityprofile_pediatric$patient_id)), "patients\n")

# =============================================================================
# Dataset 6: Small Sample Edge Cases
# =============================================================================

cat("Creating small sample edge cases dataset...\n")

# Create small dataset for edge case testing
toxicityprofile_small_sample <- data.frame(
  patient_id = sprintf("SML_%02d", 1:15),
  treatment_group = factor(sample(c("A", "B"), 15, replace = TRUE)),
  adverse_event = factor(sample(c("Fatigue", "Nausea", "Rash"), 15, replace = TRUE)),
  toxicity_grade = sample(1:3, 15, replace = TRUE),
  system_organ_class = factor(sample(c("General disorders", "Gastrointestinal disorders", "Skin disorders"), 15, replace = TRUE)),
  time_to_event = sample(1:30, 15, replace = TRUE),
  patient_age = sample(25:70, 15, replace = TRUE),
  patient_sex = factor(sample(c("Male", "Female"), 15, replace = TRUE)),
  stringsAsFactors = FALSE
)

cat("✅ Small sample dataset:", nrow(toxicityprofile_small_sample), "events,", 
    length(unique(toxicityprofile_small_sample$patient_id)), "patients\n")

# =============================================================================
# Save All Datasets
# =============================================================================

cat("\nSaving datasets...\n")

# Save as .rda files
save(toxicityprofile_oncology_trial, file = "data/toxicityprofile_oncology_trial.rda")
save(toxicityprofile_immunotherapy, file = "data/toxicityprofile_immunotherapy.rda")
save(toxicityprofile_targeted_therapy, file = "data/toxicityprofile_targeted_therapy.rda")
save(toxicityprofile_dose_escalation, file = "data/toxicityprofile_dose_escalation.rda")
save(toxicityprofile_pediatric, file = "data/toxicityprofile_pediatric.rda")
save(toxicityprofile_small_sample, file = "data/toxicityprofile_small_sample.rda")

# Save as .csv files for external use
write.csv(toxicityprofile_oncology_trial, "data/toxicityprofile_oncology_trial.csv", row.names = FALSE)
write.csv(toxicityprofile_immunotherapy, "data/toxicityprofile_immunotherapy.csv", row.names = FALSE)
write.csv(toxicityprofile_targeted_therapy, "data/toxicityprofile_targeted_therapy.csv", row.names = FALSE)
write.csv(toxicityprofile_dose_escalation, "data/toxicityprofile_dose_escalation.csv", row.names = FALSE)
write.csv(toxicityprofile_pediatric, "data/toxicityprofile_pediatric.csv", row.names = FALSE)
write.csv(toxicityprofile_small_sample, "data/toxicityprofile_small_sample.csv", row.names = FALSE)

# =============================================================================
# Create Summary Statistics
# =============================================================================

cat("Creating dataset summary information...\n")

summary_stats <- data.frame(
  Dataset = c("toxicityprofile_oncology_trial", "toxicityprofile_immunotherapy", 
              "toxicityprofile_targeted_therapy", "toxicityprofile_dose_escalation",
              "toxicityprofile_pediatric", "toxicityprofile_small_sample"),
  Events = c(nrow(toxicityprofile_oncology_trial), nrow(toxicityprofile_immunotherapy),
             nrow(toxicityprofile_targeted_therapy), nrow(toxicityprofile_dose_escalation),
             nrow(toxicityprofile_pediatric), nrow(toxicityprofile_small_sample)),
  Patients = c(length(unique(toxicityprofile_oncology_trial$patient_id)), 
               length(unique(toxicityprofile_immunotherapy$patient_id)),
               length(unique(toxicityprofile_targeted_therapy$patient_id)),
               length(unique(toxicityprofile_dose_escalation$patient_id)),
               length(unique(toxicityprofile_pediatric$patient_id)),
               length(unique(toxicityprofile_small_sample$patient_id))),
  Unique_AEs = c(length(unique(toxicityprofile_oncology_trial$adverse_event)),
                 length(unique(toxicityprofile_immunotherapy$adverse_event)),
                 length(unique(toxicityprofile_targeted_therapy$adverse_event)),
                 length(unique(toxicityprofile_dose_escalation$adverse_event)),
                 length(unique(toxicityprofile_pediatric$adverse_event)),
                 length(unique(toxicityprofile_small_sample$adverse_event))),
  Description = c("Oncology clinical trial with 3 treatment arms",
                  "Immunotherapy safety profile with immune-related AEs",
                  "Targeted therapy with characteristic toxicity pattern",
                  "Dose escalation study with dose-dependent toxicity",
                  "Pediatric safety study with age-appropriate AEs",
                  "Small sample for edge case testing"),
  Key_Features = c("CTCAE grading, multiple treatment arms, common oncology AEs",
                   "Immune-related adverse events, delayed onset patterns",
                   "Targeted therapy specific toxicities, early onset",
                   "Dose-dependent toxicity, escalating severity",
                   "Pediatric-specific AEs, milder severity profile",
                   "Minimal data for edge case validation"),
  Primary_Use_Case = c("Standard oncology safety analysis",
                       "Immune-related AE monitoring",
                       "Targeted therapy safety profiling",
                       "Dose-limiting toxicity assessment",
                       "Pediatric safety monitoring",
                       "Edge case and validation testing"),
  stringsAsFactors = FALSE
)

save(summary_stats, file = "data/toxicityprofile_datasets_summary.rda")
write.csv(summary_stats, "data/toxicityprofile_datasets_summary.csv", row.names = FALSE)

# =============================================================================
# Create Test Scenarios Documentation
# =============================================================================

test_scenarios <- data.frame(
  Scenario = c("Basic Toxicity Profile", "Treatment Comparison", "High Grade Events Analysis",
               "System Organ Class Analysis", "Time-to-Event Analysis", "Dose-Response Relationship",
               "Immunotherapy-Specific Analysis", "Pediatric Safety Profile", "Statistical Testing",
               "Confidence Interval Validation", "Edge Case Robustness", "Visualization Testing"),
  Dataset = c("toxicityprofile_oncology_trial", "toxicityprofile_oncology_trial", 
              "toxicityprofile_immunotherapy", "toxicityprofile_targeted_therapy",
              "toxicityprofile_oncology_trial", "toxicityprofile_dose_escalation",
              "toxicityprofile_immunotherapy", "toxicityprofile_pediatric",
              "toxicityprofile_targeted_therapy", "toxicityprofile_oncology_trial",
              "toxicityprofile_small_sample", "toxicityprofile_oncology_trial"),
  Analysis_Type = c("Basic frequency and grade distribution", "Group comparison with statistical tests",
                    "High grade (≥3) event analysis", "SOC-based toxicity summary",
                    "Cumulative incidence over time", "Dose-dependent toxicity analysis",
                    "Immune-related AE profiling", "Age-appropriate safety analysis",
                    "Fisher's exact test and chi-square tests", "Binomial confidence intervals",
                    "Small sample behavior", "Multiple plot types and themes"),
  Variables = c("patient_id, adverse_event, toxicity_grade", 
                "patient_id, adverse_event, toxicity_grade, treatment_group",
                "patient_id, adverse_event, toxicity_grade (filter ≥3)",
                "patient_id, adverse_event, toxicity_grade, system_organ_class",
                "patient_id, adverse_event, toxicity_grade, time_to_event",
                "patient_id, adverse_event, toxicity_grade, treatment_group (dose levels)",
                "patient_id, adverse_event, toxicity_grade, system_organ_class",
                "patient_id, adverse_event, toxicity_grade, patient_age",
                "patient_id, adverse_event, toxicity_grade, treatment_group",
                "patient_id, adverse_event, toxicity_grade",
                "patient_id, adverse_event, toxicity_grade",
                "patient_id, adverse_event, toxicity_grade"),
  Expected_Result = c("Frequency tables and grade distribution plots",
                      "Risk ratios, p-values, and comparison plots",
                      "Focus on serious adverse events only",
                      "Toxicity profile by organ system",
                      "Time-to-event curves and cumulative incidence",
                      "Dose-toxicity relationship visualization",
                      "Immune-related toxicity patterns",
                      "Pediatric-appropriate safety profile",
                      "Statistical significance testing results",
                      "Confidence intervals for incidence rates",
                      "Graceful handling of minimal data",
                      "Multiple plot types with appropriate themes"),
  stringsAsFactors = FALSE
)

save(test_scenarios, file = "data/toxicityprofile_test_scenarios.rda")
write.csv(test_scenarios, "data/toxicityprofile_test_scenarios.csv", row.names = FALSE)

# =============================================================================
# Final Summary
# =============================================================================

total_events <- sum(summary_stats$Events)
total_patients <- sum(summary_stats$Patients)
total_unique_aes <- sum(summary_stats$Unique_AEs)

cat("\n=== TOXICITYPROFILE TEST DATA GENERATION COMPLETE ===\n")
cat("✅ Total datasets created: 6\n")
cat("✅ Total adverse events:", total_events, "\n")
cat("✅ Total patients:", total_patients, "\n")
cat("✅ Total unique adverse events:", total_unique_aes, "\n")
cat("✅ Clinical scenarios covered: Oncology, Immunotherapy, Targeted therapy, Dose escalation, Pediatric\n")
cat("✅ CTCAE grading system: Grades 1-5 with realistic distributions\n")
cat("✅ Statistical features: Risk ratios, confidence intervals, time-to-event\n")
cat("✅ Visualization types: Stacked bars, dot plots, heatmaps, time-to-event curves\n")
cat("✅ Test scenarios documented: 12 comprehensive scenarios\n")

cat("\nDatasets Created:\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf("- %s (%d events, %d patients): %s\n", 
              summary_stats$Dataset[i], 
              summary_stats$Events[i], 
              summary_stats$Patients[i], 
              summary_stats$Description[i]))
}

cat("\nAll datasets include both .rda and .csv formats\n")
cat("Enhanced toxicityprofile function ready for comprehensive safety analysis testing!\n")