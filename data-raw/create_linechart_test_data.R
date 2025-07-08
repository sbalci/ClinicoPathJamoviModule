# Test data generation for linechart function
# Creates realistic clinical and research datasets for line chart analysis

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Dataset 1: Hemoglobin Levels During Cancer Treatment
create_hemoglobin_monitoring <- function() {
  n_patients <- 120
  n_visits <- 8
  
  # Visit schedule (weeks)
  visit_weeks <- c(0, 2, 4, 6, 8, 12, 16, 20)
  
  # Create patient data
  patients <- expand.grid(
    patient_id = paste0("P", sprintf("%03d", 1:n_patients)),
    visit_week = visit_weeks
  )
  
  # Assign treatment groups
  treatment_assignments <- data.frame(
    patient_id = paste0("P", sprintf("%03d", 1:n_patients)),
    treatment_group = sample(c("Control", "EPO_Low", "EPO_High"), n_patients, 
                           replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    baseline_hgb = rnorm(n_patients, mean = 9.5, sd = 1.2)
  )
  
  # Merge patient data with treatment info
  data <- merge(patients, treatment_assignments, by = "patient_id")
  data <- data[order(data$patient_id, data$visit_week), ]
  
  # Generate hemoglobin trajectories based on treatment
  data$hemoglobin_g_dl <- NA
  
  for (i in 1:nrow(data)) {
    week <- data$visit_week[i]
    treatment <- data$treatment_group[i]
    baseline <- data$baseline_hgb[i]
    
    # Treatment effects
    if (treatment == "Control") {
      # Gradual decline
      trend <- -0.05 * week
      noise <- rnorm(1, 0, 0.3)
    } else if (treatment == "EPO_Low") {
      # Modest improvement
      if (week <= 8) {
        trend <- 0.08 * week
      } else {
        trend <- 0.08 * 8 + 0.02 * (week - 8)
      }
      noise <- rnorm(1, 0, 0.25)
    } else { # EPO_High
      # Stronger improvement
      if (week <= 6) {
        trend <- 0.15 * week
      } else {
        trend <- 0.15 * 6 + 0.05 * (week - 6)
      }
      noise <- rnorm(1, 0, 0.2)
    }
    
    data$hemoglobin_g_dl[i] <- baseline + trend + noise
  }
  
  # Ensure realistic clinical ranges
  data$hemoglobin_g_dl[data$hemoglobin_g_dl < 6] <- 6
  data$hemoglobin_g_dl[data$hemoglobin_g_dl > 16] <- 16
  data$hemoglobin_g_dl <- round(data$hemoglobin_g_dl, 1)
  
  # Add some missing values (realistic dropout pattern)
  dropout_patients <- sample(unique(data$patient_id), size = 15)
  for (patient in dropout_patients) {
    dropout_week <- sample(visit_weeks[4:8], 1)
    data$hemoglobin_g_dl[data$patient_id == patient & data$visit_week >= dropout_week] <- NA
  }
  
  # Add additional clinical variables
  data$age <- rep(sample(35:80, n_patients, replace = TRUE), each = n_visits)
  data$gender <- rep(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.6, 0.4)), each = n_visits)
  data$cancer_type <- rep(sample(c("Lung", "Breast", "Colon", "Prostate"), n_patients, replace = TRUE), each = n_visits)
  
  return(data)
}

# Dataset 2: Blood Pressure Monitoring Over Time
create_blood_pressure_study <- function() {
  n_subjects <- 80
  n_months <- 12
  
  # Monthly measurements
  months <- 0:(n_months-1)
  
  # Create subject data
  subjects <- expand.grid(
    subject_id = paste0("BP", sprintf("%03d", 1:n_subjects)),
    month = months
  )
  
  # Assign intervention groups
  intervention_assignments <- data.frame(
    subject_id = paste0("BP", sprintf("%03d", 1:n_subjects)),
    intervention = sample(c("Lifestyle", "Medication", "Combined"), n_subjects, 
                         replace = TRUE, prob = c(0.35, 0.35, 0.3)),
    baseline_sbp = rnorm(n_subjects, mean = 155, sd = 15),
    baseline_dbp = rnorm(n_subjects, mean = 95, sd = 10)
  )
  
  # Merge data
  data <- merge(subjects, intervention_assignments, by = "subject_id")
  data <- data[order(data$subject_id, data$month), ]
  
  # Generate blood pressure trajectories
  data$systolic_bp <- NA
  data$diastolic_bp <- NA
  
  for (i in 1:nrow(data)) {
    month <- data$month[i]
    intervention <- data$intervention[i]
    baseline_sbp <- data$baseline_sbp[i]
    baseline_dbp <- data$baseline_dbp[i]
    
    # Intervention effects
    if (intervention == "Lifestyle") {
      # Gradual improvement
      sbp_reduction <- 2 * sqrt(month) * (1 + rnorm(1, 0, 0.3))
      dbp_reduction <- 1.2 * sqrt(month) * (1 + rnorm(1, 0, 0.3))
    } else if (intervention == "Medication") {
      # Faster initial improvement, then plateau
      if (month <= 3) {
        sbp_reduction <- 8 * (month / 3) * (1 + rnorm(1, 0, 0.2))
        dbp_reduction <- 5 * (month / 3) * (1 + rnorm(1, 0, 0.2))
      } else {
        sbp_reduction <- 8 + 2 * ((month - 3) / 9) * (1 + rnorm(1, 0, 0.25))
        dbp_reduction <- 5 + 1 * ((month - 3) / 9) * (1 + rnorm(1, 0, 0.25))
      }
    } else { # Combined
      # Best improvement
      if (month <= 2) {
        sbp_reduction <- 6 * (month / 2) * (1 + rnorm(1, 0, 0.15))
        dbp_reduction <- 4 * (month / 2) * (1 + rnorm(1, 0, 0.15))
      } else {
        sbp_reduction <- 6 + 8 * ((month - 2) / 10) * (1 + rnorm(1, 0, 0.2))
        dbp_reduction <- 4 + 4 * ((month - 2) / 10) * (1 + rnorm(1, 0, 0.2))
      }
    }
    
    data$systolic_bp[i] <- baseline_sbp - sbp_reduction + rnorm(1, 0, 3)
    data$diastolic_bp[i] <- baseline_dbp - dbp_reduction + rnorm(1, 0, 2)
  }
  
  # Ensure realistic ranges
  data$systolic_bp[data$systolic_bp < 90] <- 90
  data$systolic_bp[data$systolic_bp > 200] <- 200
  data$diastolic_bp[data$diastolic_bp < 50] <- 50
  data$diastolic_bp[data$diastolic_bp > 130] <- 130
  
  data$systolic_bp <- round(data$systolic_bp)
  data$diastolic_bp <- round(data$diastolic_bp)
  
  # Add demographic data
  data$age <- rep(sample(40:75, n_subjects, replace = TRUE), each = n_months)
  data$gender <- rep(sample(c("Male", "Female"), n_subjects, replace = TRUE), each = n_months)
  data$bmi <- rep(round(rnorm(n_subjects, 28, 4), 1), each = n_months)
  
  return(data)
}

# Dataset 3: Biomarker Response to Immunotherapy
create_biomarker_immunotherapy <- function() {
  n_patients <- 60
  
  # Irregular visit schedule (realistic clinical scenario)
  visit_days <- c(0, 21, 42, 84, 126, 168, 252, 336)
  
  # Create patient data
  patients <- expand.grid(
    patient_id = paste0("IMM", sprintf("%03d", 1:n_patients)),
    visit_day = visit_days
  )
  
  # Patient characteristics
  patient_info <- data.frame(
    patient_id = paste0("IMM", sprintf("%03d", 1:n_patients)),
    response_type = sample(c("Responder", "Non_Responder", "Progressive"), n_patients,
                          replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    baseline_il6 = rlnorm(n_patients, meanlog = 2, sdlog = 0.8),
    baseline_tnf_alpha = rlnorm(n_patients, meanlog = 1.5, sdlog = 0.6)
  )
  
  # Merge data
  data <- merge(patients, patient_info, by = "patient_id")
  data <- data[order(data$patient_id, data$visit_day), ]
  
  # Generate biomarker trajectories
  data$il6_pg_ml <- NA
  data$tnf_alpha_pg_ml <- NA
  
  for (i in 1:nrow(data)) {
    day <- data$visit_day[i]
    response <- data$response_type[i]
    baseline_il6 <- data$baseline_il6[i]
    baseline_tnf <- data$baseline_tnf_alpha[i]
    
    # Response patterns
    if (response == "Responder") {
      # Steady decrease in inflammatory markers
      il6_factor <- exp(-0.003 * day) * (1 + rnorm(1, 0, 0.2))
      tnf_factor <- exp(-0.002 * day) * (1 + rnorm(1, 0, 0.15))
    } else if (response == "Non_Responder") {
      # Minimal change
      il6_factor <- 1 + 0.1 * sin(day / 50) * (1 + rnorm(1, 0, 0.3))
      tnf_factor <- 1 + 0.05 * sin(day / 60) * (1 + rnorm(1, 0, 0.25))
    } else { # Progressive
      # Increase over time
      il6_factor <- 1 + 0.002 * day * (1 + rnorm(1, 0, 0.4))
      tnf_factor <- 1 + 0.0015 * day * (1 + rnorm(1, 0, 0.3))
    }
    
    data$il6_pg_ml[i] <- baseline_il6 * il6_factor
    data$tnf_alpha_pg_ml[i] <- baseline_tnf * tnf_factor
  }
  
  # Ensure positive values and realistic ranges
  data$il6_pg_ml[data$il6_pg_ml < 0.1] <- 0.1
  data$il6_pg_ml[data$il6_pg_ml > 100] <- 100
  data$tnf_alpha_pg_ml[data$tnf_alpha_pg_ml < 0.1] <- 0.1
  data$tnf_alpha_pg_ml[data$tnf_alpha_pg_ml > 50] <- 50
  
  data$il6_pg_ml <- round(data$il6_pg_ml, 2)
  data$tnf_alpha_pg_ml <- round(data$tnf_alpha_pg_ml, 2)
  
  # Add clinical variables
  data$age <- rep(sample(25:85, n_patients, replace = TRUE), each = length(visit_days))
  data$gender <- rep(sample(c("Male", "Female"), n_patients, replace = TRUE), each = length(visit_days))
  data$tumor_type <- rep(sample(c("Melanoma", "Lung_NSCLC", "Kidney", "Bladder"), n_patients, replace = TRUE), 
                       each = length(visit_days))
  
  return(data)
}

# Dataset 4: Quality of Life Scores Post-Surgery
create_quality_life_surgery <- function() {
  n_patients <- 100
  
  # Assessment timepoints (days post-surgery)
  timepoints <- c(7, 14, 30, 60, 90, 180, 365)
  
  # Create patient data
  patients <- expand.grid(
    patient_id = paste0("QOL", sprintf("%03d", 1:n_patients)),
    days_post_surgery = timepoints
  )
  
  # Patient characteristics
  patient_info <- data.frame(
    patient_id = paste0("QOL", sprintf("%03d", 1:n_patients)),
    surgery_type = sample(c("Minimally_Invasive", "Open_Surgery"), n_patients,
                         replace = TRUE, prob = c(0.6, 0.4)),
    baseline_qol = rnorm(n_patients, mean = 75, sd = 10),
    age_group = sample(c("Under_60", "60_Plus"), n_patients, replace = TRUE, prob = c(0.55, 0.45))
  )
  
  # Merge data
  data <- merge(patients, patient_info, by = "patient_id")
  data <- data[order(data$patient_id, data$days_post_surgery), ]
  
  # Generate QOL trajectories
  data$qol_score <- NA
  
  for (i in 1:nrow(data)) {
    day <- data$days_post_surgery[i]
    surgery <- data$surgery_type[i]
    baseline <- data$baseline_qol[i]
    age_group <- data$age_group[i]
    
    # Initial drop and recovery patterns
    if (surgery == "Minimally_Invasive") {
      # Faster recovery
      if (day <= 30) {
        recovery_factor <- 0.7 + 0.3 * (day / 30)
      } else if (day <= 90) {
        recovery_factor <- 1.0 + 0.05 * ((day - 30) / 60)
      } else {
        recovery_factor <- 1.05 + 0.02 * ((day - 90) / 275)
      }
    } else { # Open_Surgery
      # Slower recovery
      if (day <= 60) {
        recovery_factor <- 0.6 + 0.3 * (day / 60)
      } else if (day <= 180) {
        recovery_factor <- 0.9 + 0.1 * ((day - 60) / 120)
      } else {
        recovery_factor <- 1.0 + 0.05 * ((day - 180) / 185)
      }
    }
    
    # Age effect
    age_factor <- if (age_group == "Under_60") 1.0 else 0.95
    
    # Calculate QOL score
    qol_score <- baseline * recovery_factor * age_factor + rnorm(1, 0, 3)
    
    data$qol_score[i] <- qol_score
  }
  
  # Ensure realistic QOL score ranges (0-100)
  data$qol_score[data$qol_score < 0] <- 0
  data$qol_score[data$qol_score > 100] <- 100
  data$qol_score <- round(data$qol_score, 1)
  
  # Add patient demographics
  data$age <- rep(sample(30:85, n_patients, replace = TRUE), each = length(timepoints))
  data$gender <- rep(sample(c("Male", "Female"), n_patients, replace = TRUE), each = length(timepoints))
  data$comorbidities <- rep(sample(c("None", "Mild", "Moderate"), n_patients, 
                                  replace = TRUE, prob = c(0.4, 0.4, 0.2)), each = length(timepoints))
  
  return(data)
}

# Dataset 5: Tumor Size Response to Treatment
create_tumor_response_data <- function() {
  n_patients <- 75
  
  # Assessment schedule (weeks)
  assessment_weeks <- c(0, 6, 12, 18, 24, 36, 48)
  
  # Create patient data
  patients <- expand.grid(
    patient_id = paste0("TR", sprintf("%03d", 1:n_patients)),
    assessment_week = assessment_weeks
  )
  
  # Patient characteristics
  patient_info <- data.frame(
    patient_id = paste0("TR", sprintf("%03d", 1:n_patients)),
    treatment_arm = sample(c("Standard", "Experimental"), n_patients, 
                          replace = TRUE, prob = c(0.5, 0.5)),
    baseline_tumor_size = rlnorm(n_patients, meanlog = 3, sdlog = 0.5),
    response_category = sample(c("Complete_Response", "Partial_Response", "Stable_Disease", "Progressive_Disease"),
                              n_patients, replace = TRUE, prob = c(0.15, 0.35, 0.30, 0.20))
  )
  
  # Merge data
  data <- merge(patients, patient_info, by = "patient_id")
  data <- data[order(data$patient_id, data$assessment_week), ]
  
  # Generate tumor size trajectories
  data$tumor_size_cm <- NA
  
  for (i in 1:nrow(data)) {
    week <- data$assessment_week[i]
    treatment <- data$treatment_arm[i]
    baseline_size <- data$baseline_tumor_size[i]
    response <- data$response_category[i]
    
    # Response patterns
    if (response == "Complete_Response") {
      if (week <= 12) {
        size_factor <- 1 - 0.08 * week
      } else {
        size_factor <- 1 - 0.08 * 12 - 0.02 * (week - 12)
      }
      size_factor <- max(size_factor, 0.05)  # Minimum residual
    } else if (response == "Partial_Response") {
      if (week <= 18) {
        size_factor <- 1 - 0.03 * week
      } else {
        size_factor <- 1 - 0.03 * 18 - 0.005 * (week - 18)
      }
      size_factor <- max(size_factor, 0.4)  # 60% reduction max
    } else if (response == "Stable_Disease") {
      # Minimal change with some fluctuation
      size_factor <- 1 + 0.1 * sin(week / 10) * (1 + rnorm(1, 0, 0.1))
    } else { # Progressive_Disease
      # Growth over time
      size_factor <- 1 + 0.02 * week * (1 + rnorm(1, 0, 0.2))
    }
    
    # Treatment effect
    treatment_factor <- if (treatment == "Experimental") 0.9 else 1.0
    
    # Calculate tumor size
    tumor_size <- baseline_size * size_factor * treatment_factor + rnorm(1, 0, 0.2)
    
    data$tumor_size_cm[i] <- tumor_size
  }
  
  # Ensure positive values and realistic ranges
  data$tumor_size_cm[data$tumor_size_cm < 0.1] <- 0.1
  data$tumor_size_cm[data$tumor_size_cm > 15] <- 15
  data$tumor_size_cm <- round(data$tumor_size_cm, 1)
  
  # Add clinical variables
  data$age <- rep(sample(35:80, n_patients, replace = TRUE), each = length(assessment_weeks))
  data$gender <- rep(sample(c("Male", "Female"), n_patients, replace = TRUE), each = length(assessment_weeks))
  data$tumor_location <- rep(sample(c("Liver", "Lung", "Lymph_Node", "Bone"), n_patients, 
                                   replace = TRUE), each = length(assessment_weeks))
  
  return(data)
}

# Generate all datasets
print("Generating test datasets for linechart function...")

# Generate datasets
hemoglobin_data <- create_hemoglobin_monitoring()
blood_pressure_data <- create_blood_pressure_study()
biomarker_data <- create_biomarker_immunotherapy()
quality_life_data <- create_quality_life_surgery()
tumor_response_data <- create_tumor_response_data()

# Display summary information
cat("\nDataset 1: Hemoglobin Monitoring During Cancer Treatment\n")
cat("Dimensions:", nrow(hemoglobin_data), "x", ncol(hemoglobin_data), "\n")
cat("Patients:", length(unique(hemoglobin_data$patient_id)), "\n")
cat("Treatment groups:", paste(unique(hemoglobin_data$treatment_group), collapse = ", "), "\n")
cat("Visit weeks:", paste(sort(unique(hemoglobin_data$visit_week)), collapse = ", "), "\n")
cat("Hemoglobin range:", round(min(hemoglobin_data$hemoglobin_g_dl, na.rm = TRUE), 1), "-", 
    round(max(hemoglobin_data$hemoglobin_g_dl, na.rm = TRUE), 1), "g/dL\n")

cat("\nDataset 2: Blood Pressure Monitoring Study\n")
cat("Dimensions:", nrow(blood_pressure_data), "x", ncol(blood_pressure_data), "\n")
cat("Subjects:", length(unique(blood_pressure_data$subject_id)), "\n")
cat("Interventions:", paste(unique(blood_pressure_data$intervention), collapse = ", "), "\n")
cat("Follow-up months:", max(blood_pressure_data$month), "\n")
cat("Systolic BP range:", min(blood_pressure_data$systolic_bp), "-", max(blood_pressure_data$systolic_bp), "mmHg\n")

cat("\nDataset 3: Biomarker Response to Immunotherapy\n")
cat("Dimensions:", nrow(biomarker_data), "x", ncol(biomarker_data), "\n")
cat("Patients:", length(unique(biomarker_data$patient_id)), "\n")
cat("Response types:", paste(unique(biomarker_data$response_type), collapse = ", "), "\n")
cat("Visit days:", paste(sort(unique(biomarker_data$visit_day)), collapse = ", "), "\n")
cat("IL-6 range:", round(min(biomarker_data$il6_pg_ml), 2), "-", 
    round(max(biomarker_data$il6_pg_ml), 2), "pg/mL\n")

cat("\nDataset 4: Quality of Life Post-Surgery\n")
cat("Dimensions:", nrow(quality_life_data), "x", ncol(quality_life_data), "\n")
cat("Patients:", length(unique(quality_life_data$patient_id)), "\n")
cat("Surgery types:", paste(unique(quality_life_data$surgery_type), collapse = ", "), "\n")
cat("Assessment timepoints (days):", paste(sort(unique(quality_life_data$days_post_surgery)), collapse = ", "), "\n")
cat("QOL score range:", round(min(quality_life_data$qol_score), 1), "-", 
    round(max(quality_life_data$qol_score), 1), "\n")

cat("\nDataset 5: Tumor Size Response to Treatment\n")
cat("Dimensions:", nrow(tumor_response_data), "x", ncol(tumor_response_data), "\n")
cat("Patients:", length(unique(tumor_response_data$patient_id)), "\n")
cat("Treatment arms:", paste(unique(tumor_response_data$treatment_arm), collapse = ", "), "\n")
cat("Response categories:", paste(unique(tumor_response_data$response_category), collapse = ", "), "\n")
cat("Assessment weeks:", paste(sort(unique(tumor_response_data$assessment_week)), collapse = ", "), "\n")
cat("Tumor size range:", round(min(tumor_response_data$tumor_size_cm), 1), "-", 
    round(max(tumor_response_data$tumor_size_cm), 1), "cm\n")

# Save datasets
save(hemoglobin_data, file = "data/linechart_hemoglobin.rda")
save(blood_pressure_data, file = "data/linechart_blood_pressure.rda")
save(biomarker_data, file = "data/linechart_biomarker.rda")
save(quality_life_data, file = "data/linechart_quality_life.rda")
save(tumor_response_data, file = "data/linechart_tumor_response.rda")

# Also save as CSV for easy inspection
write.csv(hemoglobin_data, "data-raw/linechart_hemoglobin.csv", row.names = FALSE)
write.csv(blood_pressure_data, "data-raw/linechart_blood_pressure.csv", row.names = FALSE)
write.csv(biomarker_data, "data-raw/linechart_biomarker.csv", row.names = FALSE)
write.csv(quality_life_data, "data-raw/linechart_quality_life.csv", row.names = FALSE)
write.csv(tumor_response_data, "data-raw/linechart_tumor_response.csv", row.names = FALSE)

cat("\nTest datasets created successfully!\n")
cat("Saved as .rda files in data/ directory and .csv files in data-raw/ directory\n")

# Clinical interpretation guide
cat("\n" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("CLINICAL INTERPRETATION GUIDE FOR LINECHART TEST DATA\n")
cat(paste(rep("=", 60), collapse = "") %+% "\n")

cat("\n1. HEMOGLOBIN MONITORING DURING CANCER TREATMENT\n")
cat("   Purpose: Monitor anemia treatment response with EPO therapy\n")
cat("   Clinical context: Cancer patients often develop anemia requiring treatment\n")
cat("   Key features: Multiple treatment groups, realistic dropout patterns\n")
cat("   Expected trends: Control (decline), EPO_Low (modest improvement), EPO_High (strong improvement)\n")
cat("   Reference ranges: Normal Hgb 12-16 g/dL (women), 14-18 g/dL (men)\n")

cat("\n2. BLOOD PRESSURE MONITORING STUDY\n")
cat("   Purpose: Compare effectiveness of different hypertension interventions\n")
cat("   Clinical context: Hypertension management with lifestyle vs medication\n")
cat("   Key features: Different intervention response patterns\n")
cat("   Expected trends: All interventions show BP reduction, Combined therapy most effective\n")
cat("   Reference ranges: Normal BP <120/80 mmHg, Hypertension ≥140/90 mmHg\n")

cat("\n3. BIOMARKER RESPONSE TO IMMUNOTHERAPY\n")
cat("   Purpose: Monitor inflammatory biomarkers during cancer immunotherapy\n")
cat("   Clinical context: IL-6 and TNF-α as treatment response indicators\n")
cat("   Key features: Irregular visit schedule, different response patterns\n")
cat("   Expected trends: Responders show biomarker decrease, Non-responders stable, Progressive increase\n")
cat("   Clinical significance: Lower inflammatory markers often correlate with better outcomes\n")

cat("\n4. QUALITY OF LIFE POST-SURGERY\n")
cat("   Purpose: Compare recovery patterns between surgical approaches\n")
cat("   Clinical context: Patient-reported outcomes after different surgical procedures\n")
cat("   Key features: Initial QOL drop followed by recovery\n")
cat("   Expected trends: Minimally invasive surgery shows faster recovery\n")
cat("   QOL scale: 0-100 where higher scores indicate better quality of life\n")

cat("\n5. TUMOR SIZE RESPONSE TO TREATMENT\n")
cat("   Purpose: Assess tumor response to standard vs experimental therapy\n")
cat("   Clinical context: RECIST criteria for tumor response evaluation\n")
cat("   Key features: Different response categories with realistic patterns\n")
cat("   Expected trends: Complete/Partial responders show size reduction, Progressive disease shows growth\n")
cat("   RECIST criteria: Complete response (disappearance), Partial response (≥30% decrease)\n")

cat("\nThese datasets provide comprehensive scenarios for testing linechart:\n")
cat("• Longitudinal clinical data with realistic patterns\n")
cat("• Multiple treatment/intervention groups\n")
cat("• Realistic missing data and dropout patterns\n")
cat("• Different time scales (days, weeks, months)\n")
cat("• Clinically meaningful endpoints and reference ranges\n")
cat("• Various response patterns (improvement, decline, stability)\n")
cat("• Publication-ready visualization scenarios\n")