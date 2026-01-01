# Test data generation for missingdata function
# Creates realistic clinical datasets with various missing data patterns

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Dataset 1: Clinical Trial Data with Missing Data
create_clinical_trial_data <- function() {
  n_patients <- 200
  n_visits <- 4
  
  # Create patient baseline characteristics
  patients <- data.frame(
    patient_id = paste0("P", sprintf("%04d", 1:n_patients)),
    age = round(rnorm(n_patients, mean = 58, sd = 15)),
    gender = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.52, 0.48)),
    baseline_weight = round(rnorm(n_patients, mean = 75, sd = 15), 1),
    baseline_bmi = round(rnorm(n_patients, mean = 26, sd = 5), 1),
    treatment_arm = sample(c("Control", "Treatment_A", "Treatment_B"), 
                          n_patients, replace = TRUE, prob = c(0.33, 0.33, 0.34)),
    site = sample(c("Site_01", "Site_02", "Site_03", "Site_04", "Site_05"), 
                  n_patients, replace = TRUE),
    comorbidity_score = round(rnorm(n_patients, mean = 2.5, sd = 1.2), 1),
    stringsAsFactors = FALSE
  )
  
  # Ensure realistic ranges
  patients$age <- pmax(pmin(patients$age, 85), 18)
  patients$baseline_weight <- pmax(pmin(patients$baseline_weight, 150), 45)
  patients$baseline_bmi <- pmax(pmin(patients$baseline_bmi, 45), 16)
  patients$comorbidity_score <- pmax(pmin(patients$comorbidity_score, 8), 0)
  
  # Create longitudinal data
  visits <- expand.grid(
    patient_id = patients$patient_id,
    visit = 1:n_visits,
    stringsAsFactors = FALSE
  )
  
  # Merge with baseline data
  data <- merge(visits, patients, by = "patient_id")
  data <- data[order(data$patient_id, data$visit), ]
  
  # Add visit-specific measurements
  for (i in 1:nrow(data)) {
    patient_baseline <- patients[patients$patient_id == data$patient_id[i], ]
    visit_num <- data$visit[i]
    treatment <- data$treatment_arm[i]
    
    # Simulate treatment effects over time
    if (treatment == "Control") {
      weight_change <- rnorm(1, mean = 0, sd = 2)
      efficacy_score <- rnorm(1, mean = 50, sd = 10)
      adverse_events <- sample(0:2, 1, prob = c(0.7, 0.2, 0.1))
    } else if (treatment == "Treatment_A") {
      weight_change <- rnorm(1, mean = -1.5 * visit_num, sd = 3)
      efficacy_score <- rnorm(1, mean = 55 + 5 * visit_num, sd = 12)
      adverse_events <- sample(0:2, 1, prob = c(0.6, 0.3, 0.1))
    } else { # Treatment_B
      weight_change <- rnorm(1, mean = -2.2 * visit_num, sd = 3.5)
      efficacy_score <- rnorm(1, mean = 60 + 8 * visit_num, sd = 15)
      adverse_events <- sample(0:2, 1, prob = c(0.5, 0.35, 0.15))
    }
    
    # Add visit data
    data$weight[i] <- round(patient_baseline$baseline_weight + weight_change, 1)
    data$efficacy_score[i] <- round(pmax(pmin(efficacy_score, 100), 0), 1)
    data$adverse_events[i] <- adverse_events
    data$lab_value_1[i] <- round(rnorm(1, mean = 100, sd = 20), 1)
    data$lab_value_2[i] <- round(rnorm(1, mean = 50, sd = 15), 1)
    data$biomarker[i] <- round(rnorm(1, mean = 200, sd = 50), 2)
  }
  
  # Introduce realistic missing data patterns
  
  # 1. MCAR pattern - random missing across all variables
  mcar_rate <- 0.05
  for (var in c("weight", "efficacy_score", "lab_value_1")) {
    missing_indices <- sample(1:nrow(data), size = floor(nrow(data) * mcar_rate))
    data[[var]][missing_indices] <- NA
  }
  
  # 2. MAR pattern - missing depends on observed variables
  # Older patients more likely to have missing lab values
  older_patients <- which(data$age > 65)
  lab_missing <- sample(older_patients, size = floor(length(older_patients) * 0.3))
  data$lab_value_2[lab_missing] <- NA
  
  # Patients with adverse events more likely to have missing efficacy scores
  adverse_patients <- which(data$adverse_events > 0)
  efficacy_missing <- sample(adverse_patients, size = floor(length(adverse_patients) * 0.4))
  data$efficacy_score[efficacy_missing] <- NA
  
  # 3. MNAR pattern - missing depends on unobserved values
  # High biomarker values more likely to be missing (patient discomfort)
  high_biomarker <- which(data$biomarker > 240)
  biomarker_missing <- sample(high_biomarker, size = floor(length(high_biomarker) * 0.6))
  data$biomarker[biomarker_missing] <- NA
  
  # 4. Dropout pattern - later visits more likely to be missing
  for (patient in unique(data$patient_id)) {
    patient_data <- data[data$patient_id == patient, ]
    
    # Probability of dropout increases with visit number
    dropout_prob <- c(0.02, 0.08, 0.15, 0.25)
    
    for (visit in 2:4) {
      if (runif(1) < dropout_prob[visit]) {
        # Patient drops out - all subsequent visits are missing
        dropout_rows <- which(data$patient_id == patient & data$visit >= visit)
        data$weight[dropout_rows] <- NA
        data$efficacy_score[dropout_rows] <- NA
        data$lab_value_1[dropout_rows] <- NA
        data$lab_value_2[dropout_rows] <- NA
        data$biomarker[dropout_rows] <- NA
        break
      }
    }
  }
  
  return(data)
}

# Dataset 2: Cross-sectional Survey Data with Missing Responses
create_survey_data <- function() {
  n_respondents <- 500
  
  # Create respondent characteristics
  data <- data.frame(
    respondent_id = paste0("R", sprintf("%04d", 1:n_respondents)),
    age = round(rnorm(n_respondents, mean = 45, sd = 18)),
    education = sample(c("High School", "Bachelor", "Master", "PhD"), 
                      n_respondents, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
    income_bracket = sample(c("Low", "Medium", "High"), 
                           n_respondents, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    employment_status = sample(c("Employed", "Unemployed", "Retired", "Student"), 
                              n_respondents, replace = TRUE, prob = c(0.6, 0.1, 0.2, 0.1)),
    stringsAsFactors = FALSE
  )
  
  # Ensure realistic age ranges
  data$age <- pmax(pmin(data$age, 85), 18)
  
  # Add survey responses (1-10 scale)
  for (i in 1:nrow(data)) {
    # Satisfaction scores vary by demographics
    base_satisfaction <- case_when(
      data$income_bracket[i] == "High" ~ 7.5,
      data$income_bracket[i] == "Medium" ~ 6.5,
      TRUE ~ 5.5
    )
    
    # Education effect
    education_effect <- case_when(
      data$education[i] == "PhD" ~ 0.8,
      data$education[i] == "Master" ~ 0.4,
      data$education[i] == "Bachelor" ~ 0.2,
      TRUE ~ 0
    )
    
    data$satisfaction_overall[i] <- round(pmax(pmin(
      rnorm(1, mean = base_satisfaction + education_effect, sd = 1.5), 10), 1), 1)
    
    data$satisfaction_service[i] <- round(pmax(pmin(
      rnorm(1, mean = base_satisfaction + education_effect - 0.3, sd = 1.2), 10), 1), 1)
    
    data$satisfaction_quality[i] <- round(pmax(pmin(
      rnorm(1, mean = base_satisfaction + education_effect + 0.2, sd = 1.3), 10), 1), 1)
    
    data$satisfaction_price[i] <- round(pmax(pmin(
      rnorm(1, mean = base_satisfaction + education_effect - 1.0, sd = 1.8), 10), 1), 1)
    
    data$likelihood_recommend[i] <- round(pmax(pmin(
      rnorm(1, mean = base_satisfaction + education_effect - 0.5, sd = 1.6), 10), 1), 1)
  }
  
  # Introduce missing data patterns
  
  # 1. Item non-response - sensitive questions more likely to be missing
  # Income-related questions have higher missingness
  income_missing <- sample(1:n_respondents, size = floor(n_respondents * 0.15))
  data$satisfaction_price[income_missing] <- NA
  
  # 2. Skip patterns - unemployed respondents skip work-related questions
  unemployed_indices <- which(data$employment_status == "Unemployed")
  data$satisfaction_service[unemployed_indices] <- NA
  
  # 3. Survey fatigue - later questions more likely to be missing
  fatigue_indices <- sample(1:n_respondents, size = floor(n_respondents * 0.25))
  data$likelihood_recommend[fatigue_indices] <- NA
  
  # 4. Extreme response missing - very dissatisfied respondents less likely to complete
  dissatisfied <- which(data$satisfaction_overall <= 3)
  dropout_dissatisfied <- sample(dissatisfied, size = floor(length(dissatisfied) * 0.4))
  data$satisfaction_quality[dropout_dissatisfied] <- NA
  data$satisfaction_service[dropout_dissatisfied] <- NA
  
  # 5. Random missing across variables
  for (var in c("satisfaction_overall", "satisfaction_service", "satisfaction_quality")) {
    random_missing <- sample(1:n_respondents, size = floor(n_respondents * 0.08))
    data[[var]][random_missing] <- NA
  }
  
  return(data)
}

# Dataset 3: Laboratory Data with Technical Missing Values
create_laboratory_data <- function() {
  n_samples <- 300
  
  # Create sample characteristics
  data <- data.frame(
    sample_id = paste0("S", sprintf("%04d", 1:n_samples)),
    patient_age = round(rnorm(n_samples, mean = 55, sd = 20)),
    sample_type = sample(c("Blood", "Urine", "Tissue"), n_samples, 
                        replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    collection_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                            n_samples, replace = TRUE),
    processing_lab = sample(c("Lab_A", "Lab_B", "Lab_C"), n_samples, 
                           replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    stringsAsFactors = FALSE
  )
  
  # Ensure realistic age ranges
  data$patient_age <- pmax(pmin(data$patient_age, 95), 1)
  
  # Add laboratory measurements
  for (i in 1:nrow(data)) {
    sample_type <- data$sample_type[i]
    
    # Different reference ranges by sample type
    if (sample_type == "Blood") {
      data$glucose[i] <- round(rnorm(1, mean = 95, sd = 20), 1)
      data$cholesterol[i] <- round(rnorm(1, mean = 200, sd = 40), 1)
      data$hemoglobin[i] <- round(rnorm(1, mean = 14, sd = 2), 1)
      data$white_blood_cells[i] <- round(rnorm(1, mean = 7000, sd = 2000), 0)
    } else if (sample_type == "Urine") {
      data$glucose[i] <- round(rnorm(1, mean = 5, sd = 3), 1)
      data$protein[i] <- round(rnorm(1, mean = 15, sd = 8), 1)
      data$creatinine[i] <- round(rnorm(1, mean = 100, sd = 30), 1)
    } else { # Tissue
      data$cell_count[i] <- round(rnorm(1, mean = 50000, sd = 15000), 0)
      data$viability[i] <- round(rnorm(1, mean = 85, sd = 10), 1)
      data$marker_expression[i] <- round(rnorm(1, mean = 60, sd = 20), 1)
    }
  }
  
  # Introduce technical missing data patterns
  
  # 1. Insufficient sample volume - affects multiple measurements
  insufficient_volume <- sample(1:n_samples, size = floor(n_samples * 0.12))
  data$glucose[insufficient_volume] <- NA
  data$cholesterol[insufficient_volume] <- NA
  
  # 2. Equipment failure - affects specific labs on specific dates
  equipment_failure_dates <- sample(unique(data$collection_date), size = 10)
  for (date in equipment_failure_dates) {
    affected_samples <- which(data$collection_date == date & data$processing_lab == "Lab_B")
    data$hemoglobin[affected_samples] <- NA
  }
  
  # 3. Contamination - affects tissue samples more
  tissue_samples <- which(data$sample_type == "Tissue")
  contaminated <- sample(tissue_samples, size = floor(length(tissue_samples) * 0.2))
  data$cell_count[contaminated] <- NA
  data$viability[contaminated] <- NA
  
  # 4. Below detection limit - affects sensitive measurements
  below_detection <- which(data$glucose < 10 & !is.na(data$glucose))
  data$glucose[below_detection] <- NA
  
  # 5. Age-related measurement difficulties
  elderly_samples <- which(data$patient_age > 80)
  difficult_draw <- sample(elderly_samples, size = floor(length(elderly_samples) * 0.3))
  data$white_blood_cells[difficult_draw] <- NA
  
  return(data)
}

# Dataset 4: Longitudinal Patient Registry Data
create_patient_registry_data <- function() {
  n_patients <- 150
  max_follow_up <- 5  # years
  
  # Create patient baseline data
  patients <- data.frame(
    patient_id = paste0("PT", sprintf("%04d", 1:n_patients)),
    enrollment_date = sample(seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"), 
                            n_patients, replace = TRUE),
    age_at_enrollment = round(rnorm(n_patients, mean = 62, sd = 15)),
    gender = sample(c("Male", "Female"), n_patients, replace = TRUE),
    diagnosis = sample(c("Type_1", "Type_2", "Type_3"), n_patients, 
                      replace = TRUE, prob = c(0.2, 0.5, 0.3)),
    severity_score = round(rnorm(n_patients, mean = 3.5, sd = 1.2), 1),
    stringsAsFactors = FALSE
  )
  
  # Ensure realistic ranges
  patients$age_at_enrollment <- pmax(pmin(patients$age_at_enrollment, 85), 18)
  patients$severity_score <- pmax(pmin(patients$severity_score, 10), 0)
  
  # Create longitudinal follow-up data
  follow_up_data <- data.frame()
  
  for (i in 1:nrow(patients)) {
    patient <- patients[i, ]
    
    # Determine follow-up duration (related to severity and age)
    base_follow_up <- max_follow_up
    if (patient$severity_score > 7) base_follow_up <- base_follow_up * 0.6
    if (patient$age_at_enrollment > 75) base_follow_up <- base_follow_up * 0.7
    
    actual_follow_up <- runif(1, min = 0.5, max = base_follow_up)
    n_visits <- max(2, round(actual_follow_up * 2))  # Bi-annual visits
    
    # Create visit data
    visit_dates <- seq(from = patient$enrollment_date, 
                      by = paste(round(365 / 2), "days"), 
                      length.out = n_visits)
    
    for (visit in 1:n_visits) {
      visit_date <- visit_dates[visit]
      years_since_enrollment <- as.numeric(visit_date - patient$enrollment_date) / 365.25
      
      # Simulate disease progression
      progression_effect <- years_since_enrollment * 0.5
      
      # Add noise and individual variation
      outcome_score <- round(rnorm(1, mean = 75 - progression_effect * 10, sd = 15), 1)
      functional_score <- round(rnorm(1, mean = 80 - progression_effect * 8, sd = 12), 1)
      quality_of_life <- round(rnorm(1, mean = 70 - progression_effect * 6, sd = 18), 1)
      
      # Ensure realistic ranges
      outcome_score <- pmax(pmin(outcome_score, 100), 0)
      functional_score <- pmax(pmin(functional_score, 100), 0)
      quality_of_life <- pmax(pmin(quality_of_life, 100), 0)
      
      visit_data <- data.frame(
        patient_id = patient$patient_id,
        visit_number = visit,
        visit_date = visit_date,
        years_since_enrollment = years_since_enrollment,
        outcome_score = outcome_score,
        functional_score = functional_score,
        quality_of_life = quality_of_life,
        stringsAsFactors = FALSE
      )
      
      follow_up_data <- rbind(follow_up_data, visit_data)
    }
  }
  
  # Merge with baseline data
  data <- merge(patients, follow_up_data, by = "patient_id")
  data <- data[order(data$patient_id, data$visit_number), ]
  
  # Introduce missing data patterns
  
  # 1. Informative dropout - sicker patients more likely to drop out
  for (patient_id in unique(data$patient_id)) {
    patient_data <- data[data$patient_id == patient_id, ]
    baseline_severity <- patient_data$severity_score[1]
    
    # Higher severity = higher dropout probability
    dropout_prob <- pmax(0.1, baseline_severity / 10 * 0.4)
    
    if (runif(1) < dropout_prob) {
      dropout_visit <- sample(2:max(patient_data$visit_number), 1)
      dropout_indices <- which(data$patient_id == patient_id & data$visit_number >= dropout_visit)
      
      data$outcome_score[dropout_indices] <- NA
      data$functional_score[dropout_indices] <- NA
      data$quality_of_life[dropout_indices] <- NA
    }
  }
  
  # 2. Intermittent missing - patients miss specific visits
  for (patient_id in unique(data$patient_id)) {
    patient_data <- data[data$patient_id == patient_id, ]
    n_visits <- nrow(patient_data)
    
    if (n_visits > 3) {
      # Randomly miss 1-2 visits
      missed_visits <- sample(2:(n_visits-1), size = min(2, n_visits-2))
      missed_indices <- which(data$patient_id == patient_id & data$visit_number %in% missed_visits)
      
      # Randomly miss some measurements
      if (runif(1) < 0.3) data$outcome_score[missed_indices] <- NA
      if (runif(1) < 0.2) data$functional_score[missed_indices] <- NA
      if (runif(1) < 0.4) data$quality_of_life[missed_indices] <- NA
    }
  }
  
  # 3. Measurement-specific missing
  # Quality of life more likely to be missing in later visits
  later_visits <- which(data$visit_number > 3)
  qol_missing <- sample(later_visits, size = floor(length(later_visits) * 0.3))
  data$quality_of_life[qol_missing] <- NA
  
  return(data)
}

# Dataset 5: Multi-center Clinical Study Data
create_multicenter_data <- function() {
  n_centers <- 8
  n_patients_per_center <- 75
  total_patients <- n_centers * n_patients_per_center
  
  # Create center characteristics
  centers <- data.frame(
    center_id = paste0("C", sprintf("%02d", 1:n_centers)),
    country = sample(c("USA", "Germany", "Japan", "Brazil", "India", "UK", "France", "Canada"), 
                    n_centers, replace = FALSE),
    experience_level = sample(c("High", "Medium", "Low"), n_centers, 
                             replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    data_quality_score = round(rnorm(n_centers, mean = 8.5, sd = 1.2), 1),
    stringsAsFactors = FALSE
  )
  
  # Ensure realistic quality scores
  centers$data_quality_score <- pmax(pmin(centers$data_quality_score, 10), 5)
  
  # Create patient data
  data <- data.frame()
  
  for (center in 1:n_centers) {
    center_info <- centers[center, ]
    
    # Create patients for this center
    center_patients <- data.frame(
      patient_id = paste0(center_info$center_id, "_P", sprintf("%03d", 1:n_patients_per_center)),
      center_id = center_info$center_id,
      country = center_info$country,
      age = round(rnorm(n_patients_per_center, mean = 60, sd = 12)),
      gender = sample(c("Male", "Female"), n_patients_per_center, replace = TRUE),
      stringsAsFactors = FALSE
    )
    
    # Add measurements with center-specific patterns
    for (i in 1:nrow(center_patients)) {
      # Center quality affects measurement precision
      quality_factor <- center_info$data_quality_score / 10
      
      # Primary endpoint
      center_patients$primary_endpoint[i] <- round(rnorm(1, mean = 65, sd = 15 / quality_factor), 1)
      
      # Secondary endpoints
      center_patients$secondary_endpoint_1[i] <- round(rnorm(1, mean = 45, sd = 12 / quality_factor), 1)
      center_patients$secondary_endpoint_2[i] <- round(rnorm(1, mean = 30, sd = 10 / quality_factor), 1)
      
      # Safety measurements
      center_patients$safety_parameter_1[i] <- round(rnorm(1, mean = 100, sd = 20 / quality_factor), 1)
      center_patients$safety_parameter_2[i] <- round(rnorm(1, mean = 75, sd = 15 / quality_factor), 1)
      
      # Biomarker
      center_patients$biomarker_level[i] <- round(rnorm(1, mean = 200, sd = 50 / quality_factor), 2)
    }
    
    data <- rbind(data, center_patients)
  }
  
  # Introduce center-specific missing data patterns
  
  # 1. Data quality related missing - lower quality centers have more missing data
  for (center_id in unique(data$center_id)) {
    center_data <- data[data$center_id == center_id, ]
    quality_score <- centers$data_quality_score[centers$center_id == center_id]
    
    # Lower quality = higher missing rate
    missing_rate <- (10 - quality_score) / 10 * 0.4
    
    center_indices <- which(data$center_id == center_id)
    
    # Apply missing pattern to different variables
    for (var in c("primary_endpoint", "secondary_endpoint_1", "secondary_endpoint_2")) {
      missing_indices <- sample(center_indices, size = floor(length(center_indices) * missing_rate))
      data[[var]][missing_indices] <- NA
    }
  }
  
  # 2. Regulatory differences - some countries don't collect certain data
  # Japan doesn't collect secondary_endpoint_2
  japan_indices <- which(data$country == "Japan")
  data$secondary_endpoint_2[japan_indices] <- NA
  
  # Brazil has limited biomarker collection
  brazil_indices <- which(data$country == "Brazil")
  brazil_missing <- sample(brazil_indices, size = floor(length(brazil_indices) * 0.7))
  data$biomarker_level[brazil_missing] <- NA
  
  # 3. Experience level affects data completeness
  low_experience_centers <- centers$center_id[centers$experience_level == "Low"]
  low_exp_indices <- which(data$center_id %in% low_experience_centers)
  
  # More missing in safety parameters
  safety_missing <- sample(low_exp_indices, size = floor(length(low_exp_indices) * 0.25))
  data$safety_parameter_1[safety_missing] <- NA
  data$safety_parameter_2[safety_missing] <- NA
  
  # 4. Random missing across all centers
  for (var in c("primary_endpoint", "secondary_endpoint_1", "safety_parameter_1")) {
    random_missing <- sample(1:nrow(data), size = floor(nrow(data) * 0.05))
    data[[var]][random_missing] <- NA
  }
  
  return(data)
}

# Generate all datasets and save them
cat("Generating missing data test datasets...\n")

# Generate datasets
clinical_trial_data <- create_clinical_trial_data()
survey_data <- create_survey_data()
laboratory_data <- create_laboratory_data()
patient_registry_data <- create_patient_registry_data()
multicenter_data <- create_multicenter_data()

# Save datasets
save(clinical_trial_data, file = "data/missingdata_clinical_trial.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_trial_data, "data/missingdata_clinical_trial.RData")
  message("✓ Created missingdata_clinical_trial.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_trial_data, "data/missingdata_clinical_trial.RData")
  message("✓ Created missingdata_clinical_trial.RData")
}
save(survey_data, file = "data/missingdata_survey.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(survey_data, "data/missingdata_survey.RData")
  message("✓ Created missingdata_survey.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(survey_data, "data/missingdata_survey.RData")
  message("✓ Created missingdata_survey.RData")
}
save(laboratory_data, file = "data/missingdata_laboratory.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(laboratory_data, "data/missingdata_laboratory.RData")
  message("✓ Created missingdata_laboratory.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(laboratory_data, "data/missingdata_laboratory.RData")
  message("✓ Created missingdata_laboratory.RData")
}
save(patient_registry_data, file = "data/missingdata_patient_registry.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(patient_registry_data, "data/missingdata_patient_registry.RData")
  message("✓ Created missingdata_patient_registry.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(patient_registry_data, "data/missingdata_patient_registry.RData")
  message("✓ Created missingdata_patient_registry.RData")
}
save(multicenter_data, file = "data/missingdata_multicenter.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(multicenter_data, "data/missingdata_multicenter.RData")
  message("✓ Created missingdata_multicenter.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(multicenter_data, "data/missingdata_multicenter.RData")
  message("✓ Created missingdata_multicenter.RData")
}

# Print summaries
cat("\n=== MISSING DATA TEST GENERATION SUMMARY ===\n")

cat("\n1. Clinical Trial Data (N =", nrow(clinical_trial_data), "observations):\n")
cat("   - Longitudinal data with", length(unique(clinical_trial_data$patient_id)), "patients\n")
cat("   - Missing data patterns: MCAR, MAR, MNAR, and dropout\n")
cat("   - Variables with missing data:\n")
missing_vars <- names(clinical_trial_data)[sapply(clinical_trial_data, function(x) any(is.na(x)))]
for (var in missing_vars) {
  missing_pct <- round(sum(is.na(clinical_trial_data[[var]])) / nrow(clinical_trial_data) * 100, 1)
  cat("     -", var, ":", missing_pct, "%\n")
}

cat("\n2. Survey Data (N =", nrow(survey_data), "respondents):\n")
cat("   - Cross-sectional survey with item non-response\n")
cat("   - Missing data patterns: item non-response, skip patterns, survey fatigue\n")
cat("   - Variables with missing data:\n")
missing_vars <- names(survey_data)[sapply(survey_data, function(x) any(is.na(x)))]
for (var in missing_vars) {
  missing_pct <- round(sum(is.na(survey_data[[var]])) / nrow(survey_data) * 100, 1)
  cat("     -", var, ":", missing_pct, "%\n")
}

cat("\n3. Laboratory Data (N =", nrow(laboratory_data), "samples):\n")
cat("   - Technical missing data from laboratory processes\n")
cat("   - Missing data patterns: insufficient volume, equipment failure, contamination\n")
cat("   - Variables with missing data:\n")
missing_vars <- names(laboratory_data)[sapply(laboratory_data, function(x) any(is.na(x)))]
for (var in missing_vars) {
  missing_pct <- round(sum(is.na(laboratory_data[[var]])) / nrow(laboratory_data) * 100, 1)
  cat("     -", var, ":", missing_pct, "%\n")
}

cat("\n4. Patient Registry Data (N =", nrow(patient_registry_data), "visits):\n")
cat("   - Longitudinal registry with", length(unique(patient_registry_data$patient_id)), "patients\n")
cat("   - Missing data patterns: informative dropout, intermittent missing\n")
cat("   - Variables with missing data:\n")
missing_vars <- names(patient_registry_data)[sapply(patient_registry_data, function(x) any(is.na(x)))]
for (var in missing_vars) {
  missing_pct <- round(sum(is.na(patient_registry_data[[var]])) / nrow(patient_registry_data) * 100, 1)
  cat("     -", var, ":", missing_pct, "%\n")
}

cat("\n5. Multi-center Data (N =", nrow(multicenter_data), "patients):\n")
cat("   - Multi-center study with", length(unique(multicenter_data$center_id)), "centers\n")
cat("   - Missing data patterns: center quality, regulatory differences\n")
cat("   - Variables with missing data:\n")
missing_vars <- names(multicenter_data)[sapply(multicenter_data, function(x) any(is.na(x)))]
for (var in missing_vars) {
  missing_pct <- round(sum(is.na(multicenter_data[[var]])) / nrow(multicenter_data) * 100, 1)
  cat("     -", var, ":", missing_pct, "%\n")
}

# Create combined examples for documentation
cat("\n=== EXAMPLE USAGE SCENARIOS ===\n")

cat("\n1. Clinical Trial Missing Data Analysis:\n")
cat("   missingdata(data = clinical_trial_data, analysis_vars = c('weight', 'efficacy_score', 'lab_value_1', 'biomarker'))\n")

cat("\n2. Survey Non-response Analysis:\n")
cat("   missingdata(data = survey_data, analysis_vars = c('satisfaction_overall', 'satisfaction_service', 'satisfaction_quality'))\n")

cat("\n3. Laboratory Technical Missing Analysis:\n")
cat("   missingdata(data = laboratory_data, analysis_vars = c('glucose', 'cholesterol', 'hemoglobin'))\n")

cat("\n4. Patient Registry Dropout Analysis:\n")
cat("   missingdata(data = patient_registry_data, analysis_vars = c('outcome_score', 'functional_score', 'quality_of_life'))\n")

cat("\n5. Multi-center Data Quality Analysis:\n")
cat("   missingdata(data = multicenter_data, analysis_vars = c('primary_endpoint', 'secondary_endpoint_1', 'biomarker_level'))\n")

cat("\n=== DATA GENERATION COMPLETED ===\n")

# Print file locations
cat("\nFiles saved to:\n")
cat("- data/missingdata_clinical_trial.RData\n")
cat("- data/missingdata_survey.RData\n")
cat("- data/missingdata_laboratory.RData\n")
cat("- data/missingdata_patient_registry.RData\n")
cat("- data/missingdata_multicenter.RData\n")

cat("\nReady for use in missing data analysis testing and documentation!\n")
