# Test data generation for lollipop function
# Creates realistic clinical and research datasets for lollipop chart analysis

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Dataset 1: Patient Biomarker Levels
create_biomarker_data <- function() {
  n_patients <- 25
  
  # Create patient IDs
  patient_ids <- paste0("P", sprintf("%03d", 1:n_patients))
  
  # Generate biomarker levels with clinical realism
  set.seed(123)
  
  # Base biomarker levels with some patients having elevated levels
  biomarker_base <- c(
    rnorm(15, mean = 35, sd = 8),    # Normal range patients
    rnorm(7, mean = 65, sd = 12),    # Elevated patients
    rnorm(3, mean = 95, sd = 15)     # High-risk patients
  )
  
  # Ensure realistic ranges (5-150 ng/mL)
  biomarker_levels <- round(pmax(pmin(biomarker_base, 150), 5), 1)
  
  # Risk categorization based on biomarker levels
  risk_category <- case_when(
    biomarker_levels < 40 ~ "Low",
    biomarker_levels < 70 ~ "Medium",
    TRUE ~ "High"
  )
  
  # Patient demographics
  age <- round(rnorm(n_patients, mean = 62, sd = 12))
  gender <- sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.45, 0.55))
  
  # Create final dataset
  data <- data.frame(
    patient_id = patient_ids,
    biomarker_level = biomarker_levels,
    risk_category = risk_category,
    age = age,
    gender = gender,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Dataset 2: Treatment Response Scores
create_treatment_response_data <- function() {
  n_treatments <- 8
  
  # Treatment names
  treatments <- c("Chemotherapy_A", "Chemotherapy_B", "Immunotherapy_C", 
                  "Targeted_Therapy_D", "Combination_E", "Radiation_F", 
                  "Hormone_Therapy_G", "Experimental_H")
  
  # Generate response scores with treatment-specific patterns
  set.seed(456)
  
  # Different treatments have different expected response rates
  response_means <- c(45, 52, 78, 68, 82, 38, 61, 73)
  response_sds <- c(15, 12, 18, 14, 20, 13, 16, 22)
  
  response_scores <- round(pmax(pmin(
    mapply(rnorm, 1, response_means, response_sds),
    100), 0), 1)
  
  # Efficacy classification
  efficacy <- case_when(
    response_scores >= 75 ~ "High",
    response_scores >= 50 ~ "Medium",
    TRUE ~ "Low"
  )
  
  # Treatment costs (in thousands)
  treatment_costs <- round(runif(n_treatments, min = 15, max = 150), 1)
  
  # Side effect severity
  side_effects <- sample(c("Mild", "Moderate", "Severe"), n_treatments, 
                        replace = TRUE, prob = c(0.4, 0.4, 0.2))
  
  # Create final dataset
  data <- data.frame(
    treatment = treatments,
    response_score = response_scores,
    efficacy = efficacy,
    cost_thousands = treatment_costs,
    side_effects = side_effects,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Dataset 3: Patient Timeline (Days to Event)
create_patient_timeline_data <- function() {
  n_patients <- 18
  
  # Patient IDs
  patient_ids <- paste0("Patient_", LETTERS[1:n_patients])
  
  # Generate days to event with realistic clinical patterns
  set.seed(789)
  
  # Mix of different event types with different time patterns
  event_types <- sample(c("Response", "Progression", "Stable", "Adverse_Event"), 
                       n_patients, replace = TRUE, prob = c(0.3, 0.25, 0.3, 0.15))
  
  # Days to event based on event type
  days_to_event <- numeric(n_patients)
  for (i in 1:n_patients) {
    if (event_types[i] == "Response") {
      days_to_event[i] <- round(rexp(1, rate = 0.02) + 30)  # Typically 30-200 days
    } else if (event_types[i] == "Progression") {
      days_to_event[i] <- round(rexp(1, rate = 0.015) + 60)  # Typically 60-300 days
    } else if (event_types[i] == "Stable") {
      days_to_event[i] <- round(runif(1, min = 90, max = 365))  # 3-12 months
    } else {
      days_to_event[i] <- round(rexp(1, rate = 0.05) + 14)  # Typically 14-100 days
    }
  }
  
  # Ensure realistic range (7-365 days)
  days_to_event <- pmax(pmin(days_to_event, 365), 7)
  
  # Treatment arms
  treatment_arm <- sample(c("Control", "Experimental"), n_patients, 
                         replace = TRUE, prob = c(0.5, 0.5))
  
  # Disease stage
  disease_stage <- sample(c("I", "II", "III", "IV"), n_patients, 
                         replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.2))
  
  # Create final dataset
  data <- data.frame(
    patient_id = patient_ids,
    days_to_event = days_to_event,
    event_type = event_types,
    treatment_arm = treatment_arm,
    disease_stage = disease_stage,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Dataset 4: Survey Response Analysis
create_survey_response_data <- function() {
  n_questions <- 20
  
  # Question categories and labels
  question_categories <- c(
    rep("Service_Quality", 5),
    rep("Treatment_Efficacy", 5),
    rep("Staff_Interaction", 5),
    rep("Facility_Environment", 5)
  )
  
  question_labels <- c(
    paste0("Service_Q", 1:5),
    paste0("Treatment_Q", 1:5),
    paste0("Staff_Q", 1:5),
    paste0("Facility_Q", 1:5)
  )
  
  # Generate satisfaction scores with category-specific patterns
  set.seed(234)
  
  # Different categories have different satisfaction patterns
  satisfaction_scores <- numeric(n_questions)
  for (i in 1:n_questions) {
    if (question_categories[i] == "Service_Quality") {
      satisfaction_scores[i] <- round(rnorm(1, mean = 7.2, sd = 1.8), 1)
    } else if (question_categories[i] == "Treatment_Efficacy") {
      satisfaction_scores[i] <- round(rnorm(1, mean = 8.1, sd = 1.3), 1)
    } else if (question_categories[i] == "Staff_Interaction") {
      satisfaction_scores[i] <- round(rnorm(1, mean = 8.7, sd = 1.1), 1)
    } else {
      satisfaction_scores[i] <- round(rnorm(1, mean = 6.9, sd = 2.1), 1)
    }
  }
  
  # Ensure scores are within 1-10 range
  satisfaction_scores <- pmax(pmin(satisfaction_scores, 10), 1)
  
  # Response rates (percentage of patients who responded)
  response_rates <- round(runif(n_questions, min = 65, max = 95), 1)
  
  # Priority level for improvement
  priority_level <- case_when(
    satisfaction_scores >= 8 ~ "Low",
    satisfaction_scores >= 6 ~ "Medium",
    TRUE ~ "High"
  )
  
  # Create final dataset
  data <- data.frame(
    question = question_labels,
    satisfaction_score = satisfaction_scores,
    category = question_categories,
    response_rate = response_rates,
    priority_level = priority_level,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Dataset 5: Quality Metrics Dashboard
create_quality_metrics_data <- function() {
  n_metrics <- 16
  
  # Metric names for different model types
  metric_names <- c(
    "Accuracy", "Precision", "Recall", "F1_Score", "AUC", "Specificity", 
    "Sensitivity", "PPV", "NPV", "Balanced_Accuracy", "Cohen_Kappa",
    "Matthews_Correlation", "Log_Loss", "Brier_Score", "Calibration_Error",
    "ROC_AUC"
  )
  
  # Generate realistic performance metrics
  set.seed(567)
  
  # Different metrics have different typical ranges
  metric_values <- numeric(n_metrics)
  for (i in 1:n_metrics) {
    metric_name <- metric_names[i]
    
    if (metric_name %in% c("Accuracy", "Precision", "Recall", "F1_Score", 
                          "AUC", "Specificity", "Sensitivity", "PPV", "NPV", 
                          "Balanced_Accuracy", "ROC_AUC")) {
      # Performance metrics (0-1 range, higher is better)
      metric_values[i] <- round(rbeta(1, shape1 = 7, shape2 = 2), 3)
    } else if (metric_name == "Cohen_Kappa") {
      # Kappa coefficient (-1 to 1, higher is better)
      metric_values[i] <- round(rbeta(1, shape1 = 6, shape2 = 3) * 2 - 1, 3)
    } else if (metric_name == "Matthews_Correlation") {
      # Matthews correlation (-1 to 1, higher is better)
      metric_values[i] <- round(rbeta(1, shape1 = 5, shape2 = 4) * 2 - 1, 3)
    } else if (metric_name %in% c("Log_Loss", "Brier_Score", "Calibration_Error")) {
      # Error metrics (0-1 range, lower is better)
      metric_values[i] <- round(rbeta(1, shape1 = 2, shape2 = 6), 3)
    }
  }
  
  # Ensure realistic ranges
  metric_values <- pmax(pmin(metric_values, 1), -1)
  
  # Model types
  model_types <- sample(c("Random_Forest", "SVM", "Neural_Network", "Logistic_Regression"), 
                       n_metrics, replace = TRUE)
  
  # Confidence intervals (simulated)
  ci_lower <- round(pmax(metric_values - runif(n_metrics, 0.02, 0.08), 
                        ifelse(metric_names %in% c("Cohen_Kappa", "Matthews_Correlation"), -1, 0)), 3)
  ci_upper <- round(pmin(metric_values + runif(n_metrics, 0.02, 0.08), 1), 3)
  
  # Performance category
  performance_category <- case_when(
    metric_names %in% c("Log_Loss", "Brier_Score", "Calibration_Error") ~ 
      case_when(
        metric_values <= 0.3 ~ "Excellent",
        metric_values <= 0.6 ~ "Good",
        TRUE ~ "Poor"
      ),
    TRUE ~ case_when(
      metric_values >= 0.8 ~ "Excellent",
      metric_values >= 0.6 ~ "Good",
      TRUE ~ "Poor"
    )
  )
  
  # Create final dataset
  data <- data.frame(
    metric = metric_names,
    value = metric_values,
    model_type = model_types,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    performance_category = performance_category,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Generate all datasets and save them
cat("Generating lollipop test datasets...\n")

# Generate datasets
biomarker_data <- create_biomarker_data()
treatment_response_data <- create_treatment_response_data()
patient_timeline_data <- create_patient_timeline_data()
survey_response_data <- create_survey_response_data()
quality_metrics_data <- create_quality_metrics_data()

# Save datasets
save(biomarker_data, file = "data/lollipop_biomarker_data.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(biomarker_data, "data/lollipop_biomarker_data.RData")
  message("✓ Created lollipop_biomarker_data.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(biomarker_data, "data/lollipop_biomarker_data.RData")
  message("✓ Created lollipop_biomarker_data.RData")
}
save(treatment_response_data, file = "data/lollipop_treatment_response_data.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(treatment_response_data, "data/lollipop_treatment_response_data.RData")
  message("✓ Created lollipop_treatment_response_data.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(treatment_response_data, "data/lollipop_treatment_response_data.RData")
  message("✓ Created lollipop_treatment_response_data.RData")
}
save(patient_timeline_data, file = "data/lollipop_patient_timeline_data.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(patient_timeline_data, "data/lollipop_patient_timeline_data.RData")
  message("✓ Created lollipop_patient_timeline_data.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(patient_timeline_data, "data/lollipop_patient_timeline_data.RData")
  message("✓ Created lollipop_patient_timeline_data.RData")
}
save(survey_response_data, file = "data/lollipop_survey_response_data.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(survey_response_data, "data/lollipop_survey_response_data.RData")
  message("✓ Created lollipop_survey_response_data.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(survey_response_data, "data/lollipop_survey_response_data.RData")
  message("✓ Created lollipop_survey_response_data.RData")
}
save(quality_metrics_data, file = "data/lollipop_quality_metrics_data.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(quality_metrics_data, "data/lollipop_quality_metrics_data.RData")
  message("✓ Created lollipop_quality_metrics_data.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(quality_metrics_data, "data/lollipop_quality_metrics_data.RData")
  message("✓ Created lollipop_quality_metrics_data.RData")
}

# Print summaries
cat("\n=== LOLLIPOP TEST DATA GENERATION SUMMARY ===\n")

cat("\n1. Biomarker Data (N =", nrow(biomarker_data), "patients):\n")
cat("   - Patient biomarker levels (5-150 ng/mL)\n")
cat("   - Risk categories: Low (", sum(biomarker_data$risk_category == "Low"), 
    "), Medium (", sum(biomarker_data$risk_category == "Medium"), 
    "), High (", sum(biomarker_data$risk_category == "High"), ")\n")
cat("   - Mean biomarker level:", round(mean(biomarker_data$biomarker_level), 1), "ng/mL\n")

cat("\n2. Treatment Response Data (N =", nrow(treatment_response_data), "treatments):\n")
cat("   - Response scores (0-100):\n")
cat("   - High efficacy:", sum(treatment_response_data$efficacy == "High"), "treatments\n")
cat("   - Medium efficacy:", sum(treatment_response_data$efficacy == "Medium"), "treatments\n")
cat("   - Low efficacy:", sum(treatment_response_data$efficacy == "Low"), "treatments\n")
cat("   - Mean response score:", round(mean(treatment_response_data$response_score), 1), "\n")

cat("\n3. Patient Timeline Data (N =", nrow(patient_timeline_data), "patients):\n")
cat("   - Days to event range:", min(patient_timeline_data$days_to_event), "-", 
    max(patient_timeline_data$days_to_event), "days\n")
cat("   - Event types:\n")
for (event in unique(patient_timeline_data$event_type)) {
  cat("     -", event, ":", sum(patient_timeline_data$event_type == event), "patients\n")
}
cat("   - Median days to event:", round(median(patient_timeline_data$days_to_event), 0), "days\n")

cat("\n4. Survey Response Data (N =", nrow(survey_response_data), "questions):\n")
cat("   - Satisfaction scores (1-10):\n")
cat("   - Categories:\n")
for (category in unique(survey_response_data$category)) {
  cat("     -", category, ":", sum(survey_response_data$category == category), "questions\n")
}
cat("   - Mean satisfaction score:", round(mean(survey_response_data$satisfaction_score), 1), "\n")

cat("\n5. Quality Metrics Data (N =", nrow(quality_metrics_data), "metrics):\n")
cat("   - Performance metrics (0-1 range mostly):\n")
cat("   - Performance categories:\n")
for (perf in unique(quality_metrics_data$performance_category)) {
  cat("     -", perf, ":", sum(quality_metrics_data$performance_category == perf), "metrics\n")
}
cat("   - Mean metric value:", round(mean(quality_metrics_data$value), 3), "\n")

# Create combined examples for documentation
cat("\n=== EXAMPLE USAGE SCENARIOS ===\n")

cat("\n1. Patient Biomarker Analysis:\n")
cat("   lollipop(data = biomarker_data, dep = 'biomarker_level', group = 'patient_id')\n")

cat("\n2. Treatment Response Comparison:\n")
cat("   lollipop(data = treatment_response_data, dep = 'response_score', group = 'treatment', sortBy = 'value_desc')\n")

cat("\n3. Patient Timeline Visualization:\n")
cat("   lollipop(data = patient_timeline_data, dep = 'days_to_event', group = 'patient_id', showMean = TRUE)\n")

cat("\n4. Survey Response Dashboard:\n")
cat("   lollipop(data = survey_response_data, dep = 'satisfaction_score', group = 'question', orientation = 'horizontal')\n")

cat("\n5. Quality Metrics Dashboard:\n")
cat("   lollipop(data = quality_metrics_data, dep = 'value', group = 'metric', highlight = 'Accuracy')\n")

cat("\n=== DATA GENERATION COMPLETED ===\n")

# Print file locations
cat("\nFiles saved to:\n")
cat("- data/lollipop_biomarker_data.RData\n")
cat("- data/lollipop_treatment_response_data.RData\n")
cat("- data/lollipop_patient_timeline_data.RData\n")
cat("- data/lollipop_survey_response_data.RData\n")
cat("- data/lollipop_quality_metrics_data.RData\n")

cat("\nReady for use in lollipop function testing and documentation!\n")
