# =============================================================================
# Stage Migration Analysis Test Data Generation
# =============================================================================
# 
# Description: Creates comprehensive test data to demonstrate the Will Rogers 
#              phenomenon and evaluate stage migration analysis functions
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# Data includes:
# - Multiple staging systems (TNM 7th vs 8th edition simulation)
# - Cancer-specific staging scenarios (lung, breast, colorectal)
# - Realistic survival patterns with deliberate stage migration
# - Pathology-specific variables and staging criteria
# 
# =============================================================================

# Load required libraries with error checking
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

required_packages <- c("here", "survival", "dplyr", "survival")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed. Please install it with: install.packages('", pkg, "')", sep = ""))
  }
}

set.seed(123) # For reproducibility

# Create output directory if it doesn't exist
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# =============================================================================
# Parameters for Data Generation
# =============================================================================

n_patients <- 800    # Increased sample size for more robust analysis
followup_max <- 120  # Extended follow-up to 10 years

# =============================================================================
# Main Data Generation Function
# =============================================================================

generate_comprehensive_stage_migration_data <- function(n_patients, followup_max) {
  
  # Step 1: Generate patient IDs and comprehensive baseline characteristics
  data <- data.frame(
    patient_id = paste0("PT", sprintf("%04d", 1:n_patients)),
    age = pmax(18, pmin(95, round(rnorm(n_patients, mean = 65, sd = 12)))),
    sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.55, 0.45))),
    
    # Cancer-specific variables
    primary_site = factor(sample(c("Lung", "Breast", "Colorectal", "Prostate", "Other"), 
                                n_patients, replace = TRUE, 
                                prob = c(0.25, 0.20, 0.15, 0.15, 0.25))),
    
    histology = factor(sample(c("Adenocarcinoma", "Squamous_Cell", "Large_Cell", "Other"), 
                             n_patients, replace = TRUE,
                             prob = c(0.50, 0.25, 0.15, 0.10))),
    
    grade = factor(sample(c("Grade_1", "Grade_2", "Grade_3", "Grade_X"), 
                         n_patients, replace = TRUE,
                         prob = c(0.20, 0.40, 0.30, 0.10))),
    
    institution_type = factor(sample(c("Academic", "Community", "Cancer_Center"), 
                                    n_patients, replace = TRUE,
                                    prob = c(0.40, 0.40, 0.20)))
  )
  
  # Step 2: Generate realistic tumor characteristics based on primary site
  # T (tumor size/extent), N (lymph nodes), M (metastasis) components
  data$tumor_size_cm <- round(rnorm(n_patients, mean = 3.2, sd = 1.8), 1)
  data$tumor_size_cm <- pmax(0.1, pmin(15.0, data$tumor_size_cm))
  
  data$lymph_nodes_positive <- rpois(n_patients, lambda = 2)
  data$lymph_nodes_examined <- data$lymph_nodes_positive + rpois(n_patients, lambda = 8)
  data$lymph_nodes_examined <- pmax(data$lymph_nodes_positive, data$lymph_nodes_examined)
  
  # Step 3: Generate true disease severity (underlying continuous variable)
  # This represents the biological aggressiveness that both staging systems try to capture
  age_factor <- pmax(0, (data$age - 50) / 50)  # Age effect
  grade_factor <- as.numeric(data$grade) / 4   # Grade effect
  size_factor <- pmin(1, data$tumor_size_cm / 10)  # Size effect
  node_factor <- pmin(1, data$lymph_nodes_positive / 10)  # Node effect
  
  data$true_severity <- 
    0.3 * age_factor + 
    0.25 * grade_factor + 
    0.25 * size_factor + 
    0.20 * node_factor + 
    rnorm(n_patients, mean = 0, sd = 0.1)
  
  data$true_severity <- pmax(0, pmin(1, data$true_severity))  # Constrain to [0,1]
  
  # Step 4: Generate TNM 7th Edition staging (older, less precise)
  # Simulate older staging criteria with broader categories
  tnm_7th_score <- 
    ifelse(data$tumor_size_cm <= 2, 1, 
    ifelse(data$tumor_size_cm <= 5, 2, 
    ifelse(data$tumor_size_cm <= 7, 3, 4))) +
    ifelse(data$lymph_nodes_positive == 0, 0,
    ifelse(data$lymph_nodes_positive <= 3, 1,
    ifelse(data$lymph_nodes_positive <= 9, 2, 3))) +
    sample(c(0, 4), n_patients, replace = TRUE, prob = c(0.85, 0.15))  # Metastasis
  
  data$stage_7th_edition <- factor(
    ifelse(tnm_7th_score <= 2, "Stage_I",
    ifelse(tnm_7th_score <= 4, "Stage_II", 
    ifelse(tnm_7th_score <= 6, "Stage_III", "Stage_IV"))),
    levels = c("Stage_I", "Stage_II", "Stage_III", "Stage_IV")
  )
  
  # Step 5: Generate TNM 8th Edition staging (newer, more precise)
  # Simulate updated staging with refined criteria and additional substages
  
  # Add measurement noise and improved precision
  precise_tumor_size <- data$tumor_size_cm + rnorm(n_patients, mean = 0, sd = 0.2)
  precise_nodes <- data$lymph_nodes_positive + round(rnorm(n_patients, mean = 0, sd = 0.5))
  precise_nodes <- pmax(0, precise_nodes)
  
  # More refined staging criteria (8th edition)
  tnm_8th_score <- 
    ifelse(precise_tumor_size <= 1, 0.5,
    ifelse(precise_tumor_size <= 2, 1,
    ifelse(precise_tumor_size <= 3, 1.5,
    ifelse(precise_tumor_size <= 5, 2,
    ifelse(precise_tumor_size <= 7, 2.5, 3))))) +
    ifelse(precise_nodes == 0, 0,
    ifelse(precise_nodes <= 2, 1,
    ifelse(precise_nodes <= 5, 2,
    ifelse(precise_nodes <= 9, 2.5, 3)))) +
    sample(c(0, 4), n_patients, replace = TRUE, prob = c(0.85, 0.15))  # Metastasis
  
  data$stage_8th_edition <- factor(
    ifelse(tnm_8th_score <= 1, "Stage_IA",
    ifelse(tnm_8th_score <= 2, "Stage_IB",
    ifelse(tnm_8th_score <= 3, "Stage_IIA",
    ifelse(tnm_8th_score <= 4, "Stage_IIB",
    ifelse(tnm_8th_score <= 5, "Stage_IIIA",
    ifelse(tnm_8th_score <= 6, "Stage_IIIB", "Stage_IV")))))),
    levels = c("Stage_IA", "Stage_IB", "Stage_IIA", "Stage_IIB", 
               "Stage_IIIA", "Stage_IIIB", "Stage_IV")
  )
  
  # Step 6: Generate realistic survival times based on true severity and age
  # Use Weibull distribution for more realistic survival patterns
  shape_param <- 1.2  # Weibull shape parameter
  scale_base <- 80    # Base scale parameter (months)
  
  # Survival decreases with true severity and age
  scale_individual <- scale_base * exp(-2 * data$true_severity - 0.01 * (data$age - 65))
  
  data$survival_time <- round(rweibull(n_patients, shape = shape_param, scale = scale_individual))
  data$survival_time <- pmax(1, pmin(data$survival_time, followup_max))  # Constrain to valid range
  
  # Step 7: Generate realistic censoring patterns
  # Administrative censoring + informative censoring
  admin_censor_time <- round(runif(n_patients, min = followup_max * 0.5, max = followup_max))
  
  # Some patients lost to follow-up (more likely if younger or early stage)
  lost_to_followup_prob <- 0.15 - 0.1 * data$true_severity - 0.002 * (data$age - 40)
  lost_to_followup_prob <- pmax(0.02, pmin(0.25, lost_to_followup_prob))
  
  lost_to_followup <- rbinom(n_patients, 1, lost_to_followup_prob)
  early_censor_time <- round(runif(n_patients, min = 1, max = followup_max * 0.7))
  
  # Determine final event and time
  data$event_time <- ifelse(lost_to_followup == 1, early_censor_time, data$survival_time)
  data$censor_time <- ifelse(lost_to_followup == 1, early_censor_time, admin_censor_time)
  
  data$event <- as.integer(data$event_time <= data$censor_time)
  data$survival_months <- ifelse(data$event == 1, data$event_time, data$censor_time)
  
  # Step 8: Create multiple event format options
  data$vital_status <- factor(
    ifelse(data$event == 1, "Deceased", "Alive"),
    levels = c("Alive", "Deceased")
  )
  
  data$outcome <- factor(
    ifelse(data$event == 1, "Event", "Censored"),
    levels = c("Censored", "Event")
  )
  
  # Step 9: Add treatment variables that might affect staging interpretation
  data$treatment_era <- factor(
    sample(c("Era_2000_2010", "Era_2010_2020"), n_patients, replace = TRUE, prob = c(0.4, 0.6))
  )
  
  data$treatment_type <- factor(
    sample(c("Surgery_Only", "Surgery_Chemo", "Surgery_RT", "Multimodal"), 
           n_patients, replace = TRUE, prob = c(0.25, 0.35, 0.20, 0.20))
  )
  
  # Clean up intermediate variables
  data <- data[, !names(data) %in% c("event_time", "censor_time")]
  
  return(data)
}

# =============================================================================
# Generate Main Test Dataset
# =============================================================================

main_test_data <- generate_comprehensive_stage_migration_data(n_patients, followup_max)

# =============================================================================
# Create Will Rogers Phenomenon Scenarios
# =============================================================================

# Scenario 1: Lung Cancer Dataset (Classic TNM 7th vs 8th edition)
lung_data <- main_test_data[main_test_data$primary_site == "Lung", ]

# Add deliberate stage migration to demonstrate Will Rogers phenomenon
# Move some Stage_II patients with better prognosis to Stage_IB (8th edition)
stage_ii_candidates <- which(lung_data$stage_7th_edition == "Stage_II" & 
                            lung_data$true_severity < 0.4)
if(length(stage_ii_candidates) > 0) {
  lung_data$stage_8th_edition[stage_ii_candidates] <- "Stage_IB"
}

# Move some Stage_III patients with worse prognosis to Stage_IIB (8th edition)  
stage_iii_candidates <- which(lung_data$stage_7th_edition == "Stage_III" & 
                             lung_data$true_severity > 0.6)
if(length(stage_iii_candidates) > 0) {
  lung_data$stage_8th_edition[stage_iii_candidates] <- "Stage_IIB"
}

# Scenario 2: Create a simplified version for basic testing
stage_migration_test_data <- data.frame(
  patient_id = main_test_data$patient_id,
  age = main_test_data$age,
  sex = main_test_data$sex,
  primary_site = main_test_data$primary_site,
  tumor_size_cm = main_test_data$tumor_size_cm,
  lymph_nodes_positive = main_test_data$lymph_nodes_positive,
  grade = main_test_data$grade,
  
  # Simplified staging systems for easier testing
  old_stage = factor(
    ifelse(main_test_data$stage_7th_edition %in% c("Stage_I"), "I",
    ifelse(main_test_data$stage_7th_edition %in% c("Stage_II"), "II",
    ifelse(main_test_data$stage_7th_edition %in% c("Stage_III"), "III", "IV"))),
    levels = c("I", "II", "III", "IV")
  ),
  
  new_stage = factor(
    ifelse(main_test_data$stage_8th_edition %in% c("Stage_IA", "Stage_IB"), "I",
    ifelse(main_test_data$stage_8th_edition %in% c("Stage_IIA"), "IIA",
    ifelse(main_test_data$stage_8th_edition %in% c("Stage_IIB"), "IIB",
    ifelse(main_test_data$stage_8th_edition %in% c("Stage_IIIA"), "IIIA",
    ifelse(main_test_data$stage_8th_edition %in% c("Stage_IIIB"), "IIIB", "IV"))))),
    levels = c("I", "IIA", "IIB", "IIIA", "IIIB", "IV")
  ),
  
  # Full staging system names
  tnm_7th_edition = main_test_data$stage_7th_edition,
  tnm_8th_edition = main_test_data$stage_8th_edition,
  
  # Survival data
  survival_months = main_test_data$survival_months,
  event = main_test_data$event,
  vital_status = main_test_data$vital_status,
  
  # Additional variables
  treatment_era = main_test_data$treatment_era,
  institution_type = main_test_data$institution_type
)

# =============================================================================
# Data Quality and Will Rogers Analysis
# =============================================================================

cat("\n=== Stage Migration Test Data Generation Summary ===\n")
cat("Total patients generated:", nrow(stage_migration_test_data), "\n")
cat("Follow-up period:", followup_max, "months\n")
cat("Event rate:", round(mean(stage_migration_test_data$event) * 100, 1), "%\n\n")

# Migration pattern analysis
migration_table <- table(stage_migration_test_data$old_stage, stage_migration_test_data$new_stage)
cat("Stage Migration Pattern (Old → New):\n")
print(migration_table)

# Calculate migration percentages
total_patients <- nrow(stage_migration_test_data)
patients_migrated <- sum(as.character(stage_migration_test_data$old_stage) != as.character(stage_migration_test_data$new_stage))
migration_rate <- round((patients_migrated / total_patients) * 100, 1)

cat("\nMigration Summary:\n")
cat("Patients who changed stages:", patients_migrated, "/", total_patients, 
    "(", migration_rate, "%)\n")
cat("Patients who stayed in same stage:", total_patients - patients_migrated, 
    "(", 100 - migration_rate, "%)\n")

# Survival analysis comparison
cat("\nSurvival Analysis Comparison:\n")

# Median survival by old staging
old_survival <- aggregate(survival_months ~ old_stage, data = stage_migration_test_data, FUN = median)
cat("\nMedian survival by old staging system:\n")
print(old_survival)

# Median survival by new staging  
new_survival <- aggregate(survival_months ~ new_stage, data = stage_migration_test_data, FUN = median)
cat("\nMedian survival by new staging system:\n")
print(new_survival)

# C-index comparison
cox_old <- coxph(Surv(survival_months, event) ~ old_stage, data = stage_migration_test_data)
cox_new <- coxph(Surv(survival_months, event) ~ new_stage, data = stage_migration_test_data)

c_index_old <- summary(cox_old)$concordance[1]
c_index_new <- summary(cox_new)$concordance[1]

cat("\nDiscrimination Performance:\n")
cat("C-index for old staging system:", round(c_index_old, 3), "\n")
cat("C-index for new staging system:", round(c_index_new, 3), "\n")
cat("Improvement in C-index:", round(c_index_new - c_index_old, 3), "\n")

# =============================================================================
# Save Datasets
# =============================================================================

# Save main comprehensive dataset
output_main <- file.path(data_dir, "stage_migration_comprehensive.csv")
write.csv(main_test_data, output_main, row.names = FALSE)

# Save simplified test dataset
output_test <- file.path(data_dir, "stage_migration_test_data.csv")
write.csv(stage_migration_test_data, output_test, row.names = FALSE)

# Save lung cancer specific dataset
output_lung <- file.path(data_dir, "lung_stage_migration_data.csv")
write.csv(lung_data, output_lung, row.names = FALSE)

# Save as RDA files as well
save(main_test_data, file = file.path(data_dir, "stage_migration_comprehensive.rda"))
save(stage_migration_test_data, file = file.path(data_dir, "stage_migration_test_data.rda"))
save(lung_data, file = file.path(data_dir, "lung_stage_migration_data.rda"))

# =============================================================================
# Expected Results Summary
# =============================================================================

cat("\n=== Files Generated ===\n")
cat("1. Comprehensive dataset:", output_main, "\n")
cat("2. Simplified test dataset:", output_test, "\n") 
cat("3. Lung cancer dataset:", output_lung, "\n")

cat("\n=== Expected Will Rogers Phenomenon Evidence ===\n")
cat("- Stage migration rate:", migration_rate, "%\n")
cat("- Improved C-index indicates better discrimination\n")
cat("- Some stages should show improved survival in new system\n")
cat("- Overall survival should remain similar between systems\n")

cat("\n=== Variables for Analysis ===\n")
cat("Old staging system: 'old_stage' or 'tnm_7th_edition'\n")
cat("New staging system: 'new_stage' or 'tnm_8th_edition'\n")
cat("Survival time: 'survival_months'\n")
cat("Event indicator: 'event' (0/1) or 'vital_status' (Alive/Deceased)\n")

cat("\n=== Stage Migration Analysis data generation completed successfully ===\n")
