# Create comprehensive test data for jiwillsurvive function
# This script generates various survival datasets to test all features

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(lubridate)

# Set seed for reproducible data
set.seed(42)

# Function to generate survival times
generate_surv_times <- function(n, hazard_rate = 0.02, max_time = 60) {
  times <- rexp(n, rate = hazard_rate)
  pmin(times, max_time)  # Censor at max_time
}

# Create comprehensive survival test data
n_patients <- 200

jiwillsurvive_test_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("JWS", sprintf("%03d", 1:n_patients)),
  
  # Demographics
  age = round(rnorm(n_patients, mean = 65, sd = 12)),
  sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE)),
  
  # Treatment groups
  treatment = factor(sample(c("Standard", "Experimental", "Control"), 
                           n_patients, replace = TRUE, 
                           prob = c(0.4, 0.35, 0.25))),
  
  # Disease characteristics
  stage = factor(sample(c("I", "II", "III", "IV"), n_patients, replace = TRUE,
                       prob = c(0.2, 0.3, 0.3, 0.2))),
  grade = factor(sample(c("Low", "Intermediate", "High"), n_patients, replace = TRUE,
                       prob = c(0.3, 0.4, 0.3))),
  
  # Biomarkers
  biomarker_status = factor(sample(c("Positive", "Negative"), n_patients, replace = TRUE)),
  
  # Generate survival times based on treatment
  survival_months = case_when(
    1:n_patients %in% 1:n_patients ~ 
      generate_surv_times(n_patients, 
                         hazard_rate = ifelse(sample(c("Standard", "Experimental", "Control"), 
                                                   n_patients, replace = TRUE, 
                                                   prob = c(0.4, 0.35, 0.25)) == "Experimental", 
                                            0.015, 0.025),
                         max_time = 72)
  )
)

# Add event indicators (multiple formats for testing)
jiwillsurvive_test_data <- jiwillsurvive_test_data %>%
  mutate(
    # Event occurred if survival time < 72 months and random event
    event_occurred = ifelse(survival_months < 72 & runif(n_patients) < 0.6, 1, 0),
    
    # Multiple formats of event variable
    death_event = event_occurred,  # Numeric 0/1
    death_logical = as.logical(event_occurred),  # Logical TRUE/FALSE
    death_factor = factor(event_occurred, levels = c(0, 1), labels = c("Alive", "Dead")),  # Factor
    
    # Alternative event coding for testing
    event_status = factor(event_occurred, levels = c(0, 1), labels = c("Censored", "Event")),
    
    # Date-based variables for testing derive_followup functionality
    enrollment_date = as.Date("2018-01-01") + sample(0:730, n_patients, replace = TRUE),
    last_contact_date = enrollment_date + round(survival_months * 30.44),
    
    # Alternative date formats
    start_date_char = as.character(enrollment_date),
    end_date_char = as.character(last_contact_date),
    
    # Additional clinical variables
    performance_status = sample(0:3, n_patients, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    prior_therapy = factor(sample(c("Yes", "No"), n_patients, replace = TRUE)),
    
    # Continuous biomarker for additional testing
    biomarker_level = round(rnorm(n_patients, mean = 10, sd = 3), 2),
    
    # Risk groups
    risk_group = factor(sample(c("Low", "Intermediate", "High"), n_patients, replace = TRUE,
                              prob = c(0.3, 0.4, 0.3)))
  )

# Add some missing values to test robustness
missing_indices <- sample(1:n_patients, round(n_patients * 0.05))
jiwillsurvive_test_data$biomarker_level[missing_indices] <- NA

# Create additional smaller datasets for edge case testing

# Dataset with very short follow-up
short_followup_data <- data.frame(
  patient_id = paste0("SF", sprintf("%02d", 1:50)),
  age = round(rnorm(50, mean = 70, sd = 10)),
  sex = factor(sample(c("Male", "Female"), 50, replace = TRUE)),
  treatment = factor(sample(c("A", "B"), 50, replace = TRUE)),
  survival_days = round(runif(50, min = 1, max = 90)),  # Very short follow-up in days
  death_event = rbinom(50, 1, 0.8),  # High event rate
  enrollment_date = as.Date("2023-01-01") + sample(0:30, 50, replace = TRUE),
  death_date = as.Date("2023-01-01") + sample(31:120, 50, replace = TRUE)
)

# Dataset with very long follow-up and low event rate
long_followup_data <- data.frame(
  patient_id = paste0("LF", sprintf("%02d", 1:50)),
  age = round(rnorm(50, mean = 55, sd = 8)),
  sex = factor(sample(c("Male", "Female"), 50, replace = TRUE)),
  treatment = factor(sample(c("Standard", "Novel"), 50, replace = TRUE)),
  survival_years = round(runif(50, min = 5, max = 15), 1),  # Long follow-up in years
  death_event = rbinom(50, 1, 0.2),  # Low event rate
  study_start = as.Date("2010-01-01") + sample(0:365, 50, replace = TRUE),
  study_end = as.Date("2010-01-01") + sample(1825:5475, 50, replace = TRUE)  # 5-15 years later
)

# Dataset with single group (no grouping variable test)
single_group_data <- data.frame(
  patient_id = paste0("SG", sprintf("%02d", 1:30)),
  age = round(rnorm(30, mean = 60, sd = 15)),
  survival_months = round(runif(30, min = 1, max = 48), 1),
  event = rbinom(30, 1, 0.5),
  diagnosis_date = as.Date("2020-01-01") + sample(0:180, 30, replace = TRUE),
  followup_date = as.Date("2020-01-01") + sample(181:1440, 30, replace = TRUE)
)

# Save all datasets
save(jiwillsurvive_test_data, file = "../data/jiwillsurvive_test_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(jiwillsurvive_test_data, "../data/jiwillsurvive_test_data.omv")
  message("✓ Created jiwillsurvive_test_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(jiwillsurvive_test_data, "../data/jiwillsurvive_test_data.omv")
  message("✓ Created jiwillsurvive_test_data.omv")
}
save(short_followup_data, file = "../data/jiwillsurvive_short_followup.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(short_followup_data, "../data/jiwillsurvive_short_followup.omv")
  message("✓ Created jiwillsurvive_short_followup.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(short_followup_data, "../data/jiwillsurvive_short_followup.omv")
  message("✓ Created jiwillsurvive_short_followup.omv")
}
save(long_followup_data, file = "../data/jiwillsurvive_long_followup.rda") 

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(long_followup_data, "../data/jiwillsurvive_long_followup.omv")
  message("✓ Created jiwillsurvive_long_followup.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(long_followup_data, "../data/jiwillsurvive_long_followup.omv")
  message("✓ Created jiwillsurvive_long_followup.omv")
}
save(single_group_data, file = "../data/jiwillsurvive_single_group.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(single_group_data, "../data/jiwillsurvive_single_group.omv")
  message("✓ Created jiwillsurvive_single_group.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(single_group_data, "../data/jiwillsurvive_single_group.omv")
  message("✓ Created jiwillsurvive_single_group.omv")
}

# Display summary of main dataset
cat("=== jiwillsurvive Test Data Summary ===\n")
cat("Number of patients:", nrow(jiwillsurvive_test_data), "\n")
cat("Age range:", min(jiwillsurvive_test_data$age), "-", max(jiwillsurvive_test_data$age), "\n")
cat("Survival time range:", round(min(jiwillsurvive_test_data$survival_months), 1), "-", 
    round(max(jiwillsurvive_test_data$survival_months), 1), "months\n")
cat("Event rate:", round(mean(jiwillsurvive_test_data$death_event) * 100, 1), "%\n")
cat("Treatment groups:", levels(jiwillsurvive_test_data$treatment), "\n")
cat("Date range:", min(jiwillsurvive_test_data$enrollment_date), "to", 
    max(jiwillsurvive_test_data$last_contact_date), "\n")

# Display structure
str(jiwillsurvive_test_data)
