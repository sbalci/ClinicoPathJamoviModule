# =============================================================================
# Person-Time Analysis Test Data Generation
# =============================================================================
# 
# Description: Generates test data specifically for validating person-time 
#              calculations and incidence rate analysis in jsurvival modules
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# Data includes:
# - Varying follow-up periods to demonstrate person-time concepts
# - Multiple groups with different baseline risks
# - Time-varying exposures and staggered entry
# - Realistic clinical scenarios for person-time analysis
# 
# =============================================================================

# Load required libraries with error checking
required_packages <- c("here", "survival", "dplyr", "lubridate")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed. Please install it with: install.packages('", pkg, "')", sep = ""))
  }
}

set.seed(42) # For reproducibility

# Create output directory if it doesn't exist
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# =============================================================================
# Dataset 1: Basic Person-Time Analysis Data
# =============================================================================

n_patients <- 500
base_date <- as.Date("2020-01-01")

# Generate patient characteristics
person_time_data <- data.frame(
  patient_id = sprintf("PT%04d", 1:n_patients),
  age = round(rnorm(n_patients, mean = 65, sd = 12)),
  sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.55, 0.45))),
  treatment_group = factor(sample(c("Standard", "Experimental"), n_patients, replace = TRUE, prob = c(0.5, 0.5))),
  risk_category = factor(sample(c("Low", "Moderate", "High"), n_patients, replace = TRUE, prob = c(0.3, 0.5, 0.2)))
)

# Add dates with staggered entry (simulating enrollment over time)
person_time_data$enrollment_date <- base_date + sample(0:365, n_patients, replace = TRUE)
person_time_data$study_end_date <- as.Date("2023-12-31")

# Generate realistic follow-up periods
# Baseline hazard varies by treatment group and risk category
baseline_hazard <- 0.002 # base monthly hazard
treatment_effect <- ifelse(person_time_data$treatment_group == "Experimental", 0.7, 1.0) # 30% reduction
risk_multiplier <- case_when(
  person_time_data$risk_category == "Low" ~ 0.5,
  person_time_data$risk_category == "Moderate" ~ 1.0,
  person_time_data$risk_category == "High" ~ 2.0
)

# Age effect (older patients higher risk)
age_effect <- exp(0.02 * (person_time_data$age - 65))

# Calculate individual hazard rates
individual_hazard <- baseline_hazard * treatment_effect * risk_multiplier * age_effect

# Generate time to event using exponential distribution
time_to_event_months <- rexp(n_patients, rate = individual_hazard)

# Calculate follow-up time based on enrollment date and study end
max_possible_followup <- as.numeric(person_time_data$study_end_date - person_time_data$enrollment_date) / 30.44 # Convert to months

# Administrative censoring
followup_months <- pmin(time_to_event_months, max_possible_followup)

# Add some random loss to follow-up (informative censoring)
lost_to_followup <- rbinom(n_patients, 1, 0.15) # 15% loss to follow-up
ltfu_time <- runif(n_patients, min = 1, max = max_possible_followup * 0.8)

# Final follow-up time and event status
person_time_data$followup_months <- ifelse(lost_to_followup == 1, 
                                          pmin(ltfu_time, followup_months),
                                          followup_months)

person_time_data$event <- ifelse(lost_to_followup == 1, 0,
                                ifelse(time_to_event_months <= max_possible_followup, 1, 0))

# Add event status as factor
person_time_data$vital_status <- factor(ifelse(person_time_data$event == 1, "Deceased", "Alive"),
                                       levels = c("Alive", "Deceased"))

# Add date variables for testing date-based calculations
person_time_data$last_contact_date <- person_time_data$enrollment_date + 
  round(person_time_data$followup_months * 30.44)

# =============================================================================
# Dataset 2: Multiple Event Types (Competing Risks)
# =============================================================================

# Generate competing risks data
n_competing <- 300

competing_risks_data <- data.frame(
  patient_id = sprintf("CR%04d", 1:n_competing),
  age = round(rnorm(n_competing, mean = 70, sd = 10)),
  comorbidity_score = round(rnorm(n_competing, mean = 2, sd = 1.5)),
  treatment = factor(sample(c("Surgery", "Medical", "Palliative"), n_competing, 
                           replace = TRUE, prob = c(0.4, 0.4, 0.2)))
)

# Multiple competing events
# Cancer death, other cause death, progression
cancer_death_hazard <- 0.003
other_death_hazard <- 0.001 
progression_hazard <- 0.005

# Risk modifiers
comorbidity_effect <- exp(0.2 * pmax(0, competing_risks_data$comorbidity_score))
age_effect_competing <- exp(0.03 * (competing_risks_data$age - 70))

# Treatment effects for each event type
cancer_death_treatment_effect <- case_when(
  competing_risks_data$treatment == "Surgery" ~ 0.6,
  competing_risks_data$treatment == "Medical" ~ 0.8,
  competing_risks_data$treatment == "Palliative" ~ 1.5,
  TRUE ~ 1.0
)

other_death_treatment_effect <- case_when(
  competing_risks_data$treatment == "Surgery" ~ 1.2,
  competing_risks_data$treatment == "Medical" ~ 1.0,
  competing_risks_data$treatment == "Palliative" ~ 1.5,
  TRUE ~ 1.0
)

progression_treatment_effect <- case_when(
  competing_risks_data$treatment == "Surgery" ~ 0.4,
  competing_risks_data$treatment == "Medical" ~ 0.7,
  competing_risks_data$treatment == "Palliative" ~ 2.0,
  TRUE ~ 1.0
)

# Generate times for each event type
time_cancer_death <- rexp(n_competing, rate = cancer_death_hazard * 
                         cancer_death_treatment_effect * comorbidity_effect * age_effect_competing)
time_other_death <- rexp(n_competing, rate = other_death_hazard * 
                        other_death_treatment_effect * comorbidity_effect * age_effect_competing)
time_progression <- rexp(n_competing, rate = progression_hazard * 
                        progression_treatment_effect * age_effect_competing)

# Administrative censoring at 60 months
admin_censor <- rep(60, n_competing)

# Determine first event
first_event_time <- pmin(time_cancer_death, time_other_death, time_progression, admin_censor)
competing_risks_data$followup_months <- first_event_time

# Determine event type
competing_risks_data$event_type <- factor(
  case_when(
    first_event_time == admin_censor ~ "Censored",
    first_event_time == time_cancer_death ~ "Cancer_Death",
    first_event_time == time_other_death ~ "Other_Death",
    first_event_time == time_progression ~ "Progression",
    TRUE ~ "Censored"
  ),
  levels = c("Censored", "Cancer_Death", "Other_Death", "Progression")
)

# Binary event indicators
competing_risks_data$any_event <- ifelse(competing_risks_data$event_type == "Censored", 0, 1)
competing_risks_data$cancer_death <- ifelse(competing_risks_data$event_type == "Cancer_Death", 1, 0)

# =============================================================================
# Dataset 3: Time Interval Analysis Data
# =============================================================================

# Generate data for testing time interval calculations
n_intervals <- 200

intervals_data <- data.frame(
  patient_id = sprintf("INT%04d", 1:n_intervals),
  diagnosis_date = base_date + sample(-365:0, n_intervals, replace = TRUE), # Diagnosed in past year
  treatment_start = NA,
  progression_date = NA,
  death_date = NA
)

# Treatment starts 1-30 days after diagnosis
intervals_data$treatment_start <- intervals_data$diagnosis_date + 
  sample(1:30, n_intervals, replace = TRUE)

# Generate progression times (some patients progress)
progression_rate <- 0.4
progressed <- rbinom(n_intervals, 1, progression_rate)
time_to_progression <- rexp(n_intervals, rate = 0.01) # About 100 days median
intervals_data$progression_date <- ifelse(progressed == 1, 
                                         intervals_data$treatment_start + time_to_progression,
                                         NA)
intervals_data$progression_date <- as.Date(intervals_data$progression_date, origin = "1970-01-01")

# Generate death times
death_rate <- 0.3
died <- rbinom(n_intervals, 1, death_rate)
time_to_death <- rexp(n_intervals, rate = 0.005) # About 200 days median
intervals_data$death_date <- ifelse(died == 1,
                                   intervals_data$treatment_start + time_to_death,
                                   NA)
intervals_data$death_date <- as.Date(intervals_data$death_date, origin = "1970-01-01")

# Calculate various time intervals
intervals_data$diagnosis_to_treatment_days <- as.numeric(intervals_data$treatment_start - intervals_data$diagnosis_date)

intervals_data$diagnosis_to_progression_days <- ifelse(!is.na(intervals_data$progression_date),
                                                      as.numeric(intervals_data$progression_date - intervals_data$diagnosis_date),
                                                      NA)

intervals_data$diagnosis_to_death_days <- ifelse(!is.na(intervals_data$death_date),
                                                as.numeric(intervals_data$death_date - intervals_data$diagnosis_date),
                                                NA)

# Convert to months for analysis
intervals_data$diagnosis_to_treatment_months <- intervals_data$diagnosis_to_treatment_days / 30.44
intervals_data$diagnosis_to_progression_months <- intervals_data$diagnosis_to_progression_days / 30.44
intervals_data$diagnosis_to_death_months <- intervals_data$diagnosis_to_death_days / 30.44

# =============================================================================
# Save All Datasets
# =============================================================================

# Save main person-time dataset
write.csv(person_time_data, file.path(data_dir, "person_time_test_data.csv"), row.names = FALSE)
save(person_time_data, file = file.path(data_dir, "person_time_test_data.rda"))

# Save competing risks dataset
write.csv(competing_risks_data, file.path(data_dir, "competing_risks_person_time.csv"), row.names = FALSE)
save(competing_risks_data, file = file.path(data_dir, "competing_risks_person_time.rda"))

# Save time intervals dataset
write.csv(intervals_data, file.path(data_dir, "time_intervals_test_data.csv"), row.names = FALSE)
save(intervals_data, file = file.path(data_dir, "time_intervals_test_data.rda"))

# =============================================================================
# Calculate and Display Person-Time Metrics
# =============================================================================

cat("\n=== Person-Time Analysis Test Data Summary ===\n")

# Main dataset analysis
cat("\n1. Main Person-Time Dataset:\n")
cat("   Total patients:", nrow(person_time_data), "\n")
cat("   Total person-months:", round(sum(person_time_data$followup_months), 1), "\n")
cat("   Total events:", sum(person_time_data$event), "\n")
cat("   Overall incidence rate:", round(sum(person_time_data$event) / sum(person_time_data$followup_months) * 100, 2), "per 100 person-months\n")

# By treatment group
treatment_summary <- person_time_data %>%
  group_by(treatment_group) %>%
  summarise(
    n = n(),
    person_months = sum(followup_months),
    events = sum(event),
    rate_per_100 = events / person_months * 100,
    .groups = 'drop'
  )

cat("\n   By Treatment Group:\n")
for(i in 1:nrow(treatment_summary)) {
  cat(sprintf("   %s: %d events in %.1f person-months (rate: %.2f per 100 person-months)\n",
              treatment_summary$treatment_group[i],
              treatment_summary$events[i],
              treatment_summary$person_months[i],
              treatment_summary$rate_per_100[i]))
}

# Competing risks analysis
cat("\n2. Competing Risks Dataset:\n")
cat("   Total patients:", nrow(competing_risks_data), "\n")
cat("   Total person-months:", round(sum(competing_risks_data$followup_months), 1), "\n")

event_summary <- table(competing_risks_data$event_type)
cat("   Event distribution:\n")
for(event in names(event_summary)) {
  cat("   ", event, ":", event_summary[event], "\n")
}

# Time intervals analysis
cat("\n3. Time Intervals Dataset:\n")
cat("   Total patients:", nrow(intervals_data), "\n")
cat("   Mean diagnosis to treatment:", round(mean(intervals_data$diagnosis_to_treatment_days, na.rm = TRUE), 1), "days\n")
cat("   Patients with progression:", sum(!is.na(intervals_data$progression_date)), "\n")
cat("   Patients who died:", sum(!is.na(intervals_data$death_date)), "\n")

cat("\n=== Files Generated ===\n")
cat("1. person_time_test_data.csv - Main dataset for person-time analysis\n")
cat("2. competing_risks_person_time.csv - Competing risks scenario\n") 
cat("3. time_intervals_test_data.csv - Time interval calculations\n")

cat("\n=== Usage Instructions ===\n")
cat("Use these datasets to test:\n")
cat("1. Single Arm Survival - person_time_test_data.csv with person_time = TRUE\n")
cat("2. Survival Analysis - person_time_test_data.csv comparing treatment groups\n")
cat("3. Multivariable Survival - person_time_test_data.csv with multiple covariates\n")
cat("4. Time Interval Calculator - time_intervals_test_data.csv for date calculations\n")

cat("\n=== Expected Person-Time Results ===\n")
cat("- Different incidence rates between treatment groups\n")
cat("- Confidence intervals for rates using Poisson exact methods\n")
cat("- Time-stratified analysis showing rate changes over follow-up\n")
cat("- Proper handling of varying follow-up periods\n")

cat("\n=== Person-Time analysis test data generation completed successfully ===\n")