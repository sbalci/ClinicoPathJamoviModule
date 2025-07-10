# Create test data for onesurvival function
# This script generates various survival analysis test datasets

library(survival)
library(dplyr)
set.seed(42)

# Helper function to generate survival times
generate_survival_times <- function(n, rate = 0.1, censor_rate = 0.3) {
  # Generate event times from exponential distribution
  event_times <- rexp(n, rate = rate)
  
  # Generate censoring times
  censor_times <- rexp(n, rate = rate * censor_rate)
  
  # Observed time is minimum of event and censoring time
  times <- pmin(event_times, censor_times)
  
  # Status is 1 if event occurred before censoring
  status <- as.numeric(event_times <= censor_times)
  
  return(list(times = times, status = status))
}

# 1. Basic Survival Data (Standard case)
basic_surv <- generate_survival_times(200, rate = 0.05)
onesurvival_basic <- data.frame(
  patient_id = 1:200,
  survival_time = round(basic_surv$times, 2),
  event_status = basic_surv$status,
  age = round(rnorm(200, 65, 12)),
  sex = sample(c("Male", "Female"), 200, replace = TRUE),
  stringsAsFactors = FALSE
)

# 2. Edge Cases Dataset
onesurvival_edge_cases <- data.frame(
  # Mix of different scenarios
  patient_id = 1:50,
  
  # Various time scenarios including zeros
  survival_time = c(
    rep(0, 5),           # Zero times
    runif(10, 0.1, 1),   # Very short times
    runif(20, 1, 100),   # Normal range
    runif(10, 100, 1000), # Long times
    rep(NA, 5)           # Missing times
  ),
  
  # Various status coding
  event_status = c(
    rep(1, 15),          # Events
    rep(0, 15),          # Censored
    rep(NA, 5),          # Missing
    sample(0:1, 15, replace = TRUE)  # Mixed
  ),
  
  # Alternative status coding (for testing conversion)
  status_text = c(
    rep("Dead", 15),
    rep("Alive", 15),
    rep(NA, 5),
    sample(c("Dead", "Alive"), 15, replace = TRUE)
  ),
  
  status_logical = c(
    rep(TRUE, 15),
    rep(FALSE, 15),
    rep(NA, 5),
    sample(c(TRUE, FALSE), 15, replace = TRUE)
  ),
  
  stringsAsFactors = FALSE
)

# 3. Clinical Trial Data (with time units consideration)
clinical_surv <- generate_survival_times(150, rate = 0.002)  # Slower rate for longer times
onesurvival_clinical <- data.frame(
  subject_id = paste0("PT", sprintf("%03d", 1:150)),
  days_to_event = round(clinical_surv$times),
  death_indicator = clinical_surv$status,
  treatment_group = sample(c("Control", "Treatment"), 150, replace = TRUE, prob = c(0.5, 0.5)),
  baseline_score = round(rnorm(150, 50, 10)),
  enrollment_year = sample(2018:2023, 150, replace = TRUE),
  stringsAsFactors = FALSE
)

# 4. Problematic Data (for testing validation)
onesurvival_problematic <- data.frame(
  id = 1:30,
  
  # Problematic time values
  time_with_issues = c(
    -5, -2, -1,          # Negative times (should error)
    0, 0, 0,             # Multiple zeros
    NA, NA, NA, NA,      # Many missing
    runif(20, 1, 50)     # Some valid times
  ),
  
  # Problematic status values
  status_with_issues = c(
    2, 3, 4,             # Non-binary values
    "Yes", "No", "Maybe", # Text that needs conversion
    NA, NA, NA, NA,      # Missing
    sample(0:1, 20, replace = TRUE)
  ),
  
  # All censored scenario
  all_censored = rep(0, 30),
  
  # All events scenario  
  all_events = rep(1, 30),
  
  stringsAsFactors = FALSE
)

# 5. International Data (different coding schemes)
intl_surv <- generate_survival_times(100, rate = 0.03)
onesurvival_international <- data.frame(
  patient_nr = 1:100,
  survival_months = round(intl_surv$times * 30),  # Convert to months
  
  # German coding
  ereignis = ifelse(intl_surv$status == 1, "Verstorben", "Lebend"),
  
  # French coding  
  evenement = ifelse(intl_surv$status == 1, "Décédé", "Vivant"),
  
  # Spanish coding
  evento = ifelse(intl_surv$status == 1, "Fallecido", "Vivo"),
  
  # Numeric coding variants
  status_01 = intl_surv$status,
  status_12 = intl_surv$status + 1,  # 1=censored, 2=event
  
  age_years = round(rnorm(100, 60, 15)),
  stringsAsFactors = FALSE
)

# 6. Large Dataset (performance testing)
large_surv <- generate_survival_times(1000, rate = 0.02)
onesurvival_large <- data.frame(
  id = 1:1000,
  time = round(large_surv$times, 3),
  status = large_surv$status,
  center = sample(paste0("Center", 1:10), 1000, replace = TRUE),
  year = sample(2015:2023, 1000, replace = TRUE),
  stringsAsFactors = FALSE
)

# Save all test datasets
save(onesurvival_basic, 
     file = "data/onesurvival_basic.rda")

save(onesurvival_edge_cases, 
     file = "data/onesurvival_edge_cases.rda")

save(onesurvival_clinical, 
     file = "data/onesurvival_clinical.rda")

save(onesurvival_problematic, 
     file = "data/onesurvival_problematic.rda")

save(onesurvival_international, 
     file = "data/onesurvival_international.rda")

save(onesurvival_large, 
     file = "data/onesurvival_large.rda")

# Create a combined test data file
save(onesurvival_basic,
     onesurvival_edge_cases,
     onesurvival_clinical,
     onesurvival_problematic,
     onesurvival_international,
     onesurvival_large,
     file = "data/onesurvival_test_data.rda")

# Print summaries
cat("Created onesurvival test datasets:\n")
cat("1. onesurvival_basic: Standard survival data (n=200)\n")
cat("2. onesurvival_edge_cases: Edge cases and special scenarios (n=50)\n")
cat("3. onesurvival_clinical: Clinical trial format (n=150)\n")
cat("4. onesurvival_problematic: Data with validation issues (n=30)\n")
cat("5. onesurvival_international: International coding schemes (n=100)\n")
cat("6. onesurvival_large: Large dataset for performance (n=1000)\n")
cat("\nTotal test observations: 1530\n")

# Example usage demonstrations
cat("\n--- Example Usage ---\n")
cat("# Basic analysis:\n")
cat("result <- oneSurvival(data = onesurvival_basic, times = 'survival_time', status = 'event_status')\n\n")

cat("# Edge case testing:\n")
cat("result <- oneSurvival(data = onesurvival_edge_cases, times = 'survival_time', status = 'status_text')\n\n")

cat("# International data:\n")
cat("result <- oneSurvival(data = onesurvival_international, times = 'survival_months', status = 'ereignis')\n")