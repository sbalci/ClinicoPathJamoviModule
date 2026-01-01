# Create comprehensive test data for outcomeorganizer function
# This script generates datasets covering all analysis types and edge cases

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Function to generate realistic survival times with exponential distribution
generate_survival_times <- function(n, rate = 0.1, max_time = 120) {
  times <- rexp(n, rate = rate)
  pmin(times, max_time)  # Cap at maximum follow-up time
}

# =============================================================================
# 1. Basic Overall Survival Dataset
# =============================================================================

# Generate basic overall survival data
outcomeorganizer_basic <- data.frame(
  patient_id = 1:200,
  survival_months = generate_survival_times(200, rate = 0.05, max_time = 60),
  vital_status = sample(c("Alive", "Dead"), 200, replace = TRUE, prob = c(0.6, 0.4)),
  age = rnorm(200, mean = 65, sd = 12),
  gender = sample(c("Male", "Female"), 200, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), 200, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  stringsAsFactors = FALSE
)

# =============================================================================
# 2. Multi-event Survival Dataset (for competing risks, cause-specific, multistate)
# =============================================================================

# Generate multi-event survival data with detailed outcome categories
outcomeorganizer_multievent <- data.frame(
  patient_id = 1:300,
  survival_months = generate_survival_times(300, rate = 0.04, max_time = 72),
  detailed_outcome = sample(c("Alive_Disease_Free", "Alive_With_Disease", 
                             "Dead_Disease", "Dead_Other_Causes"), 300, 
                           replace = TRUE, prob = c(0.45, 0.15, 0.25, 0.15)),
  age = rnorm(300, mean = 62, sd = 15),
  treatment = sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"), 300, replace = TRUE),
  comorbidities = sample(c("None", "Diabetes", "Hypertension", "Both"), 300, replace = TRUE),
  tumor_grade = sample(c("Low", "Intermediate", "High"), 300, replace = TRUE),
  stringsAsFactors = FALSE
)

# =============================================================================
# 3. Recurrence/Progression Dataset (for RFS/PFS/DFS/TTP)
# =============================================================================

# Generate recurrence and progression data
outcomeorganizer_recurrence <- data.frame(
  patient_id = 1:250,
  survival_months = generate_survival_times(250, rate = 0.06, max_time = 48),
  vital_status = sample(c("Alive", "Dead"), 250, replace = TRUE, prob = c(0.7, 0.3)),
  recurrence_status = sample(c("No_Recurrence", "Local_Recurrence", "Distant_Recurrence"), 
                            250, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
  progression_status = sample(c("No_Progression", "Disease_Progression"), 250, 
                             replace = TRUE, prob = c(0.65, 0.35)),
  time_to_recurrence = runif(250, 1, 36),
  time_to_progression = runif(250, 1, 30),
  response_to_treatment = sample(c("Complete_Response", "Partial_Response", 
                                  "Stable_Disease", "Progressive_Disease"), 250, replace = TRUE),
  stringsAsFactors = FALSE
)

# Fix time_to_recurrence and time_to_progression based on status
outcomeorganizer_recurrence$time_to_recurrence[outcomeorganizer_recurrence$recurrence_status == "No_Recurrence"] <- NA
outcomeorganizer_recurrence$time_to_progression[outcomeorganizer_recurrence$progression_status == "No_Progression"] <- NA

# =============================================================================
# 4. Edge Cases Dataset
# =============================================================================

# Generate edge cases and problematic scenarios
outcomeorganizer_edge_cases <- data.frame(
  patient_id = 1:100,
  survival_months = c(
    rep(0, 10),  # Zero survival times
    runif(20, 0.1, 1),  # Very short survival times
    generate_survival_times(60, rate = 0.08, max_time = 24),  # Normal range
    rep(NA, 10)  # Missing survival times
  ),
  outcome_status = c(
    rep("Event", 30),  # All events
    rep("Censored", 30),  # All censored
    sample(c("Event", "Censored"), 30, replace = TRUE),  # Mixed
    rep(NA, 10)  # Missing status
  ),
  # Multiple missing patterns
  outcome_detailed = c(
    rep("Dead_Disease", 20),
    rep("Dead_Other", 20),
    rep("Alive_Disease", 20),
    rep("Alive_NED", 20),
    rep(NA, 20)
  ),
  # Binary coded outcomes
  binary_outcome = sample(c(0, 1), 100, replace = TRUE),
  logical_outcome = sample(c(TRUE, FALSE), 100, replace = TRUE),
  stringsAsFactors = FALSE
)

# =============================================================================
# 5. International Data Formats
# =============================================================================

# Generate international data with different language codings
outcomeorganizer_international <- data.frame(
  patient_id = 1:150,
  survival_months = generate_survival_times(150, rate = 0.07, max_time = 36),
  
  # German coding
  german_status = sample(c("Verstorben", "Lebend"), 150, replace = TRUE, prob = c(0.4, 0.6)),
  german_outcome = sample(c("Tod_Krankheit", "Tod_Andere", "Lebend_Krankheit", "Lebend_Geheilt"), 
                         150, replace = TRUE),
  
  # French coding
  french_status = sample(c("Décédé", "Vivant"), 150, replace = TRUE, prob = c(0.4, 0.6)),
  french_outcome = sample(c("Mort_Maladie", "Mort_Autre", "Vivant_Maladie", "Vivant_Guéri"), 
                         150, replace = TRUE),
  
  # Spanish coding
  spanish_status = sample(c("Fallecido", "Vivo"), 150, replace = TRUE, prob = c(0.4, 0.6)),
  spanish_outcome = sample(c("Muerte_Enfermedad", "Muerte_Otra", "Vivo_Enfermedad", "Vivo_Curado"), 
                          150, replace = TRUE),
  
  # Numeric coding systems
  numeric_status = sample(c(0, 1, 2, 3), 150, replace = TRUE),
  coded_outcome = sample(c("01", "02", "03", "04"), 150, replace = TRUE),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 6. Complex Clinical Scenarios
# =============================================================================

# Generate complex clinical scenarios with multiple events and hierarchies
outcomeorganizer_complex <- data.frame(
  patient_id = rep(1:75, each = 2),  # Multiple records per patient
  visit_number = rep(1:2, times = 75),
  survival_months = c(
    generate_survival_times(75, rate = 0.08, max_time = 24),  # First visit
    generate_survival_times(75, rate = 0.12, max_time = 36)   # Second visit
  ),
  
  # Complex outcome with priority levels
  primary_outcome = sample(c("No_Event", "Local_Recurrence", "Distant_Metastasis", 
                            "Second_Primary", "Death_Disease", "Death_Other"), 
                          150, replace = TRUE),
  
  # Event hierarchy coding
  event_priority = sample(c(1, 2, 3, 4, 5), 150, replace = TRUE),
  
  # Time-dependent variables
  treatment_change = sample(c("No_Change", "Dose_Reduction", "Treatment_Switch", 
                             "Treatment_Stop"), 150, replace = TRUE),
  
  # Interval censoring information
  interval_start = generate_survival_times(150, rate = 0.1, max_time = 12),
  interval_end = NA,  # Will be filled based on interval_start
  
  # Administrative censoring
  admin_censor_date = as.Date("2023-12-31"),
  last_followup_date = as.Date("2020-01-01") + days(round(runif(150, 1, 1000))),
  
  stringsAsFactors = FALSE
)

# Fill interval_end based on interval_start
outcomeorganizer_complex$interval_end <- outcomeorganizer_complex$interval_start + 
  runif(150, 1, 6)  # 1-6 months interval

# =============================================================================
# 7. Large Dataset for Performance Testing
# =============================================================================

# Generate large dataset for performance testing
outcomeorganizer_large <- data.frame(
  patient_id = 1:2000,
  survival_months = generate_survival_times(2000, rate = 0.05, max_time = 120),
  vital_status = sample(c("Alive", "Dead"), 2000, replace = TRUE, prob = c(0.6, 0.4)),
  outcome_detailed = sample(c("Alive_NED", "Alive_Disease", "Dead_Disease", "Dead_Other"), 
                           2000, replace = TRUE, prob = c(0.4, 0.2, 0.25, 0.15)),
  recurrence_status = sample(c("No_Recurrence", "Recurrence"), 2000, replace = TRUE, prob = c(0.7, 0.3)),
  age = rnorm(2000, mean = 65, sd = 12),
  gender = sample(c("Male", "Female"), 2000, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), 2000, replace = TRUE),
  treatment = sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"), 2000, replace = TRUE),
  hospital = sample(paste("Hospital", 1:20), 2000, replace = TRUE),
  stringsAsFactors = FALSE
)

# =============================================================================
# 8. Administrative Censoring Dataset
# =============================================================================

# Generate data specifically for administrative censoring testing
outcomeorganizer_admin_censor <- data.frame(
  patient_id = 1:100,
  survival_months = generate_survival_times(100, rate = 0.06, max_time = 60),
  vital_status = sample(c("Alive", "Dead"), 100, replace = TRUE, prob = c(0.6, 0.4)),
  
  # Date variables for administrative censoring
  study_entry_date = as.Date("2020-01-01") + days(round(runif(100, 1, 365))),
  last_contact_date = as.Date("2020-01-01") + days(round(runif(100, 400, 1200))),
  admin_cutoff_date = as.Date("2023-01-01"),
  
  # Outcome at different time points
  outcome_6m = sample(c("Alive", "Dead", "Lost"), 100, replace = TRUE, prob = c(0.8, 0.15, 0.05)),
  outcome_12m = sample(c("Alive", "Dead", "Lost"), 100, replace = TRUE, prob = c(0.7, 0.25, 0.05)),
  outcome_24m = sample(c("Alive", "Dead", "Lost"), 100, replace = TRUE, prob = c(0.6, 0.35, 0.05)),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 9. Problematic Data for Error Testing
# =============================================================================

# Generate problematic data to test error handling
outcomeorganizer_problematic <- data.frame(
  patient_id = 1:50,
  survival_months = c(
    rep(-5, 10),  # Negative survival times
    rep(0, 10),   # Zero survival times
    generate_survival_times(20, rate = 0.1, max_time = 24),  # Normal times
    rep(NA, 10)   # Missing times
  ),
  
  # Problematic outcome coding
  invalid_outcome = c(
    rep("Unknown", 10),  # Invalid category
    rep("", 10),         # Empty strings
    sample(c("Event", "Non-Event"), 20, replace = TRUE),  # Valid
    rep(NA, 10)          # Missing
  ),
  
  # Non-standard coding
  mixed_coding = c(
    sample(c("Yes", "No"), 20, replace = TRUE),
    sample(c(1, 0), 20, replace = TRUE),
    sample(c("TRUE", "FALSE"), 10, replace = TRUE)
  ),
  
  # Inconsistent data types
  inconsistent_status = c(
    rep("1", 10),
    rep(1, 10),
    rep(TRUE, 10),
    rep("Event", 10),
    rep(NA, 10)
  ),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# Save all datasets
# =============================================================================

# Save individual datasets
save(outcomeorganizer_basic, file = "data/outcomeorganizer_basic.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_basic, "data/outcomeorganizer_basic.omv")
  message("✓ Created outcomeorganizer_basic.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_basic, "data/outcomeorganizer_basic.omv")
  message("✓ Created outcomeorganizer_basic.omv")
}
save(outcomeorganizer_multievent, file = "data/outcomeorganizer_multievent.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_multievent, "data/outcomeorganizer_multievent.omv")
  message("✓ Created outcomeorganizer_multievent.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_multievent, "data/outcomeorganizer_multievent.omv")
  message("✓ Created outcomeorganizer_multievent.omv")
}
save(outcomeorganizer_recurrence, file = "data/outcomeorganizer_recurrence.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_recurrence, "data/outcomeorganizer_recurrence.omv")
  message("✓ Created outcomeorganizer_recurrence.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_recurrence, "data/outcomeorganizer_recurrence.omv")
  message("✓ Created outcomeorganizer_recurrence.omv")
}
save(outcomeorganizer_edge_cases, file = "data/outcomeorganizer_edge_cases.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_edge_cases, "data/outcomeorganizer_edge_cases.omv")
  message("✓ Created outcomeorganizer_edge_cases.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_edge_cases, "data/outcomeorganizer_edge_cases.omv")
  message("✓ Created outcomeorganizer_edge_cases.omv")
}
save(outcomeorganizer_international, file = "data/outcomeorganizer_international.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_international, "data/outcomeorganizer_international.omv")
  message("✓ Created outcomeorganizer_international.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_international, "data/outcomeorganizer_international.omv")
  message("✓ Created outcomeorganizer_international.omv")
}
save(outcomeorganizer_complex, file = "data/outcomeorganizer_complex.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_complex, "data/outcomeorganizer_complex.omv")
  message("✓ Created outcomeorganizer_complex.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_complex, "data/outcomeorganizer_complex.omv")
  message("✓ Created outcomeorganizer_complex.omv")
}
save(outcomeorganizer_large, file = "data/outcomeorganizer_large.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_large, "data/outcomeorganizer_large.omv")
  message("✓ Created outcomeorganizer_large.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_large, "data/outcomeorganizer_large.omv")
  message("✓ Created outcomeorganizer_large.omv")
}
save(outcomeorganizer_admin_censor, file = "data/outcomeorganizer_admin_censor.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_admin_censor, "data/outcomeorganizer_admin_censor.omv")
  message("✓ Created outcomeorganizer_admin_censor.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_admin_censor, "data/outcomeorganizer_admin_censor.omv")
  message("✓ Created outcomeorganizer_admin_censor.omv")
}
save(outcomeorganizer_problematic, file = "data/outcomeorganizer_problematic.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_problematic, "data/outcomeorganizer_problematic.omv")
  message("✓ Created outcomeorganizer_problematic.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(outcomeorganizer_problematic, "data/outcomeorganizer_problematic.omv")
  message("✓ Created outcomeorganizer_problematic.omv")
}

# Save combined dataset
save(outcomeorganizer_basic, outcomeorganizer_multievent, outcomeorganizer_recurrence,
     outcomeorganizer_edge_cases, outcomeorganizer_international, outcomeorganizer_complex,
     outcomeorganizer_large, outcomeorganizer_admin_censor, outcomeorganizer_problematic,
     file = "data/outcomeorganizer_test_data.rda")

# =============================================================================
# Data Summary
# =============================================================================

cat("OutcomeOrganizer Test Data Generation Complete!\n")
cat("=========================================\n")
cat("Generated datasets:\n")
cat("1. outcomeorganizer_basic: Basic overall survival (n=200)\n")
cat("2. outcomeorganizer_multievent: Multi-event outcomes (n=300)\n")
cat("3. outcomeorganizer_recurrence: Recurrence/progression data (n=250)\n")
cat("4. outcomeorganizer_edge_cases: Edge cases and missing data (n=100)\n")
cat("5. outcomeorganizer_international: International data formats (n=150)\n")
cat("6. outcomeorganizer_complex: Complex clinical scenarios (n=150)\n")
cat("7. outcomeorganizer_large: Large dataset for performance (n=2000)\n")
cat("8. outcomeorganizer_admin_censor: Administrative censoring (n=100)\n")
cat("9. outcomeorganizer_problematic: Problematic data for error testing (n=50)\n")
cat("\nTotal observations across all datasets: 3,300\n")
cat("Test coverage includes:\n")
cat("- All survival analysis types (OS, cause-specific, competing risks, RFS/PFS/DFS, TTP, multistate)\n")
cat("- International data formats (German, French, Spanish)\n")
cat("- Edge cases and missing data patterns\n")
cat("- Complex clinical scenarios with multiple events\n")
cat("- Performance testing with large datasets\n")
cat("- Administrative censoring scenarios\n")
cat("- Problematic data for error handling validation\n")
