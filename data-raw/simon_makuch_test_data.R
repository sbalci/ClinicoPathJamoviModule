# Create Simon-Makuch Test Data
# This script creates example datasets for testing Simon-Makuch time-dependent survival analysis

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(123)

# Function to create Simon-Makuch test data
create_simon_makuch_data <- function(n = 200) {
  
  # Create base patient data
  patients <- tibble(
    patient_id = 1:n,
    age = round(rnorm(n, 65, 12)),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE, 
                          prob = c(0.2, 0.3, 0.3, 0.2))),
    baseline_performance = round(rnorm(n, 80, 15))
  )
  
  # Generate survival times and events
  # Base hazard influenced by age, stage, and performance status
  base_hazard <- 0.02 + 
    0.001 * (patients$age - 65) + 
    ifelse(patients$stage == "IV", 0.03, 
           ifelse(patients$stage == "III", 0.02,
                  ifelse(patients$stage == "II", 0.01, 0))) +
    -0.0005 * (patients$baseline_performance - 80)
  
  # Generate follow-up times
  patients$follow_up_months <- round(rexp(n, base_hazard), 1)
  patients$follow_up_months <- pmax(0.5, pmin(patients$follow_up_months, 60)) # Cap at 5 years
  
  # Generate events (death = 1, censored = 0)
  event_prob <- 1 - exp(-base_hazard * patients$follow_up_months)
  patients$death_status <- rbinom(n, 1, event_prob)
  patients$death_status_factor <- factor(patients$death_status, 
                                       levels = c(0, 1), 
                                       labels = c("Alive", "Dead"))
  
  # Generate time-dependent treatment variable
  # Treatment can start at any point during follow-up
  patients$treatment_start_time <- ifelse(
    runif(n) < 0.7, # 70% eventually get treatment
    round(runif(n, 0, patients$follow_up_months * 0.8), 1), # Start before 80% of follow-up
    Inf # Never start treatment
  )
  
  # Create treatment status at different times
  patients$treatment_start_time <- ifelse(
    patients$treatment_start_time == Inf | patients$treatment_start_time >= patients$follow_up_months,
    NA, # Never received treatment during follow-up
    patients$treatment_start_time
  )
  
  # Treatment status: "Pre-treatment" before start, "On-treatment" after
  patients$treatment_status <- ifelse(
    is.na(patients$treatment_start_time), "Never-treated",
    "Time-dependent" # Will need to be handled in time-dependent analysis
  )
  
  # For time-dependent analysis, create status at baseline
  patients$baseline_treatment_status <- "Pre-treatment"
  
  # Treatment exposure level for Simon-Makuch analysis
  patients$treatment_exposure <- factor(
    ifelse(is.na(patients$treatment_start_time), "Never", "Eventually"),
    levels = c("Never", "Eventually")
  )
  
  # Add some missing values to make it realistic
  missing_indices <- sample(1:n, round(n * 0.05)) # 5% missing
  patients$baseline_performance[missing_indices] <- NA
  
  return(patients)
}

# Create main dataset
simon_makuch_data <- create_simon_makuch_data(200)

# Create a simpler example for demonstration
simon_makuch_simple <- simon_makuch_data %>%
  select(
    patient_id,
    age,
    sex,
    stage,
    follow_up_months,
    death_status,
    death_status_factor,
    treatment_start_time,
    baseline_treatment_status,
    treatment_exposure
  ) %>%
  # Add explicit time-dependent status for easier understanding
  mutate(
    treatment_start_time_clean = ifelse(is.na(treatment_start_time), 0, treatment_start_time),
    treatment_received = ifelse(is.na(treatment_start_time), "No", "Yes")
  )

# Create expanded time-dependent format for advanced users
create_time_dependent_format <- function(data) {
  
  expanded_data <- list()
  
  for (i in 1:nrow(data)) {
    patient <- data[i, ]
    
    if (is.na(patient$treatment_start_time)) {
      # Patient never received treatment
      expanded_data[[i]] <- tibble(
        patient_id = patient$patient_id,
        start_time = 0,
        stop_time = patient$follow_up_months,
        event = patient$death_status,
        treatment_status = "Pre-treatment",
        age = patient$age,
        sex = patient$sex,
        stage = patient$stage
      )
    } else {
      # Patient received treatment during follow-up
      # Pre-treatment period
      pre_treatment <- tibble(
        patient_id = patient$patient_id,
        start_time = 0,
        stop_time = patient$treatment_start_time,
        event = 0, # Cannot have event before treatment start
        treatment_status = "Pre-treatment",
        age = patient$age,
        sex = patient$sex,
        stage = patient$stage
      )
      
      # Post-treatment period
      post_treatment <- tibble(
        patient_id = patient$patient_id,
        start_time = patient$treatment_start_time,
        stop_time = patient$follow_up_months,
        event = patient$death_status,
        treatment_status = "On-treatment",
        age = patient$age,
        sex = patient$sex,
        stage = patient$stage
      )
      
      expanded_data[[i]] <- bind_rows(pre_treatment, post_treatment)
    }
  }
  
  bind_rows(expanded_data) %>%
    mutate(
      treatment_status = factor(treatment_status, 
                              levels = c("Pre-treatment", "On-treatment"))
    )
}

simon_makuch_expanded <- create_time_dependent_format(simon_makuch_simple)

# Create landmark analysis example data
create_landmark_data <- function(data, landmark_times = c(6, 12, 24)) {
  
  landmark_datasets <- list()
  
  for (time in landmark_times) {
    # Include only patients who survived to landmark time
    landmark_survivors <- data %>%
      filter(follow_up_months >= time) %>%
      mutate(
        # Adjust follow-up time from landmark
        landmark_follow_up = follow_up_months - time,
        # Treatment status at landmark time
        treatment_at_landmark = case_when(
          is.na(treatment_start_time) ~ "Never-treated",
          treatment_start_time <= time ~ "On-treatment",
          treatment_start_time > time ~ "Pre-treatment",
          TRUE ~ "Unknown"
        ),
        landmark_time = time
      )
    
    landmark_datasets[[paste0("landmark_", time)]] <- landmark_survivors
  }
  
  # Combine all landmark datasets
  bind_rows(landmark_datasets, .id = "landmark_analysis") %>%
    mutate(
      treatment_at_landmark = factor(treatment_at_landmark,
                                   levels = c("Never-treated", "Pre-treatment", "On-treatment"))
    )
}

simon_makuch_landmark <- create_landmark_data(simon_makuch_simple)

# Save datasets
save(simon_makuch_data, 
     simon_makuch_simple, 
     simon_makuch_expanded, 
     simon_makuch_landmark,
     file = "data/simon_makuch_examples.rda")

# Create jamovi-compatible omv files would need to be done through jamovi interface
# But we can export CSV files for easy import

write.csv(simon_makuch_simple, "data-raw/simon_makuch_simple.csv", row.names = FALSE)
write.csv(simon_makuch_expanded, "data-raw/simon_makuch_expanded.csv", row.names = FALSE)
write.csv(simon_makuch_landmark, "data-raw/simon_makuch_landmark.csv", row.names = FALSE)

# Print summary of created datasets
cat("Simon-Makuch Test Datasets Created:\n")
cat("=====================================\n\n")

cat("1. simon_makuch_simple: Basic dataset (n =", nrow(simon_makuch_simple), ")\n")
cat("   Variables: patient_id, age, sex, stage, follow_up_months, death_status,\n")
cat("              treatment_start_time, treatment_exposure\n\n")

cat("2. simon_makuch_expanded: Time-dependent format (n =", nrow(simon_makuch_expanded), ")\n")
cat("   Variables: patient_id, start_time, stop_time, event, treatment_status\n\n")

cat("3. simon_makuch_landmark: Landmark analysis format (n =", nrow(simon_makuch_landmark), ")\n")
cat("   Landmark times: 6, 12, 24 months\n\n")

cat("Example variable mappings for Simon-Makuch analysis:\n")
cat("- Survival Time: follow_up_months\n")
cat("- Event Indicator: death_status (0=alive, 1=dead)\n")
cat("- Time-Dependent Variable: treatment_exposure\n")
cat("- Time-Dependent Change Time: treatment_start_time_clean\n")
cat("- Time-Dependent Status: baseline_treatment_status (baseline) -> varies by time\n")
cat("- Exposed Level: 'On-treatment' or 'Yes'\n\n")

# Print some example rows
cat("Example rows from simple dataset:\n")
print(head(simon_makuch_simple[, 1:8]))

cat("\nTreatment exposure summary:\n")
print(table(simon_makuch_simple$treatment_exposure, useNA = "ifany"))

cat("\nSurvival summary:\n")
print(table(simon_makuch_simple$death_status_factor, useNA = "ifany"))
