# Generate Test Data for Stage Migration Analysis
# This script creates a synthetic dataset to demonstrate the Will Rogers phenomenon
# and test the Stage Migration Analysis module functionality

library(survival)
library(dplyr)
set.seed(123) # For reproducibility

# Define parameters for data generation
n_patients <- 500    # Number of patients
followup_max <- 60   # Maximum follow-up time in months

# Function to generate synthetic survival data
generate_stage_migration_data <- function(n_patients, followup_max) {

  # Step 1: Generate patient IDs and baseline characteristics
  data <- data.frame(
    patient_id = paste0("P", 1:n_patients),
    age = round(rnorm(n_patients, mean = 65, sd = 12)),
    sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.6, 0.4))
  )

  # Step 2: Generate true disease severity (underlying continuous variable)
  data$true_severity <- runif(n_patients, min = 0, max = 10)

  # Step 3: Generate old staging system (less precise)
  data$stage_7th <- cut(
    data$true_severity,
    breaks = c(0, 2.5, 5, 7.5, 10),
    labels = c("I", "II", "III", "IV"),
    include.lowest = TRUE
  )

  # Step 4: Generate new staging system (more precise - will create stage migration)
  # Add some measurement noise and more stage boundaries
  severity_with_noise <- data$true_severity + rnorm(n_patients, mean = 0, sd = 0.5)
  data$stage_8th <- cut(
    severity_with_noise,
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("IA", "IB", "II", "III", "IV"),
    include.lowest = TRUE
  )

  # Step 5: Generate survival times based on true severity
  # Higher severity = worse survival
  lambda <- 0.01 * exp(0.3 * data$true_severity)  # Hazard increases with severity
  data$survival_time <- round(rexp(n_patients, lambda))
  data$survival_time <- pmin(data$survival_time, followup_max)  # Cap at max follow-up

  # Step 6: Generate censoring
  # Random censoring independent of survival
  censor_time <- round(runif(n_patients, min = 0, max = followup_max))
  data$event <- as.integer(data$survival_time <= censor_time)
  data$time <- ifelse(data$event == 1, data$survival_time, censor_time)

  # Add some random early censoring/dropout
  early_dropout <- sample(1:n_patients, size = round(n_patients * 0.1))
  data$time[early_dropout] <- round(data$time[early_dropout] * runif(length(early_dropout), min = 0.1, max = 0.5))
  data$event[early_dropout] <- 0

  # Step 7: Create event as factor for testing with different event types
  data$status <- factor(
    ifelse(data$event == 1, "Dead", "Alive"),
    levels = c("Alive", "Dead")
  )

  # Return the dataset
  return(data)
}

# Generate the test data
test_data <- generate_stage_migration_data(n_patients, followup_max)

# Add some deliberate stage migration patterns to demonstrate Will Rogers phenomenon
# For some stage II patients, shift them to stage IB in the new system (with better survival than average IB)
will_rogers_candidates <- which(test_data$stage_7th == "II" & test_data$true_severity < 3.5)
test_data$stage_8th[will_rogers_candidates] <- "IB"

# For some stage III patients, shift them to stage II in the new system (with worse survival than average II)
stage_migration_candidates <- which(test_data$stage_7th == "III" & test_data$true_severity < 5.5)
test_data$stage_8th[stage_migration_candidates] <- "II"

# Verify the test dataset
summary(test_data)
table(test_data$stage_7th, test_data$stage_8th)

# Calculate stage-specific median survival to verify Will Rogers phenomenon
cat("Median survival by 7th edition staging:\n")
aggregate(time ~ stage_7th, data = test_data, FUN = median)

cat("Median survival by 8th edition staging:\n")
aggregate(time ~ stage_8th, data = test_data, FUN = median)

# Fit survival models to verify differences in prognostic performance
fit_7th <- survfit(Surv(time, event) ~ stage_7th, data = test_data)
fit_8th <- survfit(Surv(time, event) ~ stage_8th, data = test_data)

# Calculate C-index for both staging systems
cox_7th <- coxph(Surv(time, event) ~ stage_7th, data = test_data)
cox_8th <- coxph(Surv(time, event) ~ stage_8th, data = test_data)

print(paste("C-index for 7th edition staging:", round(summary(cox_7th)$concordance[1], 3)))
print(paste("C-index for 8th edition staging:", round(summary(cox_8th)$concordance[1], 3)))

# Save the data for use in jamovi
write.csv(test_data, "./data/stage_migration_test_data.csv", row.names = FALSE)

# Output information message
cat("\nTest data has been generated and saved to 'stage_migration_test_data.csv'.\n")
cat("This dataset contains", n_patients, "patients with survival data and two staging systems.\n")
cat("The dataset is designed to demonstrate the Will Rogers phenomenon with stage migration between the systems.\n")
