# Competing Survival Example Data Generation
# This script creates comprehensive test data for the competingsurvival function

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)

set.seed(42)

# Create comprehensive competing survival dataset
n_patients <- 300

competing_survival_data <- data.frame(
  # Patient identifiers
  Patient_ID = paste0("CS", sprintf("%04d", 1:n_patients)),
  
  # Demographics
  Age = round(rnorm(n_patients, 68, 12)),
  Sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE)),
  
  # Treatment groups
  Treatment = factor(sample(c("Standard_Care", "Experimental_Therapy"), 
                           n_patients, replace = TRUE)),
  
  # Disease characteristics
  Tumor_Stage = factor(sample(c("I", "II", "III", "IV"), n_patients, 
                             replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25))),
  
  Grade = factor(sample(1:3, n_patients, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
  
  # Biomarker status
  EGFR_Status = factor(sample(c("Positive", "Negative"), n_patients, 
                             replace = TRUE, prob = c(0.3, 0.7))),
  
  # Performance status
  ECOG_PS = factor(sample(0:2, n_patients, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
  
  # Follow-up time (months)
  Overall_Time = round(rweibull(n_patients, shape = 1.2, scale = 25), 1)
)

# Ensure realistic age and time ranges
competing_survival_data$Age <- pmax(18, pmin(95, competing_survival_data$Age))
competing_survival_data$Overall_Time <- pmax(0.5, pmin(72, competing_survival_data$Overall_Time))

# Create competing outcomes based on patient characteristics
# Higher risk patients more likely to die of disease
# Older patients more likely to die of other causes

risk_score <- with(competing_survival_data, {
  score <- 0
  score <- score + ifelse(Tumor_Stage == "IV", 3, 
                         ifelse(Tumor_Stage == "III", 2, 
                               ifelse(Tumor_Stage == "II", 1, 0)))
  score <- score + ifelse(Grade == 3, 2, ifelse(Grade == 2, 1, 0))
  score <- score + ifelse(EGFR_Status == "Positive", 1, 0)
  score <- score + ifelse(ECOG_PS == 2, 2, ifelse(ECOG_PS == 1, 1, 0))
  score <- score + ifelse(Age > 75, 2, ifelse(Age > 65, 1, 0))
  score <- score + ifelse(Treatment == "Standard_Care", 1, 0)
  return(score)
})

# Define outcome probabilities based on risk score
set.seed(123)
outcome_probs <- data.frame(
  risk = risk_score,
  time = competing_survival_data$Overall_Time,
  age = competing_survival_data$Age
) %>%
  mutate(
    # Higher risk = higher probability of disease death
    prob_disease_death = pmin(0.6, pmax(0.05, 0.1 + 0.05 * risk)),
    
    # Older patients = higher probability of other death  
    prob_other_death = pmin(0.25, pmax(0.02, 0.02 + 0.002 * age)),
    
    # Ensure probabilities sum to <= 1
    prob_disease_death = ifelse(prob_disease_death + prob_other_death > 0.85,
                               0.85 - prob_other_death, prob_disease_death),
    
    # Remaining probability for alive outcomes
    prob_alive = pmax(0.1, 1 - prob_disease_death - prob_other_death),
    
    # Of alive patients, those with higher risk more likely to have disease
    prob_alive_w_disease = pmin(0.8, pmax(0.1, 0.2 + 0.03 * risk)) * prob_alive,
    prob_alive_wo_disease = prob_alive - prob_alive_w_disease
  )

# Generate outcomes
set.seed(456)
outcomes <- apply(outcome_probs, 1, function(row) {
  probs <- c(row["prob_disease_death"], 
            row["prob_other_death"],
            row["prob_alive_w_disease"],
            row["prob_alive_wo_disease"])
  # Ensure probabilities are positive and sum to 1
  probs <- pmax(0, probs)
  probs <- probs / sum(probs)
  
  sample(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", "Alive_wo_Disease"),
         1, prob = probs)
})

competing_survival_data$Outcome <- factor(outcomes, 
                                         levels = c("Dead_of_Disease", "Dead_of_Other", 
                                                   "Alive_w_Disease", "Alive_wo_Disease"))

# Add some realistic variations
competing_survival_data <- competing_survival_data %>%
  mutate(
    # Adjust survival times for outcomes
    Overall_Time = case_when(
      Outcome == "Dead_of_Disease" ~ Overall_Time * runif(n(), 0.3, 0.9),
      Outcome == "Dead_of_Other" ~ Overall_Time * runif(n(), 0.4, 1.0),
      TRUE ~ Overall_Time
    ),
    
    # Round times
    Overall_Time = round(Overall_Time, 1),
    
    # Ensure minimum follow-up
    Overall_Time = pmax(0.5, Overall_Time)
  )

# Create summary statistics
outcome_summary <- competing_survival_data %>%
  group_by(Outcome) %>%
  summarise(
    Count = n(),
    Percentage = round(n() / nrow(competing_survival_data) * 100, 1),
    Median_Time = round(median(Overall_Time), 1),
    Mean_Age = round(mean(Age), 1),
    .groups = 'drop'
  )

# Print summary
cat("Competing Survival Dataset Summary:\n")
cat("==================================\n")
cat("Total patients:", nrow(competing_survival_data), "\n")
cat("Follow-up range:", min(competing_survival_data$Overall_Time), "-", 
    max(competing_survival_data$Overall_Time), "months\n\n")

print(outcome_summary)

# Treatment group summary
treatment_summary <- competing_survival_data %>%
  group_by(Treatment, Outcome) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = Outcome, values_from = Count, values_fill = 0)

cat("\nTreatment Group Distribution:\n")
print(treatment_summary)

# Stage distribution
stage_summary <- competing_survival_data %>%
  group_by(Tumor_Stage, Outcome) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = Outcome, values_from = Count, values_fill = 0)

cat("\nStage Distribution:\n")
print(stage_summary)

# Save the dataset
save(competing_survival_data, file = "data/competing_survival_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(competing_survival_data, "data/competing_survival_data.omv")
  message("✓ Created competing_survival_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(competing_survival_data, "data/competing_survival_data.omv")
  message("✓ Created competing_survival_data.omv")
}

cat("\nDataset saved as 'competing_survival_data.rda'\n")
cat("Variables included:\n")
cat("- Patient_ID: Unique identifier\n")
cat("- Age: Patient age (18-95)\n") 
cat("- Sex: Male/Female\n")
cat("- Treatment: Standard_Care/Experimental_Therapy\n")
cat("- Tumor_Stage: I/II/III/IV\n")
cat("- Grade: 1/2/3\n")
cat("- EGFR_Status: Positive/Negative\n")
cat("- ECOG_PS: 0/1/2\n")
cat("- Overall_Time: Follow-up time (months)\n")
cat("- Outcome: Dead_of_Disease/Dead_of_Other/Alive_w_Disease/Alive_wo_Disease\n")
