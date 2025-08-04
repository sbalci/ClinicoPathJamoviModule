# Create comprehensive test dataset for Advanced Raincloud Plot
# This script generates a realistic clinical trial dataset that tests all features

library(dplyr)
library(tibble)
library(tidyr)

set.seed(42)  # For reproducible results

# Sample sizes for different groups
n_per_group <- 150
n_timepoints <- 3
n_patients <- n_per_group * 2  # Two treatment arms

# Generate patient IDs
patient_ids <- paste0("PT", sprintf("%03d", 1:n_patients))

# Treatment arms
treatment_arms <- c("Placebo", "Drug A")
time_points <- c("Baseline", "Week 4", "Week 12")

# Create base patient characteristics
base_data <- tibble(
  patient_id = rep(patient_ids, each = n_timepoints),
  treatment_arm = rep(rep(treatment_arms, each = n_per_group), each = n_timepoints),
  time_point = rep(time_points, times = n_patients),
  visit_number = rep(1:n_timepoints, times = n_patients),
  
  # Demographics (same across visits for each patient)
  age = rep(round(rnorm(n_patients, 55, 12)), each = n_timepoints),
  gender = rep(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.6, 0.4)), each = n_timepoints),
  disease_stage = rep(sample(c("Early", "Advanced"), n_patients, replace = TRUE, prob = c(0.7, 0.3)), each = n_timepoints),
  biomarker_status = rep(sample(c("Positive", "Negative"), n_patients, replace = TRUE, prob = c(0.4, 0.6)), each = n_timepoints)
)

# Generate primary efficacy outcome (tumor size reduction %)
# Baseline: 0% change
# Drug A shows improvement over time, Placebo shows minimal change
generate_tumor_response <- function(treatment, visit, patient_baseline) {
  if (visit == 1) {
    return(0)  # Baseline is 0% change
  }
  
  if (treatment == "Placebo") {
    # Placebo: slight worsening on average
    base_effect <- ifelse(visit == 2, 5, 8)  # 5% increase at week 4, 8% at week 12
    noise <- rnorm(1, 0, 15)
  } else {
    # Drug A: improvement over time
    base_effect <- ifelse(visit == 2, -15, -25)  # 15% reduction at week 4, 25% at week 12
    noise <- rnorm(1, 0, 12)
    
    # Some patients are non-responders
    if (runif(1) < 0.3) {  # 30% non-responders
      base_effect <- base_effect * 0.3
    }
  }
  
  return(base_effect + noise)
}

# Add primary outcome
base_data <- base_data %>%
  group_by(patient_id) %>%
  mutate(
    tumor_size_change = sapply(1:n(), function(i) {
      generate_tumor_response(treatment_arm[i], visit_number[i], patient_id[i])
    })
  ) %>%
  ungroup()

# Generate biomarker levels (log-normal distribution)
# Higher baseline levels, treatment reduces levels over time
base_data <- base_data %>%
  group_by(patient_id) %>%
  mutate(
    baseline_biomarker = ifelse(visit_number == 1, 
                               exp(rnorm(1, 4.5, 0.8)),  # Log-normal, mean ~90
                               NA)) %>%
  fill(baseline_biomarker) %>%
  mutate(
    biomarker_level = case_when(
      visit_number == 1 ~ baseline_biomarker,
      treatment_arm == "Placebo" ~ baseline_biomarker * exp(rnorm(1, 0.1, 0.3)),  # Slight increase
      treatment_arm == "Drug A" ~ baseline_biomarker * exp(rnorm(1, -0.4, 0.4))   # Reduction
    )
  ) %>%
  select(-baseline_biomarker) %>%
  ungroup()

# Generate quality of life scores (0-100 scale)
# Higher scores = better quality of life
base_data <- base_data %>%
  group_by(patient_id) %>%
  mutate(
    baseline_qol = ifelse(visit_number == 1, 
                         pmax(20, pmin(80, rnorm(1, 50, 15))),  # Baseline QoL 20-80
                         NA)) %>%
  fill(baseline_qol) %>%
  mutate(
    qol_score = case_when(
      visit_number == 1 ~ baseline_qol,
      treatment_arm == "Placebo" ~ pmax(0, pmin(100, baseline_qol + rnorm(1, -2, 8))),  # Slight decline
      treatment_arm == "Drug A" ~ pmax(0, pmin(100, baseline_qol + rnorm(1, 8, 10)))    # Improvement
    )
  ) %>%
  select(-baseline_qol) %>%
  ungroup()

# Generate pain scores (0-10 scale, Likert-like)
# Higher scores = more pain
base_data <- base_data %>%
  group_by(patient_id) %>%
  mutate(
    baseline_pain = ifelse(visit_number == 1, 
                          round(pmax(2, pmin(9, rnorm(1, 6, 2)))),
                          NA)) %>%
  fill(baseline_pain) %>%
  mutate(
    pain_score = case_when(
      visit_number == 1 ~ baseline_pain,
      treatment_arm == "Placebo" ~ round(pmax(0, pmin(10, baseline_pain + rnorm(1, 0.5, 1.5)))),
      treatment_arm == "Drug A" ~ round(pmax(0, pmin(10, baseline_pain + rnorm(1, -1.5, 1.8))))
    )
  ) %>%
  select(-baseline_pain) %>%
  ungroup()

# Add missing data patterns (realistic dropout)
set.seed(42)
dropout_patients <- sample(patient_ids, round(n_patients * 0.15))  # 15% dropout

base_data <- base_data %>%
  mutate(
    # Missing data increases over time, more in placebo group
    missing_prob = case_when(
      patient_id %in% dropout_patients & visit_number == 3 ~ 0.8,
      patient_id %in% dropout_patients & visit_number == 2 ~ 0.3,
      treatment_arm == "Placebo" & visit_number == 3 ~ 0.2,
      treatment_arm == "Placebo" & visit_number == 2 ~ 0.1,
      treatment_arm == "Drug A" & visit_number == 3 ~ 0.1,
      treatment_arm == "Drug A" & visit_number == 2 ~ 0.05,
      TRUE ~ 0
    ),
    
    # Apply missing data
    tumor_size_change = ifelse(runif(n()) < missing_prob, NA, tumor_size_change),
    biomarker_level = ifelse(runif(n()) < missing_prob, NA, biomarker_level),
    qol_score = ifelse(runif(n()) < missing_prob, NA, qol_score),
    pain_score = ifelse(runif(n()) < missing_prob, NA, pain_score)
  ) %>%
  select(-missing_prob)

# Add some extreme outliers for testing outlier handling
outlier_indices <- sample(nrow(base_data), 10)
base_data$biomarker_level[outlier_indices] <- base_data$biomarker_level[outlier_indices] * runif(10, 3, 8)

# Create final dataset
advancedraincloud_data <- base_data %>%
  arrange(patient_id, visit_number) %>%
  mutate(
    # Convert categorical variables to factors with proper levels
    treatment_arm = factor(treatment_arm, levels = c("Placebo", "Drug A")),
    time_point = factor(time_point, levels = time_points),
    gender = factor(gender),
    disease_stage = factor(disease_stage, levels = c("Early", "Advanced")),
    biomarker_status = factor(biomarker_status, levels = c("Negative", "Positive")),
    pain_score = factor(pain_score, levels = 0:10, ordered = TRUE),
    
    # Add response classifications
    tumor_responder = case_when(
      is.na(tumor_size_change) ~ NA,
      tumor_size_change <= -30 ~ "Complete Response",
      tumor_size_change <= -10 ~ "Partial Response", 
      tumor_size_change <= 10 ~ "Stable Disease",
      TRUE ~ "Progressive Disease"
    ),
    tumor_responder = factor(tumor_responder, 
                           levels = c("Progressive Disease", "Stable Disease", 
                                    "Partial Response", "Complete Response")),
    
    # Add derived variables for testing
    age_group = factor(ifelse(age < 65, "< 65 years", "≥ 65 years")),
    baseline_biomarker_high = factor(ifelse(
      visit_number == 1 & biomarker_level > 100, "High", "Normal"
    ))
  ) %>%
  group_by(patient_id) %>%
  fill(baseline_biomarker_high) %>%
  ungroup()

# Create cross-sectional versions for testing non-longitudinal features
advancedraincloud_baseline <- advancedraincloud_data %>%
  filter(time_point == "Baseline") %>%
  select(-visit_number, -time_point)

advancedraincloud_endpoint <- advancedraincloud_data %>%
  filter(time_point == "Week 12") %>%
  select(-visit_number, -time_point)

# Calculate change scores for longitudinal analysis
advancedraincloud_change <- advancedraincloud_data %>%
  filter(time_point %in% c("Baseline", "Week 12")) %>%
  select(patient_id, treatment_arm, time_point, tumor_size_change, biomarker_level, qol_score, pain_score) %>%
  mutate(pain_score_numeric = as.numeric(as.character(pain_score))) %>%
  select(-pain_score) %>%
  pivot_wider(names_from = time_point, 
              values_from = c(tumor_size_change, biomarker_level, qol_score, pain_score_numeric),
              names_sep = "_") %>%
  mutate(
    tumor_change = `tumor_size_change_Week 12` - tumor_size_change_Baseline,
    biomarker_change = biomarker_level_Baseline - `biomarker_level_Week 12`,  # Reduction is positive
    qol_change = `qol_score_Week 12` - qol_score_Baseline,
    pain_change = pain_score_numeric_Baseline - `pain_score_numeric_Week 12`  # Reduction is positive
  ) %>%
  select(patient_id, treatment_arm, ends_with("_change")) %>%
  filter(complete.cases(.))

# Save datasets
usethis::use_data(advancedraincloud_data, overwrite = TRUE)
usethis::use_data(advancedraincloud_baseline, overwrite = TRUE) 
usethis::use_data(advancedraincloud_endpoint, overwrite = TRUE)
usethis::use_data(advancedraincloud_change, overwrite = TRUE)

# Print summary for documentation
message("=== ADVANCED RAINCLOUD TEST DATASETS CREATED ===")
message("\n1. advancedraincloud_data (longitudinal): ", nrow(advancedraincloud_data), " rows, ", ncol(advancedraincloud_data), " columns")
message("2. advancedraincloud_baseline (cross-sectional): ", nrow(advancedraincloud_baseline), " rows, ", ncol(advancedraincloud_baseline), " columns")
message("3. advancedraincloud_endpoint (cross-sectional): ", nrow(advancedraincloud_endpoint), " rows, ", ncol(advancedraincloud_endpoint), " columns")
message("4. advancedraincloud_change (change scores): ", nrow(advancedraincloud_change), " rows, ", ncol(advancedraincloud_change), " columns")

# Show structure
str(advancedraincloud_data)