# Create comprehensive test datasets for jforester function
# This script generates realistic meta-analysis data for forest plot visualization

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Dataset 1: Meta-analysis of treatment effectiveness (Odds Ratios)
# Simulating a meta-analysis of drug vs placebo across multiple studies

jforester_meta_analysis_data <- data.frame(
  study_name = c(
    "Smith et al. 2020", "Johnson et al. 2019", "Brown et al. 2021", 
    "Davis et al. 2018", "Wilson et al. 2020", "Miller et al. 2019",
    "Garcia et al. 2021", "Anderson et al. 2020", "Taylor et al. 2018",
    "Thomas et al. 2021", "Jackson et al. 2019", "White et al. 2020"
  ),
  
  # Odds ratios with some variation around effect
  odds_ratio = c(
    1.45, 1.23, 1.78, 1.12, 1.89, 1.34,
    1.56, 1.67, 1.28, 1.43, 1.91, 1.52
  ),
  
  # Lower confidence intervals
  or_lower_ci = c(
    1.12, 0.89, 1.34, 0.78, 1.45, 1.01,
    1.18, 1.29, 0.95, 1.09, 1.52, 1.14
  ),
  
  # Upper confidence intervals
  or_upper_ci = c(
    1.88, 1.70, 2.37, 1.61, 2.46, 1.78,
    2.06, 2.16, 1.72, 1.88, 2.40, 2.03
  ),
  
  # Sample sizes
  sample_size = c(
    245, 189, 156, 312, 134, 278,
    198, 167, 289, 203, 145, 221
  ),
  
  # Number of events
  events = c(
    89, 67, 78, 98, 56, 112,
    76, 69, 108, 82, 61, 87
  ),
  
  # Study characteristics for grouping
  country = c(
    "USA", "UK", "Canada", "Germany", "France", "Australia",
    "Sweden", "Netherlands", "Italy", "Spain", "Norway", "Denmark"
  ),
  
  study_quality = c(
    "High", "High", "Medium", "High", "Medium", "High",
    "High", "Medium", "High", "High", "Medium", "High"
  ),
  
  stringsAsFactors = FALSE
)

# Dataset 2: Risk Ratios for cardiovascular outcomes
jforester_cardio_rr_data <- data.frame(
  trial_name = c(
    "CARDIAC-1", "HEART-PROTECT", "CARDIO-SHIELD", "VASCULAR-GUARD",
    "PREVENT-CVD", "CARDIAC-CARE", "HEART-SAFE", "CVD-PREVENT"
  ),
  
  # Risk ratios
  risk_ratio = c(0.82, 0.91, 0.76, 0.88, 0.79, 0.85, 0.74, 0.90),
  
  # Confidence intervals
  rr_lower = c(0.68, 0.79, 0.62, 0.74, 0.65, 0.71, 0.61, 0.76),
  rr_upper = c(0.99, 1.05, 0.93, 1.05, 0.96, 1.02, 0.90, 1.07),
  
  # Study details
  participants = c(8934, 12456, 6789, 15678, 5432, 9876, 7234, 11234),
  events_treatment = c(234, 345, 189, 423, 156, 267, 178, 298),
  events_control = c(287, 378, 248, 481, 197, 314, 241, 331),
  
  # Follow-up duration (years)
  followup_years = c(5.2, 6.8, 4.1, 7.3, 3.9, 5.7, 4.8, 6.2),
  
  stringsAsFactors = FALSE
)

# Dataset 3: Hazard Ratios for survival analysis
jforester_survival_hr_data <- data.frame(
  study_id = paste0("SURV-", sprintf("%03d", 1:10)),
  
  # Hazard ratios
  hazard_ratio = c(
    0.67, 0.82, 0.59, 0.75, 0.71, 0.78, 0.64, 0.86, 0.69, 0.73
  ),
  
  # Confidence intervals
  hr_lower = c(
    0.52, 0.68, 0.45, 0.61, 0.57, 0.63, 0.50, 0.71, 0.55, 0.58
  ),
  hr_upper = c(
    0.86, 0.99, 0.77, 0.92, 0.88, 0.96, 0.82, 1.04, 0.87, 0.92
  ),
  
  # Sample characteristics
  total_patients = c(567, 834, 423, 712, 645, 789, 456, 923, 578, 691),
  deaths = c(189, 267, 145, 234, 201, 256, 152, 298, 187, 223),
  
  # Cancer types for subgroup analysis
  cancer_type = c(
    "Lung", "Breast", "Colorectal", "Lung", "Breast", 
    "Prostate", "Colorectal", "Lung", "Breast", "Prostate"
  ),
  
  treatment_type = c(
    "Immunotherapy", "Targeted", "Chemotherapy", "Immunotherapy", "Targeted",
    "Hormone", "Chemotherapy", "Targeted", "Immunotherapy", "Hormone"
  ),
  
  stringsAsFactors = FALSE
)

# Dataset 4: Mean Differences for continuous outcomes
jforester_mean_diff_data <- data.frame(
  study_ref = c(
    "Cognitive Study A", "Cognitive Study B", "Cognitive Study C",
    "Cognitive Study D", "Cognitive Study E", "Cognitive Study F",
    "Cognitive Study G", "Cognitive Study H"
  ),
  
  # Mean differences in cognitive scores
  mean_difference = c(2.34, 1.87, 3.12, 1.56, 2.78, 2.91, 1.23, 2.45),
  
  # Confidence intervals
  md_lower = c(0.89, 0.45, 1.67, 0.23, 1.34, 1.56, -0.12, 0.98),
  md_upper = c(3.79, 3.29, 4.57, 2.89, 4.22, 4.26, 2.58, 3.92),
  
  # Study characteristics
  sample_n = c(89, 134, 76, 156, 98, 112, 187, 145),
  intervention_duration_weeks = c(12, 16, 8, 20, 14, 18, 10, 22),
  
  # Age groups
  age_group = c(
    "65-75", "75-85", "65-75", ">85", "75-85", 
    "65-75", ">85", "75-85"
  ),
  
  # Intervention types
  intervention = c(
    "Exercise", "Cognitive Training", "Exercise", "Diet",
    "Combined", "Exercise", "Diet", "Cognitive Training"
  ),
  
  stringsAsFactors = FALSE
)

# Dataset 5: Standardized Mean Differences for effect sizes
jforester_smd_data <- data.frame(
  paper_citation = c(
    "Williams 2021", "Roberts 2020", "Evans 2019", "Parker 2021",
    "Collins 2020", "Murphy 2019", "Cooper 2021", "Reed 2020"
  ),
  
  # Standardized mean differences (Cohen's d)
  cohens_d = c(0.45, 0.67, 0.32, 0.78, 0.56, 0.89, 0.23, 0.61),
  
  # Confidence intervals
  smd_lower = c(0.12, 0.34, -0.01, 0.45, 0.23, 0.56, -0.10, 0.28),
  smd_upper = c(0.78, 1.00, 0.65, 1.11, 0.89, 1.22, 0.56, 0.94),
  
  # Sample sizes for each group
  treatment_n = c(45, 67, 89, 34, 56, 78, 91, 52),
  control_n = c(43, 69, 87, 36, 54, 76, 89, 50),
  
  # Effect size categories
  effect_magnitude = c(
    "Medium", "Medium", "Small", "Large", "Medium", 
    "Large", "Small", "Medium"
  ),
  
  # Research domains
  domain = c(
    "Education", "Psychology", "Medicine", "Education",
    "Psychology", "Medicine", "Education", "Psychology"
  ),
  
  stringsAsFactors = FALSE
)

# Dataset 6: Complex meta-analysis with subgroups
jforester_complex_meta_data <- data.frame(
  study_label = paste0("Study ", LETTERS[1:15]),
  
  # Effect estimates (odds ratios)
  effect_estimate = c(
    1.23, 1.45, 0.89, 1.67, 1.12, 1.78, 0.95, 1.34,
    1.56, 0.82, 1.91, 1.28, 1.43, 0.76, 1.52
  ),
  
  # Confidence intervals
  lower_ci = c(
    0.98, 1.12, 0.67, 1.29, 0.87, 1.34, 0.71, 1.01,
    1.18, 0.59, 1.52, 0.95, 1.09, 0.54, 1.14
  ),
  upper_ci = c(
    1.54, 1.88, 1.18, 2.16, 1.44, 2.37, 1.27, 1.78,
    2.06, 1.14, 2.40, 1.72, 1.88, 1.07, 2.03
  ),
  
  # Study characteristics
  total_participants = c(
    567, 834, 423, 712, 645, 789, 456, 923, 578, 691,
    734, 512, 889, 456, 623
  ),
  
  study_design = rep(c("RCT", "Cohort", "Case-Control"), length.out = 15),
  
  geographic_region = rep(c("North America", "Europe", "Asia", "Australia", "South America"), 
                         length.out = 15),
  
  publication_year = sample(2015:2021, 15, replace = TRUE),
  
  risk_of_bias = sample(c("Low", "Moderate", "High"), 15, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  
  stringsAsFactors = FALSE
)

# Save all datasets
save(jforester_meta_analysis_data, 
     file = "data/jforester_meta_analysis_data.rda", 
     compress = "xz")

save(jforester_cardio_rr_data, 
     file = "data/jforester_cardio_rr_data.rda", 
     compress = "xz")

save(jforester_survival_hr_data, 
     file = "data/jforester_survival_hr_data.rda", 
     compress = "xz")

save(jforester_mean_diff_data, 
     file = "data/jforester_mean_diff_data.rda", 
     compress = "xz")

save(jforester_smd_data, 
     file = "data/jforester_smd_data.rda", 
     compress = "xz")

save(jforester_complex_meta_data, 
     file = "data/jforester_complex_meta_data.rda", 
     compress = "xz")

# Print summary information
cat("Created jforester test datasets:\n")
cat("1. jforester_meta_analysis_data: Meta-analysis with odds ratios (n=", nrow(jforester_meta_analysis_data), ")\n")
cat("2. jforester_cardio_rr_data: Cardiovascular risk ratios (n=", nrow(jforester_cardio_rr_data), ")\n")
cat("3. jforester_survival_hr_data: Survival hazard ratios (n=", nrow(jforester_survival_hr_data), ")\n")
cat("4. jforester_mean_diff_data: Mean differences (n=", nrow(jforester_mean_diff_data), ")\n")
cat("5. jforester_smd_data: Standardized mean differences (n=", nrow(jforester_smd_data), ")\n")
cat("6. jforester_complex_meta_data: Complex meta-analysis (n=", nrow(jforester_complex_meta_data), ")\n")

cat("\nAll datasets saved to data/ directory\n")
