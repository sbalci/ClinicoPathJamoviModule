# TidyDensity Example Data Generation
# This script creates example datasets for demonstrating TidyDensity functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(TidyDensity)
library(dplyr)
library(usethis)

# Set seed for reproducibility
set.seed(12345)

# 1. Basic Normal Distribution Example
# Simple dataset for educational purposes
tidydensity_normal_example <- TidyDensity::tidy_normal(
  .n = 100,
  .mean = 100,  # Could represent systolic blood pressure
  .sd = 15,
  .num_sims = 3
) %>%
  dplyr::mutate(
    distribution_name = "Normal",
    clinical_context = "Systolic Blood Pressure (mmHg)"
  )

# 2. Clinical Biomarker Distribution Examples
# Multiple distributions representing different biomarkers

# Gamma distribution for PSA levels
tidydensity_gamma_psa <- TidyDensity::tidy_gamma(
  .n = 150,
  .shape = 2,
  .scale = 2,
  .num_sims = 2
) %>%
  dplyr::mutate(
    distribution_name = "Gamma",
    clinical_context = "PSA Levels (ng/mL)",
    biomarker_type = "Tumor Marker"
  )

# Beta distribution for response rates
tidydensity_beta_response <- TidyDensity::tidy_beta(
  .n = 200,
  .shape1 = 3,
  .shape2 = 7,
  .num_sims = 4
) %>%
  dplyr::mutate(
    distribution_name = "Beta",
    clinical_context = "Response Rate Proportion",
    study_type = "Clinical Trial"
  )

# Log-normal distribution for drug concentrations
tidydensity_lognormal_drug <- TidyDensity::tidy_lognormal(
  .n = 120,
  .meanlog = 2,
  .sdlog = 0.5,
  .num_sims = 2
) %>%
  dplyr::mutate(
    distribution_name = "Log-Normal",
    clinical_context = "Drug Concentration (μg/mL)",
    pharmacology = "PK Study"
  )

# 3. Survival/Time-to-Event Example
# Exponential distribution for survival times
tidydensity_exponential_survival <- TidyDensity::tidy_exponential(
  .n = 100,
  .rate = 0.1,  # corresponds to median survival of ~7 time units
  .num_sims = 3
) %>%
  dplyr::mutate(
    distribution_name = "Exponential",
    clinical_context = "Survival Time (months)",
    study_design = "Oncology Trial"
  )

# 4. Count Data Example
# Poisson distribution for adverse events
tidydensity_poisson_events <- TidyDensity::tidy_poisson(
  .n = 180,
  .lambda = 2.5,
  .num_sims = 3
) %>%
  dplyr::mutate(
    distribution_name = "Poisson",
    clinical_context = "Number of Adverse Events",
    safety_profile = "Drug Safety Study"
  )

# 5. Laboratory Values Example
# Comprehensive dataset with multiple distributions for lab values
create_lab_values_dataset <- function() {
  
  # Hemoglobin (normal distribution)
  hgb <- TidyDensity::tidy_normal(.n = 50, .mean = 14, .sd = 2, .num_sims = 1) %>%
    dplyr::mutate(lab_test = "Hemoglobin", units = "g/dL", reference_range = "12-16")
  
  # White Blood Cell Count (gamma distribution)
  wbc <- TidyDensity::tidy_gamma(.n = 50, .shape = 3, .scale = 2, .num_sims = 1) %>%
    dplyr::mutate(lab_test = "WBC", units = "x10^3/μL", reference_range = "4-11")
  
  # Platelet Count (gamma distribution, higher values)
  plt <- TidyDensity::tidy_gamma(.n = 50, .shape = 4, .scale = 60, .num_sims = 1) %>%
    dplyr::mutate(lab_test = "Platelets", units = "x10^3/μL", reference_range = "150-450")
  
  # Combine datasets
  lab_values <- dplyr::bind_rows(hgb, wbc, plt) %>%
    dplyr::mutate(
      patient_category = "Routine Lab Work",
      clinical_significance = "Standard Hematology Panel"
    )
  
  return(lab_values)
}

tidydensity_lab_values <- create_lab_values_dataset()

# 6. Quality Control Dataset
# Multiple simulations for quality control analysis
tidydensity_qc_analysis <- TidyDensity::tidy_normal(
  .n = 80,
  .mean = 250,  # Target value for analytical instrument
  .sd = 12,     # Acceptable variation
  .num_sims = 10  # Multiple quality control runs
) %>%
  dplyr::mutate(
    distribution_name = "Normal",
    clinical_context = "Quality Control Measurements",
    instrument_type = "Automated Analyzer",
    target_value = 250,
    acceptable_cv = 5  # 5% coefficient of variation
  )

# 7. Power Analysis Example Dataset
# Different effect sizes for power calculations
create_power_analysis_data <- function() {
  
  # Small effect size
  small_effect <- TidyDensity::tidy_normal(.n = 100, .mean = 0.2, .sd = 1, .num_sims = 5) %>%
    dplyr::mutate(effect_size = "Small (0.2)", power_analysis = "Sample Size Calculation")
  
  # Medium effect size
  medium_effect <- TidyDensity::tidy_normal(.n = 100, .mean = 0.5, .sd = 1, .num_sims = 5) %>%
    dplyr::mutate(effect_size = "Medium (0.5)", power_analysis = "Sample Size Calculation")
  
  # Large effect size
  large_effect <- TidyDensity::tidy_normal(.n = 100, .mean = 0.8, .sd = 1, .num_sims = 5) %>%
    dplyr::mutate(effect_size = "Large (0.8)", power_analysis = "Sample Size Calculation")
  
  power_data <- dplyr::bind_rows(small_effect, medium_effect, large_effect) %>%
    dplyr::mutate(
      study_design = "RCT Power Analysis",
      alpha_level = 0.05,
      desired_power = 0.80
    )
  
  return(power_data)
}

tidydensity_power_analysis <- create_power_analysis_data()

# 8. Educational Examples Dataset
# Comprehensive comparison of distribution shapes
create_distribution_comparison <- function() {
  
  # Create multiple distributions with same sample size for comparison
  n_obs <- 100
  n_sims <- 2
  
  normal_dist <- TidyDensity::tidy_normal(.n = n_obs, .mean = 0, .sd = 1, .num_sims = n_sims) %>%
    dplyr::mutate(distribution_family = "Normal", shape_characteristic = "Symmetric, Bell-shaped")
  
  gamma_dist <- TidyDensity::tidy_gamma(.n = n_obs, .shape = 2, .scale = 1, .num_sims = n_sims) %>%
    dplyr::mutate(distribution_family = "Gamma", shape_characteristic = "Right-skewed, Positive")
  
  beta_dist <- TidyDensity::tidy_beta(.n = n_obs, .shape1 = 2, .shape2 = 5, .num_sims = n_sims) %>%
    dplyr::mutate(distribution_family = "Beta", shape_characteristic = "Bounded [0,1], Flexible")
  
  uniform_dist <- TidyDensity::tidy_uniform(.n = n_obs, .min = 0, .max = 1, .num_sims = n_sims) %>%
    dplyr::mutate(distribution_family = "Uniform", shape_characteristic = "Flat, Equal probability")
  
  comparison_data <- dplyr::bind_rows(normal_dist, gamma_dist, beta_dist, uniform_dist) %>%
    dplyr::mutate(
      educational_purpose = "Distribution Shape Comparison",
      statistical_properties = "Teaching Statistics"
    )
  
  return(comparison_data)
}

tidydensity_distribution_comparison <- create_distribution_comparison()

# Save all datasets
use_data_multi_format(tidydensity_normal_example, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_gamma_psa, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_beta_response, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_lognormal_drug, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_exponential_survival, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_poisson_events, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_lab_values, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_qc_analysis, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_power_analysis, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(tidydensity_distribution_comparison, overwrite = TRUE, save_csv = TRUE)

# Create CSV versions for easier access
write.csv(tidydensity_normal_example, "data/tidydensity_normal_example.csv", row.names = FALSE)
write.csv(tidydensity_gamma_psa, "data/tidydensity_gamma_psa.csv", row.names = FALSE)
write.csv(tidydensity_beta_response, "data/tidydensity_beta_response.csv", row.names = FALSE)
write.csv(tidydensity_lognormal_drug, "data/tidydensity_lognormal_drug.csv", row.names = FALSE)
write.csv(tidydensity_exponential_survival, "data/tidydensity_exponential_survival.csv", row.names = FALSE)
write.csv(tidydensity_poisson_events, "data/tidydensity_poisson_events.csv", row.names = FALSE)
write.csv(tidydensity_lab_values, "data/tidydensity_lab_values.csv", row.names = FALSE)
write.csv(tidydensity_qc_analysis, "data/tidydensity_qc_analysis.csv", row.names = FALSE)
write.csv(tidydensity_power_analysis, "data/tidydensity_power_analysis.csv", row.names = FALSE)
write.csv(tidydensity_distribution_comparison, "data/tidydensity_distribution_comparison.csv", row.names = FALSE)

# Print summary information
cat("TidyDensity example datasets created successfully!\n")
cat("Datasets generated:\n")
cat("1. tidydensity_normal_example - Basic normal distribution\n")
cat("2. tidydensity_gamma_psa - PSA biomarker levels\n") 
cat("3. tidydensity_beta_response - Clinical response rates\n")
cat("4. tidydensity_lognormal_drug - Drug concentrations\n")
cat("5. tidydensity_exponential_survival - Survival times\n")
cat("6. tidydensity_poisson_events - Adverse event counts\n")
cat("7. tidydensity_lab_values - Laboratory values\n")
cat("8. tidydensity_qc_analysis - Quality control data\n")
cat("9. tidydensity_power_analysis - Power analysis scenarios\n")
cat("10. tidydensity_distribution_comparison - Educational comparisons\n")
