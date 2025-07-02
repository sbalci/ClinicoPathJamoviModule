# Create comprehensive test datasets for coefplot function
# This script generates realistic datasets for testing coefficient plots

library(dplyr)

set.seed(123)

# Create comprehensive regression analysis dataset
n_patients <- 400

# Helper function to create realistic clinical correlations
create_correlated_data <- function(n, base_var, correlation = 0.3, noise_sd = 1) {
  correlated_var <- base_var * correlation + rnorm(n, 0, noise_sd)
  return(correlated_var)
}

# Generate primary dataset for coefficient plot testing
coefplot_test_data <- data.frame(
  # Patient identifiers
  patient_id = 1:n_patients,
  
  # Demographics
  age = round(rnorm(n_patients, 65, 12)),
  sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.45, 0.55))),
  bmi = rnorm(n_patients, 26, 4),
  
  # Clinical characteristics
  smoking_status = factor(sample(c("Never", "Former", "Current"), n_patients, 
                                replace = TRUE, prob = c(0.4, 0.4, 0.2))),
  
  # Disease stage (with realistic progression)
  stage_numeric = sample(1:4, n_patients, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
  
  # Laboratory values
  hemoglobin = rnorm(n_patients, 12.5, 2),
  creatinine = rlnorm(n_patients, 0, 0.3),
  
  # Biomarkers
  biomarker_a = rnorm(n_patients, 50, 15),
  biomarker_b = rnorm(n_patients, 100, 25)
)

# Create realistic correlations and derived variables
coefplot_test_data <- coefplot_test_data %>%
  mutate(
    # Ensure realistic ranges
    age = pmax(18, pmin(95, age)),
    bmi = pmax(15, pmin(45, bmi)),
    hemoglobin = pmax(7, pmin(18, hemoglobin)),
    creatinine = pmax(0.5, pmin(5, creatinine)),
    biomarker_a = pmax(0, pmin(200, biomarker_a)),
    biomarker_b = pmax(0, pmin(300, biomarker_b)),
    
    # Create stage factor
    stage = factor(stage_numeric, levels = 1:4, labels = c("I", "II", "III", "IV")),
    
    # Create treatment variable (more likely for advanced stages)
    treatment_prob = ifelse(stage_numeric >= 3, 0.8, 0.3),
    treatment = factor(ifelse(runif(n_patients) < treatment_prob, "Yes", "No")),
    
    # Create correlated risk score
    risk_score = 10 + 
                0.1 * age + 
                0.5 * stage_numeric + 
                ifelse(sex == "Male", 2, 0) +
                ifelse(smoking_status == "Current", 3, 
                       ifelse(smoking_status == "Former", 1, 0)) +
                0.02 * biomarker_a +
                rnorm(n_patients, 0, 2),
    
    # Continuous outcome (e.g., quality of life score)
    quality_of_life = 80 - 
                     0.2 * age +
                     ifelse(sex == "Female", 3, 0) -
                     2 * stage_numeric -
                     ifelse(smoking_status == "Current", 5, 0) +
                     ifelse(treatment == "Yes", 8, 0) +
                     0.1 * biomarker_b +
                     rnorm(n_patients, 0, 8),
    
    # Binary outcome (e.g., treatment response)
    response_logit = -1.5 + 
                    0.02 * age +
                    ifelse(sex == "Male", 0.3, 0) +
                    0.4 * stage_numeric +
                    ifelse(treatment == "Yes", 1.2, 0) +
                    0.01 * biomarker_a +
                    rnorm(n_patients, 0, 0.5),
    
    treatment_response = factor(ifelse(plogis(response_logit) > 0.5, "Responder", "Non-responder")),
    
    # Survival time (exponential with covariates)
    hazard_linear = 0.01 * age +
                   ifelse(sex == "Male", 0.2, 0) +
                   0.3 * stage_numeric +
                   ifelse(treatment == "Yes", -0.4, 0) +
                   0.005 * biomarker_a,
    
    survival_time = rexp(n_patients, rate = exp(hazard_linear - 2)),
    
    # Censoring (administrative and random)
    censoring_time = runif(n_patients, 24, 60), # 2-5 years follow-up
    observed_time = pmin(survival_time, censoring_time),
    event_occurred = factor(ifelse(survival_time <= censoring_time, "Yes", "No")),
    
    # Count outcome (e.g., number of hospitalizations)
    count_linear = 0.5 + 
                  0.01 * age +
                  0.2 * stage_numeric +
                  ifelse(treatment == "Yes", -0.3, 0),
    
    hospitalization_count = rpois(n_patients, lambda = exp(count_linear)),
    
    # Ensure realistic ranges for outcomes
    quality_of_life = pmax(0, pmin(100, quality_of_life)),
    observed_time = pmax(0.1, observed_time),
    survival_time = pmax(0.1, survival_time)
  )

# Create a simplified dataset for basic testing
coefplot_simple_data <- coefplot_test_data %>%
  select(
    patient_id,
    age,
    sex,
    stage,
    treatment,
    biomarker_a,
    quality_of_life,
    treatment_response,
    observed_time,
    event_occurred,
    hospitalization_count
  ) %>%
  slice_head(n = 200)  # Smaller dataset for quick testing

# Create a dataset with specific scenarios for edge case testing
coefplot_edge_cases <- data.frame(
  # Minimal dataset
  outcome_continuous = rnorm(50, 50, 10),
  outcome_binary = factor(sample(c("Yes", "No"), 50, replace = TRUE)),
  predictor_1 = rnorm(50, 0, 1),
  predictor_2 = rnorm(50, 10, 2),
  category = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
  
  # Survival data
  time_var = rexp(50, 0.1),
  event_var = factor(sample(c("Event", "Censored"), 50, replace = TRUE, prob = c(0.6, 0.4))),
  
  # Count data
  count_outcome = rpois(50, 3)
)

# Add some missing values for realistic testing
coefplot_edge_cases$predictor_1[sample(1:50, 5)] <- NA
coefplot_edge_cases$category[sample(1:50, 3)] <- NA

# Create a dataset for advanced features testing
coefplot_advanced_data <- coefplot_test_data %>%
  mutate(
    # Additional predictors for model comparison
    genetic_score = rnorm(n_patients, 0, 1),
    comorbidity_index = rpois(n_patients, 2),
    
    # Interaction effects
    age_stage_interaction = age * stage_numeric,
    
    # Transformed variables
    log_biomarker_a = log(biomarker_a + 1),
    squared_age = age^2,
    
    # Additional categorical variables
    center = factor(sample(LETTERS[1:5], n_patients, replace = TRUE)),
    insurance = factor(sample(c("Private", "Public", "None"), n_patients, 
                             replace = TRUE, prob = c(0.6, 0.35, 0.05)))
  )

# Save all datasets
usethis::use_data(coefplot_test_data, overwrite = TRUE)
usethis::use_data(coefplot_simple_data, overwrite = TRUE)
usethis::use_data(coefplot_edge_cases, overwrite = TRUE)
usethis::use_data(coefplot_advanced_data, overwrite = TRUE)

# Print summary information
cat("✅ Created coefficient plot test datasets:\n\n")

cat("📊 coefplot_test_data:\n")
cat("   - Size:", nrow(coefplot_test_data), "patients\n")
cat("   - Variables:", ncol(coefplot_test_data), "\n")
cat("   - Outcome types: Continuous, binary, survival, count\n")
cat("   - Realistic clinical correlations and effects\n\n")

cat("📊 coefplot_simple_data:\n")
cat("   - Size:", nrow(coefplot_simple_data), "patients\n")
cat("   - Variables:", ncol(coefplot_simple_data), "\n")
cat("   - Essential variables for basic testing\n")
cat("   - Subset of main dataset for quick analysis\n\n")

cat("📊 coefplot_edge_cases:\n")
cat("   - Size:", nrow(coefplot_edge_cases), "observations\n")
cat("   - Variables:", ncol(coefplot_edge_cases), "\n")
cat("   - Edge cases: Missing data, minimal samples\n")
cat("   - All regression outcome types included\n\n")

cat("📊 coefplot_advanced_data:\n")
cat("   - Size:", nrow(coefplot_advanced_data), "patients\n")
cat("   - Variables:", ncol(coefplot_advanced_data), "\n")
cat("   - Advanced features: Interactions, transformations\n")
cat("   - Multi-center and complex modeling scenarios\n\n")

cat("💡 Usage examples:\n")
cat("   coefplot(data = coefplot_simple_data, dep = 'quality_of_life', \n")
cat("            covs = c('age', 'sex', 'stage'), model_type = 'linear')\n\n")
cat("   coefplot(data = coefplot_test_data, dep = 'treatment_response',\n")
cat("            covs = c('age', 'stage', 'biomarker_a'), model_type = 'logistic')\n\n")
cat("   coefplot(data = coefplot_test_data, dep = 'event_occurred', time_var = 'observed_time',\n")
cat("            covs = c('age', 'stage', 'treatment'), model_type = 'cox')\n")

# Display variable summary
cat("\n📋 Variable Summary:\n")
str(coefplot_test_data)