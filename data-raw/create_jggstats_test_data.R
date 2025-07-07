# Create test data for jggstats function
# This script generates comprehensive datasets for testing enhanced statistical visualization

set.seed(123)

# Load required libraries
library(dplyr)

# 1. Linear Model Test Data
linear_model_data <- data.frame(
  outcome = rnorm(200, 50, 15),
  age = sample(18:80, 200, replace = TRUE),
  sex = sample(c("Male", "Female"), 200, replace = TRUE),
  treatment = sample(c("Control", "Treatment A", "Treatment B"), 200, replace = TRUE),
  baseline_score = rnorm(200, 100, 20),
  weight = runif(200, 0.5, 2.0),  # Survey weights
  group = sample(c("Group1", "Group2", "Group3"), 200, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Graduate"), 200, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 200, replace = TRUE)
)

# Add some interactions to the outcome
linear_model_data$outcome <- linear_model_data$outcome + 
  ifelse(linear_model_data$sex == "Female", 5, 0) +
  ifelse(linear_model_data$treatment == "Treatment A", 8, 0) +
  ifelse(linear_model_data$treatment == "Treatment B", 12, 0) +
  0.2 * linear_model_data$age +
  0.1 * linear_model_data$baseline_score

# 2. Logistic Regression Test Data
logistic_model_data <- data.frame(
  disease = rbinom(300, 1, 0.3),
  age = sample(25:75, 300, replace = TRUE),
  sex = sample(c("Male", "Female"), 300, replace = TRUE),
  smoking = sample(c("Never", "Former", "Current"), 300, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  family_history = sample(c("Yes", "No"), 300, replace = TRUE, prob = c(0.2, 0.8)),
  bmi = rnorm(300, 26, 4),
  biomarker_level = rnorm(300, 5, 2),
  weight = runif(300, 0.8, 1.5),
  clinic = sample(c("A", "B", "C", "D"), 300, replace = TRUE),
  risk_score = rnorm(300, 0, 1)
)

# Adjust disease probability based on predictors
log_odds <- -2 + 
  0.05 * logistic_model_data$age + 
  ifelse(logistic_model_data$sex == "Male", 0.5, 0) +
  ifelse(logistic_model_data$smoking == "Current", 1.2, 
         ifelse(logistic_model_data$smoking == "Former", 0.6, 0)) +
  ifelse(logistic_model_data$family_history == "Yes", 0.8, 0) +
  0.1 * logistic_model_data$bmi +
  0.3 * logistic_model_data$biomarker_level

logistic_model_data$disease <- rbinom(300, 1, plogis(log_odds))

# 3. Likert Scale Survey Data
likert_survey_data <- data.frame(
  respondent_id = 1:400,
  age_group = sample(c("18-30", "31-45", "46-60", "60+"), 400, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Graduate", "PhD"), 400, replace = TRUE),
  department = sample(c("Sales", "Marketing", "Operations", "HR", "IT"), 400, replace = TRUE),
  experience_years = sample(1:25, 400, replace = TRUE),
  
  # Likert scale items (1-5 scale)
  satisfaction_work = sample(1:5, 400, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1)),
  satisfaction_pay = sample(1:5, 400, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.25, 0.1)),
  satisfaction_benefits = sample(1:5, 400, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.35, 0.1)),
  workload_manageable = sample(1:5, 400, replace = TRUE, prob = c(0.2, 0.3, 0.25, 0.2, 0.05)),
  recommend_company = sample(1:5, 400, replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.35, 0.2)),
  
  # Survey weights
  weight = runif(400, 0.5, 2.0),
  region = sample(c("North", "South", "East", "West", "Central"), 400, replace = TRUE)
)

# 4. Survival Analysis Data
survival_analysis_data <- data.frame(
  patient_id = 1:250,
  age = sample(40:85, 250, replace = TRUE),
  sex = sample(c("Male", "Female"), 250, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), 250, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
  treatment = sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"), 250, replace = TRUE),
  biomarker_positive = sample(c("Yes", "No"), 250, replace = TRUE, prob = c(0.4, 0.6)),
  comorbidity_score = sample(0:3, 250, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
  hospital = sample(c("Hospital_A", "Hospital_B", "Hospital_C"), 250, replace = TRUE),
  
  # Survival outcomes
  survival_time = rexp(250, rate = 0.1),
  event = rbinom(250, 1, 0.6),
  weight = runif(250, 0.8, 1.2)
)

# 5. Mixed Effects Model Data (Longitudinal)
mixed_effects_data <- data.frame(
  subject_id = rep(1:80, each = 5),
  visit = rep(1:5, 80),
  time_months = rep(c(0, 3, 6, 12, 24), 80),
  score = rnorm(400, 50, 10),
  age = rep(sample(20:70, 80, replace = TRUE), each = 5),
  sex = rep(sample(c("Male", "Female"), 80, replace = TRUE), each = 5),
  treatment_group = rep(sample(c("Placebo", "Low Dose", "High Dose"), 80, replace = TRUE), each = 5),
  baseline_severity = rep(sample(c("Mild", "Moderate", "Severe"), 80, replace = TRUE), each = 5),
  clinic_id = rep(sample(1:8, 80, replace = TRUE), each = 5),
  weight = runif(400, 0.9, 1.1)
)

# Add time trends to the score
mixed_effects_data$score <- mixed_effects_data$score + 
  ifelse(mixed_effects_data$treatment_group == "High Dose", 
         2 * mixed_effects_data$time_months, 0) +
  ifelse(mixed_effects_data$treatment_group == "Low Dose", 
         1 * mixed_effects_data$time_months, 0) +
  rnorm(400, 0, 5)  # Random error

# 6. Survey Proportion Data
survey_proportion_data <- data.frame(
  respondent_id = 1:500,
  political_preference = sample(c("Liberal", "Conservative", "Independent", "Other"), 500, 
                              replace = TRUE, prob = c(0.35, 0.30, 0.25, 0.10)),
  age_category = sample(c("18-29", "30-44", "45-59", "60+"), 500, replace = TRUE),
  education_level = sample(c("No Degree", "High School", "Some College", "Bachelor+"), 500, 
                         replace = TRUE, prob = c(0.15, 0.30, 0.25, 0.30)),
  income_bracket = sample(c("Under 30k", "30-50k", "50-75k", "75k+"), 500, replace = TRUE),
  region = sample(c("Northeast", "Southeast", "Midwest", "Southwest", "West"), 500, replace = TRUE),
  urban_rural = sample(c("Urban", "Suburban", "Rural"), 500, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  
  # Survey responses
  approve_policy = sample(c("Strongly Approve", "Approve", "Neutral", "Disapprove", "Strongly Disapprove"), 
                        500, replace = TRUE),
  voting_likelihood = sample(c("Definitely", "Probably", "Maybe", "Probably Not", "Definitely Not"), 
                           500, replace = TRUE),
  
  # Survey weights (to account for sampling design)
  weight = runif(500, 0.3, 2.5),
  stratum = sample(1:10, 500, replace = TRUE)
)

# 7. Cross-tabulation Analysis Data
cross_tab_data <- data.frame(
  case_id = 1:350,
  disease_status = sample(c("Positive", "Negative"), 350, replace = TRUE, prob = c(0.3, 0.7)),
  test_result = sample(c("Positive", "Negative"), 350, replace = TRUE),
  age_group = sample(c("Young", "Middle", "Old"), 350, replace = TRUE),
  sex = sample(c("Male", "Female"), 350, replace = TRUE),
  risk_category = sample(c("Low", "Medium", "High"), 350, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  clinic_type = sample(c("Academic", "Community", "Private"), 350, replace = TRUE),
  insurance = sample(c("Public", "Private", "None"), 350, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
  weight = runif(350, 0.8, 1.5),
  geographical_region = sample(c("Urban", "Suburban", "Rural"), 350, replace = TRUE)
)

# Adjust test_result based on disease_status (add some accuracy)
cross_tab_data$test_result <- ifelse(
  cross_tab_data$disease_status == "Positive",
  sample(c("Positive", "Negative"), 350, replace = TRUE, prob = c(0.85, 0.15)),  # 85% sensitivity
  sample(c("Positive", "Negative"), 350, replace = TRUE, prob = c(0.10, 0.90))   # 90% specificity
)

# 8. Weighted Analysis Data (Complex Survey Design)
weighted_analysis_data <- data.frame(
  household_id = 1:300,
  household_size = sample(1:6, 300, replace = TRUE, prob = c(0.25, 0.35, 0.20, 0.12, 0.06, 0.02)),
  income_thousands = round(rlnorm(300, log(50), 0.6), 1),
  head_education = sample(c("Less than HS", "High School", "Some College", "Bachelor", "Graduate"), 
                        300, replace = TRUE, prob = c(0.12, 0.25, 0.23, 0.25, 0.15)),
  geography = sample(c("Metropolitan", "Micropolitan", "Rural"), 300, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
  state_region = sample(c("Northeast", "Midwest", "South", "West"), 300, replace = TRUE),
  
  # Health outcomes
  health_rating = sample(c("Excellent", "Very Good", "Good", "Fair", "Poor"), 
                       300, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.15, 0.05)),
  insurance_coverage = sample(c("Full", "Partial", "None"), 300, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
  
  # Complex survey weights (post-stratification, non-response, etc.)
  sampling_weight = runif(300, 0.2, 3.5),
  poststrat_weight = runif(300, 0.8, 1.4),
  final_weight = runif(300, 0.3, 4.0),
  
  # Survey design variables
  stratum = sample(1:8, 300, replace = TRUE),
  cluster = sample(1:50, 300, replace = TRUE)
)

# Calculate final weight as product of component weights
weighted_analysis_data$final_weight <- weighted_analysis_data$sampling_weight * 
                                     weighted_analysis_data$poststrat_weight

# 9. Model Comparison Data (Educational Achievement)
model_comparison_data <- data.frame(
  student_id = 1:400,
  test_score = rnorm(400, 75, 15),
  socioeconomic_status = sample(c("Low", "Middle", "High"), 400, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  parent_education = sample(c("No Degree", "High School", "Some College", "Bachelor+"), 400, 
                          replace = TRUE, prob = c(0.15, 0.35, 0.25, 0.25)),
  school_type = sample(c("Public", "Private", "Charter"), 400, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
  class_size = sample(15:35, 400, replace = TRUE),
  teacher_experience = sample(1:30, 400, replace = TRUE),
  school_funding = sample(c("Low", "Medium", "High"), 400, replace = TRUE),
  special_programs = sample(c("Yes", "No"), 400, replace = TRUE, prob = c(0.3, 0.7)),
  hours_homework = sample(0:6, 400, replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.25, 0.2, 0.08, 0.02)),
  extracurricular = sample(c("None", "Sports", "Arts", "Academic", "Multiple"), 400, 
                         replace = TRUE, prob = c(0.2, 0.25, 0.2, 0.15, 0.2)),
  district = sample(c("District_A", "District_B", "District_C", "District_D"), 400, replace = TRUE)
)

# Adjust test scores based on predictors
model_comparison_data$test_score <- model_comparison_data$test_score +
  ifelse(model_comparison_data$socioeconomic_status == "High", 10,
         ifelse(model_comparison_data$socioeconomic_status == "Middle", 5, 0)) +
  ifelse(model_comparison_data$parent_education == "Bachelor+", 8,
         ifelse(model_comparison_data$parent_education == "Some College", 4,
                ifelse(model_comparison_data$parent_education == "High School", 2, 0))) +
  ifelse(model_comparison_data$school_type == "Private", 5, 0) +
  rnorm(400, 0, 5)

# Save all datasets
save(linear_model_data, file = "data/linear_model_data.rda")
save(logistic_model_data, file = "data/logistic_model_data.rda")
save(likert_survey_data, file = "data/likert_survey_data.rda")
save(survival_analysis_data, file = "data/survival_analysis_data.rda")
save(mixed_effects_data, file = "data/mixed_effects_data.rda")
save(survey_proportion_data, file = "data/survey_proportion_data.rda")
save(cross_tab_data, file = "data/cross_tab_data.rda")
save(weighted_analysis_data, file = "data/weighted_analysis_data.rda")
save(model_comparison_data, file = "data/model_comparison_data.rda")

# Create combined dataset for comprehensive testing
jggstats_comprehensive_data <- list(
  linear_model = linear_model_data,
  logistic_model = logistic_model_data,
  likert_survey = likert_survey_data,
  survival_analysis = survival_analysis_data,
  mixed_effects = mixed_effects_data,
  survey_proportion = survey_proportion_data,
  cross_tabulation = cross_tab_data,
  weighted_analysis = weighted_analysis_data,
  model_comparison = model_comparison_data
)

save(jggstats_comprehensive_data, file = "data/jggstats_comprehensive_data.rda")

message("Successfully created comprehensive test datasets for jggstats function:")
message("- linear_model_data.rda")
message("- logistic_model_data.rda")
message("- likert_survey_data.rda")
message("- survival_analysis_data.rda")
message("- mixed_effects_data.rda")
message("- survey_proportion_data.rda")
message("- cross_tab_data.rda")
message("- weighted_analysis_data.rda")
message("- model_comparison_data.rda")
message("- jggstats_comprehensive_data.rda")