#' Example Datasets for Bayesian Decision Curve Analysis
#'
#' These datasets provide realistic examples for demonstrating Bayesian Decision Curve Analysis
#' functionality across different clinical scenarios.
#'
#' @name bayesdca_examples
#' @docType data
#' @format Data frames with various structures for different DCA scenarios
#' @author ClinicoPath package team

# 1. Cancer Screening Dataset
#' @rdname bayesdca_examples
#' @description \code{cancer_screening_data}: A dataset simulating cancer screening scenario
#' with biomarkers and clinical risk factors for early detection analysis.
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

cancer_screening_data <- data.frame(
  # Patient demographics
  patient_id = 1:500,
  age = round(rnorm(500, 55, 12)),
  sex = sample(c("Male", "Female"), 500, replace = TRUE, prob = c(0.45, 0.55)),
  family_history = rbinom(500, 1, 0.15),
  
  # Biomarkers (as probabilities for DCA)
  biomarker_panel_prob = plogis(rnorm(500, -2, 1.2)),  # Panel of multiple biomarkers
  single_biomarker_prob = plogis(rnorm(500, -2.5, 1)),   # Single biomarker
  clinical_score_prob = plogis(rnorm(500, -1.8, 0.8)),   # Clinical risk score
  
  # Traditional binary tests
  imaging_positive = rbinom(500, 1, 0.12),
  biopsy_indicated = rbinom(500, 1, 0.08),
  
  # Outcome: cancer detected within 2 years
  cancer_detected = rbinom(500, 1, 0.05)  # 5% prevalence typical for screening
)

# Make biomarker probabilities correlated with outcome for realism
cancer_screening_data$biomarker_panel_prob <- with(cancer_screening_data,
  plogis(-3 + 2.5 * cancer_detected + rnorm(500, 0, 0.3)))
cancer_screening_data$single_biomarker_prob <- with(cancer_screening_data,
  plogis(-3.2 + 2 * cancer_detected + rnorm(500, 0, 0.4)))
cancer_screening_data$clinical_score_prob <- with(cancer_screening_data,
  plogis(-2.8 + 1.8 * cancer_detected + rnorm(500, 0, 0.5)))

# 2. Treatment Decision Dataset
#' @rdname bayesdca_examples
#' @description \code{treatment_decision_data}: A dataset for evaluating treatment
#' selection models where intervention carries significant risks and costs.
treatment_decision_data <- data.frame(
  # Patient identifiers
  patient_id = 1:300,
  
  # Clinical variables
  age = round(rnorm(300, 68, 15)),
  comorbidity_score = round(rnorm(300, 3, 2)),
  disease_stage = sample(c("Early", "Intermediate", "Advanced"), 300, 
                        replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  
  # Prediction model outputs (probabilities)
  genomic_risk_score = plogis(rnorm(300, -0.5, 1.1)),      # Genomic test
  clinical_prediction_model = plogis(rnorm(300, -0.3, 0.9)), # Clinical model
  combined_model = plogis(rnorm(300, -0.2, 1.0)),           # Combined approach
  
  # Simple risk categorizations
  high_risk_clinical = rbinom(300, 1, 0.25),
  positive_biomarker = rbinom(300, 1, 0.18),
  
  # Outcome: treatment failure or adverse outcome
  treatment_failure = rbinom(300, 1, 0.22)  # 22% failure rate
)

# Make predictions correlated with outcome
treatment_decision_data$genomic_risk_score <- with(treatment_decision_data,
  plogis(-1.2 + 2.8 * treatment_failure + rnorm(300, 0, 0.4)))
treatment_decision_data$clinical_prediction_model <- with(treatment_decision_data,
  plogis(-1.0 + 2.3 * treatment_failure + rnorm(300, 0, 0.5)))
treatment_decision_data$combined_model <- with(treatment_decision_data,
  plogis(-0.8 + 3.0 * treatment_failure + rnorm(300, 0, 0.3)))

# 3. Diagnostic Test Evaluation Dataset
#' @rdname bayesdca_examples  
#' @description \code{diagnostic_test_data}: A dataset for comparing multiple
#' diagnostic tests with different performance characteristics.
diagnostic_test_data <- data.frame(
  # Patient information
  patient_id = 1:400,
  presenting_symptoms = sample(c("Typical", "Atypical", "Asymptomatic"), 400,
                              replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  
  # Continuous test results (as probabilities)
  rapid_test_prob = plogis(rnorm(400, -1, 1.2)),          # Quick, cheaper test
  standard_test_prob = plogis(rnorm(400, -0.5, 1.0)),     # Gold standard
  novel_biomarker_prob = plogis(rnorm(400, -0.8, 1.1)),   # New technology
  
  # Binary test results
  point_of_care_positive = rbinom(400, 1, 0.15),
  imaging_abnormal = rbinom(400, 1, 0.20),
  lab_elevated = rbinom(400, 1, 0.18),
  
  # True disease status
  disease_present = rbinom(400, 1, 0.16)  # 16% prevalence in diagnostic setting
)

# Make test results correlated with disease
diagnostic_test_data$rapid_test_prob <- with(diagnostic_test_data,
  plogis(-2.0 + 3.5 * disease_present + rnorm(400, 0, 0.6)))
diagnostic_test_data$standard_test_prob <- with(diagnostic_test_data,
  plogis(-1.2 + 4.0 * disease_present + rnorm(400, 0, 0.4)))
diagnostic_test_data$novel_biomarker_prob <- with(diagnostic_test_data,
  plogis(-1.5 + 3.8 * disease_present + rnorm(400, 0, 0.5)))

# 4. External Prevalence Dataset
#' @rdname bayesdca_examples
#' @description \code{external_prevalence_data}: A smaller validation dataset
#' with different prevalence than the main study, useful for external prevalence adjustment.
external_prevalence_data <- data.frame(
  # Validation cohort (different setting/population)
  patient_id = 1:150,
  
  # Same predictors but different distribution
  model_score_A = plogis(rnorm(150, -0.8, 1.0)),
  model_score_B = plogis(rnorm(150, -1.2, 1.1)), 
  simple_test = rbinom(150, 1, 0.22),
  
  # Outcome with higher prevalence (specialist setting)
  outcome = rbinom(150, 1, 0.35)  # 35% vs ~20% in main studies
)

# Make predictors correlated with outcome
external_prevalence_data$model_score_A <- with(external_prevalence_data,
  plogis(-0.5 + 2.2 * outcome + rnorm(150, 0, 0.5)))
external_prevalence_data$model_score_B <- with(external_prevalence_data,
  plogis(-0.8 + 2.8 * outcome + rnorm(150, 0, 0.4)))

# 5. Multi-scenario Comprehensive Dataset
#' @rdname bayesdca_examples
#' @description \code{comprehensive_dca_data}: A comprehensive dataset that can be used
#' to demonstrate various DCA features and parameter combinations.
comprehensive_dca_data <- data.frame(
  # Identifiers and demographics  
  id = 1:600,
  age = round(rnorm(600, 58, 16)),
  gender = sample(c("Male", "Female"), 600, replace = TRUE),
  
  # Multiple prediction approaches
  model_excellent = plogis(rnorm(600, -1, 1.0)),      # High discrimination
  model_good = plogis(rnorm(600, -1, 1.3)),           # Good discrimination  
  model_poor = plogis(rnorm(600, -1, 1.8)),           # Poor discrimination
  model_miscalibrated = plogis(rnorm(600, 0.5, 1.2)), # Overconfident
  
  # Binary tests with different characteristics
  sensitive_test = rbinom(600, 1, 0.25),               # High sensitivity
  specific_test = rbinom(600, 1, 0.08),                # High specificity
  balanced_test = rbinom(600, 1, 0.15),                # Balanced performance
  
  # Biomarkers for direction testing
  biomarker_high_good = rnorm(600, 50, 15),            # Higher = better (≥)
  biomarker_low_good = rnorm(600, 30, 10),             # Lower = better (≤)
  
  # Binary outcome
  outcome = rbinom(600, 1, 0.18)  # 18% prevalence
)

# Create realistic correlations with outcome
comprehensive_dca_data$model_excellent <- with(comprehensive_dca_data,
  plogis(-2.2 + 4.5 * outcome + rnorm(600, 0, 0.3)))
comprehensive_dca_data$model_good <- with(comprehensive_dca_data,
  plogis(-1.8 + 3.2 * outcome + rnorm(600, 0, 0.5)))
comprehensive_dca_data$model_poor <- with(comprehensive_dca_data,
  plogis(-1.5 + 1.8 * outcome + rnorm(600, 0, 0.7)))
comprehensive_dca_data$model_miscalibrated <- with(comprehensive_dca_data,
  plogis(0.8 + 2.5 * outcome + rnorm(600, 0, 0.4)))

# Adjust biomarkers based on outcome
comprehensive_dca_data$biomarker_high_good <- with(comprehensive_dca_data,
  biomarker_high_good + 15 * outcome + rnorm(600, 0, 5))
comprehensive_dca_data$biomarker_low_good <- with(comprehensive_dca_data,
  biomarker_low_good - 8 * outcome + rnorm(600, 0, 4))

# Convert biomarkers to 0-1 range for some analyses
comprehensive_dca_data$biomarker_high_prob <- with(comprehensive_dca_data,
  (biomarker_high_good - min(biomarker_high_good)) / 
  (max(biomarker_high_good) - min(biomarker_high_good)))

# Save datasets for package
use_data_multi_format(cancer_screening_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(treatment_decision_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(diagnostic_test_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(external_prevalence_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(comprehensive_dca_data, overwrite = TRUE, save_csv = TRUE)

# Print summary of created datasets
cat("✅ Created 5 example datasets for Bayesian DCA:\n\n")

cat("📊 cancer_screening_data (n=500):\n")
cat("   - Biomarker panels, clinical scores, imaging results\n")
cat("   - Cancer detection outcome (5% prevalence)\n")
cat("   - Ideal for screening threshold analysis (1-10%)\n\n")

cat("🏥 treatment_decision_data (n=300):\n") 
cat("   - Genomic scores, clinical models, risk categories\n")
cat("   - Treatment failure outcome (22% prevalence)\n")
cat("   - Ideal for treatment threshold analysis (15-45%)\n\n")

cat("🔬 diagnostic_test_data (n=400):\n")
cat("   - Rapid tests, standard tests, novel biomarkers\n")
cat("   - Disease presence outcome (16% prevalence)\n")
cat("   - Ideal for diagnostic threshold analysis (5-50%)\n\n")

cat("🌍 external_prevalence_data (n=150):\n")
cat("   - Validation cohort with different prevalence (35%)\n")
cat("   - Demonstrates external prevalence adjustment\n")
cat("   - Use with main datasets for prevalence correction\n\n")

cat("🔧 comprehensive_dca_data (n=600):\n")
cat("   - Multiple model types and performance levels\n")
cat("   - Binary tests with different characteristics\n") 
cat("   - Bidirectional biomarkers for direction testing\n")
cat("   - Comprehensive feature demonstration\n\n")

cat("💡 Usage examples:\n")
cat("   bayesdca(data = cancer_screening_data, outcomes = 'cancer_detected', \n")
cat("            predictors = c('biomarker_panel_prob', 'clinical_score_prob'))\n\n")
cat("   bayesdca(data = treatment_decision_data, outcomes = 'treatment_failure',\n")
cat("            predictors = c('genomic_risk_score', 'combined_model'),\n")
cat("            thresholdMin = 0.15, thresholdMax = 0.45)\n")
