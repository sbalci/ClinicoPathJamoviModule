# Generate example medical AI validation dataset
# This dataset simulates an AI model predicting disease diagnosis
# compared to human expert assessment and biomarkers

set.seed(42)

# Sample size
n <- 200

# Generate predictor variables

# AI model score (continuous, 0-1)
AI_score <- rbeta(n, 2, 2)  # Beta distribution centered around 0.5

# Human expert score (continuous, 0-1, correlated with AI)
human_score <- pmin(pmax(AI_score + rnorm(n, 0, 0.15), 0), 1)

# Clinical biomarker 1 (standardized)
biomarker1 <- rnorm(n, 0, 1)

# Clinical biomarker 2 (standardized)
biomarker2 <- rnorm(n, 0, 1)

# Age (years)
age <- round(rnorm(n, 55, 15))
age <- pmin(pmax(age, 20), 90)  # Constrain to reasonable range

# Generate outcome variable (diagnosis)
# True diagnosis depends on AI score, biomarkers, and age with some noise
logit_prob <- -2 +
  4 * AI_score +
  0.5 * biomarker1 +
  0.3 * biomarker2 +
  0.02 * (age - 55) +
  rnorm(n, 0, 0.5)

prob_positive <- 1 / (1 + exp(-logit_prob))
diagnosis <- ifelse(runif(n) < prob_positive, "positive", "negative")

# Create data frame
medical_ai_data <- data.frame(
  patient_id = paste0("P", sprintf("%03d", 1:n)),
  AI_score = round(AI_score, 3),
  human_score = round(human_score, 3),
  biomarker1 = round(biomarker1, 2),
  biomarker2 = round(biomarker2, 2),
  age = age,
  diagnosis = factor(diagnosis, levels = c("negative", "positive")),
  stringsAsFactors = FALSE
)

# Add some missing values to make it realistic (5%)
missing_indices <- sample(1:n, size = floor(0.05 * n))
medical_ai_data$biomarker1[missing_indices[1:5]] <- NA
medical_ai_data$biomarker2[missing_indices[6:10]] <- NA

# Save dataset
usethis::use_data(medical_ai_data, overwrite = TRUE)

# Create documentation
# This will go in R/data.R or similar
cat('
#\' Medical AI Validation Example Dataset
#\'
#\' A simulated dataset for demonstrating AI model validation methods.
#\' Contains AI predictions, human expert scores, clinical biomarkers,
#\' and gold-standard diagnosis for 200 patients.
#\'
#\' @format A data frame with 200 rows and 7 variables:
#\' \\describe{
#\'   \\item{patient_id}{Patient identifier (character)}
#\'   \\item{AI_score}{AI model predicted probability (0-1)}
#\'   \\item{human_score}{Human expert assessment score (0-1)}
#\'   \\item{biomarker1}{Clinical biomarker 1 (standardized)}
#\'   \\item{biomarker2}{Clinical biomarker 2 (standardized)}
#\'   \\item{age}{Patient age in years}
#\'   \\item{diagnosis}{Gold standard diagnosis (negative/positive)}
#\' }
#\'
#\' @details
#\' This dataset was generated to demonstrate AI model validation techniques
#\' including cross-validation, calibration assessment, and model comparison.
#\' The AI_score is designed to have good but imperfect discrimination,
#\' making it suitable for demonstrating various validation metrics.
#\'
#\' The outcome (diagnosis) was generated based on a logistic model with
#\' AI_score, biomarkers, and age as predictors, plus random noise to
#\' simulate real-world variability.
#\'
#\' @examples
#\' data(medical_ai_data)
#\'
#\' # Basic cross-validation
#\' \\dontrun{
#\' aivalidation(
#\'   data = medical_ai_data,
#\'   predictorVars = c("AI_score", "biomarker1"),
#\'   outcomeVar = "diagnosis",
#\'   positiveLevel = "positive",
#\'   crossValidation = "10-fold",
#\'   showCrossValidation = TRUE
#\' )
#\' }
#\'
#\' # Model comparison
#\' \\dontrun{
#\' aivalidation(
#\'   data = medical_ai_data,
#\'   predictorVars = c("AI_score", "human_score", "biomarker1", "biomarker2"),
#\'   outcomeVar = "diagnosis",
#\'   positiveLevel = "positive",
#\'   compareModels = TRUE,
#\'   delongTest = TRUE
#\' )
#\' }
#\'
#\' @source Simulated data generated for package demonstration
"medical_ai_data"
', file = "R/medical_ai_data_documentation.R")
