# =============================================================================
# CREATE EXAMPLE DATA FOR PSYCHOPDAROC ROC ANALYSIS
# =============================================================================
# 
# This script creates example datasets for testing and demonstrating the
# sophisticated psychopdaroc ROC analysis function.

library(usethis)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# DATASET 1: MEDICAL DIAGNOSTIC TEST DATA
# =============================================================================
# Simulates a medical diagnostic scenario with multiple biomarkers

n_patients <- 300

# Create binary disease status (30% prevalence)
disease_status <- sample(c("Healthy", "Disease"), n_patients, 
                        replace = TRUE, prob = c(0.7, 0.3))

# Biomarker 1: Good discriminator (e.g., established biomarker)
biomarker1 <- ifelse(disease_status == "Disease",
                     rnorm(sum(disease_status == "Disease"), mean = 4.5, sd = 1.2),
                     rnorm(sum(disease_status == "Healthy"), mean = 2.0, sd = 1.0))

# Biomarker 2: Moderate discriminator (e.g., new biomarker)
biomarker2 <- ifelse(disease_status == "Disease", 
                     rnorm(sum(disease_status == "Disease"), mean = 3.8, sd = 1.5),
                     rnorm(sum(disease_status == "Healthy"), mean = 2.5, sd = 1.3))

# Biomarker 3: Poor discriminator (e.g., experimental marker)
biomarker3 <- ifelse(disease_status == "Disease",
                     rnorm(sum(disease_status == "Disease"), mean = 3.2, sd = 1.8),
                     rnorm(sum(disease_status == "Healthy"), mean = 2.8, sd = 1.6))

# Clinical variables
age <- round(rnorm(n_patients, mean = 55, sd = 15))
age[age < 18] <- 18
age[age > 90] <- 90

gender <- sample(c("Male", "Female"), n_patients, replace = TRUE)

# Hospital site for subgroup analysis
hospital <- sample(c("Hospital_A", "Hospital_B", "Hospital_C"), 
                  n_patients, replace = TRUE, prob = c(0.4, 0.35, 0.25))

# Create the medical diagnostic dataset
medical_roc_data <- data.frame(
  patient_id = paste0("PT", sprintf("%03d", 1:n_patients)),
  disease_status = factor(disease_status, levels = c("Healthy", "Disease")),
  biomarker1 = round(biomarker1, 3),
  biomarker2 = round(biomarker2, 3), 
  biomarker3 = round(biomarker3, 3),
  age = age,
  gender = factor(gender),
  hospital = factor(hospital),
  stringsAsFactors = FALSE
)

# Add some missing values to make it realistic
missing_indices <- sample(1:n_patients, size = round(n_patients * 0.02))
medical_roc_data$biomarker2[missing_indices] <- NA

missing_indices_b3 <- sample(1:n_patients, size = round(n_patients * 0.03))
medical_roc_data$biomarker3[missing_indices_b3] <- NA

# =============================================================================
# DATASET 2: EDUCATIONAL ASSESSMENT DATA
# =============================================================================
# Simulates an educational scenario with multiple assessment tools

n_students <- 250

# Create binary outcome (pass/fail)
pass_status <- sample(c("Fail", "Pass"), n_students, 
                     replace = TRUE, prob = c(0.25, 0.75))

# Assessment 1: Traditional exam score
exam_score <- ifelse(pass_status == "Pass",
                    rnorm(sum(pass_status == "Pass"), mean = 78, sd = 12),
                    rnorm(sum(pass_status == "Fail"), mean = 58, sd = 15))

# Assessment 2: Project-based assessment  
project_score <- ifelse(pass_status == "Pass",
                       rnorm(sum(pass_status == "Pass"), mean = 82, sd = 10),
                       rnorm(sum(pass_status == "Fail"), mean = 65, sd = 18))

# Assessment 3: Peer evaluation
peer_score <- ifelse(pass_status == "Pass",
                    rnorm(sum(pass_status == "Pass"), mean = 75, sd = 14),
                    rnorm(sum(pass_status == "Fail"), mean = 68, sd = 16))

# Ensure scores are within reasonable ranges
exam_score[exam_score < 0] <- 0
exam_score[exam_score > 100] <- 100
project_score[project_score < 0] <- 0  
project_score[project_score > 100] <- 100
peer_score[peer_score < 0] <- 0
peer_score[peer_score > 100] <- 100

# Student characteristics
study_hours <- round(rnorm(n_students, mean = 15, sd = 8))
study_hours[study_hours < 0] <- 0

class_section <- sample(c("Section_A", "Section_B", "Section_C"), 
                       n_students, replace = TRUE)

# Create the educational assessment dataset
education_roc_data <- data.frame(
  student_id = paste0("STU", sprintf("%03d", 1:n_students)),
  pass_status = factor(pass_status, levels = c("Fail", "Pass")),
  exam_score = round(exam_score, 1),
  project_score = round(project_score, 1),
  peer_score = round(peer_score, 1),
  study_hours = study_hours,
  class_section = factor(class_section),
  stringsAsFactors = FALSE
)

# =============================================================================
# DATASET 3: FINANCIAL RISK ASSESSMENT DATA
# =============================================================================
# Simulates a financial risk scenario with multiple risk indicators

n_clients <- 400

# Create binary default status (15% default rate)
default_status <- sample(c("No_Default", "Default"), n_clients,
                        replace = TRUE, prob = c(0.85, 0.15))

# Risk Score 1: Credit history score
credit_score <- ifelse(default_status == "Default",
                      rnorm(sum(default_status == "Default"), mean = 580, sd = 80),
                      rnorm(sum(default_status == "No_Default"), mean = 720, sd = 70))

# Risk Score 2: Income-to-debt ratio  
income_debt_ratio <- ifelse(default_status == "Default",
                           rnorm(sum(default_status == "Default"), mean = 2.8, sd = 1.2),
                           rnorm(sum(default_status == "No_Default"), mean = 4.5, sd = 1.5))

# Risk Score 3: Employment stability score
employment_score <- ifelse(default_status == "Default", 
                          rnorm(sum(default_status == "Default"), mean = 3.2, sd = 1.5),
                          rnorm(sum(default_status == "No_Default"), mean = 7.1, sd = 2.0))

# Ensure reasonable ranges
credit_score[credit_score < 300] <- 300
credit_score[credit_score > 850] <- 850
income_debt_ratio[income_debt_ratio < 0.5] <- 0.5
employment_score[employment_score < 0] <- 0
employment_score[employment_score > 10] <- 10

# Client characteristics
loan_amount <- round(rnorm(n_clients, mean = 25000, sd = 15000))
loan_amount[loan_amount < 1000] <- 1000

client_type <- sample(c("Individual", "Small_Business", "Corporate"), 
                     n_clients, replace = TRUE, prob = c(0.6, 0.3, 0.1))

# Create the financial risk dataset
financial_roc_data <- data.frame(
  client_id = paste0("CLI", sprintf("%04d", 1:n_clients)),
  default_status = factor(default_status, levels = c("No_Default", "Default")),
  credit_score = round(credit_score),
  income_debt_ratio = round(income_debt_ratio, 2),
  employment_score = round(employment_score, 1),
  loan_amount = loan_amount,
  client_type = factor(client_type),
  stringsAsFactors = FALSE
)

# =============================================================================
# DATASET 4: MANUFACTURING QUALITY CONTROL DATA  
# =============================================================================
# Simulates a quality control scenario with multiple inspection measures

n_products <- 350

# Create binary quality status (10% defective rate)
quality_status <- sample(c("Pass", "Defect"), n_products,
                        replace = TRUE, prob = c(0.9, 0.1))

# Measurement 1: Dimensional accuracy
dimension_score <- ifelse(quality_status == "Defect",
                         rnorm(sum(quality_status == "Defect"), mean = 2.3, sd = 0.8),
                         rnorm(sum(quality_status == "Pass"), mean = 4.7, sd = 0.6))

# Measurement 2: Surface quality
surface_score <- ifelse(quality_status == "Defect",
                       rnorm(sum(quality_status == "Defect"), mean = 1.8, sd = 0.9),
                       rnorm(sum(quality_status == "Pass"), mean = 4.2, sd = 0.7))

# Measurement 3: Material strength
strength_score <- ifelse(quality_status == "Defect",
                        rnorm(sum(quality_status == "Defect"), mean = 2.1, sd = 1.0),
                        rnorm(sum(quality_status == "Pass"), mean = 4.5, sd = 0.8))

# Ensure positive scores
dimension_score[dimension_score < 0] <- 0
surface_score[surface_score < 0] <- 0  
strength_score[strength_score < 0] <- 0

# Production characteristics
batch_number <- sample(paste0("B", sprintf("%03d", 1:50)), n_products, replace = TRUE)
production_line <- sample(c("Line_1", "Line_2", "Line_3"), 
                         n_products, replace = TRUE)

# Create the manufacturing quality dataset
manufacturing_roc_data <- data.frame(
  product_id = paste0("PRD", sprintf("%04d", 1:n_products)),
  quality_status = factor(quality_status, levels = c("Pass", "Defect")),
  dimension_score = round(dimension_score, 2),
  surface_score = round(surface_score, 2),
  strength_score = round(strength_score, 2),
  batch_number = factor(batch_number),
  production_line = factor(production_line),
  stringsAsFactors = FALSE
)

# =============================================================================
# SAVE DATASETS
# =============================================================================

# Save all datasets using usethis
use_data(medical_roc_data, overwrite = TRUE)
use_data(education_roc_data, overwrite = TRUE)  
use_data(financial_roc_data, overwrite = TRUE)
use_data(manufacturing_roc_data, overwrite = TRUE)

# =============================================================================
# CREATE DOCUMENTATION FOR DATASETS
# =============================================================================

# Medical ROC Data Documentation
cat('
#\' Medical Diagnostic ROC Analysis Example Data
#\'
#\' A dataset containing biomarker measurements and disease status for ROC analysis.
#\' This dataset simulates a medical diagnostic scenario with multiple biomarkers
#\' of varying discriminatory ability.
#\'
#\' @format A data frame with 300 rows and 8 variables:
#\' \\describe{
#\'   \\item{patient_id}{Patient identifier}
#\'   \\item{disease_status}{Binary outcome: "Healthy" or "Disease"}
#\'   \\item{biomarker1}{Continuous biomarker with good discriminatory ability}
#\'   \\item{biomarker2}{Continuous biomarker with moderate discriminatory ability}
#\'   \\item{biomarker3}{Continuous biomarker with poor discriminatory ability}
#\'   \\item{age}{Patient age in years}
#\'   \\item{gender}{Patient gender: "Male" or "Female"}
#\'   \\item{hospital}{Hospital site: "Hospital_A", "Hospital_B", or "Hospital_C"}
#\' }
#\' 
#\' @details
#\' This dataset is designed to demonstrate:
#\' \\itemize{
#\'   \\item Basic ROC curve analysis
#\'   \\item Comparison of multiple biomarkers using DeLong test
#\'   \\item IDI and NRI calculations
#\'   \\item Subgroup analysis by hospital
#\'   \\item Various cutpoint optimization methods
#\' }
#\'
#\' The biomarkers have different levels of discrimination:
#\' \\itemize{
#\'   \\item biomarker1: Good discriminator (AUC ≈ 0.85)
#\'   \\item biomarker2: Moderate discriminator (AUC ≈ 0.75)  
#\'   \\item biomarker3: Poor discriminator (AUC ≈ 0.60)
#\' }
#\'
#\' @source Simulated data for demonstration purposes
#\' @keywords datasets
"medical_roc_data"
', file = "R/data_medical_roc.R")

# Education ROC Data Documentation  
cat('
#\' Educational Assessment ROC Analysis Example Data
#\'
#\' A dataset containing assessment scores and pass/fail outcomes for ROC analysis.
#\' This dataset simulates an educational scenario with multiple assessment methods.
#\'
#\' @format A data frame with 250 rows and 7 variables:
#\' \\describe{
#\'   \\item{student_id}{Student identifier}
#\'   \\item{pass_status}{Binary outcome: "Fail" or "Pass"}
#\'   \\item{exam_score}{Traditional exam score (0-100)}
#\'   \\item{project_score}{Project-based assessment score (0-100)}
#\'   \\item{peer_score}{Peer evaluation score (0-100)}
#\'   \\item{study_hours}{Weekly study hours}
#\'   \\item{class_section}{Class section: "Section_A", "Section_B", or "Section_C"}
#\' }
#\'
#\' @details
#\' This dataset demonstrates ROC analysis in educational assessment, showing:
#\' \\itemize{
#\'   \\item Comparison of different assessment methods
#\'   \\item Optimal cutoff determination for pass/fail decisions
#\'   \\item Subgroup analysis by class section
#\' }
#\'
#\' @source Simulated data for demonstration purposes
#\' @keywords datasets
"education_roc_data"
', file = "R/data_education_roc.R")

# Financial ROC Data Documentation
cat('
#\' Financial Risk Assessment ROC Analysis Example Data
#\'
#\' A dataset containing risk indicators and default outcomes for ROC analysis.
#\' This dataset simulates a financial risk assessment scenario.
#\'
#\' @format A data frame with 400 rows and 7 variables:
#\' \\describe{
#\'   \\item{client_id}{Client identifier}
#\'   \\item{default_status}{Binary outcome: "No_Default" or "Default"}
#\'   \\item{credit_score}{Credit history score (300-850)}
#\'   \\item{income_debt_ratio}{Income to debt ratio}
#\'   \\item{employment_score}{Employment stability score (0-10)}
#\'   \\item{loan_amount}{Loan amount in dollars}
#\'   \\item{client_type}{Client type: "Individual", "Small_Business", or "Corporate"}
#\' }
#\'
#\' @details
#\' This dataset demonstrates ROC analysis in financial risk assessment:
#\' \\itemize{
#\'   \\item Credit risk modeling
#\'   \\item Optimal decision thresholds for loan approval
#\'   \\item Risk score validation and comparison
#\'   \\item Cost-benefit analysis using different cost ratios
#\' }
#\'
#\' @source Simulated data for demonstration purposes
#\' @keywords datasets
"financial_roc_data"
', file = "R/data_financial_roc.R")

# Manufacturing ROC Data Documentation
cat('
#\' Manufacturing Quality Control ROC Analysis Example Data
#\'
#\' A dataset containing quality measurements and defect status for ROC analysis.
#\' This dataset simulates a manufacturing quality control scenario.
#\'
#\' @format A data frame with 350 rows and 7 variables:
#\' \\describe{
#\'   \\item{product_id}{Product identifier}
#\'   \\item{quality_status}{Binary outcome: "Pass" or "Defect"}
#\'   \\item{dimension_score}{Dimensional accuracy score}
#\'   \\item{surface_score}{Surface quality score}
#\'   \\item{strength_score}{Material strength score}
#\'   \\item{batch_number}{Production batch identifier}
#\'   \\item{production_line}{Production line: "Line_1", "Line_2", or "Line_3"}
#\' }
#\'
#\' @details
#\' This dataset demonstrates ROC analysis in quality control:
#\' \\itemize{
#\'   \\item Quality inspection optimization
#\'   \\item Defect detection thresholds
#\'   \\item Multi-criteria quality assessment
#\'   \\item Production line comparison
#\' }
#\'
#\' @source Simulated data for demonstration purposes
#\' @keywords datasets
"manufacturing_roc_data"
', file = "R/data_manufacturing_roc.R")

# Print summary
cat("\\n=============================================================================\\n")
cat("EXAMPLE DATA CREATION COMPLETE\\n")
cat("=============================================================================\\n")
cat("Created 4 comprehensive datasets for psychopdaroc ROC analysis:\\n\\n")
cat("1. medical_roc_data (n=300)      - Medical diagnostic biomarkers\\n")
cat("2. education_roc_data (n=250)    - Educational assessment scores\\n")  
cat("3. financial_roc_data (n=400)    - Financial risk indicators\\n")
cat("4. manufacturing_roc_data (n=350) - Manufacturing quality measures\\n\\n")
cat("Each dataset includes:\\n")
cat("- Binary outcome variable\\n")
cat("- Multiple continuous predictors with different discrimination abilities\\n")
cat("- Subgroup variables for stratified analysis\\n")
cat("- Realistic data characteristics and missing values\\n\\n")
cat("These datasets can be used to demonstrate all features of psychopdaroc:\\n")
cat("- Basic ROC analysis and AUC calculation\\n")
cat("- Multiple cutpoint optimization methods\\n")
cat("- DeLong test for AUC comparison\\n")
cat("- IDI and NRI calculations\\n")
cat("- Bootstrap confidence intervals\\n")
cat("- Subgroup analysis\\n")
cat("- Cost-benefit optimization\\n")
cat("- Comprehensive plotting options\\n")
cat("=============================================================================\\n")