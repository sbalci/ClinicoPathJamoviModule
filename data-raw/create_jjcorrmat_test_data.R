# Create test data for jjcorrmat function
library(MASS)

set.seed(42)

# Create correlation matrix test data with multiple continuous variables
# Simulating biomarker data for clinical research

# Define correlation structure
correlation_matrix <- matrix(c(
  1.0,  0.7,  0.5,  0.3,  0.2,
  0.7,  1.0,  0.6,  0.4,  0.1,
  0.5,  0.6,  1.0,  0.8,  0.3,
  0.3,  0.4,  0.8,  1.0,  0.5,
  0.2,  0.1,  0.3,  0.5,  1.0
), nrow = 5)

# Generate multivariate normal data
n <- 200
biomarker_data <- mvrnorm(n = n, 
                         mu = c(50, 25, 15, 8, 3), 
                         Sigma = correlation_matrix * 10)

# Create data frame with meaningful names
jjcorrmat_test_data <- data.frame(
  patient_id = 1:n,
  ki67_percent = pmax(0, pmin(100, biomarker_data[,1])),
  p53_score = pmax(0, pmin(50, biomarker_data[,2])),
  her2_intensity = pmax(0, pmin(30, biomarker_data[,3])),
  tumor_size_mm = pmax(5, pmin(50, biomarker_data[,4])),
  age_years = pmax(18, pmin(90, biomarker_data[,5] + 55)),
  tumor_grade = sample(c("Grade 1", "Grade 2", "Grade 3"), n, 
                      replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  hormone_status = sample(c("ER+/PR+", "ER+/PR-", "ER-/PR+", "ER-/PR-"), n,
                         replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1)),
  treatment_response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                             n, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  study_center = sample(paste("Center", LETTERS[1:5]), n, replace = TRUE)
)

# Round numeric variables appropriately
jjcorrmat_test_data$ki67_percent <- round(jjcorrmat_test_data$ki67_percent, 1)
jjcorrmat_test_data$p53_score <- round(jjcorrmat_test_data$p53_score, 1)
jjcorrmat_test_data$her2_intensity <- round(jjcorrmat_test_data$her2_intensity, 1)
jjcorrmat_test_data$tumor_size_mm <- round(jjcorrmat_test_data$tumor_size_mm, 1)
jjcorrmat_test_data$age_years <- round(jjcorrmat_test_data$age_years)

# Convert categorical variables to factors
jjcorrmat_test_data$tumor_grade <- as.factor(jjcorrmat_test_data$tumor_grade)
jjcorrmat_test_data$hormone_status <- as.factor(jjcorrmat_test_data$hormone_status)
jjcorrmat_test_data$treatment_response <- as.factor(jjcorrmat_test_data$treatment_response)
jjcorrmat_test_data$study_center <- as.factor(jjcorrmat_test_data$study_center)

# Save the dataset
usethis::use_data(jjcorrmat_test_data, overwrite = TRUE)

# Preview the data
print("jjcorrmat_test_data structure:")
str(jjcorrmat_test_data)
print("First few rows:")
head(jjcorrmat_test_data)
print("Summary statistics:")
summary(jjcorrmat_test_data)

# Test correlation matrix
continuous_vars <- c("ki67_percent", "p53_score", "her2_intensity", "tumor_size_mm", "age_years")
correlation_result <- cor(jjcorrmat_test_data[, continuous_vars], use = "complete.obs")
print("Correlation matrix:")
print(round(correlation_result, 2))