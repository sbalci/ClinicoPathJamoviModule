# Create test data for jjhistostats function
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(42)

# Create histogram test data suitable for clinical research
# Simulating continuous biomarker distributions with different shapes

# Generate multiple continuous variables with different distributions
n_total <- 300

jjhistostats_test_data <- data.frame(
  patient_id = 1:n_total,
  
  # Normal distribution: Age in years
  age_years = round(rnorm(n_total, mean = 65, sd = 12)),
  
  # Right-skewed distribution: Tumor size (mm) - typical clinical pattern
  tumor_size_mm = round(rexp(n_total, rate = 0.1) + 5, 1),
  
  # Left-skewed distribution: Ejection fraction (%)
  ejection_fraction = round(100 - rgamma(n_total, shape = 2, scale = 5), 1),
  
  # Bimodal distribution: PSA levels (ng/mL) - combining normal and elevated
  psa_level = c(
    rlnorm(n_total * 0.7, meanlog = 0.5, sdlog = 0.8),  # Normal range
    rlnorm(n_total * 0.3, meanlog = 2.5, sdlog = 0.6)   # Elevated range
  )[sample(n_total)],
  
  # Uniform-like distribution: Laboratory score (0-100)
  lab_score = round(runif(n_total, min = 20, max = 95), 1),
  
  # Nearly normal distribution: BMI
  bmi = round(rnorm(n_total, mean = 26.5, sd = 4.2), 1),
  
  # Categorical grouping variables
  disease_stage = factor(
    sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 
           n_total, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
    levels = c("Stage I", "Stage II", "Stage III", "Stage IV"),
    ordered = TRUE
  ),
  
  treatment_group = factor(
    sample(c("Control", "Treatment A", "Treatment B"), 
           n_total, replace = TRUE, prob = c(0.33, 0.34, 0.33))
  ),
  
  hospital_site = factor(
    sample(paste("Hospital", LETTERS[1:5]), 
           n_total, replace = TRUE)
  ),
  
  gender = factor(
    sample(c("Male", "Female"), n_total, replace = TRUE, prob = c(0.6, 0.4))
  ),
  
  # Additional continuous variables for comprehensive testing
  hemoglobin_level = round(
    ifelse(sample(c("Male", "Female"), n_total, replace = TRUE, prob = c(0.6, 0.4)) == "Male",
           rnorm(n_total, mean = 14.5, sd = 1.8),
           rnorm(n_total, mean = 12.8, sd = 1.5)), 1),
  
  creatinine_level = round(rnorm(n_total, mean = 1.1, sd = 0.3), 2),
  
  # Highly skewed distribution: C-reactive protein (mg/L)
  crp_level = round(rexp(n_total, rate = 0.2) + 0.5, 1)
)

# Apply realistic constraints
jjhistostats_test_data$age_years <- pmax(18, pmin(95, jjhistostats_test_data$age_years))
jjhistostats_test_data$tumor_size_mm <- pmax(3, pmin(150, jjhistostats_test_data$tumor_size_mm))
jjhistostats_test_data$ejection_fraction <- pmax(15, pmin(80, jjhistostats_test_data$ejection_fraction))
jjhistostats_test_data$psa_level <- pmax(0.1, pmin(100, round(jjhistostats_test_data$psa_level, 2)))
jjhistostats_test_data$bmi <- pmax(15, pmin(45, jjhistostats_test_data$bmi))
jjhistostats_test_data$hemoglobin_level <- pmax(7, pmin(18, jjhistostats_test_data$hemoglobin_level))
jjhistostats_test_data$creatinine_level <- pmax(0.5, pmin(5.0, jjhistostats_test_data$creatinine_level))
jjhistostats_test_data$crp_level <- pmax(0.1, pmin(200, jjhistostats_test_data$crp_level))

# Add some realistic missing values
missing_indices <- sample(nrow(jjhistostats_test_data), size = 15)
jjhistostats_test_data$psa_level[missing_indices[1:5]] <- NA
jjhistostats_test_data$ejection_fraction[missing_indices[6:10]] <- NA
jjhistostats_test_data$crp_level[missing_indices[11:15]] <- NA

# Create correlated variables for advanced testing
correlation_factor <- 0.6
jjhistostats_test_data$correlated_biomarker <- round(
  correlation_factor * scale(jjhistostats_test_data$tumor_size_mm)[,1] +
  sqrt(1 - correlation_factor^2) * rnorm(n_total), 2)

# Normalize correlated biomarker to realistic range
jjhistostats_test_data$correlated_biomarker <- round(
  (jjhistostats_test_data$correlated_biomarker - min(jjhistostats_test_data$correlated_biomarker, na.rm = TRUE)) * 50 + 10, 1)

# Save the dataset
use_data_multi_format(jjhistostats_test_data, overwrite = TRUE, save_csv = TRUE)

# Preview the data
print("jjhistostats_test_data structure:")
str(jjhistostats_test_data)
print("First few rows:")
head(jjhistostats_test_data)
print("Summary statistics:")
summary(jjhistostats_test_data)

# Check distributions of key variables
print("Distribution characteristics:")
continuous_vars <- c("age_years", "tumor_size_mm", "ejection_fraction", "psa_level", "bmi")

for (var in continuous_vars) {
  cat("\n", var, ":\n")
  cat("  Mean:", round(mean(jjhistostats_test_data[[var]], na.rm = TRUE), 2), "\n")
  cat("  SD:", round(sd(jjhistostats_test_data[[var]], na.rm = TRUE), 2), "\n")
  cat("  Skewness:", round(moments::skewness(jjhistostats_test_data[[var]], na.rm = TRUE), 2), "\n")
  cat("  Range:", round(range(jjhistostats_test_data[[var]], na.rm = TRUE), 2), "\n")
}

# Check group distributions
print("\nGroup distributions:")
print("Disease stage:")
table(jjhistostats_test_data$disease_stage)

print("Treatment group:")
table(jjhistostats_test_data$treatment_group)

print("Hospital site:")
table(jjhistostats_test_data$hospital_site)

# Check correlations
continuous_vars_for_cor <- c("age_years", "tumor_size_mm", "correlated_biomarker", "bmi", "hemoglobin_level")
cor_matrix <- cor(jjhistostats_test_data[, continuous_vars_for_cor], use = "complete.obs")
print("Correlation matrix:")
print(round(cor_matrix, 2))
