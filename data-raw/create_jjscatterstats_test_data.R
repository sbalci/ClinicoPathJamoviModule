# Create test data for jjscatterstats function
# This script generates various datasets to test scatter plot functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(usethis)

# Basic scatter plot test data
set.seed(123)
jjscatterstats_basic <- data.frame(
  x_continuous = rnorm(150, 50, 10),
  y_continuous = rnorm(150, 25, 8),
  group_factor = factor(rep(c("Group_A", "Group_B", "Group_C"), each = 50)),
  id = 1:150
)

# Add some correlation between x and y
jjscatterstats_basic$y_continuous <- jjscatterstats_basic$y_continuous + 
  0.7 * jjscatterstats_basic$x_continuous + rnorm(150, 0, 5)

# Advanced test data with multiple scenarios
set.seed(456)
jjscatterstats_advanced <- data.frame(
  height_cm = rnorm(200, 170, 15),
  weight_kg = rnorm(200, 70, 12),
  age_years = sample(18:80, 200, replace = TRUE),
  gender = factor(rep(c("Male", "Female"), each = 100)),
  treatment_group = factor(rep(c("Control", "Treatment_A", "Treatment_B", "Treatment_C"), each = 50)),
  response_score = rnorm(200, 5, 2),
  biomarker_level = exp(rnorm(200, 2, 0.5))
)

# Add realistic correlations
jjscatterstats_advanced$weight_kg <- jjscatterstats_advanced$weight_kg + 
  0.8 * (jjscatterstats_advanced$height_cm - 170) + 
  ifelse(jjscatterstats_advanced$gender == "Male", 10, -5) +
  rnorm(200, 0, 3)

jjscatterstats_advanced$response_score <- jjscatterstats_advanced$response_score + 
  0.3 * scale(jjscatterstats_advanced$biomarker_level)[,1] +
  ifelse(jjscatterstats_advanced$treatment_group %in% c("Treatment_A", "Treatment_B"), 1.5, 0) +
  rnorm(200, 0, 1)

# Clinical research test data
set.seed(789)
jjscatterstats_clinical <- data.frame(
  patient_id = paste0("PT_", sprintf("%03d", 1:120)),
  tumor_size_mm = rgamma(120, shape = 2, rate = 0.1),
  ki67_percentage = rbeta(120, 2, 8) * 100,
  age_at_diagnosis = rnorm(120, 65, 12),
  stage = factor(sample(c("I", "II", "III", "IV"), 120, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))),
  histology = factor(sample(c("Adenocarcinoma", "Squamous_Cell", "Other"), 120, replace = TRUE, prob = c(0.6, 0.3, 0.1))),
  survival_months = rweibull(120, shape = 1.5, scale = 30),
  mutation_count = rpois(120, lambda = 15)
)

# Add realistic medical correlations
jjscatterstats_clinical$ki67_percentage <- pmax(0, pmin(100, 
  jjscatterstats_clinical$ki67_percentage + 
  0.2 * jjscatterstats_clinical$tumor_size_mm +
  ifelse(jjscatterstats_clinical$stage %in% c("III", "IV"), 15, 0) +
  rnorm(120, 0, 5)
))

jjscatterstats_clinical$survival_months <- pmax(1,
  jjscatterstats_clinical$survival_months - 
  0.1 * jjscatterstats_clinical$tumor_size_mm -
  0.05 * jjscatterstats_clinical$ki67_percentage +
  ifelse(jjscatterstats_clinical$stage == "I", 10, 
         ifelse(jjscatterstats_clinical$stage == "II", 5, 
                ifelse(jjscatterstats_clinical$stage == "III", -5, -15))) +
  rnorm(120, 0, 3)
)

# Performance test data (larger dataset)
set.seed(1001)
jjscatterstats_performance <- data.frame(
  measurement_1 = rnorm(1000, 100, 25),
  measurement_2 = rnorm(1000, 50, 15),
  batch = factor(rep(paste0("Batch_", 1:10), each = 100)),
  lab_id = factor(rep(c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E"), each = 200)),
  quality_score = rnorm(1000, 7.5, 1.5),
  processing_time = rexp(1000, rate = 0.1)
)

# Edge case test data (with missing values, outliers, etc.)
set.seed(2002)
jjscatterstats_edge_cases <- data.frame(
  variable_x = c(rnorm(80, 50, 10), rep(NA, 10), c(100, 150, -50, 200)),  # Some NAs and outliers
  variable_y = c(rnorm(80, 25, 5), c(100, -20, 75, 80, 60, 45, 30, 35, 40, 50), rep(NA, 4)),
  category = factor(c(rep(c("Cat_1", "Cat_2", "Cat_3"), each = 30), rep("Cat_4", 4))),
  small_values = runif(94, 0.001, 0.1),
  large_values = runif(94, 1e6, 1e8)
)

# Statistical test scenarios data
set.seed(3003)
jjscatterstats_stats <- data.frame(
  # Strong positive correlation
  strong_pos_x = rnorm(100, 0, 1),
  strong_pos_y = NA,
  
  # Weak correlation  
  weak_x = rnorm(100, 0, 1),
  weak_y = NA,
  
  # No correlation
  no_corr_x = rnorm(100, 0, 1),
  no_corr_y = rnorm(100, 0, 1),
  
  # Non-linear relationship
  nonlinear_x = seq(-3, 3, length.out = 100),
  nonlinear_y = NA,
  
  scenario = factor(rep(c("Linear", "Exponential", "Logarithmic", "Polynomial"), each = 25))
)

# Generate correlated variables
jjscatterstats_stats$strong_pos_y <- 2 * jjscatterstats_stats$strong_pos_x + rnorm(100, 0, 0.5)
jjscatterstats_stats$weak_y <- 0.3 * jjscatterstats_stats$weak_x + rnorm(100, 0, 1.5)
jjscatterstats_stats$nonlinear_y <- jjscatterstats_stats$nonlinear_x^2 + rnorm(100, 0, 0.8)

# Save all datasets
use_data_multi_format(jjscatterstats_basic, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjscatterstats_advanced, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjscatterstats_clinical, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjscatterstats_performance, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjscatterstats_edge_cases, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjscatterstats_stats, overwrite = TRUE, save_csv = TRUE)

# Print summary information
cat("Test datasets created for jjscatterstats:\n")
cat("1. jjscatterstats_basic: Basic scatter plot with 3 groups (n=150)\n")
cat("2. jjscatterstats_advanced: Advanced scenarios with multiple variables (n=200)\n")
cat("3. jjscatterstats_clinical: Clinical research data (n=120)\n")
cat("4. jjscatterstats_performance: Large dataset for performance testing (n=1000)\n")
cat("5. jjscatterstats_edge_cases: Edge cases with NAs and outliers (n=94)\n")
cat("6. jjscatterstats_stats: Various statistical correlation scenarios (n=100)\n")
