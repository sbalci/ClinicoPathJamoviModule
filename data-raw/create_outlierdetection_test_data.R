# Create comprehensive test data for outlierdetection function
# This script generates datasets covering all outlier detection scenarios and edge cases

library(dplyr)
library(MASS)

# Set seed for reproducibility
set.seed(456)

# =============================================================================
# 1. Basic Outlier Detection Dataset
# =============================================================================

# Generate basic dataset with clear univariate outliers
outlierdetection_basic <- data.frame(
  patient_id = 1:200,
  age = c(rnorm(180, mean = 65, sd = 12), rep(c(25, 95), each = 10)),  # Age outliers: 180 + 20 = 200
  weight = c(rnorm(185, mean = 70, sd = 15), c(30, 40, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260)),  # Weight outliers: 185 + 15 = 200
  height = c(rnorm(190, mean = 170, sd = 10), rep(c(130, 220), each = 5)),  # Height outliers: 190 + 10 = 200
  bmi = c(rnorm(195, mean = 24, sd = 4), c(15, 16, 40, 42, 45)),  # BMI outliers: 195 + 5 = 200
  bp_systolic = c(rnorm(185, mean = 120, sd = 20), rep(c(80, 200), c(8, 7))),  # BP outliers: 185 + 15 = 200
  bp_diastolic = c(rnorm(190, mean = 80, sd = 10), rep(c(50, 120), each = 5)),  # BP outliers: 190 + 10 = 200
  lab_value = c(rnorm(190, mean = 50, sd = 10), rep(c(5, 150), each = 5)),  # Lab outliers: 190 + 10 = 200
  treatment_response = c(rnorm(195, mean = 75, sd = 15), c(10, 20, 140, 150, 160)),  # Response outliers: 195 + 5 = 200
  stringsAsFactors = FALSE
)

# =============================================================================
# 2. Multivariate Outlier Dataset
# =============================================================================

# Generate multivariate dataset with outliers that are normal individually but outliers together
set.seed(456)

# Create correlated normal data
mu <- c(50, 100, 25)
sigma <- matrix(c(100, 80, 30, 80, 200, 50, 30, 50, 50), nrow = 3)
normal_data <- mvrnorm(180, mu = mu, Sigma = sigma)

# Add multivariate outliers (normal individually, but unusual combinations)
multivariate_outliers <- rbind(
  c(45, 95, 80),   # High Z but normal X and Y
  c(55, 105, 10),  # Low Z but normal X and Y
  c(30, 150, 25),  # High Y but normal X and Z
  c(70, 50, 25),   # Low Y but normal X and Z
  c(90, 100, 30),  # High X but normal Y and Z
  c(10, 100, 20),  # Low X but normal Y and Z
  c(25, 75, 5),    # Low on multiple dimensions
  c(75, 125, 45),  # High on multiple dimensions
  c(80, 60, 50),   # Unusual combination 1
  c(20, 140, 15),  # Unusual combination 2
  c(45, 120, 5),   # Unusual combination 3
  c(55, 80, 45),   # Unusual combination 4
  c(40, 160, 35),  # Unusual combination 5
  c(60, 40, 15),   # Unusual combination 6
  c(35, 90, 50),   # Unusual combination 7
  c(65, 110, 10),  # Unusual combination 8
  c(50, 140, 40),  # Unusual combination 9
  c(50, 60, 35),   # Unusual combination 10
  c(25, 120, 20),  # Unusual combination 11
  c(75, 80, 30)    # Unusual combination 12
)

all_data <- rbind(normal_data, multivariate_outliers)

outlierdetection_multivariate <- data.frame(
  patient_id = 1:200,
  biomarker_1 = all_data[, 1],
  biomarker_2 = all_data[, 2],
  biomarker_3 = all_data[, 3],
  clinical_score = all_data[, 1] * 0.5 + all_data[, 2] * 0.3 + all_data[, 3] * 0.2 + rnorm(200, 0, 5),
  composite_index = sqrt(all_data[, 1]^2 + all_data[, 2]^2 + all_data[, 3]^2) + rnorm(200, 0, 10),
  stringsAsFactors = FALSE
)

# =============================================================================
# 3. Edge Cases Dataset
# =============================================================================

# Generate edge cases for robust testing
outlierdetection_edge_cases <- data.frame(
  patient_id = 1:100,
  
  # All identical values
  identical_values = rep(50, 100),
  
  # Nearly identical values
  nearly_identical = c(rep(50, 95), c(50.1, 50.2, 49.9, 49.8, 50.05)),
  
  # Very small variance
  small_variance = rnorm(100, mean = 100, sd = 0.1),
  
  # Large variance
  large_variance = rnorm(100, mean = 50, sd = 100),
  
  # Extreme values
  extreme_values = c(rnorm(90, mean = 50, sd = 10), rep(c(-1000, 1000), each = 5)),
  
  # Missing data patterns
  missing_data = c(rnorm(80, mean = 75, sd = 15), rep(NA, 20)),
  
  # Discrete values
  discrete_values = sample(c(1, 2, 3, 4, 5), 100, replace = TRUE),
  
  # Binary-like values
  binary_like = sample(c(0, 1), 100, replace = TRUE, prob = c(0.9, 0.1)),
  
  # Skewed distribution
  skewed_data = c(rexp(100, rate = 0.1)),
  
  # Bimodal distribution
  bimodal_data = c(rnorm(50, mean = 20, sd = 5), rnorm(50, mean = 80, sd = 5)),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 4. International Clinical Data
# =============================================================================

# Generate international clinical data with different units and scales
outlierdetection_international <- data.frame(
  patient_id = 1:150,
  
  # European units
  weight_kg = c(rnorm(140, mean = 70, sd = 15), c(30, 35, 140, 150, 160, 170, 180, 190, 200, 210)),
  height_cm = c(rnorm(140, mean = 170, sd = 10), c(120, 125, 210, 215, 220, 225, 230, 235, 240, 245)),
  temperature_celsius = c(rnorm(145, mean = 37, sd = 0.5), c(32, 33, 42, 43, 44)),
  
  # US units (converted)
  weight_lbs = c(rnorm(140, mean = 154, sd = 33), c(66, 77, 308, 330, 352, 374, 396, 418, 440, 462)),
  height_inches = c(rnorm(140, mean = 67, sd = 4), c(47, 49, 83, 85, 87, 89, 91, 93, 95, 97)),
  temperature_fahrenheit = c(rnorm(145, mean = 98.6, sd = 0.9), c(90, 91, 108, 109, 110)),
  
  # Laboratory values (different reference ranges)
  glucose_mmol = c(rnorm(140, mean = 5.5, sd = 1.2), c(2, 2.5, 15, 20, 25, 30, 35, 40, 45, 50)),
  glucose_mg_dl = c(rnorm(140, mean = 99, sd = 22), c(36, 45, 270, 360, 450, 540, 630, 720, 810, 900)),
  
  # Medication dosages
  dosage_mg = c(rnorm(145, mean = 50, sd = 20), c(5, 10, 200, 300, 400)),
  
  # Scores and indices
  pain_scale = c(sample(1:8, 145, replace = TRUE), rep(c(0, 10), c(3, 2))),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 5. Large Dataset for Performance Testing
# =============================================================================

# Generate large dataset for performance testing
outlierdetection_large <- data.frame(
  patient_id = 1:2000,
  variable_1 = c(rnorm(1900, mean = 50, sd = 10), rep(c(10, 90), each = 50)),
  variable_2 = c(rnorm(1900, mean = 100, sd = 20), rep(c(20, 180), each = 50)),
  variable_3 = c(rnorm(1900, mean = 75, sd = 15), rep(c(25, 125), each = 50)),
  variable_4 = c(rnorm(1900, mean = 25, sd = 5), rep(c(5, 45), each = 50)),
  variable_5 = c(rnorm(1900, mean = 200, sd = 50), rep(c(50, 400), each = 50)),
  variable_6 = c(rnorm(1900, mean = 300, sd = 80), rep(c(80, 600), each = 50)),
  variable_7 = c(rnorm(1900, mean = 150, sd = 30), rep(c(60, 300), each = 50)),
  variable_8 = c(rnorm(1900, mean = 80, sd = 20), rep(c(20, 160), each = 50)),
  variable_9 = c(rnorm(1900, mean = 60, sd = 12), rep(c(30, 120), each = 50)),
  variable_10 = c(rnorm(1900, mean = 40, sd = 8), rep(c(15, 80), each = 50)),
  stringsAsFactors = FALSE
)

# =============================================================================
# 6. Clinical Laboratory Dataset
# =============================================================================

# Generate realistic clinical laboratory data with outliers
outlierdetection_clinical <- data.frame(
  patient_id = 1:300,
  
  # Complete Blood Count (CBC)
  hemoglobin = c(rnorm(280, mean = 14, sd = 2), rep(c(5, 6, 20, 22), each = 5)),
  hematocrit = c(rnorm(280, mean = 42, sd = 5), rep(c(15, 18, 65, 70), each = 5)),
  white_blood_cells = c(rnorm(285, mean = 7, sd = 2), rep(c(1, 2, 25, 30, 35), each = 3)),
  platelets = c(rnorm(285, mean = 250, sd = 50), rep(c(20, 30, 800, 900, 1000), each = 3)),
  
  # Chemistry Panel
  glucose = c(rnorm(285, mean = 95, sd = 15), rep(c(40, 45, 300, 400, 500), each = 3)),
  creatinine = c(rnorm(285, mean = 1.0, sd = 0.3), rep(c(0.2, 0.3, 5, 8, 10), each = 3)),
  bun = c(rnorm(285, mean = 15, sd = 5), rep(c(3, 5, 80, 100, 120), each = 3)),
  sodium = c(rnorm(285, mean = 140, sd = 3), rep(c(115, 120, 160, 165, 170), each = 3)),
  potassium = c(rnorm(285, mean = 4.0, sd = 0.5), rep(c(2.0, 2.5, 7.0, 7.5, 8.0), each = 3)),
  
  # Liver Function Tests
  alt = c(rnorm(285, mean = 25, sd = 10), rep(c(5, 8, 200, 300, 400), each = 3)),
  ast = c(rnorm(285, mean = 30, sd = 12), rep(c(8, 10, 250, 350, 450), each = 3)),
  bilirubin = c(rnorm(285, mean = 0.8, sd = 0.3), rep(c(0.1, 0.2, 15, 20, 25), each = 3)),
  
  # Cardiac markers
  troponin = c(rnorm(285, mean = 0.02, sd = 0.01), rep(c(0.001, 0.005, 5, 10, 15), each = 3)),
  ck_mb = c(rnorm(285, mean = 3, sd = 1), rep(c(0.5, 1, 50, 80, 100), each = 3)),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 7. Psychological/Behavioral Data
# =============================================================================

# Generate psychological assessment data with outliers
outlierdetection_psychological <- data.frame(
  patient_id = 1:200,
  
  # Likert scale data (1-7)
  anxiety_score = c(sample(2:6, 185, replace = TRUE), rep(c(1, 7), c(8, 7))),
  depression_score = c(sample(2:6, 185, replace = TRUE), rep(c(1, 7), c(8, 7))),
  stress_score = c(sample(2:6, 185, replace = TRUE), rep(c(1, 7), c(8, 7))),
  
  # Continuous psychological measures
  iq_score = c(rnorm(185, mean = 100, sd = 15), rep(c(50, 60, 160, 170, 180), each = 3)),
  reaction_time = c(rnorm(185, mean = 500, sd = 100), rep(c(200, 250, 1200, 1500, 2000), each = 3)),
  memory_score = c(rnorm(185, mean = 75, sd = 12), rep(c(30, 40, 98, 99, 100), each = 3)),
  
  # Behavioral measures
  sleep_hours = c(rnorm(185, mean = 7.5, sd = 1.2), rep(c(2, 3, 15, 18, 20), each = 3)),
  exercise_minutes = c(rnorm(185, mean = 45, sd = 20), rep(c(0, 5, 300, 400, 500), each = 3)),
  screen_time_hours = c(rnorm(185, mean = 6, sd = 2), rep(c(0, 1, 20, 24, 30), each = 3)),
  
  # Quality of life measures
  quality_of_life = c(rnorm(185, mean = 70, sd = 15), rep(c(10, 20, 98, 99, 100), each = 3)),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 8. Temporal/Longitudinal Data
# =============================================================================

# Generate longitudinal data with temporal outliers
outlierdetection_temporal <- data.frame(
  patient_id = rep(1:50, each = 4),  # 50 patients, 4 time points each
  time_point = rep(c("baseline", "3months", "6months", "12months"), times = 50),
  
  # Measurements that should be stable over time
  stable_measure = c(rnorm(180, mean = 50, sd = 5), rep(c(10, 20, 90, 100), each = 5)),
  
  # Measurements that should improve over time
  treatment_response = c(
    rep(c(30, 40, 50, 60), 45) + rnorm(180, 0, 5),  # Normal progression
    rep(c(30, 20, 10, 5), 5)  # Outlier: getting worse instead of better
  ),
  
  # Measurements with seasonal variation
  seasonal_measure = c(
    rep(c(40, 50, 60, 50), 45) + rnorm(180, 0, 8),  # Normal seasonal pattern
    rep(c(40, 90, 100, 20), 5)  # Outlier: extreme variation
  ),
  
  # Biomarker with expected decline
  biomarker_decline = c(
    rep(c(100, 90, 80, 70), 45) + rnorm(180, 0, 10),  # Normal decline
    rep(c(100, 120, 150, 200), 5)  # Outlier: increasing instead of decreasing
  ),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# 9. Problematic Data for Error Testing
# =============================================================================

# Generate problematic data to test error handling
outlierdetection_problematic <- data.frame(
  patient_id = 1:50,
  
  # All NA values
  all_na = rep(NA, 50),
  
  # Mostly NA values
  mostly_na = c(rnorm(5, mean = 50, sd = 10), rep(NA, 45)),
  
  # Infinite values
  infinite_values = c(rnorm(40, mean = 50, sd = 10), rep(c(Inf, -Inf), each = 5)),
  
  # Very large numbers
  very_large = c(rnorm(40, mean = 50, sd = 10), rep(c(1e10, 1e15), each = 5)),
  
  # Character data that should be numeric
  character_data = c(as.character(rnorm(45, mean = 50, sd = 10)), rep(c("invalid", "error"), c(3, 2))),
  
  # Mixed data types
  mixed_types = c(rnorm(40, mean = 50, sd = 10), rep(c("text", "123abc"), each = 5)),
  
  # Single unique value
  single_value = rep(42, 50),
  
  # Two unique values only
  two_values = sample(c(10, 90), 50, replace = TRUE),
  
  # Extreme outliers
  extreme_outliers = c(rnorm(45, mean = 50, sd = 10), c(1e6, -1e6, 1e8, -1e8, 1e10)),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# Save all datasets
# =============================================================================

# Save individual datasets
save(outlierdetection_basic, file = "data/outlierdetection_basic.rda")
save(outlierdetection_multivariate, file = "data/outlierdetection_multivariate.rda")
save(outlierdetection_edge_cases, file = "data/outlierdetection_edge_cases.rda")
save(outlierdetection_international, file = "data/outlierdetection_international.rda")
save(outlierdetection_large, file = "data/outlierdetection_large.rda")
save(outlierdetection_clinical, file = "data/outlierdetection_clinical.rda")
save(outlierdetection_psychological, file = "data/outlierdetection_psychological.rda")
save(outlierdetection_temporal, file = "data/outlierdetection_temporal.rda")
save(outlierdetection_problematic, file = "data/outlierdetection_problematic.rda")

# Save combined dataset
save(outlierdetection_basic, outlierdetection_multivariate, outlierdetection_edge_cases,
     outlierdetection_international, outlierdetection_large, outlierdetection_clinical,
     outlierdetection_psychological, outlierdetection_temporal, outlierdetection_problematic,
     file = "data/outlierdetection_test_data.rda")

# =============================================================================
# Data Summary
# =============================================================================

cat("OutlierDetection Test Data Generation Complete!\n")
cat("================================================\n")
cat("Generated datasets:\n")
cat("1. outlierdetection_basic: Basic univariate outliers (n=200)\n")
cat("2. outlierdetection_multivariate: Multivariate outliers (n=200)\n")
cat("3. outlierdetection_edge_cases: Edge cases and extreme values (n=100)\n")
cat("4. outlierdetection_international: International clinical data (n=150)\n")
cat("5. outlierdetection_large: Large dataset for performance (n=2000)\n")
cat("6. outlierdetection_clinical: Clinical laboratory data (n=300)\n")
cat("7. outlierdetection_psychological: Psychological assessment data (n=200)\n")
cat("8. outlierdetection_temporal: Longitudinal temporal data (n=200)\n")
cat("9. outlierdetection_problematic: Problematic data for error testing (n=50)\n")
cat("\nTotal observations across all datasets: 3,400\n")
cat("Test coverage includes:\n")
cat("- Univariate and multivariate outliers\n")
cat("- Edge cases (identical values, extreme variance)\n")
cat("- International data formats and units\n")
cat("- Large datasets for performance testing\n")
cat("- Clinical laboratory reference ranges\n")
cat("- Psychological and behavioral measures\n")
cat("- Temporal/longitudinal outlier patterns\n")
cat("- Problematic data for error handling validation\n")
cat("- Various data types and scales\n")
cat("- Missing data patterns\n")
cat("- Different outlier detection scenarios\n")