# =============================================================================
# Test Data Generation for jpps Function (Predictive Power Score Analysis)
# =============================================================================
# This script generates comprehensive test datasets for the jpps function,
# covering various scenarios and data types to ensure robust testing.

library(tibble)
library(dplyr)

# Set seed for reproducibility
set.seed(20250707)

# =============================================================================
# Dataset 1: Linear Relationships - Simple Case
# =============================================================================
# Perfect linear relationship for basic PPS testing
jpps_linear_data <- tibble(
  x = 1:50,
  y_perfect = 2 * x + 1,                    # Perfect linear (PPS ≈ 1.0)
  y_strong = 2 * x + 1 + rnorm(50, 0, 2),   # Strong linear (PPS ≈ 0.8-0.9)
  y_moderate = 2 * x + 1 + rnorm(50, 0, 10), # Moderate linear (PPS ≈ 0.4-0.6)
  y_weak = 2 * x + 1 + rnorm(50, 0, 20),     # Weak linear (PPS ≈ 0.1-0.3)
  y_noise = rnorm(50, 50, 15)                # No relationship (PPS ≈ 0.0)
)

# =============================================================================
# Dataset 2: Non-linear Relationships - Complex Case  
# =============================================================================
# Non-linear relationships where PPS should outperform correlation
jpps_nonlinear_data <- tibble(
  x = seq(-3, 3, length.out = 60),
  y_quadratic = x^2 + rnorm(60, 0, 0.5),       # Quadratic relationship
  y_cubic = x^3 + rnorm(60, 0, 2),             # Cubic relationship
  y_exponential = exp(x/2) + rnorm(60, 0, 1),  # Exponential relationship
  y_sine = 3 * sin(2 * x) + rnorm(60, 0, 0.3), # Sinusoidal relationship
  y_step = ifelse(x < 0, 1, ifelse(x < 1, 3, 5)) + rnorm(60, 0, 0.2), # Step function
  y_absolute = abs(x) + rnorm(60, 0, 0.3)       # Absolute value relationship
)

# =============================================================================
# Dataset 3: Mixed Data Types - Categorical and Numerical
# =============================================================================
# Mixed categorical and numerical variables for comprehensive PPS testing
jpps_mixed_data <- tibble(
  # Categorical variables
  category_ordered = factor(sample(c("Low", "Medium", "High"), 80, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
                           levels = c("Low", "Medium", "High"), ordered = TRUE),
  category_nominal = factor(sample(c("Red", "Blue", "Green", "Yellow"), 80, replace = TRUE)),
  binary_var = factor(sample(c("Yes", "No"), 80, replace = TRUE)),
  
  # Numerical variables influenced by categorical
  score = case_when(
    category_ordered == "Low" ~ rnorm(sum(category_ordered == "Low"), 30, 8),
    category_ordered == "Medium" ~ rnorm(sum(category_ordered == "Medium"), 50, 10),
    category_ordered == "High" ~ rnorm(sum(category_ordered == "High"), 70, 12)
  ),
  
  # Continuous variables
  age = round(runif(80, 18, 65)),
  income = exp(rnorm(80, 10, 0.5)),  # Log-normal distribution
  
  # Variables with missing values
  incomplete_var = ifelse(runif(80) < 0.15, NA, rnorm(80, 100, 20))
) %>%
  rowwise() %>%
  mutate(
    # Target influenced by multiple variables
    target = case_when(
      binary_var == "Yes" ~ score * 1.2 + age * 0.5 + rnorm(1, 0, 10),
      binary_var == "No" ~ score * 0.8 + age * 0.3 + rnorm(1, 0, 15)
    )
  ) %>%
  ungroup()

# =============================================================================
# Dataset 4: Correlation vs PPS Comparison Data
# =============================================================================
# Designed to show where PPS detects relationships that correlation misses
jpps_comparison_data <- tibble(
  x = runif(100, -5, 5),
  
  # High correlation, high PPS (linear)
  linear_strong = 0.9 * x + rnorm(100, 0, 0.5),
  
  # Low correlation, high PPS (non-linear)
  u_shaped = x^2 + rnorm(100, 0, 1),
  threshold = ifelse(x > 0, x^1.5, 0) + rnorm(100, 0, 0.5),
  
  # High correlation, medium PPS (linear with outliers)
  linear_outliers = 0.8 * x + ifelse(runif(100) < 0.05, rnorm(5, 0, 10), rnorm(95, 0, 1)),
  
  # Low correlation, low PPS (noise)
  random_noise = rnorm(100, 0, 3),
  
  # Categorical with clear pattern
  category_clear = factor(ifelse(x < -2, "A", ifelse(x < 2, "B", "C")))
)

# =============================================================================
# Dataset 5: Time Series / Sequential Data
# =============================================================================
# Sequential data with trends and patterns
jpps_timeseries_data <- tibble(
  time_index = 1:120,
  
  # Linear trend
  trend = 0.5 * time_index + rnorm(120, 0, 5),
  
  # Seasonal pattern
  seasonal = 10 * sin(2 * pi * time_index / 12) + rnorm(120, 0, 2),
  
  # Autoregressive pattern
  ar_component = NA
)

# Generate AR(1) process
jpps_timeseries_data$ar_component[1] <- rnorm(1, 0, 1)
for(i in 2:120) {
  jpps_timeseries_data$ar_component[i] <- 0.7 * jpps_timeseries_data$ar_component[i-1] + rnorm(1, 0, 1)
}

# Add combined target
jpps_timeseries_data <- jpps_timeseries_data %>%
  mutate(
    combined_target = trend + seasonal + 0.3 * ar_component + rnorm(120, 0, 3),
    lagged_target = lag(combined_target, 1),
    moving_avg = rollmean(combined_target, k = 5, fill = NA, align = "right")
  )

# =============================================================================
# Dataset 6: High-Dimensional Data for Matrix Analysis
# =============================================================================
# Multiple variables for comprehensive matrix PPS analysis
jpps_matrix_data <- tibble(
  # Economic indicators
  gdp_growth = rnorm(75, 2.5, 1.2),
  inflation = rnorm(75, 3.0, 1.5),
  unemployment = rnorm(75, 5.5, 2.0),
  interest_rate = rnorm(75, 4.0, 1.8),
  
  # Market indicators  
  stock_index = rnorm(75, 1000, 200),
  volatility = abs(rnorm(75, 15, 5)),
  volume = exp(rnorm(75, 8, 0.8)),
  
  # Create some relationships between variables
  bond_yield = 0.7 * interest_rate + 0.3 * inflation + rnorm(75, 0, 0.5),
  exchange_rate = -0.4 * gdp_growth + 0.6 * inflation + rnorm(75, 0, 0.8),
  consumer_confidence = -2 * unemployment + 0.5 * gdp_growth + rnorm(75, 50, 10)
)

# =============================================================================
# Dataset 7: Clinical/Medical Research Data
# =============================================================================
# Medical research context with typical clinical variables
jpps_clinical_data <- tibble(
  patient_id = 1:150,
  age = round(rnorm(150, 45, 15)),
  gender = factor(sample(c("Male", "Female"), 150, replace = TRUE)),
  bmi = rnorm(150, 26, 4),
  
  # Risk factors
  smoking = factor(sample(c("Never", "Former", "Current"), 150, replace = TRUE, prob = c(0.4, 0.35, 0.25))),
  alcohol_units = pmax(0, rnorm(150, 8, 6)),
  exercise_hours = pmax(0, rnorm(150, 3, 2)),
  
  # Clinical measurements
  systolic_bp = rnorm(150, 130, 20),
  diastolic_bp = rnorm(150, 80, 12),
  cholesterol = rnorm(150, 5.2, 1.1),
  glucose = rnorm(150, 5.5, 1.5),
  
  # Outcome variables
  cvd_risk = case_when(
    age > 55 & smoking == "Current" & systolic_bp > 140 ~ rbinom(sum(age > 55 & smoking == "Current" & systolic_bp > 140), 1, 0.7),
    age > 55 & (smoking == "Current" | systolic_bp > 140) ~ rbinom(sum(age > 55 & (smoking == "Current" | systolic_bp > 140)), 1, 0.4),
    age > 55 ~ rbinom(sum(age > 55), 1, 0.2),
    TRUE ~ rbinom(length(age[!(age > 55)]), 1, 0.05)
  ),
  
  # Continuous outcome
  health_score = 100 - 0.5 * age - 5 * (smoking == "Current") - 0.1 * systolic_bp + 2 * exercise_hours + rnorm(150, 0, 10)
) %>%
  mutate(
    health_score = pmax(0, pmin(100, health_score)),  # Bound between 0-100
    cvd_risk = factor(cvd_risk, labels = c("Low", "High"))
  )

# =============================================================================
# Dataset 8: Edge Cases and Stress Testing
# =============================================================================
# Special cases for testing robustness
jpps_edge_cases <- tibble(
  # Constant variables
  constant_var = rep(42, 50),
  
  # Nearly constant with tiny variation
  near_constant = 100 + rnorm(50, 0, 0.001),
  
  # Extreme outliers
  with_outliers = c(rnorm(45, 10, 2), rep(1000, 5)),
  
  # Very small values
  tiny_values = abs(rnorm(50, 0, 0.0001)),
  
  # Very large values
  huge_values = rnorm(50, 1e6, 1e5),
  
  # All missing except few
  mostly_missing = c(rnorm(5, 50, 10), rep(NA, 45)),
  
  # Binary outcomes
  binary_outcome = factor(sample(c(0, 1), 50, replace = TRUE)),
  
  # Target with known relationship to constant (should be PPS = 0)
  target_vs_constant = rnorm(50, 25, 5)
)

# =============================================================================
# Save all datasets
# =============================================================================

# Save individual datasets
save(jpps_linear_data, file = "data/jpps_linear_data.rda", compress = "xz")
save(jpps_nonlinear_data, file = "data/jpps_nonlinear_data.rda", compress = "xz")
save(jpps_mixed_data, file = "data/jpps_mixed_data.rda", compress = "xz")
save(jpps_comparison_data, file = "data/jpps_comparison_data.rda", compress = "xz")
save(jpps_timeseries_data, file = "data/jpps_timeseries_data.rda", compress = "xz")
save(jpps_matrix_data, file = "data/jpps_matrix_data.rda", compress = "xz")
save(jpps_clinical_data, file = "data/jpps_clinical_data.rda", compress = "xz")
save(jpps_edge_cases, file = "data/jpps_edge_cases.rda", compress = "xz")

# =============================================================================
# Generate Documentation
# =============================================================================

jpps_test_datasets_info <- list(
  jpps_linear_data = list(
    description = "Linear relationships of varying strength for basic PPS testing",
    variables = c("x", "y_perfect", "y_strong", "y_moderate", "y_weak", "y_noise"),
    use_case = "Testing PPS detection of linear relationships vs noise",
    n_obs = 50
  ),
  
  jpps_nonlinear_data = list(
    description = "Non-linear relationships where PPS should outperform correlation",
    variables = c("x", "y_quadratic", "y_cubic", "y_exponential", "y_sine", "y_step", "y_absolute"),
    use_case = "Demonstrating PPS advantages over correlation for non-linear patterns",
    n_obs = 60
  ),
  
  jpps_mixed_data = list(
    description = "Mixed categorical and numerical variables for comprehensive testing",
    variables = c("category_ordered", "category_nominal", "binary_var", "score", "age", "income", "incomplete_var", "target"),
    use_case = "Testing PPS with mixed data types and missing values",
    n_obs = 80
  ),
  
  jpps_comparison_data = list(
    description = "Data designed to show PPS vs correlation differences",
    variables = c("x", "linear_strong", "u_shaped", "threshold", "linear_outliers", "random_noise", "category_clear"),
    use_case = "Comparing PPS and correlation performance",
    n_obs = 100
  ),
  
  jpps_timeseries_data = list(
    description = "Sequential data with trends, seasonality, and autoregression",
    variables = c("time_index", "trend", "seasonal", "ar_component", "combined_target", "lagged_target", "moving_avg"),
    use_case = "Testing PPS with time series patterns",
    n_obs = 120
  ),
  
  jpps_matrix_data = list(
    description = "High-dimensional economic/financial data for matrix analysis",
    variables = c("gdp_growth", "inflation", "unemployment", "interest_rate", "stock_index", "volatility", "volume", "bond_yield", "exchange_rate", "consumer_confidence"),
    use_case = "Large matrix PPS analysis with realistic variable relationships",
    n_obs = 75
  ),
  
  jpps_clinical_data = list(
    description = "Clinical research data with patient characteristics and outcomes",
    variables = c("patient_id", "age", "gender", "bmi", "smoking", "alcohol_units", "exercise_hours", "systolic_bp", "diastolic_bp", "cholesterol", "glucose", "cvd_risk", "health_score"),
    use_case = "Medical research context with typical clinical variables",
    n_obs = 150
  ),
  
  jpps_edge_cases = list(
    description = "Edge cases and stress testing scenarios",
    variables = c("constant_var", "near_constant", "with_outliers", "tiny_values", "huge_values", "mostly_missing", "binary_outcome", "target_vs_constant"),
    use_case = "Testing robustness and edge case handling",
    n_obs = 50
  )
)

save(jpps_test_datasets_info, file = "data/jpps_test_datasets_info.rda", compress = "xz")

# =============================================================================
# Print Summary
# =============================================================================
cat("Generated 8 test datasets for jpps function:\n")
cat("=====================================\n\n")

for(dataset_name in names(jpps_test_datasets_info)) {
  info <- jpps_test_datasets_info[[dataset_name]]
  cat(sprintf("📊 %s (%d observations)\n", dataset_name, info$n_obs))
  cat(sprintf("   Purpose: %s\n", info$description))
  cat(sprintf("   Use case: %s\n", info$use_case))
  cat(sprintf("   Variables: %s\n\n", paste(info$variables, collapse = ", ")))
}

cat("All datasets saved to data/ directory with documentation.\n")
cat("Ready for comprehensive jpps function testing! 🚀\n")