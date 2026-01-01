# =============================================================================
# Bayesian Decision Curve Analysis Test Data Generation
# =============================================================================
# 
# Description: Generates synthetic medical diagnostic test data for evaluating
#              Bayesian decision curve analysis methods
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# Data includes:
# - True disease status (binary outcome)
# - Model prediction scores (continuous, 0-1)
# - Binary diagnostic test results
# - Weak binary test for comparison
# 
# =============================================================================

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

if (!require(here, quietly = TRUE)) {
  stop("Package 'here' is required but not installed. Please install it with: install.packages('here')")
}

# Set seed for reproducibility
set.seed(123)

# Parameters
N <- 500              # Sample size
prevalence <- 0.2     # Disease prevalence (20%)
model_auc <- 0.75     # Target AUC for model predictions

# Create output directory if it doesn't exist
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# Generate true disease status (prevalence = 0.2)
disease <- rbinom(N, 1, prevalence)

# Generate model prediction scores to achieve target AUC
# Using logistic transformation to ensure 0-1 range
model_score <- rnorm(N, mean = disease * 1, sd = 1)
model_prediction <- plogis(model_score)

# Generate binary test results with realistic performance characteristics
# Binary test: sensitivity = 75%, specificity = 80%
binary_test <- rep(0, N)
binary_test[disease == 1] <- rbinom(sum(disease), 1, 0.75)        # 75% sensitivity
binary_test[disease == 0] <- rbinom(sum(disease == 0), 1, 0.20)   # 20% false positive rate (80% specificity)

# Generate weak binary test for comparison
# Weak test: sensitivity = 60%, specificity = 70%
weak_test <- rep(0, N)
weak_test[disease == 1] <- rbinom(sum(disease), 1, 0.60)          # 60% sensitivity
weak_test[disease == 0] <- rbinom(sum(disease == 0), 1, 0.30)     # 30% false positive rate (70% specificity)

# Create final dataset with proper factor labels
bayesdca_test_data <- data.frame(
    outcome = factor(disease, levels = c(0, 1), labels = c("No Disease", "Disease")),
    model_prediction = round(model_prediction, 4),
    binary_test = factor(binary_test, levels = c(0, 1), labels = c("Negative", "Positive")),
    weak_test = factor(weak_test, levels = c(0, 1), labels = c("Negative", "Positive"))
)

# Add case IDs
bayesdca_test_data$case_id <- paste0("Case_", sprintf("%03d", 1:N))

# Reorder columns for better presentation
bayesdca_test_data <- bayesdca_test_data[, c("case_id", "outcome", "model_prediction", "binary_test", "weak_test")]

# Print summary statistics
cat("\n=== Bayesian DCA Test Data Summary ===\n")
cat("Total cases:", N, "\n")
cat("Disease prevalence:", round(mean(disease) * 100, 1), "%\n")
cat("Model prediction range:", round(range(bayesdca_test_data$model_prediction), 3), "\n")

# Performance summary
cat("\nTest Performance Summary:\n")
cat("Binary test sensitivity:", round(mean(binary_test[disease == 1]) * 100, 1), "%\n")
cat("Binary test specificity:", round(mean(1 - binary_test[disease == 0]) * 100, 1), "%\n")
cat("Weak test sensitivity:", round(mean(weak_test[disease == 1]) * 100, 1), "%\n")
cat("Weak test specificity:", round(mean(1 - weak_test[disease == 0]) * 100, 1), "%\n")

# Save the data in multiple formats
output_csv <- file.path(data_dir, "bayesdca_test_data.csv")
output_rda <- file.path(data_dir, "bayesdca_test_data.rda")

# Save as CSV
write.csv(bayesdca_test_data, output_csv, row.names = FALSE)
cat("Saved CSV file:", output_csv, "\n")

# Save as RDA
save(bayesdca_test_data, file = output_rda)
cat("Saved RDA file:", output_rda, "\n")

# Print first few rows for verification
cat("\nFirst 6 rows of generated data:\n")
print(head(bayesdca_test_data))

cat("\n=== Data generation completed successfully ===\n")
