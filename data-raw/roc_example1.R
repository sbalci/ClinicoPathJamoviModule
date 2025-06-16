# =============================================================================
# ROC Analysis Example Data Generation
# =============================================================================
# 
# Description: Generates example data for ROC analysis demonstrating basic
#              ROC curve analysis and diagnostic test evaluation
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# Data includes:
# - Binary outcome (Healthy/Ill)
# - Two continuous test measurements
# - ROC analysis with multiple metrics
# 
# =============================================================================

# Load required libraries with error checking
required_packages <- c("here", "pROC", "ggplot2", "dplyr", "ROCR")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed. Please install it with: install.packages('", pkg, "')", sep = ""))
  }
}

# Set seed for reproducibility
set.seed(123)

# Create output directory if it doesn't exist
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# =============================================================================
# Generate Example ROC Data
# =============================================================================

# Create example data with binary outcome
n <- 100
test_values <- c(rnorm(n/2, mean = 10, sd = 2), rnorm(n/2, mean = 15, sd = 3))
true_status <- factor(rep(c("Healthy", "Ill"), each = n/2))

roc_example_data <- data.frame(
  ID = 1:n,
  Disease_Status = true_status,
  Test_M1 = test_values
)

# Add correlated test measurement with some noise
roc_example_data$Test_M2 <- roc_example_data$Test_M1 + rnorm(n, mean = 0, sd = 2)

# =============================================================================
# Basic ROC Analysis
# =============================================================================

# Convert to numeric for ROC analysis
response <- as.numeric(roc_example_data$Disease_Status == "Ill")

# Calculate ROC curve
basic_roc <- pROC::roc(
  response = response,
  predictor = roc_example_data$Test_M2,
  direction = ">",
  levels = c(0, 1)
)

# Extract key statistics
auc <- basic_roc$auc
auc_ci <- ci.auc(basic_roc)
coords <- coords(basic_roc, "best", ret = c("threshold", "sensitivity", "specificity"))

# Calculate additional diagnostic metrics
prevalence <- mean(response)
ppv <- (coords["sensitivity"] * prevalence) /
       ((coords["sensitivity"] * prevalence) + ((1 - coords["specificity"]) * (1 - prevalence)))
npv <- (coords["specificity"] * (1 - prevalence)) /
       ((coords["specificity"] * (1 - prevalence)) + ((1 - coords["sensitivity"]) * prevalence))
lrp <- coords["sensitivity"] / (1 - coords["specificity"])
lrn <- (1 - coords["sensitivity"]) / coords["specificity"]

# =============================================================================
# Save Data and Results
# =============================================================================

# Save example data
output_csv <- file.path(data_dir, "roc_example_data.csv")
output_rda <- file.path(data_dir, "roc_example_data.rda")

write.csv(roc_example_data, output_csv, row.names = FALSE)
save(roc_example_data, file = output_rda)

# =============================================================================
# Print Analysis Results
# =============================================================================

cat("\n=== ROC Analysis Example Results ===\n")
cat("Dataset size:", n, "cases\n")
cat("Disease prevalence:", round(prevalence * 100, 1), "%\n\n")

cat("ROC Analysis Results:\n")
cat("AUC:", round(auc, 3), "\n")
cat("95% CI:", round(auc_ci[1], 3), "-", round(auc_ci[3], 3), "\n")
cat("Optimal threshold (Youden index):", round(coords["threshold"], 2), "\n")
cat("Sensitivity at optimal threshold:", round(coords["sensitivity"], 3), "\n")
cat("Specificity at optimal threshold:", round(coords["specificity"], 3), "\n\n")

cat("Additional Diagnostic Metrics:\n")
cat("PPV (Positive Predictive Value):", round(ppv, 3), "\n")
cat("NPV (Negative Predictive Value):", round(npv, 3), "\n")
cat("LR+ (Positive Likelihood Ratio):", round(lrp, 2), "\n")
cat("LR- (Negative Likelihood Ratio):", round(lrn, 3), "\n")

cat("\nData files saved:\n")
cat("- CSV file:", output_csv, "\n")
cat("- RDA file:", output_rda, "\n")

cat("\n=== ROC example data generation completed successfully ===\n")