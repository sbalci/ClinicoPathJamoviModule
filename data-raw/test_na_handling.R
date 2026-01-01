# Test NA handling in advancedraincloud function
# This script demonstrates the enhanced NA handling capabilities

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(jmvcore)

# Create test data with NA values to simulate real-world missing data
set.seed(123)
test_data <- data.frame(
  id = rep(1:15, each = 2),
  timepoint = rep(c("Baseline", "Follow-up"), 15),
  score = c(rnorm(15, 50, 10), rnorm(15, 55, 12)),
  group = rep(c("Treatment", "Control"), each = 15),
  stringsAsFactors = FALSE
)

# Introduce NA values to test handling
test_data$id[c(3, 7, 12)] <- NA  # Missing IDs
test_data$score[c(5, 18, 23)] <- NA  # Missing scores
test_data$group[c(9)] <- NA  # Missing group

cat("Original test data dimensions:", nrow(test_data), "rows\n")
cat("NA values in ID:", sum(is.na(test_data$id)), "\n")
cat("NA values in score:", sum(is.na(test_data$score)), "\n")
cat("NA values in group:", sum(is.na(test_data$group)), "\n")

cat("\nTest data created successfully!\n")
cat("Use this data with advancedraincloud to test NA handling:\n")
cat("- Longitudinal connections with missing IDs\n")
cat("- Change analysis with missing values\n")
cat("- Complete case analysis reporting\n")

# Expected behavior:
# 1. Function should use complete.cases to remove rows with NAs
# 2. Should provide clear reporting of data exclusions
# 3. Should inform user about impact on longitudinal analysis
# 4. Should continue analysis with clean data
