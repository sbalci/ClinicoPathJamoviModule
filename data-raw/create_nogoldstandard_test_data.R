# Create test data for nogoldstandard function
# This script generates synthetic diagnostic test data without a gold standard

set.seed(42)  # For reproducibility

# Number of patients
n <- 300

# True disease status (unknown in practice, but we create it for simulation)
# 30% disease prevalence
true_disease <- rbinom(n, 1, 0.30)

# Create 5 diagnostic tests with different characteristics
# Test 1: High sensitivity (90%), moderate specificity (85%)
test1 <- ifelse(true_disease == 1,
                rbinom(n, 1, 0.90),  # Sensitivity
                rbinom(n, 1, 0.15))  # 1 - Specificity

# Test 2: Moderate sensitivity (75%), high specificity (95%)
test2 <- ifelse(true_disease == 1,
                rbinom(n, 1, 0.75),  # Sensitivity
                rbinom(n, 1, 0.05))  # 1 - Specificity

# Test 3: Balanced test (85% sensitivity, 85% specificity)
test3 <- ifelse(true_disease == 1,
                rbinom(n, 1, 0.85),  # Sensitivity
                rbinom(n, 1, 0.15))  # 1 - Specificity

# Test 4: Low sensitivity (70%), very high specificity (98%)
test4 <- ifelse(true_disease == 1,
                rbinom(n, 1, 0.70),  # Sensitivity
                rbinom(n, 1, 0.02))  # 1 - Specificity

# Test 5: High sensitivity (95%), low specificity (70%)
test5 <- ifelse(true_disease == 1,
                rbinom(n, 1, 0.95),  # Sensitivity
                rbinom(n, 1, 0.30))  # 1 - Specificity

# Convert to factor format with meaningful labels
test1 <- factor(ifelse(test1 == 1, "positive", "negative"), 
                levels = c("negative", "positive"))
test2 <- factor(ifelse(test2 == 1, "positive", "negative"), 
                levels = c("negative", "positive"))
test3 <- factor(ifelse(test3 == 1, "positive", "negative"), 
                levels = c("negative", "positive"))
test4 <- factor(ifelse(test4 == 1, "positive", "negative"), 
                levels = c("negative", "positive"))
test5 <- factor(ifelse(test5 == 1, "positive", "negative"), 
                levels = c("negative", "positive"))

# Create data frame
nogoldstandard_test_data <- data.frame(
  patient_id = 1:n,
  test1_result = test1,
  test2_result = test2,
  test3_result = test3,
  test4_result = test4,
  test5_result = test5,
  age = round(rnorm(n, 50, 15)),  # Age with mean 50, SD 15
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  stringsAsFactors = FALSE
)

# Add some missing values to simulate real-world data
# About 5% missing for each test
missing_indices <- function(n, prop = 0.05) {
  sample(1:n, size = floor(n * prop), replace = FALSE)
}

nogoldstandard_test_data$test3_result[missing_indices(n)] <- NA
nogoldstandard_test_data$test4_result[missing_indices(n)] <- NA
nogoldstandard_test_data$test5_result[missing_indices(n)] <- NA

# Create a smaller dataset for quick testing
nogoldstandard_test_data_small <- nogoldstandard_test_data[1:50, ]

# Save the datasets
usethis::use_data(nogoldstandard_test_data, overwrite = TRUE)
usethis::use_data(nogoldstandard_test_data_small, overwrite = TRUE)

# Print summary
cat("Created nogoldstandard_test_data with", nrow(nogoldstandard_test_data), "patients\n")
cat("True disease prevalence:", mean(true_disease), "\n")
cat("\nTest results summary:\n")
for (i in 1:5) {
  test_col <- paste0("test", i, "_result")
  if (test_col %in% names(nogoldstandard_test_data)) {
    cat(sprintf("Test %d: %.1f%% positive\n", 
                i, 
                100 * mean(nogoldstandard_test_data[[test_col]] == "positive", na.rm = TRUE)))
  }
}

# Calculate agreement between tests
cat("\nPairwise agreement between tests:\n")
for (i in 1:4) {
  for (j in (i+1):5) {
    test1_col <- paste0("test", i, "_result")
    test2_col <- paste0("test", j, "_result")
    agreement <- mean(nogoldstandard_test_data[[test1_col]] == 
                     nogoldstandard_test_data[[test2_col]], na.rm = TRUE)
    cat(sprintf("Test %d vs Test %d: %.1f%%\n", i, j, 100 * agreement))
  }
}