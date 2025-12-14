# Generate example dataset for PCA Loading Significance Test
# This dataset demonstrates the permutation-based loading significance testing

# Set seed for reproducibility
set.seed(42)

# Sample size
n <- 100

# Create correlated variables with different patterns
# Scenario 1: Highly correlated variables (should load together on PC1)
var1 <- rnorm(n, mean = 50, sd = 10)
var2 <- var1 * 0.8 + rnorm(n, sd = 3)  # High correlation with var1
var3 <- var1 * 0.7 + rnorm(n, sd = 4)  # Moderate correlation with var1

# Scenario 2: Independent second dimension (should load on PC2)
var4 <- rnorm(n, mean = 100, sd = 15)
var5 <- var4 * 0.75 + rnorm(n, sd = 5)  # Correlated with var4

# Scenario 3: Weak third dimension (should load on PC3)
var6 <- rnorm(n, mean = 75, sd = 12)
var7 <- var6 * 0.6 + rnorm(n, sd = 8)

# Scenario 4: Noise variable (should NOT have significant loadings)
noise_var <- rnorm(n, mean = 0, sd = 1)

# Create data frame
pcaloadingtest_data <- data.frame(
  var1 = var1,
  var2 = var2,
  var3 = var3,
  var4 = var4,
  var5 = var5,
  var6 = var6,
  var7 = var7,
  noise = noise_var
)

# Add variable with spaces to test escapeVariableNames
pcaloadingtest_data[["Body Mass Index"]] <- rnorm(n, mean = 25, sd = 3)
pcaloadingtest_data[["Blood Pressure"]] <- rnorm(n, mean = 120, sd = 15)

# Add some missing values to test NA handling (5% missing)
missing_indices <- sample(1:n, size = round(n * 0.05))
pcaloadingtest_data$var1[missing_indices] <- NA

# Add metadata
attr(pcaloadingtest_data, "description") <- "Example dataset for PCA loading significance testing using permV method"
attr(pcaloadingtest_data, "source") <- "Simulated data with known correlation structure"
attr(pcaloadingtest_data, "notes") <- paste0(
  "Variables var1-var3 should load together on PC1. ",
  "Variables var4-var5 should load on PC2. ",
  "Variables var6-var7 should load on PC3. ",
  "'noise' variable should have no significant loadings. ",
  "Variables with spaces test escapeVariableNames handling."
)

# Save to data/
usethis::use_data(pcaloadingtest_data, overwrite = TRUE)

# Print summary
cat("\n=== PCA Loading Test Example Data Generated ===\n")
cat("Dimensions:", nrow(pcaloadingtest_data), "rows x", ncol(pcaloadingtest_data), "columns\n")
cat("Variables:\n")
print(names(pcaloadingtest_data))
cat("\nMissing values:", sum(is.na(pcaloadingtest_data)), "\n")
cat("\nCorrelation structure (first 8 vars):\n")
print(round(cor(pcaloadingtest_data[, 1:8], use = "pairwise.complete.obs"), 2))
cat("\nSaved to: data/pcaloadingtest_data.rda\n")
