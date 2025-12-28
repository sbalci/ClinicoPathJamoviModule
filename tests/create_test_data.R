# Script to Create Test Data for oddsratio Function
# Run this in jamovi using the Rj module or in R console

# Create comprehensive test dataset
set.seed(123)
n <- 250

# Binary outcome with meaningful labels
mortality <- factor(
    sample(c("Alive", "Dead"), n, replace = TRUE, prob = c(0.65, 0.35)),
    levels = c("Alive", "Dead")
)

# Binary predictors
sex <- factor(
    sample(c("Male", "Female"), n, replace = TRUE),
    levels = c("Male", "Female")
)

lvi <- factor(
    sample(c("Absent", "Present"), n, replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("Absent", "Present")
)

pni <- factor(
    sample(c("Absent", "Present"), n, replace = TRUE, prob = c(0.7, 0.3)),
    levels = c("Absent", "Present")
)

# Continuous predictors
age <- round(rnorm(n, mean = 55, sd = 13))
age <- pmax(25, pmin(85, age))  # Constrain to realistic range

tumor_size <- round(rnorm(n, mean = 3.5, sd = 1.8), 1)
tumor_size <- pmax(0.5, pmin(10, tumor_size))

# Categorical with >2 levels
tumor_grade <- factor(
    sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    levels = c("Grade 1", "Grade 2", "Grade 3")
)

# Create dataset
test_data <- data.frame(
    PatientID = 1:n,
    Mortality5yr = mortality,
    Sex = sex,
    Age = age,
    LVI = lvi,
    PNI = pni,
    TumorSize = tumor_size,
    TumorGrade = tumor_grade
)

# Add some missing data to test robustness (5%)
missing_indices <- sample(1:n, round(n * 0.05))
test_data$LVI[missing_indices] <- NA

# Summary
cat("Test dataset created!\n")
cat("Dimensions:", nrow(test_data), "rows ×", ncol(test_data), "columns\n\n")

cat("Variable summary:\n")
cat("================\n")
cat("Binary outcome:\n")
print(table(test_data$Mortality5yr, useNA = "ifany"))

cat("\nBinary predictors:\n")
cat("Sex:\n")
print(table(test_data$Sex, useNA = "ifany"))
cat("LVI:\n")
print(table(test_data$LVI, useNA = "ifany"))
cat("PNI:\n")
print(table(test_data$PNI, useNA = "ifany"))

cat("\nContinuous predictors:\n")
cat("Age: mean =", round(mean(test_data$Age), 1), ", SD =", round(sd(test_data$Age), 1), "\n")
cat("TumorSize: mean =", round(mean(test_data$TumorSize), 1), ", SD =", round(sd(test_data$TumorSize), 1), "\n")

cat("\nCategorical predictor:\n")
cat("TumorGrade:\n")
print(table(test_data$TumorGrade, useNA = "ifany"))

# Save to CSV for jamovi
write.csv(test_data, "oddsratio_test_data.csv", row.names = FALSE)
cat("\n✓ Data saved to: oddsratio_test_data.csv\n")
cat("  Load this file in jamovi to run tests\n")

# Display first few rows
cat("\nFirst 10 rows:\n")
print(head(test_data, 10))
