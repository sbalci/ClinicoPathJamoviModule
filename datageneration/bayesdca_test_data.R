# Create example dataset
set.seed(123)
N <- 500

# Generate true disease status (prevalence = 0.2)
disease <- rbinom(N, 1, 0.2)

# Generate model prediction (AUC ~ 0.75)
model_score <- rnorm(N, mean = disease*1, sd = 1)
model_prediction <- plogis(model_score)  # Convert to probability

# Generate binary test (sens = 0.75, spec = 0.80)
binary_test <- rep(0, N)
binary_test[disease == 1] <- rbinom(sum(disease), 1, 0.75)  # 75% sensitivity
binary_test[disease == 0] <- rbinom(sum(disease == 0), 1, 0.20)  # 80% specificity (20% false positives)

# Generate weak binary test (sens = 0.60, spec = 0.70)
weak_test <- rep(0, N)
weak_test[disease == 1] <- rbinom(sum(disease), 1, 0.60)  # 60% sensitivity
weak_test[disease == 0] <- rbinom(sum(disease == 0), 1, 0.30)  # 70% specificity (30% false positives)

# Create final dataset
test_data <- data.frame(
    outcome = factor(disease, levels = c(0, 1), labels = c("No Disease", "Disease")),
    model_prediction = model_prediction,
    binary_test = binary_test,
    weak_test = weak_test
)

# Save the data
write.csv(test_data, "./data/bayesdca_test_data.csv", row.names = FALSE)
